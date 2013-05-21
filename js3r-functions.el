(require 'cl)
(require 'dash)
(require 'yasnippet)

;; Turn parameter into local var in local function

(defun js3r-localize-parameter ()
  (interactive)
  (js3r--guard)
  (if (js3-name-node-p (js3-node-at-point))
      (js3r--localize-parameter-pull)
    (js3r--localize-parameter-push)))

(defun js3r--localize-parameter-push ()
  (let* ((node (js3-node-at-point))
         (arg-node (or (js3r--closest-node-where 'js3r--parent-is-call-node node)
                       (error "Place cursor on argument to localize.")))
         (call-node (js3-node-parent arg-node))
         (value (js3-node-string arg-node))
         (target (js3-call-node-target call-node))
         (fn (if (js3-name-node-p target)
                 (js3r--local-fn-from-name-node target)
               (error "Can only localize parameter for local functions.")))
         (usages (js3r--function-usages fn))
         (index (car (--keep (when (eq arg-node it) it-index)
                             (js3-call-node-args call-node))))
         (name (js3-name-node-name (nth index (js3-function-node-params fn)))))
    (js3r--localize-parameter fn usages index name value)))

(defun js3r--localize-parameter-pull ()
  (let* ((name-node (js3-node-at-point))
         (name (if (js3-name-node-p name-node)
                   (js3-name-node-name name-node)
                 (error "Place cursor on parameter to localize.")))
         (fn (or (js3r--closest-node-where 'js3r--is-local-function name-node)
                 (error "Can only localize parameter in local functions.")))
         (index (or (js3r--param-index-for name fn)
                    (error "%S isn't a parameter to this function." name)))
         (usages (js3r--function-usages fn))
         (examples (-distinct (--map (js3r--argument index it) usages)))
         (value (js3r--choose-one "Value: " examples)))
    (js3r--localize-parameter fn usages index name value)))

(defun js3r--localize-parameter (fn usages index name value)
  (save-excursion
    (js3r--goto-fn-body-beg fn)
    (save-excursion
      (--each usages (js3r--remove-argument-at-index index it)))
    (newline-and-indent)
    (insert "var " name " = " value ";")
    (js3r--remove-parameter-at-index index fn)))

(defun js3r--parent-is-call-node (node)
  (js3-call-node-p (js3-node-parent node)))

(defun js3r--local-fn-from-name-node (name-node)
  (->> name-node
    (js3r--local-usages-of-name-node)
    (-map 'js3-node-parent)
    (-first 'js3-function-node-p)))

(defun js3r--param-index-for (name fn)
  (car (--keep (when (equal name (js3-name-node-name it)) it-index)
               (js3-function-node-params fn))))

(defun js3r--argument (index call-node)
  (js3-node-string (nth index (js3-call-node-args call-node))))

(defun js3r--remove-parameter-at-index (index fn)
  (js3r--delete-node-in-params (nth index (js3-function-node-params fn))))

(defun js3r--remove-argument-at-index (index call-node)
  (js3r--delete-node-in-params (nth index (js3-call-node-args call-node))))

(defun js3r--delete-node-in-params (node)
  (goto-char (js3-node-abs-pos node))
  (delete-char (js3-node-len node))
  (if (and (looking-back "(")
           (looking-at ", "))
      (delete-char 2)
    (when (looking-back ", ")
      (delete-char -2))))

(defun js3r--choose-one (prompt options)
  (when examples
    (if (cdr examples)
        (completing-read prompt examples)
      (car examples))))

;; Introduce parameter in local function

(defun js3r-introduce-parameter ()
  (interactive)
  (js3r--guard)
  (if (use-region-p)
      (js3r--introduce-parameter-between (region-beginning) (region-end))
    (let ((node (js3r--closest 'js3r--expression-p)))
      (js3r--introduce-parameter-between (js3-node-abs-pos node)
                                         (js3-node-abs-end node)))))

(defun js3r--introduce-parameter-between (beg end)
  (unless (js3r--single-complete-expression-between-p beg end)
    (error "Can only introduce single, complete expressions as parameter."))

  (let ((fn (js3r--closest-node-where 'js3r--is-local-function (js3-node-at-point))))
    (unless fn
      (error "Can only introduce parameter in local functions."))
    (save-excursion
      (let ((name (read-string "Parameter name: "))
            (val (buffer-substring beg end))
            (usages (js3r--function-usages fn)))
        (goto-char beg)
        (save-excursion
          (-each usages (-partial 'js3r--add-parameter val)))
        (delete-char (- end beg))
        (insert name)
        (js3r--add-parameter name fn)
        (query-replace val name nil (js3-node-abs-pos fn) (js3r--fn-body-end fn))))))

(defun js3r--function-usages (fn)
  (-map 'js3-node-parent (js3r--function-usages-name-nodes fn)))

(defun js3r--function-usages-name-nodes (fn)
  (let ((name-node (or (js3-function-node-name fn)
                       (js3-var-init-node-target (js3-node-parent fn)))))
    (remove name-node (js3r--local-usages-of-name-node name-node))))

(defun js3r--add-parameter (name node)
  (save-excursion
    (js3r--goto-closing-paren node)
    (unless (looking-back "(")
      (insert ", "))
    (insert name)))

(defun js3r--goto-closing-paren (node)
  (goto-char (js3-node-abs-pos node))
  (search-forward "(")
  (forward-char -1)
  (forward-list)
  (forward-char -1))

(defun js3r--goto-fn-body-beg (fn)
  (goto-char (js3-node-abs-pos fn))
  (search-forward "{"))

(defun js3r--fn-body-end (fn)
  (save-excursion
    (js3r--goto-fn-body-beg fn)
    (forward-char -1)
    (forward-list)
    (point)))

(defun js3r--is-local-function (node)
  (or (js3r--is-var-function-expression node)
      (js3r--is-function-declaration node)))

(defun js3r--is-method (node)
  (and (js3-function-node-p node)
       (js3-object-prop-node-p (js3-node-parent node))))

(defun js3r--is-var-function-expression (node)
  (and (js3-function-node-p node)
       (js3-var-init-node-p (js3-node-parent node))))

(defun js3r--is-assigned-function-expression (node)
  (and (js3-function-node-p node)
       (js3-assign-node-p (js3-node-parent node))))

(defun js3r--is-function-declaration (node)
  (let ((parent (js3-node-parent node)))
    (and (js3-function-node-p node)
         (not (js3-assign-node-p parent))
         (not (js3-var-init-node-p parent))
         (not (js3-object-prop-node-p parent)))))

;; Change from a list of arguments to a parameter object

(defun js3r-arguments-to-object ()
  (interactive)
  (js3r--guard)
  (let ((node (js3-node-at-point)))
    (unless (and (looking-at "(")
                 (or (js3-function-node-p node)
                     (js3-call-node-p node)
                     (js3-new-node-p node)))
      (error "Place point right before the opening paren in the call or function."))

    (-when-let* ((target (js3r--node-target node))
                 (fn (and (js3-name-node-p target)
                          (js3r--local-fn-from-name-node target))))
      (setq node fn))
    (if (js3-function-node-p node)
        (js3r--arguments-to-object-for-function node)
      (js3r--arguments-to-object-for-args-with-unknown-function (js3r--node-args node)))))

(defun js3r--arguments-to-object-for-function (function-node)
  (let ((params (js3-function-node-params function-node)))
    (when (null params)
      (error "No params to convert."))
    (save-excursion
      (js3r--execute-changes
       (-concat
        ;; change parameter list to just (params)
        (list
         (list :beg (+ (js3-node-abs-pos function-node) (js3-function-node-lp function-node))
               :end (+ (js3-node-abs-pos function-node) (js3-function-node-rp function-node) 1)
               :contents "(params)"))

        ;; add params. in front of function local param usages
        (let* ((local-param-name-nodes (--mapcat (-> it
                                                   (js3-node-abs-pos)
                                                   (js3r--local-name-node-at-point)
                                                   (js3r--local-usages-of-name-node))
                                                 params))
               (local-param-name-usages (--remove (js3-function-node-p (js3-node-parent it))
                                                  local-param-name-nodes))
               (local-param-name-positions (-map 'js3-node-abs-pos local-param-name-usages)))
          (--map
           (list :beg it :end it :contents "params.")
           local-param-name-positions))

        ;; update usages of function
        (let ((names (-map 'js3-name-node-name params))
              (usages (js3r--function-usages function-node)))
          (--map
           (js3r--changes/arguments-to-object it names)
           usages)))))))

(defun js3r--changes/arguments-to-object (node names)
  (let ((args (js3r--node-args node)))
    (list :beg (+ (js3-node-abs-pos node) (js3r--node-lp node))
          :end (+ (js3-node-abs-pos node) (js3r--node-rp node) 1)
          :contents (js3r--create-object-with-arguments names args))))

(defun js3r--arguments-to-object-for-args-with-unknown-function (args)
  (when (null args)
    (error "No arguments to convert."))
  (let ((names (--map-indexed
                (format "${%d:%s}"
                        (1+ it-index)
                        (if (js3-name-node-p it)
                            (js3-name-node-name it)
                          "key"))
                args)))
    (yas/expand-snippet (js3r--create-object-with-arguments names args)
                        (point)
                        (save-excursion (forward-list) (point)))))

(defun js3r--create-object-with-arguments (names args)
  (let (arg key result)
    (--dotimes (length args)
      (setq arg (nth it args))
      (setq key (nth it names))
      (setq result
            (concat result
                    (format "    %s: %s,\n"
                            key
                            (buffer-substring (js3-node-abs-pos arg)
                                              (js3-node-abs-end arg))))))
    (concat "({\n" (substring result 0 -2) "\n})")))

;; Extract Function and Extract Method

(defun js3r-extract-function (name)
  (interactive "sName of new function: ")
  (js3r--extract-fn
   name
   #'(lambda ()
       (unless (js3r--looking-at-function-declaration)
         (goto-char (js3-node-abs-pos (js3r--closest 'js3-expr-stmt-node-p)))))
   "%s(%s);"
   "function %s(%s) {\n%s\n}\n\n"))

(defun js3r-extract-method (name)
  (interactive "sName of new method: ")
  (js3r--extract-fn
   name
   #'(lambda ()
       (goto-char (js3-node-abs-pos (js3r--closest 'js3-object-prop-node-p))))
   "this.%s(%s);"
   "%s: function (%s) {\n%s\n},\n\n"))

(defun js3r--extract-fn (name goto-position call-template function-template)
  (js3r--guard)
  (unless (use-region-p)
    (error "Mark the expressions to extract first."))
  (save-excursion
    (let* ((parent (js3r--first-common-ancestor-in-region (region-beginning) (region-end)))
           (block (js3r--closest-node-where 'js3-block-node-p parent))
           (fn (js3r--closest-node-where 'js3-function-node-p block))
           (exprs (js3r--marked-expressions-in-block block))
           (vars (-mapcat 'js3r--name-node-decendants exprs))
           (local (--filter (js3r--local-to-fn-p fn it) vars))
           (names (-distinct (-map 'js3-name-node-name local)))
           (declared-in-exprs (-map 'js3r--var-init-node-target-name (-mapcat 'js3r--var-init-node-decendants exprs)))
           (outside-exprs (-difference (js3-block-node-kids block) exprs))
           (outside-var-uses (-map 'js3-name-node-name (-mapcat 'js3r--name-node-decendants outside-exprs)))
           (declared-in-but-used-outside (-intersection declared-in-exprs outside-var-uses))
           (export-var (car declared-in-but-used-outside))
           (params (-difference names declared-in-exprs))
           (params-string (mapconcat 'identity (reverse params) ", "))
           (first (car exprs))
           (last (car (last exprs)))
           (beg (js3-node-abs-pos (car exprs)))
           (end (js3-node-abs-end last))
           (contents (buffer-substring beg end)))
      (goto-char beg)
      (delete-region beg end)
      (when (js3-return-node-p last)
        (insert "return "))
      (when export-var
        (setq contents (concat contents "\nreturn " export-var ";"))
        (insert "var " export-var " = "))
      (insert (format call-template name params-string))
      (goto-char (js3-node-abs-pos fn))
      (funcall goto-position)
      (let ((start (point)))
        (insert (format function-template name params-string contents))
        (indent-region start (1+ (point)))))))

(defun js3r--var-init-node-target-name (node)
  (js3-name-node-name
   (js3-var-init-node-target node)))

(defun js3r--function-around-region ()
  (or
   (js3r--closest-node-where 'js3-function-node-p
                             (js3r--first-common-ancestor-in-region
                              (region-beginning)
                              (region-end)))
   (error "This only works when you mark stuff inside a function")))

(defun js3r--marked-expressions-in-block (fn)
  (-select 'js3r--node-is-marked (js3-block-node-kids fn)))

(defun js3r--node-is-marked (node)
  (and
   (<= (region-beginning) (js3-node-abs-end node))
   (>= (region-end) (js3-node-abs-pos node))))

(defun js3r--name-node-decendants (node)
  (-select 'js3-name-node-p (js3r--decendants node)))

(defun js3r--var-init-node-decendants (node)
  (-select 'js3-var-init-node-p (js3r--decendants node)))

(defun js3r--decendants (node)
  (let (vars)
    (js3-visit-ast node
                   '(lambda (node end-p)
                      (unless end-p
                        (setq vars (cons node vars)))))
    vars))

(defun js3r--local-to-fn-p (fn name-node)
  (let* ((name (js3-name-node-name name-node))
         (scope (js3-node-get-enclosing-scope name-node))
         (scope (js3-get-defining-scope scope name)))
    (eq fn scope)))

;; Toggle between function name() {} and var name = function ();

(defun js3r-toggle-function-expression-and-declaration ()
  (interactive)
  (save-excursion
    (js3r--find-closest-function)
    (cond
     ((js3r--looking-at-var-function-expression) (js3r--transform-function-expression-to-declaration))
     ((js3r--looking-at-function-declaration) (js3r--transform-function-declaration-to-expression))
     (t (error "Can only toggle between function declarations and free standing function expressions.")))))

(defun js3r--find-closest-function ()
  (end-of-line)
  (word-search-backward "function")
  (while (er--point-inside-string-p)
    (word-search-backward "function")))

(defun js3r--looking-at-method ()
  (and (looking-at "function")
       (looking-back ": ?")))

(defun js3r--looking-at-function-declaration ()
  (and (looking-at "function")
       (looking-back "^ *")))

(defun js3r--looking-at-var-function-expression ()
  (and (looking-at "function")
       (looking-back "^ *var [a-z_$]+ = ")))

(defun js3r--transform-function-expression-to-declaration ()
  (when (js3r--looking-at-var-function-expression)
    (delete-char 9)
    (forward-list)
    (forward-list)
    (delete-char 1)
    (backward-list)
    (backward-list)
    (delete-backward-char 3)
    (back-to-indentation)
    (delete-char 4)
    (insert "function ")))

(defun js3r--transform-function-declaration-to-expression ()
  (when (js3r--looking-at-function-declaration)
    (delete-char 9)
    (insert "var ")
    (search-forward "(")
    (backward-char 1)
    (insert " = function ")
    (forward-list)
    (forward-list)
    (insert ";")))

(provide 'js3r-functions)
