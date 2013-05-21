(require 'multiple-cursors-core)
(require 'dash)

;; Helpers

(defun js3r--name-node-at-point (&optional pos)
  (setq pos (or pos (point)))
  (let ((current-node (js3-node-at-point pos)))
    (unless (js3-name-node-p current-node)
      (setq current-node (js3-node-at-point (- (point) 1))))
    (if (not (and current-node (js3-name-node-p current-node)))
        (error "Point is not on an identifier.")
      current-node)))

(defun js3r--local-name-node-at-point (&optional pos)
  (setq pos (or pos (point)))
  (let ((current-node (js3r--name-node-at-point pos)))
    (unless (js3r--local-name-node-p current-node)
      (error "Point is not on a local identifier"))
    current-node))

(defun js3r--local-name-node-p (node)
  (and (js3-name-node-p node)
       (not (save-excursion ; not key in object literal { key: value }
              (goto-char (+ (js3-node-abs-pos node) (js3-node-len node)))
              (looking-at "[\n\t ]*:")))
       (not (save-excursion ; not property lookup on object
              (goto-char (js3-node-abs-pos node))
              (looking-back "\\.[\n\t ]*")))))

(defun js3r--local-usages-of-name-node (name-node)
  (unless (js3r--local-name-node-p name-node)
    (error "Node is not on a local identifier"))
  (let* ((name (js3-name-node-name name-node))
         (scope (js3-node-get-enclosing-scope name-node))
         (scope (js3-get-defining-scope scope name))
         (current-start (js3-node-abs-pos name-node))
         (current-end (+ current-start (js3-node-len name-node)))
         (result nil))
    (js3-visit-ast
     scope
     (lambda (node end-p)
       (when (and (not end-p)
                  (js3r--local-name-node-p node)
                  (string= name (js3-name-node-name node)))
         (add-to-list 'result node))
       t))
    result))

(defun js3r--local-var-positions (name-node)
  (-map 'js3-node-abs-pos (js3r--local-usages-of-name-node name-node)))

(defun js3r--var-defining-node (var-node)
  (unless (js3r--local-name-node-p var-node)
    (error "Node is not on a local identifier"))
  (let* ((name (js3-name-node-name var-node))
         (scope (js3-node-get-enclosing-scope var-node))
         (scope (js3-get-defining-scope scope name)))
    (js3-symbol-ast-node
     (js3-scope-get-symbol scope name))))


;; Add to jslint globals annotation

(defun current-line-contents ()
  "Find the contents of the current line, minus indentation."
  (buffer-substring (save-excursion (back-to-indentation) (point))
                    (save-excursion (end-of-line) (point))))

(require 'thingatpt)

(defun js3r-add-to-globals-annotation ()
  (interactive)
  (let ((var (word-at-point)))
    (save-excursion
      (beginning-of-buffer)
      (when (not (string-match "^/\\*global " (current-line-contents)))
        (newline)
        (forward-line -1)
        (insert "/*global */")
        (newline)
        (forward-line -1))
      (while (not (string-match "*/" (current-line-contents)))
        (forward-line))
      (end-of-line)
      (delete-char -2)
      (unless (looking-back "global ")
        (while (looking-back " ")
          (delete-char -1))
        (insert ", "))
      (insert (concat var " */")))))


;; Rename variable

(defun js3r-rename-var ()
  "Renames the variable on point and all occurrences in its lexical scope."
  (interactive)
  (js3r--guard)
  (let* ((current-node (js3r--local-name-node-at-point))
         (len (js3-node-len current-node))
         (current-start (js3-node-abs-pos current-node))
         (current-end (+ current-start len)))
    (push-mark current-end)
    (goto-char current-start)
    (activate-mark)
    (save-excursion
      (mapc (lambda (beg)
              (when (not (= beg current-start))
                (goto-char beg)
                (set-mark (+ beg len))
                (mc/create-fake-cursor-at-point)))
            (js3r--local-var-positions current-node))))
  (mc/maybe-multiple-cursors-mode))

(add-to-list 'mc--default-cmds-to-run-once 'js3r-rename-var)

;; Change local variable to use this. instead

(defun js3r-var-to-this ()
  "Changes the variable on point to use this.var instead."
  (interactive)
  (js3r--guard)
  (save-excursion
    (let ((node (js3-node-at-point)))
      (when (js3-var-decl-node-p node)
        (let ((kids (js3-var-decl-node-kids node)))
          (when (cdr kids)
            (error "Currently does not support converting multivar statements."))
          (goto-char (js3-node-abs-pos (car kids))))))
    (--each (js3r--local-var-positions (js3r--local-name-node-at-point))
      (goto-char it)
      (when (looking-back "var ")
        (delete-char -4))
      (insert "this."))))

;; Inline var

(defun js3r-inline-var ()
  (interactive)
  (js3r--guard)
  (save-excursion
    (let* ((current-node (js3r--local-name-node-at-point))
           (definer (js3r--var-defining-node current-node))
           (definer-start (js3-node-abs-pos definer))
           (var-init-node (js3-node-parent definer))
           (initializer (js3-var-init-node-initializer
                         var-init-node)))
      (unless initializer
        (error "Var is not initialized when defined."))
      (let* ((var-len (js3-node-len current-node))
             (init-beg (js3-node-abs-pos initializer))
             (init-end (+ init-beg (js3-node-len initializer)))
             (contents (buffer-substring init-beg init-end)))
        (mapc (lambda (beg)
                (when (not (= beg definer-start))
                  (goto-char beg)
                  (delete-char var-len)
                  (insert contents)))
              (js3r--local-var-positions current-node))
        (js3r--delete-var-init-node var-init-node)
        ))))


(defun js3r--was-single-var ()
  (or (string= "var ;" (current-line-contents))
      (string= "," (current-line-contents))))

(defun js3r--was-starting-var ()
  (looking-back "var "))

(defun js3r--was-ending-var ()
  (looking-at ";"))

(defun js3r--delete-var-init-node (node)
  (goto-char (js3-node-abs-pos node))
  (delete-char (js3-node-len node))
  (cond
   ((js3r--was-single-var)
    (beginning-of-line)
    (delete-char (save-excursion (end-of-line) (current-column)))
    (delete-blank-lines))

   ((js3r--was-starting-var)
    (delete-char 1)
    (if (looking-at " ")
        (delete-char 1)
      (join-line -1)))

   ((js3r--was-ending-var)
    (if (looking-back ", ")
        (delete-char -1)
      (join-line)
      (delete-char 1))
    (delete-char -1))

   (t (delete-char 2)
      )))

;; two cases
;;   - it's the only var -> remove the line
;;   - there are several vars -> remove the node then clean up commas


;; Extract variable

(defun js3r--start-of-parent-stmt ()
  (js3-node-abs-pos (js3-node-parent-stmt (js3-node-at-point))))

(defun js3r--object-literal-key-behind (pos)
  (save-excursion
    (goto-char pos)
    (when (looking-back "\\sw: ?")
      (backward-char 2)
      (js3-name-node-name (js3r--name-node-at-point)))))

(defun js3r--line-above-is-blank ()
  (save-excursion
    (forward-line -1)
    (string= "" (current-line-contents))))

(defun js3r-extract-var ()
  (interactive)
  (js3r--guard)
  (if (use-region-p)
      (js3r--extract-var-between (region-beginning) (region-end))
    (let ((node (js3r--closest 'js3r--expression-p)))
      (js3r--extract-var-between (js3-node-abs-pos node)
                                 (js3-node-abs-end node)))))

(defun js3r--extract-var-between (beg end)
  (interactive "r")
  (unless (js3r--single-complete-expression-between-p beg end)
    (error "Can only extract single, complete expressions to var."))

  (let ((deactivate-mark nil)
        (expression (buffer-substring beg end))
        (orig-var-end (make-marker))
        new-var-end
        (name (or (js3r--object-literal-key-behind beg) "name")))

    (delete-region beg end)
    (insert name)
    (set-marker orig-var-end (point))

    (goto-char (js3r--start-of-parent-stmt))
    (insert "var " name)
    (setq new-var-end (point))
    (insert " = " expression ";")
    (when (or (js3r--line-above-is-blank)
              (string-match-p "^function " expression))
      (newline))
    (newline)
    (indent-region new-var-end orig-var-end)
    (save-excursion
      (goto-char new-var-end)
      (set-mark (- (point) (length name)))
      (mc/create-fake-cursor-at-point))
    (goto-char orig-var-end)
    (set-mark (- (point) (length name)))
    (set-marker orig-var-end nil))
  (mc/maybe-multiple-cursors-mode))

;; Split var declaration

(defun js3r-split-var-declaration ()
  (interactive)
  (js3r--guard)
  (save-excursion
    (let* ((declaration (or (js3r--closest #'js3-var-decl-node-p) (error "No var declaration at point.")))
           (kids (js3-var-decl-node-kids declaration))
           (stmt (js3-node-parent-stmt declaration)))
      (goto-char (js3-node-abs-end stmt))
      (mapc (lambda (kid)
              (insert "var " (js3-node-string kid) ";")
              (newline)
              (if (save-excursion
                    (goto-char (js3-node-abs-end kid))
                    (looking-at ", *\n *\n"))
                  (newline)))
            kids)
      (delete-char -1) ;; delete final newline
      (let ((end (point)))
        (js3r--goto-and-delete-node stmt)
        (indent-region (point) end)))))

(provide 'js3r-vars)
