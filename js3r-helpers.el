(require 'dash)
(require 's)

(defun js3r--fix-special-modifier-combinations (key)
  (case key
    ("C-s-i" "s-TAB")
    ("C-s-m" "s-RET")
    (otherwise key)))

(defun js3r--key-pairs-with-modifier (modifier keys)
  (->> (string-to-list keys)
    (--map (js3r--fix-special-modifier-combinations
            (concat modifier (char-to-string it))))
    (s-join " ")
    (read-kbd-macro)))

(defun js3r--key-pairs-with-prefix (prefix keys)
  (read-kbd-macro (concat prefix " " keys)))

(defun js3r--guard ()
  (when js3-parsed-errors
    (error "Can't refactor while buffer has parse errors.")))

(defun js3r--current-quotes-char ()
  "The char that is the current quote delimiter"
  (nth 3 (syntax-ppss)))

(defalias 'js3r--point-inside-string-p 'js3r--current-quotes-char)

(defun js3r--closest-node-where (p node)
  (if (or (null node)
          (apply p node nil))
      node
    (js3r--closest-node-where p (js3-node-parent node))))

(defun js3r--closest (p)
  (save-excursion
    (cond
     ((bolp) (back-to-indentation))
     ((looking-at ";") (forward-char -1))
     ((looking-back ";") (forward-char -2))
     ((looking-back "}") (forward-char -1)))
    (js3r--closest-node-where p (js3-node-at-point))))

(defun js3r--goto-and-delete-node (node)
  (goto-char (js3-node-abs-pos node))
  (delete-char (js3-node-len node)))


(defun js3r--path-to-root (node)
  (when node
    (cons node (js3r--path-to-root (js3-node-parent node)))))

(defun js3r--first-common-ancestor (node1 node2)
  (if (eq node1 node2)
      node1
    (let ((path1 (reverse (js3r--path-to-root node1)))
          (path2 (reverse (js3r--path-to-root node2)))
          (last-common nil))
      (while (eq (car path1) (car path2))
        (setq last-common (car path1))
        (setq path1 (cdr path1))
        (setq path2 (cdr path2)))
      last-common)))

(defun js3r--first-common-ancestor-in-region (beg end)
  (js3r--first-common-ancestor (js3-node-at-point beg)
                               (js3-node-at-point end)))

;; abstract away node type on some common property getters
(defun js3r--node-target (node)
  (cond
   ((js3-call-node-p node) (js3-call-node-target node))
   ((js3-new-node-p node) (js3-new-node-target node))
   (:else nil)))

(defun js3r--node-args (node)
  (cond
   ((js3-call-node-p node) (js3-call-node-args node))
   ((js3-new-node-p node) (js3-new-node-args node))
   (:else nil)))

(defun js3r--node-lp (node)
  (cond
   ((js3-call-node-p node) (js3-call-node-lp node))
   ((js3-new-node-p node) (js3-new-node-lp node))
   (:else nil)))

(defun js3r--node-rp (node)
  (cond
   ((js3-call-node-p node) (js3-call-node-rp node))
   ((js3-new-node-p node) (js3-new-node-rp node))
   (:else nil)))

;; finding expressions and arguments

(defun js3r--argument-p (node)
  (let ((parent (js3-node-parent node)))
    (and (js3-call-node-p parent)
         (member node (js3-call-node-args parent)))))

(defun js3r--expression-p (node)
  (or (js3-call-node-p node)
      (js3-string-node-p node)
      (js3r--argument-p node)
      (and (js3-prop-get-node-p node)
           (not (js3-call-node-p (js3-node-parent node))))))

(defun js3r--single-complete-expression-between-p (beg end)
  (let ((ancestor (js3r--first-common-ancestor-in-region beg (- end 1))))
    (and (= beg (js3-node-abs-pos ancestor))
         (= end (js3-node-abs-end ancestor)))))


;; executing a list of changes
;; ensures changes are executed from last to first

(defun js3r--by-end-descending (change1 change2)
  (> (plist-get change1 :end)
     (plist-get change2 :end)))

(defun js3r--any-overlapping-changes (sorted-changes)
  (--any?
   (let ((one (car it))
         (two (cadr it)))
     (< (plist-get one :beg)
        (plist-get two :end)))
   (-partition-in-steps 2 1 sorted-changes)))

(defun js3r--execute-changes (changes)
  (when changes
   (let ((sorted-changes (sort changes 'js3r--by-end-descending)))
     (when (js3r--any-overlapping-changes sorted-changes)
       (error "These changes overlap, cannot execute properly."))
     (let ((abs-end (set-marker (make-marker) (1+ (plist-get (car sorted-changes) :end))))
           (abs-beg (plist-get (car (last sorted-changes)) :beg)))
       (--each sorted-changes
         (goto-char (plist-get it :beg))
         (delete-char (- (plist-get it :end) (plist-get it :beg)))
         (insert (plist-get it :contents)))
       (indent-region abs-beg abs-end)
       (set-marker abs-end nil)))))

(provide 'js3r-helpers)
