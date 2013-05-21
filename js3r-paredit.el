(require 'dash)

(defun js3r--slurpable-node-p (node)
  (or (js3-function-node-p node)
      (js3-if-node-p node)
      (js3-for-node-p node)
      (js3-while-node-p node)))

(defun js3r--standalone-node-p (node)
  (or (js3-stmt-node-p node)
      (and (js3-function-node-p node)
           (eq 'FUNCTION_STATEMENT (js3-function-node-form node)))))

(defun js3r-forward-slurp ()
  (interactive)
  (js3r--guard)
  (let* ((slurpable (js3r--closest 'js3r--slurpable-node-p))
         (standalone (if (js3r--standalone-node-p slurpable)
                   slurpable
                 (js3-node-parent-stmt slurpable)))
         (next-sibling (js3-node-next-sibling standalone))
         (beg (js3-node-abs-pos next-sibling))
         (end (1+ (js3-node-abs-end next-sibling))) ;; include whitespace after statement
         (text (buffer-substring beg end)))
    (save-excursion
      (delete-region beg end)
      (goto-char (js3-node-abs-end slurpable))
      (forward-char -1)
      (when (looking-back "{ *") (newline))
      (setq beg (point))
      (insert text)
      (indent-region beg end))))

(provide 'js3r-paredit)

;;; js3r-paredit.el ends here
