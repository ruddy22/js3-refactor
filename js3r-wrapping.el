(require 'yasnippet)

(defvar js3r--space-str " \t\n")

(defun js3r--skip-region-whitespace ()
  (let ((p-first (< (point) (mark))))
    (unless p-first
      (exchange-point-and-mark))
    (skip-chars-forward js3r--space-str)
    (exchange-point-and-mark)
    (skip-chars-backward js3r--space-str)
    (when p-first
      (exchange-point-and-mark))))

(defun js3r-unwrap ()
  (interactive)
  (js3r--guard)
  (let (beg end)
    (if (use-region-p)
        (progn
          (js3r--skip-region-whitespace)
          (setq beg (min (point) (mark)))
          (setq end (max (point) (mark))))
      (let ((stmt (js3-node-parent-stmt (js3-node-at-point))))
        (setq beg (js3-node-abs-pos stmt))
        (setq end (js3-node-abs-end stmt))))
    (let* ((ancestor (js3-node-parent-stmt
                      (js3r--first-common-ancestor-in-region beg end)))
           (abeg (js3-node-abs-pos ancestor))
           (aend (js3-node-abs-end ancestor)))
      (save-excursion
        (goto-char end)
        (delete-char (- aend end))
        (goto-char abeg)
        (delete-char (- beg abeg)))
      (indent-region (point-min) (point-max)))))

(defun js3r-wrap-in-for-loop (beg end)
  (interactive "r")
  (js3r--skip-region-whitespace)
  (setq beg (min (point) (mark)))
  (setq end (max (point) (mark)))
  (let ((yas/wrap-around-region t))
    (yas/expand-snippet "for (var i = 0, l = ${1:length}; i < l; i++) {\n$0\n}"
                        beg end)))

(provide 'js3r-wrapping)
