;; Console.log stuff at point (or region)

(defun js3r-log-this ()
  (interactive)
  (js3r--guard)
  (let* ((log-info (js3r--figure-out-what-to-log-where))
         (stmt (car log-info))
         (pos (cdr log-info)))
    (save-excursion
      (goto-char pos)
      (when (looking-at "[;{]")
        (forward-char 1))
      (newline-and-indent)
      (insert "console.log(\"" stmt " = \", " stmt ");"))))

(defun js3r--figure-out-what-to-log-where ()
  (let ((parent-stmt (js3-node-parent-stmt (js3-node-at-point))))

    (if (use-region-p)
        (cons (buffer-substring (region-beginning) (region-end))
              (js3r--find-suitable-log-position-around parent-stmt))

      (let* ((node (js3r--name-node-at-point))
             (parent (js3-node-parent node)))

        (cond

         ((js3-function-node-p parent)
          (cons (js3-name-node-name node)
                (js3-node-abs-pos (js3-function-node-body parent))))

         ((js3-prop-get-node-p parent)
          (cons (buffer-substring (js3-node-abs-pos parent) (js3-node-abs-end parent))
                (js3r--find-suitable-log-position-around parent-stmt)))

         (:else
          (cons (js3-name-node-name node)
                (js3r--find-suitable-log-position-around parent-stmt))))))))

(defun js3r--find-suitable-log-position-around (parent-stmt)
  (if (js3-return-node-p parent-stmt)
      (save-excursion
        (goto-char (js3-node-abs-pos parent-stmt))
        (beginning-of-line)
        (forward-char -1)
        (point))
    (js3-node-abs-end parent-stmt)))

;; Split a string

(defun js3r-split-string ()
  (interactive)
  (when (js3r--point-inside-string-p)
    (if (looking-back " \"")
        (progn
          (forward-char -2)
          (insert "  +")
          (forward-char -2))
      (if (looking-at (regexp-quote "\" + \""))
          (delete-char 5)
        (insert "\" + \"")))))

;; Make sure commas are placed correctly when moving a line up or down
;; in an object or array literal.

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun js3r--current-line-is-prefixed-with-list-item-start ()
  (save-excursion
    (back-to-indentation)
    (looking-back "\\({\\|\\[\\|,\\)\\(\s\\|\n\\)*"))) ; { or [ or , then space

(defun js3r--current-line-is-postfixed-with-list-item-end ()
  (save-excursion
    (end-of-line)
    (or (looking-back ",\s*") ; line ends in comma
        (looking-at "\\(\s\\|\n\\)*\\(\\]\\|}\\)")))) ; space then ] or }

(defun js3r--current-line-is-a-list-item ()
  (and (js3r--current-line-is-prefixed-with-list-item-start)
       (js3r--current-line-is-postfixed-with-list-item-end)))

(defun js3r--next-line-is-a-list-item ()
  (save-excursion
    (forward-line)
    (js3r--current-line-is-a-list-item)))

(defun js3r--previous-line-is-a-list-item ()
  (save-excursion
    (forward-line -1)
    (js3r--current-line-is-a-list-item)))

(defun js3r--current-line-has-comma ()
  (save-excursion
    (end-of-line)
    (looking-back ",\s*")))

(defun js3r--previous-line-has-comma ()
  (save-excursion
    (forward-line -1)
    (js3r--current-line-has-comma)))

(defun js3r--move-line-down-as-list-item ()
  (move-line-down)
  (if (not (js3r--previous-line-has-comma))
      (save-excursion
        (end-of-line)
        (delete-char -1)
        (forward-line -1)
        (end-of-line)
        (insert ","))))

(defun js3r--move-line-up-as-list-item ()
  (move-line-up)
  (if (not (js3r--current-line-has-comma))
      (save-excursion
        (end-of-line)
        (insert ",")
        (forward-line)
        (end-of-line)
        (delete-char -1))))

(defun js3r-move-line-down ()
  (interactive)
  (if (and (js3r--current-line-is-a-list-item)
           (js3r--next-line-is-a-list-item))
      (js3r--move-line-down-as-list-item)
    (move-line-down))
  (funcall indent-line-function))

(defun js3r-move-line-up ()
  (interactive)
  (if (and (js3r--current-line-is-a-list-item)
           (js3r--previous-line-is-a-list-item))
      (js3r--move-line-up-as-list-item)
    (move-line-up))
  (funcall indent-line-function))

(provide 'js3r-conveniences)
