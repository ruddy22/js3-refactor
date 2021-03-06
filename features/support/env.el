(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq js3-refactor-root-path project-directory))

(add-to-list 'load-path js3-refactor-root-path)

(require 'dash)
(require 'multiple-cursors)
(require 'js3-refactor)
(require 'espuds)
(require 'ert)

(Setup
 (js3r-add-keybindings-with-prefix "C-c C-m"))

(Before
 (switch-to-buffer
  (get-buffer-create "*js3-refactor*"))
 (multiple-cursors-mode 0)
 (erase-buffer)
 (transient-mark-mode 1)
 (cua-mode 0)
 (delete-selection-mode 0)
 (set-default 'indent-tabs-mode nil)
 (setq set-mark-default-inactive nil)
 (deactivate-mark))

(After
 (yas/exit-all-snippets))
