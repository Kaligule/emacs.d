

;; Nonediting things



;; Packet manager
;; Found here: http://stackoverflow.com/a/10093312

; list the packages you want
(setq package-list '(markdown-mode))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))










;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)




;; when started, emacs should default to markdown mode and an empty document
;; found here: http://emacsredux.com/blog/2014/07/25/configure-the-scratch-buffers-mode/
(setq inhibit-splash-screen t)
(setq initial-major-mode 'markdown-mode)
(setq initial-scratch-message nil)

;; Certain fileextentions should result to certain modes
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.plt\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))







;; Editing things

;; unbind things
;;at some point I should simply unbind every default keybinding
(global-unset-key (kbd "C-x C-s"))
(global-unset-key (kbd "C-x C-f"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "M-w"))
(global-unset-key (kbd "C-y"))
(global-unset-key (kbd "M-y"))

;; some normal stuff
(global-set-key (kbd "C-w") 'delete-window)
(global-set-key (kbd "C-S-w") 'delete-other-windows)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'write-file)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(global-set-key (kbd "C-c") 'kill-ring-save)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-S-v") 'yank-pop)
(global-set-key (kbd "C-n") 'split-window-horizontally)
(global-set-key (kbd "C-S-n") 'split-window-vertically)

;; Incremental search (made it up myself)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
(setq isearch-lazy-highlight nil)






;; emacsd-tile.el -- tiling windows for emacs
;; Found here: https://gist.github.com/mariusae/287633
;; Short blogpost here: http://monkey.org/~marius/emacs-as-a-tiling-window-manager.html

(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window  (selected-window))
             (this-buffer  (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start   (window-start this-window))
             (other-start  (window-start other-window)))
        (set-window-buffer this-window  other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start  this-window  other-start)
        (set-window-start  other-window this-start)))))
 
(global-set-key (kbd "M-S-<down>") (lambda () (interactive) (swap-with 'down)))
(global-set-key (kbd "M-S-<up>") (lambda () (interactive) (swap-with 'up)))
(global-set-key (kbd "M-S-<left>") (lambda () (interactive) (swap-with 'left)))
(global-set-key (kbd "M-S-<right>") (lambda () (interactive) (swap-with 'right)))

(global-set-key (kbd "M-C-S-<down>") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-C-S-<up>") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-C-S-<left>") (lambda () (interactive) (enlarge-window -1 t)))
(global-set-key (kbd "M-C-S-<right>") (lambda () (interactive) (enlarge-window 1 t)))

(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)



