

;; Nonediting things



;; Packet manager
;; Found here: http://stackoverflow.com/a/10093312

; list the packages you want
(setq package-list '(markdown-mode))
(setq package-list '(undo-tree))
(setq package-list '(comment-dwim-2w))
(setq package-list '(expand-region))

; list the repositories containing them
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("elpa" . "http://tromey.com/elpa/")
			 ("melpa" . "http://melpa.org/packages/")
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








;; undo tree always
(global-undo-tree-mode 1)

;; disable toolbar (wich provides buttons for save, open...). Use your keyboard.
(tool-bar-mode -1)


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

;; I learned about how to define a minor mode here: http://stackoverflow.com/a/3116381
;; Tiling Minor Mode
(define-minor-mode tiling-mode
  "Tiling mode, so emacs windows can be managed similar to X windows with i3."
  ;; The initial value - Set to 1 to enable by default
  nil
  ;; The indicator for the mode line.
  " tile"
  ;; The minor mode keymap
  `(
    (,(kbd "C-v") . split-window-vertically)
    (,(kbd "C-h") . split-window-horizontally)

    (,(kbd "<left>") . windmove-left)
    (,(kbd "<right>") . windmove-right)
    (,(kbd "<down>") . windmove-down)
    (,(kbd "<up>") . windmove-up)

    (,(kbd "C-<right>") . (lambda () (interactive) (swap-with 'right)))
    (,(kbd "C-<left>") . (lambda () (interactive) (swap-with 'left)))
    (,(kbd "C-<down>") . (lambda () (interactive) (swap-with 'down)))
    (,(kbd "C-<up>") . (lambda () (interactive) (swap-with 'up)))

    (,(kbd "S-<right>") . (lambda () (interactive) (enlarge-window 1 t)))
    (,(kbd "S-<left>") . (lambda () (interactive) (enlarge-window -1 t)))
    (,(kbd "S-<down>") . (lambda () (interactive) (enlarge-window 1)))
    (,(kbd "S-<up>") . (lambda () (interactive) (enlarge-window -1)))

    )
   ;; Make mode global rather than buffer local
   :global 1
)






;; Editing things

;; unbind things
;;at some point I should simply unbind every default keybinding
(global-unset-key (kbd "C-x C-s"))
(global-unset-key (kbd "C-x C-f"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "M-w"))
(global-unset-key (kbd "C-y"))
(global-unset-key (kbd "M-y"))
(global-unset-key (kbd "M-<down>"))
(global-unset-key (kbd "M-<up>"))
(global-unset-key (kbd "M-<right>"))
(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "C-_"))

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
(global-set-key (kbd "C-t") 'tiling-mode)
(global-set-key (kbd "C-#") 'comment-dwim-2)
(global-set-key (kbd "C-a") 'er/expand-region)
(global-set-key (kbd "C-S-a") 'er/contract-region)



;; Incremental search (made it up myself)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
(setq isearch-lazy-highlight nil)






