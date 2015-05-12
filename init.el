

;; Nonediting things



;; Packet manager
;; Found here: http://stackoverflow.com/a/10093312

; list the packages you want
(setq package-list '(markdown-mode))
(setq package-list '(undo-tree))
(setq package-list '(comment-dwim-2w))
(setq package-list '(expand-region))
(setq package-list '(magit))
(setq package-list '(helm))
(setq package-list '(haskell-mode))
(setq package-list '(hydra))




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

;; Magit
;; (setq magit-last-seen-setup-instructions "1.4.0")


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

(defhydra hydra-zoom ()
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("RET" nil "done" :color blue))
(global-set-key (kbd "C-p") 'hydra-zoom/body)

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



(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-.") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-S-i")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-ff-file-name-history-use-recentf t
      helm-M-x-fuzzy-match                  t); fuzzy matching for helm-M-x


(helm-mode 1)





;; Editing things

;; Move lines up and down
;; Found here: http://www.emacswiki.org/emacs/MoveLine

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "<C-M-up>") 'move-line-up)
(global-set-key (kbd "<C-M-down>") 'move-line-down)


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
(global-unset-key (kbd "M-;"))

;; some normal stuff
(global-set-key (kbd "C-w") 'delete-window)
(global-set-key (kbd "C-S-w") 'delete-other-windows)
(global-set-key (kbd "C-o") 'helm-find-files)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'write-file)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-_") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(global-set-key (kbd "C-c") 'kill-ring-save)
;; (global-set-key (kbd "C-v") 'helm-show-kill-ring)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-S-v") 'yank-pop)
(global-set-key (kbd "C-t") 'tiling-mode)
(global-set-key (kbd "C-#") 'comment-dwim-2)
(global-set-key (kbd "C-a") 'er/expand-region)
(global-set-key (kbd "C-S-a") 'er/contract-region)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "M-x") 'helm-M-x)



;; Incremental search (made it up myself)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
(setq isearch-lazy-highlight nil)







