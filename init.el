

;; Nonediting things



;; Packet manager
;; Found here: http://stackoverflow.com/a/10093312

;; list the packages you want
;; Meta packages
(setq package-list '(helm))
(setq package-list '(hydra))
;; certain commands and functions
(setq package-list '(magit))
(setq package-list '(undo-tree))
(setq package-list '(expand-region))
(setq package-list '(comment-dwim-2w))
;; Modes
(setq package-list '(haskell-mode))
(setq package-list '(markdown-mode))




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
(global-set-key (kbd "C-+") 'hydra-zoom/body)
(global-set-key (kbd "C--") 'hydra-zoom/body)

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

;; Nicer window management with hydra
;; Found inspiration here: https://github.com/abo-abo/hydra/wiki/Window-Management
(when (fboundp 'winner-mode)
  (winner-mode 1))
(require 'windmove)

(defhydra hydra-window (:color red
                        :hint nil)
  "
 Split: _v_ertical _h_orizontal
Delete: _w_indow other _W_indows
  Move: Arrowkeys
  Size: Shift-Arrowkeys
Frames: _f_rame
  Misc: _u_ndo  _r_edo"
  ("<right>" windmove-right)
  ("<left>" windmove-left)
  ("<up>" windmove-up)
  ("<down>" windmove-down)
  ("S-<right>" hydra-move-splitter-right)
  ("S-<left>" hydra-move-splitter-left)
  ("S-<up>" hydra-move-splitter-up)
  ("S-<down>" hydra-move-splitter-down)
  ("v" split-window-right)
  ("h" split-window-below)
  ;("t" transpose-frame "'")
  ("u" winner-undo)
  ("r" winner-redo) ;;Fixme, not working?
  ("w" delete-window)
  ("W" delete-other-windows :color blue)
  ("f" new-frame :exit t)
  ("RET" nil "done" :color blue))

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))




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

;; Indentation
;; No tabs, only spaces
(setq-default indent-tabs-mode nil)

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
(global-set-key (kbd "C-w") 'hydra-window/body)
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







;; Mode specific

;; Haskell

;; Indentation (must be enabled explicitly)
;; Found here: https://wiki.haskell.org/Emacs/Indentation
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-assignment
                  (regexp . "\\(\\s-+\\)=\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-arrows
                  (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-left-arrows
                  (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))

;; found here: https://wiki.haskell.org/Emacs/Inferior_Haskell_processes
(require 'inf-haskell)
(defhydra hydra-haskell-things ()
  "haskell things"
  ("l" inferior-haskell-load-file "load in ghci" :color  blue)
  ("t" inferior-haskell-type "type" :color  blue)
  ("i" inferior-haskell-info "info" :color  blue)
  ("d" inferior-haskell-find-definition "definition" :color  blue)
  ("RET" nil "done" :color blue))
(global-set-key (kbd "C-P") 'hydra-haskell-things/body)

