;;; emacs configuration file

;; Setting the search path for binaries
(defvar paths '("/usr/local/bin"
                "/Library/TeX/texbin"
                "/usr/bin"
                "/bin"
                "/usr/sbin"
                "/sbin"
                "/Applications/Postgres.app/Contents/Versions/latest/bin"))
(setq exec-path (append exec-path paths))
(setenv "PATH"
        (concat (getenv "PATH")
                ":"
                (mapconcat 'identity paths ":")))

;; Package Management
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Get rid of all the window stuff
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; In GUI mode, still use native Emacs controls
(when (display-graphic-p)
  (setq use-dialog-box nil)
  (setq-default cursor-type 'bar)
  (setq blink-cursor-interval 1.0))

;; Empty line at EOF
(setq require-final-newline t)

;; Delete trailing whitespace upon saving
(add-hook 'write-final-hooks 'delete-trailing-whitespace)

;; Better window movement
(windmove-default-keybindings)

;; Trailing whitespace
(setq-default show-trailing-whitespace t)

;; Spaces for indentation
(setq-default indent-tabs-mode nil tab-width 4)

(defun setup-lisp-mode ()
  (paredit-mode)
  (setq tab-width 2))

(add-hook 'lisp-mode-hook 'setup-lisp-mode)
(add-hook 'emacs-list-mode-hook 'setup-list-mode)
(add-hook 'clojure-mode-hook 'setup-lisp-mode)

;; We need UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; We do not need syntax highlighting
(global-font-lock-mode 0)

;; Line numbers
(line-number-mode 0)
(global-linum-mode 1)
(column-number-mode 1)

;; Highlight the current line
(global-hl-line-mode 1)

;; Truncate long lines instead of wrapping them
(setq truncate-lines t)
(setq truncate-partial-width-windows t)

;; Backups
(defvar backup-dir (expand-file-name "~/.emacs.d/bckp"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/atsv"))

(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory autosave-dir)

;; Disable that mouse
;; Requires the 'disable-mouse' package
(global-disable-mouse-mode)

;; Bind magit to a shortcut
(global-set-key (kbd "C-x g") 'magit-status)

;; 120 character width
(setq-default fill-column 120)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "fbfe57e4331e1c9f1eb850a902960ae07b9d1eb74e36d07f30cd88f1215f1f3b" "63dd8ce36f352b92dbf4f80e912ac68216c1d7cf6ae98195e287fd7c7f7cb189" "23ccf46b0d05ae80ee0661b91a083427a6c61e7a260227d37e36833d862ccffc" default)))
 '(package-selected-packages
   (quote
    (js2-highlight-vars kanban leuven-theme git-timemachine company org-present rjsx-mode emmet-mode nyan-mode plsql json-mode clojure-mode paredit rainbow-delimiters projectile tao-theme org neotree magit disable-mouse))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; A nice theme
(load-theme 'leuven)

;; Projectile mode
(projectile-global-mode)

(global-set-key (kbd "C-x 2") (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(add-to-list 'default-frame-alist '(font . "Hasklig-11"))
(set-face-attribute 'default t :font "Hasklig-11")

;; Good javascript
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

;; Emmet
;;(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . emmet-mode))
(add-hook 'rjsx-mode-hook 'emmet-mode)
(setq emmet-expand-jsx-className? t)

(defun sgml-indent ()
  (set (make-local-variable 'sgml-basic-offset) 4))

(add-hook 'html-mode-hook 'sgml-indent)
(add-hook 'rjsx-mode-hook 'sgml-indent)

;; Make an active region replaceable
(delete-selection-mode 1)

;; Auto completion
(add-hook 'after-init-hook 'global-company-mode)

;; Settings for org-mode's presenter mode
(add-hook 'org-present-mode-hook
          (lambda ()
            (linum-mode 0)
            (local-set-key (kbd "<next>") 'org-present-next)
            (local-set-key (kbd "<prior>") 'org-present-prev)
            (org-present-big)
            (org-display-inline-images)
            (org-present-hide-cursor)
            (org-present-read-only)))
(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (linum-mode t)
            (org-present-small)
            (org-remove-inline-images)
            (org-present-show-cursor)
            (org-present-read-write)))

(global-set-key (kbd "C-^") 'org-present)

;; File tree configuration
(setq neo-theme 'ascii)

;; We do not want to be annoyed
(setq ring-bell-function 'ignore)

;; Highlight long lines
(defface hi-nasty '((t :foreground "#00bb00")) "Warning face")
(defface hi-console '((t :foreground "#ff6600")) "Face for console.log/...")
(defface hi-todo '((t :background "#bb0000" :foreground "#ffffff")) "TODO face")

(defun highlight-stuff ()
  (highlight-lines-matching-regexp ".\\{120\\}" 'hi-nasty)
  (highlight-regexp "\\<\\(FIXME\\|TODO\\)" 'hi-todo))

(add-hook 'find-file-hook 'highlight-stuff)
(add-hook 'rjsx-mode-hook
          (lambda ()
            (highlight-regexp "console\\.\\(warn\\|log\\|error\\|table\\|dir\\)" 'hi-console)
            (js2-highlight-vars-mode)))

(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
(put 'scroll-left 'disabled nil)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
