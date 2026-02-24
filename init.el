;; ~/.emacs.d/init.el

;; ------------------------------------------------------------
;; System
;; ------------------------------------------------------------
(global-set-key (kbd "C-\\") 'ignore)
(global-set-key (kbd "<kanji>") 'ignore)
(global-set-key (kbd "M-<kanji>") 'ignore)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(require 'server)
(unless (server-running-p)
  (server-start))
(setq browse-url-browser-function 'eww-browse-url)
(add-hook 'prog-mode-hook #'goto-address-mode)
(add-hook 'text-mode-hook #'goto-address-mode)
(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'vterm)
            (vterm)))
(setq initial-buffer-choice
      (lambda ()
        (require 'vterm)
        (vterm)
        (current-buffer)))

;; ------------------------------------------------------------
;; Search
;; ------------------------------------------------------------
(setq dabbrev-case-fold-search t)

;; ------------------------------------------------------------
;; Color Scheme
;; ------------------------------------------------------------
(load-theme 'manoj-dark t)

;; ------------------------------------------------------------
;; Font
;; ------------------------------------------------------------
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 160)

(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "PlemolJP Console"))
(set-fontset-font t 'katakana-jisx0201
                  (font-spec :family "PlemolJP Console"))

;; ------------------------------------------------------------
;; Projectile
;; ------------------------------------------------------------
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1))

;; ------------------------------------------------------------
;; Default locale / encoding
;; ------------------------------------------------------------
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; ------------------------------------------------------------
;; Appearance / UI
;; ------------------------------------------------------------
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq-default indent-tabs-mode nil)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save" user-emacs-directory) t)))

;; ------------------------------------------------------------
;; Control
;; ------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)
(setq select-enable-clipboard t)
(defun my/list-buffers-and-focus ()
  (interactive)
  (list-buffers)
  (other-window 1))
(global-set-key (kbd "C-x C-b") #'my/list-buffers-and-focus)
(defun my/open-init-file ()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c C-s") #'my/open-init-file)
(with-eval-after-load 'java-mode
  (define-key java-mode-map (kbd "C-c C-s") #'my/open-init-file))

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "<backspace>") 'delete-backward-char)

(defun move-line-up()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
(defun move-line-down()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; ------------------------------------------------------------
;; Grep
;; ------------------------------------------------------------
(setq grep-command "grep -rnH --color=auto -e ")
(defun my/grep-switch-to-results ()
  (when (eq major-mode 'grep-mode)
    (switch-to-buffer (current-buffer))))
(add-hook 'grep-mode-hook #'my/grep-switch-to-results)

;; ------------------------------------------------------------
;; Package management
;; ------------------------------------------------------------
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; ------------------------------------------------------------
;; Window movement
;; ------------------------------------------------------------
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-l") 'windmove-right)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "M-h") #'windmove-left)
  (define-key vterm-mode-map (kbd "M-j") #'windmove-down)
  (define-key vterm-mode-map (kbd "M-k") #'windmove-up)
  (define-key vterm-mode-map (kbd "M-l") #'windmove-right))

;; ------------------------------------------------------------
;; Window resize
;; ------------------------------------------------------------
(global-set-key (kbd "M-H")
                (lambda () (interactive) (shrink-window-horizontally 1)))
(global-set-key (kbd "M-L")
                (lambda () (interactive) (enlarge-window-horizontally 1)))
(global-set-key (kbd "M-J")
                (lambda () (interactive) (shrink-window 1)))
(global-set-key (kbd "M-K")
                (lambda () (interactive) (enlarge-window 1)))

(add-hook 'vterm-mode-hook
          (lambda ()
            (local-set-key (kbd "M-H")
                           (lambda () (interactive) (shrink-window-horizontally 1)))
            (local-set-key (kbd "M-L")
                           (lambda () (interactive) (enlarge-window-horizontally 1)))
            (local-set-key (kbd "M-J")
                           (lambda () (interactive) (shrink-window 1)))
            (local-set-key (kbd "M-K")
                           (lambda () (interactive) (enlarge-window 1)))))

;; ------------------------------------------------------------
;; Vterm
;; ------------------------------------------------------------
(use-package vterm
  :ensure t)
(use-package vterm
  :ensure t
  :bind (("C-c v" . vterm)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(manoj-dark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ------------------------------------------------------------
;; JDK Source
;; ------------------------------------------------------------
(defun my-jdk-java-find-file ()
  "Select a .java file under ~/workspace/git/jdk/src and open it."
  (interactive)
  (let* ((default-directory "~/workspace/git/jdk/src")
         (files (split-string
                 (shell-command-to-string
                  "find . -maxdepth 10 -type f -name \"*.java\"")
                 "\n" t))
         (completion-styles '(substring flex basic))
         (completion-ignore-case t)
         (choice (completing-read "JDK Java file: " files nil t)))
    (when choice
      (find-file (expand-file-name choice default-directory)))))

(global-set-key (kbd "C-x j") #'my-jdk-java-find-file)
