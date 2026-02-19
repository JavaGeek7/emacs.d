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
;; LSP(Java)
;; ------------------------------------------------------------

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  ((java-mode . lsp))
  :custom
  (lsp-keymap-prefix "C-c l"))

(use-package lsp-java
  :after lsp-mode
  :hook (java-mode . lsp))

(with-eval-after-load 'lsp-java
  (add-hook 'java-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-format-buffer nil t)
              (add-hook 'before-save-hook #'lsp-organize-imports nil t))))

(with-eval-after-load 'lsp-java
  (setq lsp-java-completion-filtered-types []))

;; ------------------------------------------------------------
;; organize-imports-java
;; ------------------------------------------------------------
(use-package organize-imports-java
  :after java-mode
  :config
  (define-key java-mode-map (kbd "C-S-o")
    #'organize-imports-java-do-imports))

;; ------------------------------------------------------------
;; LSP UI / Completion / Diagnostics
;; ------------------------------------------------------------

;; UI: doc popup, sideline, etc.
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-enable t)
  (lsp-ui-sideline-enable t))

;; Completion frontend
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1))

;; Diagnostics
(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(manoj-dark))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ------------------------------------------------------------
;; Window movement
;; ------------------------------------------------------------
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-l") 'windmove-right)

;; Window resize
(global-set-key (kbd "M-H")
                (lambda () (interactive) (shrink-window-horizontally 5)))
(global-set-key (kbd "M-L")
                (lambda () (interactive) (enlarge-window-horizontally 5)))
(global-set-key (kbd "M-J")
                (lambda () (interactive) (shrink-window 5)))
(global-set-key (kbd "M-K")
                (lambda () (interactive) (enlarge-window 5)))

;; ------------------------------------------------------------
;; TAGS
;; ------------------------------------------------------------
(setq tags-revert-without-query t)

(defun jni-auto-visit-tags ()
  (let* ((root (or (projectile-project-root)
                   (vc-root-dir)
                   default-directory))
         (tags-file (expand-file-name "TAGS" root)))
    (when (file-exists-p tags-file)
      (visit-tags-table tags-file))))

(add-hook 'java-mode-hook #'jni-auto-visit-tags)
(add-hook 'c-mode-hook    #'jni-auto-visit-tags)

;; ------------------------------------------------------------
;; Vterm
;; ------------------------------------------------------------
(use-package vterm
  :ensure t)
(use-package vterm
  :ensure t
  :bind (("C-c v" . vterm)))
