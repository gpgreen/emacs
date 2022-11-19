(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Garbage autosave and backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/saves" t)))

;; emacs package linter
(use-package package-lint)

;; http://company-mode.github.io/
(use-package company
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  :config
  (global-company-mode)
  (setq company-tooltip-align-annotations t))

;; (use-package dumb-jump
;;   :bind (("M-g o" . dumb-jump-go-other-window)
;;          ("M-g j" . dumb-jump-go)
;;          ("M-g b" . dumb-jump-back)
;;          ("M-g i" . dumb-jump-go-prompt)
;;          ("M-g x" . dumb-jump-go-prefer-external)
;;          ("M-g z" . dumb-jump-go-prefer-external-other-window))
;;   :config
;;   (setq dumb-jump-selector 'ivy)
;;   (dumb-jump-mode))


(defun truncate-compilation-mode-hook ()
  (setq truncate-lines nil)
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook 'compilation-mode-hook 'truncate-compilation-mode-hook)


;; InteractivelyDoThings (replacement for iswitchb)
(use-package ido
  :init
  (ido-mode t))

;; show parenthesis
(use-package paren
  :init
  (show-paren-mode t)
  (setq show-paren-style 'mixed))

;; define the word at point without a browser
(use-package define-word
  :config
  (global-set-key (kbd "C-c e q") 'define-word-at-point))

;; ensure environment variables inside emacs look the same as in shell
;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; open files as other user
(use-package sudo-edit)

;; Shared clipboard between terminal emacs and x11. xclip should be installed
(use-package xclip
  :config
  (xclip-mode 1))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package toml-mode)

(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (setq tab-width 4)))
  (add-hook 'c-mode-hook
            '(lambda ()
               (c-toggle-auto-hungry-state 1)
               (setq c-basic-offset 4)
               (c-set-style "stroustrup")))
  (add-hook 'c++-mode-hook
            '(lambda ()
               (c-toggle-auto-hungry-state 1)
               (setq c-basic-offset 4)
               (c-set-style "stroustrup"))))

(provide 'init-common)
