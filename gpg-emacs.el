;;; gpg-emacs --- Summary
;;
;; ggreen@gbit-builder.com
;;
;;; Commentary:
;;
;; Packages we are using that need to be installed with Emacs package manager
;;  arduino-mode
;;  cargo
;;  company
;;  dart-mode
;;  dart-server
;;  flycheck
;;  flycheck-rust
;;  ggtags
;;  lsp-mode
;;  lsp-ui
;;  lv
;;  magit
;;  markdown-mode
;;  org
;;  org-beautify-theme
;;  org-bullets
;;  org-drill
;;  org-plus-contrib
;;  python-mode
;;  realgud
;;  rustic
;;  spinner
;;  web-beautify
;;  yasnippet
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display-message-or-buffer "Loading gpg's stuff")

;; Install use-package that we require for managing all other dependencies

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; which-key wait a second and possible continuations of key sequence are shown

(use-package which-key
  :ensure
  :init
  (which-key-mode))

;; shows a list to narrow selection

;(use-package selectrum
;  :ensure
;  :init
;  (selectrum-mode)
;  :custom
;  (completion-styles '(flex substring partial-completion)))

;; a theme

(load-theme 'leuven t)
(recentf-mode 1)
(setq recentf-max-saved-items 100
      inhibit-startup-message t
      ring-bell-function 'ignore)

(tool-bar-mode 0)

;; set the path
(add-to-list 'load-path "~/emacs")

;; overload M-x (execute-extended-command) to allow a working directory
(defun in-directory (dir)
  "Runs execute-extended-command with default-directory set to the given
directory."
  (interactive "DIn directory: ")
  (let ((default-directory dir))
    (call-interactively 'execute-extended-command)))
(global-set-key (kbd "M-x") 'in-directory)

(use-package gud
  :bind
  ;; bind keys for invoking debugger
  ("<f5>" . 'gdb))

(use-package font-switching
  :bind
  ;; set the keys for switching fonts
  ("C-<f8>" . 'font-switching-cycle-font-next)
  ("C-<f7>" . 'font-switching-cycle-font-previous))

;; Magit
(use-package magit
  :bind
  ("C-x g" . 'magit-status)
  ("C-x M-g". 'magit-dispatch-popup))

;; useful function keys
;; --------------------
;; have a look on /usr/share/emacs/site-lisp/function-keys.el
;; extensions or changes of the keymap original definitions
;; will be found in loaddefs.el.
;; (global-set-key [M-left]  'backward-word)
;; (global-set-key [M-right] 'forward-word)
;; (global-set-key [M-up]    'beginning-of-line)
;; (global-set-key [M-down]  'end-of-line)
;; (global-set-key [find]   'isearch-forward) ; Search
;; (global-set-key [select] 'set-mark-command) ; Mark

;; load the paren package and adjust it
(use-package paren
  :init
  (show-paren-mode t)
  (setq show-paren-style 'mixed))

;; Shell stuff
(defun gpg/shell-hook ()
  (setq shell-completion-fignore '("~" "#" "%"))
  (add-hook 'comint-output-filter-functions
	    'comint-watch-for-password-prompt)
  (put 'eval-expression 'disabled nil)
  (rename-uniquely)
; win32 bash shell stuff
  (if (string= system-type "windows-nt")
      (
;       (setq comint-scroll-show-maximum-output 'this)
       (setq comint-completion-addsuffix t)
       (setq comint-eol-on-send t)
       (setq w32-quote-process-args ?\")
       (put 'eval-expression 'disabled nil)
       (make-variable-buffer-local 'comint-completion-addsuffix))))
(add-hook 'shell-mode-hook 'gpg/shell-hook)

;; text mode
(defun gpg/text-mode-hook ()
  (auto-fill-mode 1)
  (setq set-fill-column 70)
  )
(add-hook 'text-mode-hook 'gpg/text-mode-hook)

;;;; FLYCHECK
(use-package flycheck :ensure)

;;; Global
(use-package global
  :hook (c-mode c++-mode java-mode))

;; python mode
(autoload 'python-mode "python-mode" "Python editing mode." t)
;; put python output into a separate frame (for stability reasons)
(setq special-display-buffer-names '("*Python*" "*Python Output*" "*Python3*"))
;; my hook into python-mode
(defun gpg/python-mode-hook()
  (define-key python-mode-map (kbd "<f7>") 'pdb)
  (if (zerop (buffer-size))
      (gpg-python-insert-new-buffer-strings))
  (setq indent-tabs-mode nil))
  (ggtags-mode 1)
(add-hook 'python-mode-hook 'gpg/python-mode-hook)

(require 'intellij-java-style)
(c-add-style "intellij" intellij-java-style)
(customize-set-variable 'c-default-style
                        '((java-mode . "intellij")
                          (awk-mode . "awk")
                          (other . "linux")))

(defun eos/setup-java ()
  (interactive)
  (define-key java-mode-map (kbd "M-,") 'pop-tag-mark)
  (define-key java-mode-map (kbd "C-c M-i") 'java-imports-add-import-dwim)
  (ggtags-mode 1)
  (c-set-style "intellij" t)
  (subword-mode 1)
  (toggle-truncate-lines 1)
  (setq-local indent-tabs-mode nil)
  ;; Generic java stuff things
  (setq-local fci-rule-column 99)
  (setq-local fill-column 140)
  (when (fboundp 'eos/turn-on-whitespace-mode)
    (whitespace-mode -1)
    (eos/turn-on-whitespace-mode))
  ;; remove the stupid company-eclim backend
  (when (boundp 'company-backends)
    (setq company-backends (delete 'company-eclim company-backends)))
  ;; hide the initial comment in the file (usually a license) if hs-minor-mode
  ;; is enabled
  (when (boundp' hs-minor-mode)
    (hs-hide-initial-comment-block)))

(add-hook 'java-mode-hook #'eos/setup-java)

;; Make emacs' compile recognize broken gradle output
;;(require 'compile)
;;(add-to-list 'compilation-error-regexp-alist
;;             '("^:[^/.\n]+\\(/.+\\):\\([[:digit:]]+\\):" 1 2))

(use-package java-imports
  :ensure t
  :config
  ;; Elasticsearch's import style
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
  (add-hook 'java-mode-hook 'java-imports-scan-file))

;; c-mode
;;
(defun gpg/c-mode-hook ()
  (ggtags-mode 1)
  (c-set-style "stroustrup" t)
  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-map [f4] 'speedbar-get-focus)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-hook 'gpg/c-mode-hook)

;; c++-mode
;;
(defun gpg/c++-mode-hook ()
  (ggtags-mode 1)
  (c-set-style "stroustrup" t)
  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-map [f4] 'speedbar-get-focus)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))
(add-hook 'c++-mode-hook 'gpg/c++-mode-hook)

;; javascript

(use-package js2-mode
  :bind (:map js2-mode-map
	      ("C-c b" . web-beautify-js))
  :config
  (add-hook 'js2-mode-hook 'gpg/js-mode-hook))
  
(defun gpg/js-mode-hook ()
  (setq indent-tabs-mode nil))

;(add-hook 'js-mode-hook 'gpg-js-mode-hook)
;(add-hook 'js-mode-hook 'js2-minor-mode)
;(add-hook 'js2-mode-hook 'ac-js2-mode)

;(eval-after-load 'js2-mode
;  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;(eval-after-load 'json-mode
;  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
;(eval-after-load 'sgml-mode
;  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
;(eval-after-load 'css-mode
;  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;; Erlang
(setq erlang-root-dir "~/lib/erlang")
(setq exec-path (cons "~/bin" exec-path))
;(require 'erlang-start)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rustic = basic rust-mode + additions

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'gpg/rustic-mode-hook))

(defun gpg/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors

(use-package flycheck :ensure)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

(use-package yasnippet
  :ensure
  :config
  (setq yas-snippet-dirs
	'("~/emacs/snippets"))
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :ensure
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode :ensure)

;;; org-mode
(defun gpg/org-mode-hook ()
  ;; org-bullets
  (setq org-use-property-inheritance t)
  (org-bullets-mode 1))
(add-hook 'org-mode-hook 'gpg/org-mode-hook)

;; org-babel setup languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (C . t)
   (python . t)
   (sed . t)
   (awk . t)
   (gnuplot . t)
   (latex . t)
   (sql . t)
   (css . t)
   (js . t)))

;; org-drill
(require 'cl)
(require 'org-drill)

;; auto-mode stuff
(setq auto-mode-alist 
      (append '(("\\.icc$" . c++-mode)
		("\\.ino$" . c++-mode)
                ("\\.mk$" . makefile-mode)
		("\\.mkd$" . makefile-mode)
		) auto-mode-alist))

;; iswitchb
(iswitchb-mode 1)

;; gnuserv configuration
;(require 'gnuserv)
;(gnuserv-start)

;;; notmuch
(autoload 'notmuch "notmuch" "Notmuch mail" t)
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
;; otherwise it tries to send through OS associated mail client
(setq message-send-mail-function 'message-send-mail-with-sendmail)
;; we substitute sendmail with msmtp
(setq sendmail-program "msmtp")
;;need to tell msmtp which account we're using
(setq message-sendmail-extra-arguments '("-a" "eskimo"))
;; you might want to set the following too
(setq mail-host-address "bit-builder.com")
(setq user-full-name "Greg Green")
(setq user-mail-address "ggreen@bit-builder.com")

;;; binary diff
(load "binary-diff")

;;; beancount
(use-package beancount
  :init
  (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode)))

;;;;;;;;;;
;; the end
