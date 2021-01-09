;;; gpg-emacs --- Summary
;;
;; ggreen@gbit-builder.com
;;
;;; Commentary:
;;
;; Packages we are using that need to be installed with Emacs package manager
;;  cargo
;;  company
;;  dart-mode
;;  dart-server
;;  flycheck
;;  flycheck-rust
;;  ggtags
;;  magit
;;  org
;;  org-beautify-theme
;;  org-bullets
;;  org-drill
;;  org-plus-contrib
;;  python-mode
;;  racer
;;  realgud
;;  rust-mode
;;  use-package
;;  web-beautify
;;
;; Need cedet installed separately
;;  on windows use:
;;  unzip cedet.zip somewhere
;;  execute build:
;;    cd cedet-1.0pre6
;;    emacs -Q -l cedet-build.el -f cedet-build
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display-message-or-buffer "Loading gpg's stuff")

;; set the path
(add-to-list 'load-path "~/emacs")

;; setup 'use-package', this will lazy load packages when used
(eval-when-compile
  (require 'use-package))

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; useful function keys
;; --------------------
;; have a look on /usr/share/emacs/site-lisp/function-keys.el
;; extensions or changes of the keymap original definitions
;; will be found in loaddefs.el.
(global-set-key [M-left]  'backward-word)
(global-set-key [M-right] 'forward-word)
(global-set-key [M-up]    'beginning-of-line)
(global-set-key [M-down]  'end-of-line)
(global-set-key [find]   'isearch-forward) ; Search
(global-set-key [select] 'set-mark-command) ; Mark

;; load the paren package and adjust it
(load "paren")
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Shell stuff
(defun gpg-shell-setup ()
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
(add-hook 'shell-mode-hook 'gpg-shell-setup)

;; text mode
(defun gpg-text-mode-hook ()
  (auto-fill-mode 1)
  (setq set-fill-column 70)
  )
(add-hook 'text-mode-hook 'gpg-text-mode-hook)

;;;; FLYCHECK
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; Global
(use-package global
  :hook (c-mode c++-mode java-mode))

;; python mode
(autoload 'python-mode "python-mode" "Python editing mode." t)
;; put python output into a separate frame (for stability reasons)
(setq special-display-buffer-names '("*Python*" "*Python Output*" "*Python3*"))
;; my hook into python-mode
(defun gpg-python-mode-hook()
  (define-key python-mode-map (kbd "<f7>") 'pdb)
  (if (zerop (buffer-size))
      (gpg-python-insert-new-buffer-strings))
  (setq indent-tabs-mode nil))
(add-hook 'python-mode-hook 'gpg-python-mode-hook)

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
(require 'compile)
(add-to-list 'compilation-error-regexp-alist
             '("^:[^/.\n]+\\(/.+\\):\\([[:digit:]]+\\):" 1 2))

(use-package java-imports
  :ensure t
  :config
  ;; Elasticsearch's import style
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
  (add-hook 'java-mode-hook 'java-imports-scan-file))

;; c-mode
;;
(defun gpg-c-mode-hook ()
  (ggtags-mode 1)
  (c-set-style "stroustrup" t)
  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-map [f4] 'speedbar-get-focus)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-hook 'gpg-c-mode-hook)

;; c++-mode
;;
(defun gpg-c++-mode-hook ()
  (ggtags-mode 1)
  (c-set-style "stroustrup" t)
  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-map [f4] 'speedbar-get-focus)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))
(add-hook 'c++-mode-hook 'gpg-c++-mode-hook)

;; javascript
(defun gpg-js-mode-hook ()
  (setq indent-tabs-mode nil))

(add-hook 'js-mode-hook 'gpg-js-mode-hook)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
;(eval-after-load 'sgml-mode
;  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

;; Erlang
(setq erlang-root-dir "~/lib/erlang")
(setq exec-path (cons "~/bin" exec-path))
;(require 'erlang-start)

;;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path "/home/ggreen/src/rust/src")
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;; org-mode
(defun org-mode-gpg-setup ()
  ;; org-bullets
  (setq org-use-property-inheritance t)
  (org-bullets-mode 1))
(add-hook 'org-mode-hook 'org-mode-gpg-setup)

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

(require 'font-switching)
;; set the keys for switching fonts
(global-set-key (kbd "<C-f8>") 'font-switching-cycle-font-next)
(global-set-key (kbd "<C-f7>") 'font-switching-cycle-font-previous)

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
(use-package beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

;;;;;;;;;;
;; the end
