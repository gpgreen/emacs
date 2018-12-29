;;; gpg-emacs --- Summary
;;
;; ggreen@gbit-builder.com
;;
;;; Commentary:
;;
;; Packages we are using that need to be installed with Emacs package manager
;;  ac-js2
;;  cargo
;;  company
;;  company-racer
;;  dart-mode
;;  flycheck
;;  flycheck-rust
;;  ggtags
;;  java-imports
;;  lispy
;;  magit
;;  ob-dart
;;  ob-rust
;;  org-beautify-theme
;;  org-bullets
;;  org-plus-contrib
;;  python-mode
;;  racer
;;  realgud
;;  rust-mode
;;  use-package
;;  web-beautify
;;  worf
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

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: Tou must place this *before* any CEDET component
;; gets activated by another package (Gnus, auth-source, ...).
(setq cedet-root-path
      (cond
       ((string-equal system-type "windows-nt")
	(file-name-as-directory "c:/src/cedet/"))
       ((string-equal system-type "gnu/linux")
	(file-name-as-directory "~/src/cedet/"))))
(load-file (concat cedet-root-path "cedet-devel-load.el"))
(load-file (concat cedet-root-path "contrib/cedet-contrib-load.el"))

;; select which submodes we want to activate
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)

;; Enable Semantic
(semantic-mode 1)

;; load contrib library
(require 'eassist)

;; advanced functionality for name completion
(require 'semantic/ia)

;; system header files via gcc
(require 'semantic/bovine/gcc)

;; turn on column numbers
(setq column-number-mode t)

;; customization of modes
(defun alexott/cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )
(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'scheme-mode-hook 'alexott/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'erlang-mode-hook 'alexott/cedet-hook)

(defun alexott/c-mode-cedet-hook ()
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)

(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))

;; SRecode
(global-srecode-minor-mode 1)

;; Enable EDE (Project Management) features
(global-ede-mode 1)
;(ede-enable-generic-projects)

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
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Global
(use-package global
  :hook (c-mode c++-mode java-mode))

;; python mode
(autoload 'python-mode "python-mode" "Python editing mode." t)
;; put python output into a separate frame (for stability reasons)
(setq special-display-buffer-names '("*Python*" "*Python Output*"))
;; my hook into python-mode
(defun gpg-python-mode-hook()
  (define-key py-mode-map [f7] 'pdb)
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
   (dart . t)
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

;;; binary diff
(load "binary-diff")

;; EDE projects
(if (file-readable-p "~/emacs/ede-projects.el")
    (load "~/emacs/ede-projects.el"))

;;;;;;;;;;
;; the end
