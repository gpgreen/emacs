;; $Id: .gnu-emacs,v 1.11 2004/09/06 21:03:10 ggreen Exp $

;;; LOCATION SWITCHER MACRO
;;; macro to switch between different working locations
;;; first form is at home, second is at work
;;; If the file "~/.emacs-home" exists, we are at home
(defmacro switch-location (home work)
  (list 'if 
	(file-exists-p "~/.emacs-home")
	(cons 'progn home)
	(cons 'progn work)))

;; Some macros
;(defmacro Xlaunch (&rest x)
;  (list 'if (eq window-system 'x)(cons 'progn x)))

;; Set Font Lock Mode
(global-font-lock-mode t)
; maximize font lock mode
(setq font-lock-mode-maximum-decoration t)
;(require 'font-lock)
;(setq font-lock-use-default-fonts nil)
;(setq font-lock-use-default-colors nil)
;(copy-face 'default 'font-lock-string-face)
; builtin
; comment-delimiter
; comment
; constant
; doc
; function-name
; keyword
; negation-char
; preprocessor
; regexp-grouping-backslash
; regexp-grouping-construct
; string
; type
; variable-name
; warning
;(set-face-foreground 'font-lock-string-face "Sienna")
;(copy-face 'italic 'font-lock-comment-face)
;(set-face-foreground 'font-lock-comment-face "Red")
;(copy-face 'bold 'font-lock-function-name-face)
;(set-face-foreground 'font-lock-function-name-face "MediumBlue")
;(copy-face 'default 'font-lock-keyword-face)
;(set-face-foreground 'font-lock-keyword-face "SteelBlue")
;(copy-face 'default 'font-lock-type-face)
;(set-face-foreground 'font-lock-type-face "DarkOliveGreen")

;;; some colors
(set-face-foreground 'mode-line "red")
(set-face-background 'mode-line "lemonchiffon")
(setq transient-mark-mode 't)
;(Xlaunch (make-face-bold 'bold-italic))
;(set-face-foreground 'bold-italic "Blue")

;; default geometry
(setq default-frame-alist
      '(
	(width . 80) (height . 50)
;	(cursor-color . "Ivory")
;	(cursor-type . box)
;	(foreground-color . "Black")
;	(background-color . "Ivory")
))

;; turn on column numbers
(setq column-number-mode t)

;; Window stuff
(setq special-display-frame-alist
      '((height . 15) (width . 81) (unsplittable . nil))
)

;; set the path
(setq load-path 
      (nconc
       load-path
       (list
	"~/emacs"
;	"~/emacs/html-helper-mode"
	"~/go/misc/emacs"
	"~/lib/erlang/lib/tools-2.6.6.4/emacs")))
;	"~/emacs/ilisp-5.12.0")))

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

;(load "random_back")

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

;; load html-helper-mode
;(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;(setq html-helper-do-write-file-hooks t)
;(setq html-helper-build-new-buffer t)
;(switch-location
; ((setq html-helper-address-string
;       "<a href=\"mailto:ggreen@bit-builder.com\">Greg Green &lt;ggreen@bit-builder.com&gt;</a>"))
; ((setq html-helper-address-string 
;       "<a href=\"mailto:gregory.p.green@boeing.com\">Greg Green &lt;gregory.p.green@boeing.com&gt;</a>")))

;;; CEDET
;; Load CEDET
;(load-file "~/emacs/cedet-1.0beta3b/common/cedet.el")
;; Enabling SEMANTIC minor modes. See semantic/INSTALL for more ideas.
;(semantic-load-enable-code-helpers)

;; python mode
(autoload 'python-mode "python-mode" "Python editing mode." t)
;; put python output into a separate frame (for stability reasons)
(setq special-display-buffer-names '("*Python*" "*Python Output*"))
;; my hook into python-mode
(defun gpg-python-mode-hook()
  (load "gpg-python-stuff")
  (load "gpg-python-template")
  (define-key py-mode-map [f3] 'gpg-python-find-module)
  (define-key py-mode-map [f4] 'speedbar-get-focus)
  (define-key py-mode-map [f5] 'gpg-python-insert-new-buffer-strings)
  (define-key py-mode-map [f6] 'gpg-python-run-current-buffer)
  (define-key py-mode-map [f7] 'pdb)
;  (if (zerop (buffer-size))
;      (gpg-python-insert-new-buffer-strings))
  (setq indent-tabs-mode nil)
)
(add-hook 'python-mode-hook 'gpg-python-mode-hook)

;; make C-mode customizations
(defun gpg-c-mode-common-hook ()
  (c-set-style "stroustrup")
  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-map [f4] 'speedbar-get-focus)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode t))

(defun gpg-c++-mode-hook ()
  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords (one line)
  (setq c-C++-access-key "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\)\\>[ \t]*:")
  ;; modify the colour of slots to match public, private, etc ...
  (font-lock-add-keywords 'c++-mode
			  '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
  ;; make new font for rest of qt keywords
  (make-face 'qt-keywords-face)
  (set-face-foreground 'qt-keywords-face "deeppink2")
  ;; qt keywords
  (font-lock-add-keywords 'c++-mode
			  '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode
			  '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
  (font-lock-add-keywords 'c++-mode
			  '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))

  ;; C++ stuff
  ;(load "c++-stuff")
)

;(require 'jde)
;(defun gpg-java-mode-hook ()
  ;; jde
;  (setq jde-compile-option-command-line-args "-deprecation")
;  (setq jde-compile-option-debug (quote ("all" (t t t))))
; (setq jde-global-classpath (quote ("/usr/home1/gpgreen/java:/usr/local/java/postgresql.jar:/usr/local/java/jdk1.1.7/lib/classes.zip:/usr/local/java/junit2.1/junit.jar"))))
;  (setq jde-jdk-doc-url "file:/usr/java/webdocs/api/packages.html"))
  
;;; GO
;; needs go-mode-load.el
;; see load-path at top of file
;require 'go-mode-load)

;; compile command
;(switch-location
; ((setq compile-command "make"))
; ((setq compile-command "pmake -f RtMakefile")))

;; set the hooks
(add-hook 'c-mode-common-hook 'gpg-c-mode-common-hook)
(add-hook 'c++-mode-hook 'gpg-c++-mode-hook)
;(add-hook 'java-mode-hook 'gpg-java-mode-hook)

;;; LISP
;; lisp mode hook
;(require 'ilisp)

(defun gpg-ilisp-load-hook ()
  ;(setq ilisp-*use-fsf-compliant-keybindings* t)
  ;;; Configuration of Erik Naggum's HyperSpec access package.
  (setq common-lisp-hyperspec-root
	"file:/usr/local/share/lisp/HyperSpec/")
  ;; CLISP (Bruno Haible and Michael Stoll)
  (autoload 'clisp-hs   "ilisp" "Inferior Haible/Stoll CLISP Common Lisp." t)
  (setq clisp-hs-program "clisp -I"))
;; set the hooks
;(add-hook 'ilisp-load-hook 'gpg-ilisp-load-hook)

;; Erlang
(setq erlang-root-dir "~/lib/erlang")
(setq exec-path (cons "~/bin" exec-path))
;(require 'erlang-start)

;; Tex stuff
(setq tex-dvi-view-command "xdvi")

;; auto-mode stuff
(setq auto-mode-alist 
      (append '(("\\.html$" . html-helper-mode)
		("\\.h$" . c++-mode)
		("\\.icc$" . c++-mode)
                ("\\.mk$" . makefile-mode)
		("\\.mkd$" . makefile-mode)
		("akefile" . makefile-mode)
		("\\.py$" . python-mode)
		("\\.pyw$" . python-mode)
		("SConstruct" . python-mode)
		("SConscript" . python-mode)
		("\\.clpr.c$" . prolog-mode)
		("\\.lisp$" . lisp-mode)
		("\\.lsp$" . lisp-mode)
		("\\.cl$" . lisp-mode)
		("\\.ss$" . scheme-mode)
		("\\.scm$" . scheme-mode)
		("\\.java$" . java-mode)
		("\\.erl$" . erlang-mode)
		) auto-mode-alist))

;; printer stuff
(setq lpr-command "lpr")

;; iswitchb
(iswitchb-mode 1)
;(iswitchb-default-keybindings)

;; gnuserv configuration
;(require 'gnuserv)
;(gnuserv-start)

;;;;;;;;;;
;; the end
