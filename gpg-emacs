;; $Id: .gnu-emacs,v 1.11 2004/09/06 21:03:10 ggreen Exp $

;; Set Font Lock Mode
(global-font-lock-mode t)
; Some new Colors for Font-lock.
;(setq font-lock-support-mode 'lazy-lock-mode)
(setq font-lock-mode-maximum-decoration t)
(require 'font-lock)
(setq font-lock-use-default-fonts nil)
(setq font-lock-use-default-colors nil)
(copy-face 'default 'font-lock-string-face)
(set-face-foreground 'font-lock-string-face "Sienna")
(copy-face 'italic 'font-lock-comment-face)
(set-face-foreground 'font-lock-comment-face "Red")
(copy-face 'bold 'font-lock-function-name-face)
(set-face-foreground 'font-lock-function-name-face "MediumBlue")
(copy-face 'default 'font-lock-keyword-face)
(set-face-foreground 'font-lock-keyword-face "SteelBlue")
(copy-face 'default 'font-lock-type-face)
(set-face-foreground 'font-lock-type-face "DarkOliveGreen")
(set-face-foreground 'modeline "red")
(set-face-background 'modeline "lemonchiffon")
(setq transient-mark-mode 't)
(make-face-bold 'bold-italic)
(set-face-foreground 'bold-italic "Blue")

;; default geometry
(setq default-frame-alist
      '(
;	(width . 80) (height . 50)
;	(cursor-color . "Ivory")
	(cursor-type . box)
	(foreground-color . "Black")
	(background-color . "Ivory")))

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
	"~/emacs/jdee-2.4.1/lisp"
	"~/emacs/jdibug-0.5"
	"~/go/misc/emacs")))
;	"~/lib/erlang/lib/tools-2.6.6.4/emacs")))
;	"~/emacs/ilisp-5.12.0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CEDET
;; Load CEDET
(load-file "~/emacs/cedet-1.1/common/cedet.el")

;; Enable EDE (Project Management) features
;(global-ede-mode 1)

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")

;(ede-java-root-project "just-sample"
;               :file "/Users/zeyuec/workspace/android/just-sample/build.xml"
;               :srcroot '("src"
;                          "../third-party/library/src")
;               :localclasspath '("libs/third.party.jar")
;               :classpath '("/Users/zeyuec/bin/android-sdk-macosx/platforms/android-10/android.jar"))

;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
(semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberent ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languges only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)

;; 4. enable auto-complete
(require 'semantic-ia)
(defun my-cedet-hook ()
  ;; functions which are disabled
  ;; (local-set-key "\C-cp" 'semantic-ia-show-summary)
  ;; (local-set-key "\C-cl" 'semantic-ia-show-doc)
  ;; (local-set-key "." 'semantic-complete-self-insert)
  ;; (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\M-n" 'semantic-ia-complete-symbol-menu)  ;; auto completet by menu
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cR" 'semantic-symref) 
  (local-set-key "\C-cr" 'semantic-symref-symbol)  
  (local-set-key [f2] 'jde-ant-build)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
)
(add-hook 'c-mode-common-hook 'my-cedet-hook)

(require 'jde)
(require 'jdibug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; ; no tabs forever more
(setq-default indent-tabs-mode nil)
;; ;; tabs are 4 wide
(setq tab-width 4)

;; lets show column numbers, shall we?
(setq column-number-mode t)

;; Shell stuff
;(setq explicit-shell-file-name "c:/cygwin/bin/bash.exe")
;(setq shell-file-name "c:/cygwin/bin/bash.exe")
(defun gpg-shell-setup ()
  (setq shell-completion-fignore '("~" "#" "%"))
  (add-hook 'comint-output-filter-functions
	    'comint-watch-for-password-prompt)
  (put 'eval-expression 'disabled nil)
  (rename-uniquely)
)
(add-hook 'shell-mode-hook 'gpg-shell-setup)

;; text mode
(defun gpg-text-mode-hook ()
  (auto-fill-mode 1)
  (setq set-fill-column 70)
)
(add-hook 'text-mode-hook 'gpg-text-mode-hook)

;(setq jedi:setup-keys t)

;; python mode
;(require 'epc)
;(add-hook 'python-mode-hook 'jedi:setup)
(autoload 'python-mode "python-mode" "Python editing mode." t)
;; put python output into a separate frame (for stability reasons)
(setq special-display-buffer-names '("*Python*" "*Python Output*"))
;; my hook into python-mode
(defun gpg-python-mode-hook()
;  (load "gpg-python-stuff")
;  (load "gpg-python-template")
;  (define-key py-mode-map [f3] 'gpg-python-find-module)
;  (define-key py-mode-map [f4] 'speedbar-get-focus)
;  (define-key py-mode-map [f5] 'gpg-python-insert-new-buffer-strings)
;  (define-key py-mode-map [f6] 'gpg-python-run-current-buffer)
  (define-key py-mode-map [f7] 'pdb)
  (setq indent-tab-mode nil)
  (setq tab-width 4)
  (setq py-python-command "c:\\python27\\scripts\\ipython.exe")
;  (if (zerop (buffer-size)) 
;      (gpg-python-insert-new-buffer-strings))
)
(add-hook 'python-mode-hook 'gpg-python-mode-hook)

;; Tcl stuff
(defun my-tcl-mode-hook ()
  ;; slate keywords
  (make-face 'sl8-keywords-face)
  (set-face-foreground 'sl8-keywords-face "deeppink2")
  (font-lock-add-keywords 'tcl-mode
			  '(("sl8[A-Za-z]*" . 'sl8-keywords-face)))
  (setq indent-tab-mode nil)
  (setq tab-width 4))
(add-hook 'tcl-mode-hook 'my-tcl-mode-hook)

;; make C-mode customizations
(defun gpg-c-mode-common-hook ()
  (c-set-style "stroustrup")
  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-map [f4] 'speedbar-get-focus)
  (setq c-basic-offset 4)
  (setq tab-width 4))

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

;;; GO
;; needs go-mode-load.el
;; see load-path at top of file
(require 'go-mode-load)

;; set the hooks
(add-hook 'c-mode-common-hook 'gpg-c-mode-common-hook)
(add-hook 'c++-mode-hook 'gpg-c++-mode-hook)

;; Erlang
;(setq erlang-root-dir "~/lib/erlang")
;(setq exec-path (cons "~/bin" exec-path))
;(require 'erlang-start)

;; Tex stuff
(setq tex-dvi-view-command "xdvi")

;; auto-mode stuff
(setq auto-mode-alist 
      (append '(("\\.h$" . c++-mode)
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
;		("\\.java$" . java-mode)
		("\\.erl$" . erlang-mode)
		) auto-mode-alist))

;; printer stuff
(setq lpr-command "lpr")
;(setq ps-lpr-command "c:\\Program Files\\Ghostgum\\gsview\\gsprint.exe")
;(setq ps-lpr-switches '("-query"))
;(setq ps-printer-name t)

;; iswitchb
(iswitchb-mode 1)
;(iswitchb-default-keybindings)

;; gnuserv configuration
;(require 'gnuserv)
;(gnuserv-start)

;;;;;;;;;;
;; the end
