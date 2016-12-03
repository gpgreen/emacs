;;; gpg-emacs --- Summary
;; ggreen@gbit-builder.com
;;
;;; Commentary:
;;
;; Packages we are using that need to be installed with emacs package manager
; magit
; flycheck
; ac-js2
; web-beautify
; java-imports
; ggtags
; realgud
; worf
; lispy
; use-package
; org-plus-contrib
; org-beautify-theme
; org-bullets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display-message-or-buffer "Loading gpg's stuff")

;; turn on column numbers
(setq column-number-mode t)

;; set the path
(setq load-path 
      (nconc
       load-path
       (list
	"~/emacs"
	"~/go/misc/emacs"
	"~/lib/erlang/lib/tools-2.6.6.4/emacs")))

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

;;;; FLYCHECK
(add-hook 'after-init-hook #'global-flycheck-mode)

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

;n mode
;; via http://emacs.stackexchange.com/questions/17327/how-to-have-c-offset-style-correctly-detect-a-java-constructor-and-change-indent
(defun my/point-in-defun-declaration-p ()
  (let ((bod (save-excursion (c-beginning-of-defun)
                             (point))))
    (<= bod
        (point)
        (save-excursion (goto-char bod)
                        (re-search-forward "{")
                        (point)))))

(defun my/is-string-concatenation-p ()
  "Returns true if the previous line is a string concatenation"
  (save-excursion
    (let ((start (point)))
      (forward-line -1)
      (if (re-search-forward " \\\+$" start t) t nil))))

(defun my/inside-java-lambda-p ()
  "Returns true if point is the first statement inside of a lambda"
  (save-excursion
    (c-beginning-of-statement-1)
    (let ((start (point)))
      (forward-line -1)
      (if (search-forward " -> {" start t) t nil))))

(defun my/trailing-paren-p ()
  "Returns true if point is a training paren and semicolon"
  (save-excursion
    (end-of-line)
    (let ((endpoint (point)))
      (beginning-of-line)
      (if (re-search-forward "[ ]*);$" endpoint t) t nil))))

(defun my/prev-line-call-with-no-args-p ()
  "Return true if the previous line is a function call with no arguments"
  (save-excursion
    (let ((start (point)))
      (forward-line -1)
      (if (re-search-forward ".($" start t) t nil))))

(defun my/arglist-cont-nonempty-indentation (arg)
  (if (my/inside-java-lambda-p)
      '+
    (if (my/is-string-concatenation-p)
        16 ;; TODO don't hard-code
      (unless (my/point-in-defun-declaration-p) '++))))

(defun my/statement-block-intro (arg)
  (if (and (c-at-statement-start-p) (my/inside-java-lambda-p)) 0 '+))

(defun my/block-close (arg)
  (if (my/inside-java-lambda-p) '- 0))

(defun my/arglist-close (arg) (if (my/trailing-paren-p) 0 '--))

(defun my/arglist-intro (arg)
  (if (my/prev-line-call-with-no-args-p) '++ 0))

(defconst intellij-java-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    ;; the following preserves Javadoc starter lines
    (c-offsets-alist
     .
     ((inline-open . 0)
      (topmost-intro-cont    . +)
      (statement-block-intro . my/statement-block-intro)
      (block-close           . my/block-close)
      (knr-argdecl-intro     . +)
      (substatement-open     . +)
      (substatement-label    . +)
      (case-label            . +)
      (label                 . +)
      (statement-case-open   . +)
      (statement-cont        . +)
      (arglist-intro         . my/arglist-intro)
      (arglist-cont-nonempty . (my/arglist-cont-nonempty-indentation c-lineup-arglist))
      (arglist-close         . my/arglist-close)
      (inexpr-class          . 0)
      (access-label          . 0)
      (inher-intro           . ++)
      (inher-cont            . ++)
      (brace-list-intro      . +)
      (func-decl-cont        . ++))))
  "Elasticsearch's Intellij Java Programming Style")

(c-add-style "intellij" intellij-java-style)
(customize-set-variable 'c-default-style
                        '((java-mode . "intellij")
                          (awk-mode . "awk")
                          (other . "gnu")))

(defun eos/setup-java ()
  (interactive)
  (define-key java-mode-map (kbd "M-,") 'pop-tag-mark)
  (define-key java-mode-map (kbd "C-c M-i") 'java-imports-add-import-dwim)
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

(defun gpg-c++-mode-hook ()
  (c-set-style "stroustrup")
  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-map [f4] 'speedbar-get-focus)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
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
			  '(("\\<Q[A-Z][A-Za-z2-3]*" . 'qt-keywords-face)))

  (setq indent-tabs-mode nil)
  ;; C++ stuff
  ;(load "c++-stuff")
)

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

;;; GO
;; needs go-mode-load.el
;; see load-path at top of file
;require 'go-mode-load)

;; set the hooks
(add-hook 'c++-mode-hook 'gpg-c++-mode-hook)
;(add-hook 'java-mode-hook 'gpg-java-mode-hook)

;; Erlang
(setq erlang-root-dir "~/lib/erlang")
(setq exec-path (cons "~/bin" exec-path))
;(require 'erlang-start)

;; Tex stuff
(setq tex-dvi-view-command "xdvi")

;;; org-mode
;; The following lines are always needed.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cy" 'pam-drill)

;; org-babel setup languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (C . t)
   (python . t)
;   (sed . t)
   (awk . t)
   (gnuplot . t)
   (latex . t)
   (sql . t)
   (css . t)
   (js . t)))

;; org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; org-drill
(require 'cl)
(require 'org-drill)

;; auto-mode stuff
(setq auto-mode-alist 
      (append '(("\\.h$" . c++-mode)
		("\\.icc$" . c++-mode)
                ("\\.mk$" . makefile-mode)
		("\\.mkd$" . makefile-mode)
		("akefile" . makefile-mode)
		("SConstruct" . python-mode)
		("SConscript" . python-mode)
		("\\.clpr.c$" . prolog-mode)
		("\\.lisp$" . lisp-mode)
		("\\.lsp$" . lisp-mode)
		("\\.cl$" . lisp-mode)
		("\\.ss$" . scheme-mode)
		("\\.scm$" . scheme-mode)
		("\\.erl$" . erlang-mode)
;		("\\.gradle$" . groovy-mode)
		) auto-mode-alist))

;; printer stuff
(setq lpr-command "lpr")

;; iswitchb
(iswitchb-mode 1)

;; gnuserv configuration
;(require 'gnuserv)
;(gnuserv-start)

;;; notmuch
(autoload 'notmuch "notmuch" "Notmuch mail" t)

;;;;;;;;;;
;; the end
