;;; init.el --- Initialization file for Emacs

;;; Commentary:
;; Emacs Startup File --- initialization for Emacs


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;(require 'package)

;;; Code:

;;; (setq debug-on-error t)
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Setup straight package manager, this installs the manager on first time run
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'use-package)

;; configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; where to find all the init file lisp code
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; -- TREE-SITTER --
;; Check for non-nil environment variable to see if tree-sitter should be used
;; ---------------------------------------------------------------------------
(setq ggreen-use-tree-sitter-modes (getenv "EMACS_USE_TREESITTER_MODES"))

;; load up all the init file code
(require 'init-common)
(require 'init-beancount)
(require 'init-fontaine)
(require 'init-font-switching)
(require 'init-js)
(require 'init-magit)
(require 'init-markdown)
(require 'init-mastodon)
(require 'init-notmuch)
(require 'init-nov)
(require 'init-org-mode)
(require 'init-proced)
(require 'init-projectile)
(require 'init-scad-mode)
(require 'init-shx)
(require 'init-slime)
(require 'init-themes)
(require 'init-ui-customizations)
(require 'init-web-mode)
(require 'init-yasnippet)
(if ggreen-use-tree-sitter-modes
    (progn
     (require 'init-treesitter)
     (require 'init-rust-ts-mode))
  (require 'init-rust-mode))
