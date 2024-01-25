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

;; load up all the init file code
(require 'init-common)
(require 'init-beancount)
(require 'init-fontaine)
(require 'init-font-switching)
(require 'init-js)
(require 'init-magit)
(require 'init-markdown)
(require 'init-notmuch)
(require 'init-nov)
(require 'init-org-mode)
(require 'init-proced)
(require 'init-projectile)
(require 'init-realgud)
(require 'init-rust-mode)
(require 'init-scad-mode)
(require 'init-shx)
(require 'init-slime)
(require 'init-themes)
(require 'init-treemacs)
;(require 'init-treesitter)
(require 'init-ui-customizations)
(require 'init-web-mode)
(require 'init-yasnippet)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key
            [105])
     (:name "unread" :query "tag:unread" :key
            [117])
     (:name "flagged" :query "tag:flagged" :key
            [102])
     (:name "sent" :query "tag:sent" :key
            [116])
     (:name "drafts" :query "tag:draft" :key
            [100])
     (:name "all mail" :query "*" :key
            [97])))
 '(smtpmail-smtp-server "mail.eskimo.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
