;; interface customizations

;; (if (display-graphic-p)
;; )
;(menu-bar-mode -1)
(tool-bar-mode 0)
(line-number-mode 1)
;(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq compilation-scroll-output t)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; ask for 'y' or 'n' instead of the long form
(fset 'yes-or-no-p 'y-or-n-p)

;; show parenthesis
(use-package paren
  :init
  (show-paren-mode t)
  (setq show-paren-style 'mixed))

;; https://github.com/Malabarba/beacon
;; Whenever the window scrolls a light will shine on top of your cursor so you know where it is
(use-package beacon
  :config
  (beacon-mode 1))

;; (use-package page-break-lines
;;   :config
;;   (global-page-break-lines-mode))

;; spruces up modes with icons
;; icon packages must be installed using
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; add useful stuff to opening emacs buffer
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5))))

(provide 'init-ui-customizations)
