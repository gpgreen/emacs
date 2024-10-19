;; interface customizations

;; (if (display-graphic-p)
;; )
;(menu-bar-mode -1)
(tool-bar-mode 0)
(line-number-mode 1)
;(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(if (window-system)
    (progn
      (set-frame-width (selected-frame) 320)
      (set-frame-height (selected-frame) 80)))
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
  (dashboard-setup-startup-hook) ;; necessary
  (setq dashboard-set-navigator t)
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-icon-type 'all-the-icons) ;; use 'all-the-icons' package
  ;; (setq dashboard-icon-type 'nerd-icons) ;; use 'nerd-icons' package
  ;; (setq dashboard-set-heading-icons t) ;; add icons to widget headings
  (setq dashboard-set-file-icons t) ;; add icons to widget items
  ;; a custom widget
  (defun dashboard-insert-custom (list-size)
    (insert "Custom text"))
  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
                                        ;  (add-to-list 'dashboard-items '(custom) t)
  ;; set the widgets to each show 5 items
  (setq dashboard-items '((projects . 5)
                          (recents  . 5)
                          (agenda . 5)
                          (custom . 5)
                          (bookmarks . 5)
                          (registers . 5))))

(provide 'init-ui-customizations)
