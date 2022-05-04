(use-package beancount
  :straight
  (beancount
   :type git
   :host github
   :repo "beancount/beancount-mode")
  :config
  (add-hook 'beancount-mode-hook
            (lambda () (setq-local electric-indent-chars nil)))
  (add-hook 'beancount-mode-hook #'outline-minor-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode)))

(provide 'init-beancount)
