(use-package beancount
  :straight
  (beancount
   :type git
   :host github
   :repo "beancount/beancount-mode")
  :hook ((beancount-mode-hook outline-minor-mode)
         (beancount-mode-hook flymake-bean-check-enable))
  :init
  (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode)))

(provide 'init-beancount)
