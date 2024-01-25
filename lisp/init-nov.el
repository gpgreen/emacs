(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :mode ("\\.epub\\'" . nov-mode))

(provide 'init-nov)
