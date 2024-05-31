(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . 'projectile-command-map)
              ("C-c p" . 'projectile-command-map)))

(use-package rg
  :init
  (rg-enable-menu))

(provide 'init-projectile)
