(use-package scad-mode
  :config
  (add-hook 'scad-mode-hook
            '(lambda ()
               (setq tab-width 4)
               (c-set-style "k&r"))))

(provide 'init-scad-mode)
