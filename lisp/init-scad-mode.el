(use-package scad-mode
  :config
  (add-hook 'scad-mode-hook
            '(lambda ()
               (c-toggle-auto-hungry-state 1)
               (setq c-basic-offset 4)
               (c-set-style "stroustrup"))))

(provide 'init-scad-mode)
