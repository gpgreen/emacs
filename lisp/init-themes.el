(use-package leuven-theme
  :config
  (global-set-key (kbd "C-c q 1") (lambda() (interactive) (load-theme 'leuven t)))
  (load-theme 'leuven t))

(use-package solarized-theme
  :config
  (global-set-key (kbd "C-c q 2") (lambda() (interactive) (load-theme 'solarized-light t)))
  (global-set-key (kbd "C-c q 3") (lambda() (interactive) (load-theme 'solarized-dark t))))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light)
  (sml/setup))

(use-package rich-minority
  :config
  (setq rm-blacklist
      (format "^ \\(%s\\)$"
              (mapconcat #'identity
                         '("Fly.*" "Projectile.*" "NoMouse.*" "ivy.*" "company.*" "ARev.*" "Org-roam.*")
                        "\\|"))))

(provide 'init-themes)
