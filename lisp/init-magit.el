(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch)
  (global-set-key (kbd "C-c g") 'magit-file-dispatch)
  (setq global-magit-file-mode 1))

(provide 'init-magit)
