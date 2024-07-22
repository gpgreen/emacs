(use-package fish-mode
  :straight
  (fish-mode
   :type git
   :host github
   :repo "wwwjfy/emacs-fish")
  :hook
  (fish-mode-hook . (lambda ()
                      (add-hook 'before-save-hook) 'fish_indent-before-save)))

(provide 'init-fish)
