;;; slime
;;;
;;; common lisp mode
(use-package slime
  :init
  (setq inferior-lisp-program "/usr/local/bin/clisp"))

(provide 'init-slime)
