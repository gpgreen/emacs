;;; mastodon
;;;
;;; common lisp mode
(use-package mastodon
  :init
  (setq mastodon-instance-url "https://hachyderm.io"
          mastodon-active-user "waverider"))

(provide 'init-mastodon)
