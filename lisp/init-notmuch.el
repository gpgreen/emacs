(use-package notmuch
  :config
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  ;; otherwise it tries to send through OS associated mail client
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  ;; we substitute sendmail with msmtp
  (setq sendmail-program "msmtp")
  ;;need to tell msmtp which account we're using
  (setq message-sendmail-extra-arguments '("-a" "eskimo"))
  ;; you might want to set the following too
  (setq mail-host-address "bit-builder.com")
  (setq user-full-name "Greg Green")
  (setq user-mail-address "ggreen@bit-builder.com"))

(provide 'init-notmuch)
