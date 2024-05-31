;;; We want to alias mail identities, use gmail when from gmail, bitbuilder otherwise
;;; this takes effect when the mail is being setup
(use-package gnus-alias
  :config
  ;; Define two identities, "bitbuilder" and "gmail"
  (setq gnus-alias-identity-alist
      '(("bitbuilder"
         nil ;; Does not refer to any other identity
         "Greg Green <ggreen@bit-builder.com>" ;; Sender address
         nil ;; No organization header
         (("Bcc" . "ggreen@bit-builder.com"))
         nil ;; No extra body text
         "~/.signature")
        ("gmail"
         nil
         "Greg Green <gpgreen@gmail.com>"
         nil ;; No organization headers
         (("Bcc" . "gpgreen@gmail.com"))
         nil
         "~/.signature.gmail")))
  ;; Use "bitbuilder" identity by default
  (setq gnus-alias-default-identity "bitbuilder")
  ;; Define rules to match gmail identity
  (setq gnus-alias-identity-rules
      '(("gmail" ("any" "gpgreen@\\(gmail\\.com\\|help\\.gmail.com\\)" both) "gmail")))
  ;; Determine identity when message-mode loads
  (add-hook 'message-setup-hook 'gnus-alias-determine-identity))

;;; Use notmuch email client
(use-package notmuch
  :config
  ;; tag changes when replying
  (setq notmuch-message-replied-tags '("+replied" "-inbox"))
  (setq notmuch-message-forwarded-tags '("+forwarded" "-inbox"))
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key
                 [105])
          (:name "unread" :query "tag:unread" :key
                 [117])
          (:name "flagged" :query "tag:flagged" :key
                 [102])
          (:name "sent" :query "tag:sent" :key
                 [116])
          (:name "drafts" :query "tag:draft" :key
                 [100])
          (:name "all mail" :query "*" :key
                 [97])))

  ;; you might want to set the following too
  (setq mail-host-address "bit-builder.com")
  (setq user-full-name "Greg Green")
  (setq user-mail-address "ggreen@bit-builder.com")
  ;; messages with Fcc headers, save via my handler
  (setq message-fcc-handler-function 'ggreen-message-fcc-handler)
  (add-hook 'message-send-hook 'ggreen-notmuch-mua-empty-subject-check)
  (add-hook 'message-send-hook 'local-gnus-compose-mode)
  :init
  (setq send-mail-function 'smtpmail-send-it)
  (setq smtpmail-smtp-server "mail.eskimo.com")
      ;;        (setq smtpmail-debug-info t)
      ;;        (setq smtpmail-debug-verb t)
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'ssl))

;; make sure the subject line is not empty when sending email
(defun ggreen-notmuch-mua-empty-subject-check ()
  "Request confirmation before sending a message with empty subject"
  (when (and (null (message-field-value "Subject"))
             (not (y-or-n-p "Subject is empty, send anyway? ")))
    (error "Sending message cancelled: empty subject.")))

;; Configure known SMTP servers. Emacs prompts for passwords and saves them in ~/.authinfo
(setq ggreen-smtp-accounts          ;; Format: Sender Mail address - SMTP Server - Port - Username
      '(("gpgreen@gmail.com" "smtp.gmail.com" 465 "gpgreen@gmail.com")
        ("ggreen@bit-builder.com" "mail.eskimo.com" 465 "ggreen")
        ))

;; Set the SMTP Server according to the mail address we use for sending
(defun set-smtp-server-message-send-and-exit ()
  "Set SMTP server from list of multiple ones and send mail."
  (interactive)
  (message-remove-header "X-Message-SMTP-Method") ;; Remove. We always determine it by the From field
  (let ((sender
         (message-fetch-field "From")))
    (loop for (addr server port usr) in ggreen-smtp-accounts
          when (string-match addr sender)
          do (message-add-header (format "X-Message-SMTP-Method: smtp %s %d %s" server port usr)))
    (let ((xmess
           (message-fetch-field "X-Message-SMTP-Method")))
      (if xmess
          (progn
            (message (format "Sending message using '%s' with config '%s'" sender xmess))
            (message-send-and-exit))
        (error "Could not find SMTP Server for this Sender address: %s. You might want to correct it or add it to the SMTP Server list 'smtp-accounts'" sender)))))

;; Send emails via multiple servers
(defun local-gnus-compose-mode ()
  "Keys."
  (local-set-key (kbd "C-c C-c")  'set-smtp-server-message-send-and-exit))

;;; Save the email after it has been sent using Maildir mailbox format
(defun ggreen-message-fcc-handler (maildir)
  "Store message in `MAILDIR' after
successful sending. "
  (let ((sentdir "/home/ggreen/Mail/sent/cur"))
    (make-directory sentdir t)
    (let ( (buf (current-buffer))
           (field-to (or (message-fetch-field "Newsgroups") (message-fetch-field "To")))
           (field-subject (message-fetch-field "Subject"))
           file )
      (setq file (concat sentdir "/" (format-time-string "%F_%T") "_" field-to "_" field-subject))
      (with-temp-file file
        (insert-buffer buf)))))


(provide 'init-notmuch)
