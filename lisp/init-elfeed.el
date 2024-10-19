(use-package elfeed
  :config
  (setq 'elfeed-feeds '(
                        ;; programming
                        ("https://www.reddit.com/r/emacs.rss" emacs)
                        ("https://www.reddit.com/r/rust.rss" rust)
                        ;; writers
                        ("https://whatever.scalzi.com/feed" scalzi)
                        ;; youtube
                        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCynGrIaI5vsJQgHJAIp9oSg" airplanes))
        (setq-default elfeed-search-filter "@2-days-ago +unread")
        (setq-default elfeed-search-title-max-width 100)
        (setq-default elfeed-search-title-min-width 100)))

(provide 'init-elfeed)
