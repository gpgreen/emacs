(use-package elfeed
  :config
  (setq-default elfeed-search-filter "@2-days-ago +unread")
  (setq-default elfeed-search-title-max-width 180)
  (setq-default elfeed-search-title-min-width 100)
  (setq elfeed-feeds '(
                       ;; news
                       ("https://feeds.arstechnica.com/arstechnica/index" ars)
                       ("https://lwn.net/headlines/rss" lwn)
                       ;; programming
                       ("https://www.reddit.com/r/emacs.rss" emacs)
                       ("https://www.reddit.com/r/rust.rss" rust)
                       ;; writers
                       ("https://whatever.scalzi.com/feed" scalzi)
                       ;; youtube
                                        ; gregs airplanes and automobiles
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCynGrIaI5vsJQgHJAIp9oSg" airplanes)
                                        ; marcus house
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBNHHEoiSF8pcLgqLKVugOw" space)
                                        ; wind rover
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7m3fuU8ASlIKz9LnV-vvJg" boats)
                                        ; flying miata
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9n-JCksLvTHhe6emcgpzCQ" miata)
                                        ; aoki body art
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBpfX_l0tCsNDVNRQthQyYQ" bodyart))))

(provide 'init-elfeed)
