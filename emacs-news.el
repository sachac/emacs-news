;; [[file:index.org::+begin_src emacs-lisp :var feeds=feeds :results silent][No heading:1]]
(let ((feeds '("https://planet.emacslife.com/atom.xml" "https://tv.dyne.org/feeds/videos.xml?videoChannelId=2459" "https://spectra.video/feeds/videos.xml?videoChannelId=5014" "https://fediverse.tv/feeds/videos.xml?videoChannelId=26905" "https://tracker.orgmode.org/reports/requests-open.xml" "https://tracker.orgmode.org/reports/announcements-open.xml")))
(customize-save-variable 'emacs-news-rss-feeds feeds)
)
;; No heading:1 ends here
