;;; RSS feed
(require-package 'elfeed)

(setq elfeed-feeds
      '(("https://coolshell.cn/feed" tec)
	"http://www.aaronsw.com/2002/feeds/pgessays.rss"
	("http://www.paulgraham.com/rss.html" tec)))



(provide 'init-rss-feed)
