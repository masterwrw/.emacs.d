;; Copy from prelude config
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el
(defun prelude-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
  PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro prelude-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "prelude-%s" search-engine-name)) ()
     ,(format "Search %s with a query or region if any." search-engine-name)
     (interactive)
     (prelude-search ,search-engine-url ,search-engine-prompt)))

(prelude-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(prelude-install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(prelude-install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(prelude-install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")
(prelude-install-search-engine "bing"       "https://www.bing.com/search?q="               "Bing: ")



(defhydra hydra-web-search (:exit t)
  ("SPC" nil "quit" :exit t)
  ("w" prelude-google "Google")
  ("d" prelude-duckduckgo "DuckduckGo")
  ("g" prelude-github "Github")
  ("b" prelude-bing "Bing")
  ("y" prelude-youtube "Youtube"))


(defhydra+ hydra-funcs (:idle 1.0)
  ("SPC" nil "quit" :exit t)
  ("w" hydra-web-search/body "Web" :exit t))






(provide 'init-web-search)
