;;; Interface plus

;; Cursor shape
(setq-default cursor-type 'box)

;; Make cursor not blink
(blink-cursor-mode 0)


;; diminish, Diminished modes are minor modes with no modeline display
(require-package 'diminish)
(require 'diminish)


;; which-key
(use-package which-key
             :ensure t
             :config
             (which-key-mode)
             (diminish 'which-key-mode)
             (which-key-setup-side-window-bottom)
	     ;(which-key-setup-minibuffer)
	     )


;; swiper configuration
;; Using swiper so ido no longer needed
;(require 'init-ido)
(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
 ;   (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))


;; smex configuration
(require-package 'smex)

;; smex or counsel-M-x?
(defvar my-use-smex nil
  "Use `smex' instead of `counsel-M-x' when press M-x.")

(defun my-M-x ()
  (interactive)
  (cond
    (my-use-smex
      (smex))
    ((fboundp 'counsel-M-x)
     ;; `counsel-M-x' will use `smex' to remember history
     (counsel-M-x))
    ((fboundp 'smex)
     (smex))
    (t
      (execute-extended-command))))


;; tabbar
;(use-package tabbar
;  :ensure t
;  :config
;  (tabbar-mode 1))

;; elscreen, Emacs window session manager, like tab bar.
(require-package 'elscreen)
(require 'elscreen)
;(elscreen-start)
;(require 'setup-elscreen)


;; beacon, Highlight the cursor whenever the window scrolls
;; This package will be slow under windows
(if (not *is-windows*)
    (use-package beacon
      :ensure t
      :config
      (beacon-mode 1)
      (diminish 'beacon-mode)))

;; Highlight current line
(global-hl-line-mode)


;; smooth-scrolling, Make emacs scroll smoothly
(require-package 'smooth-scrolling)
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)



;; fullframe, Generalized automatic execution in a single frame
(require-package 'fullframe)
(require 'fullframe)


;; Icons
(require-package 'all-the-icons)
(require 'all-the-icons)


;; auto-dim-other-buffers, Makes non-current buffers less prominent
;(require-package 'auto-dim-other-buffers)
;(require 'auto-dim-other-buffers)
;(add-hook 'after-init-hook (lambda ()
;  (when (fboundp 'auto-dim-other-buffers-mode)
;    (auto-dim-other-buffers-mode t))))
;(custom-set-faces '(auto-dim-other-buffers-face ((t (:background "DodgerBlue1")))))


;; openwith
(require-package 'openwith)
(when (require 'openwith nil 'noerror)
  (setq openwith-associations
	(list
	 (list (openwith-make-extension-regexp
		'("mpg" "mpeg" "mp3" "mp4"
		  "avi" "wmv" "wav" "mov" "flv"
		  "ogm" "ogg" "mkv"))
	       "vlc"
	       '(file))
	 (list (openwith-make-extension-regexp
		'("xbm" "pbm" "pgm" "ppm" "pnm"
		  "png" "gif" "bmp" "tif" "jpeg" "jpg"))
	       "geeqie"
	       '(file))
	 (list (openwith-make-extension-regexp
		'("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
	       "libreoffice"
	       '(file))
	 '("\\.lyx" "lyx" (file))
	 '("\\.chm" "kchmviewer" (file))
	 (list (openwith-make-extension-regexp
		'("pdf" "ps" "ps.gz" "dvi"))
	       "okular"
	       '(file))
	 ))
  (openwith-mode 1)
  (diminish 'openwith-mode))


;; Fast setp upward and downward
(defun my-fast-step-upward ()
  "Step 3 lines up, recenteres the screen."
  (interactive)
  (forward-line -3)
  (recenter))

(defun my-fast-step-downward ()
  "Step 3 lines down, recenteres the screen."
  (interactive)
  (forward-line 3)
  (recenter))


(defun my-auto-switch-buffer ()
  "Switch to previous buffer no ask"
  (interactive)
  (switch-to-buffer nil))



(defun my-server-start ()
  "Start server for open file only one emacs"
  (interactive)
  (require 'server)
  (if (not (server-running-p))
      (server-start)))


;; Copy from prelude config: https://github.com/bbatsov/prelude/blob/master/core/prelude-core.el
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



;;; Quick scroll
(defun scroll-up-lines ()
  "Scroll up lines"
  (interactive)
  (scroll-up 10)
  (forward-line 10))

(defun scroll-down-lines ()
  "Scroll down lines"
  (interactive)
  (scroll-down 10)
  (forward-line -10))


;; dydra package
(require-package 'hydra)
(require 'hydra)




;; persp-mode, windows/buffers sets shared among frames + save/load.
;(require-package 'persp-mode)
;(with-eval-after-load "persp-mode"
;  ;; .. all settings you want here
;  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
;(require 'persp-mode)




(provide 'init-interface-plus)
