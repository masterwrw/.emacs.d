(require 'org)
(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))
(put 'narrow-to-region 'disabled nil)
