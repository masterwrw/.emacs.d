(package-initialize)

(require 'org)

(setq config-file (expand-file-name "config.org" "~/.emacs.d/"))
(when (file-readable-p config-file)
  (org-babel-load-file config-file))
