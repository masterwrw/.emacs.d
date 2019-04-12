(require-maybe 'proj-gen)
(autoload 'proj-gen-project "proj-gen" "" t)

(setq proj-gen-template-dir (concat "~/.emacs.d/lisp/proj-gen/tmpl/"))
(setq proj-gen-file-encoding 'utf-8)
(setq proj-gen-init-git t)

      


(provide 'init-proj-gen)

