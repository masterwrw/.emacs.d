(require-maybe 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(setq ido-auto-merge-delay-time 10000) ;; disable auto search file
(eye--print-time "require ido")




(provide 'init-ido)

