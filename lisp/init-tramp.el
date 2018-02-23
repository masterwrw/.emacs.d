;;; Remote edit configuration

(if *is-windows*
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh")
  )


(provide 'init-tramp)
