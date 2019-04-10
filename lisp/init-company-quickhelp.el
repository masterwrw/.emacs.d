
(with-eval-after-load 'company
  (require-maybe 'company-quickhelp)
  ;;(company-quickhelp-mode)
  ;;(setq company-quickhelp-delay 1.0)
 
  ;; 手动触发显示
  (autoload 'company-quickhelp-manual-begin "company-quickhelp" "quickhelp" t) ;; company-quickhelp-manual-begin is not autoload function, must define.
  (setq company-quickhelp-delay nil)
  (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
  )


(provide 'init-company-quickhelp)
