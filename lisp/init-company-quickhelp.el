
(with-eval-after-load 'company
  (require-maybe 'company-quickhelp)
  ;;(company-quickhelp-mode)
  ;;(setq company-quickhelp-delay 1.0)
 
  ;; 手动触发显示
  (setq company-quickhelp-delay nil)
  (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
  )


(provide 'init-company-quickhelp)
