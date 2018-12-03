(when (equal system-type 'windows-nt)
  (progn
    ;; org-wiki
    (setq org-wiki-location-list
	  '("d:/cloud/YandexDisk/notebook/tec"    ;; First wiki (root directory) is the default. 
            "d:/cloud/YandexDisk/notebook/private"
            ))

    ;; org-brain
    (setq org-brain-path "d:/cloud/YandexDisk/notebook/map")
    
    ))

(provide 'init-system)
