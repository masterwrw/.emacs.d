(use-package qt-pro-mode
  :ensure t
  :mode ("\\.pro\\'" "\\.pri\\'")
  :config
  (add-hook 'qt-pro-mode 'yas-minor-mode))

(use-package qml-mode
  :ensure t
  :init
  (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
  (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
  )

(use-package company-qml
  :ensure t
  :config
  (add-hook 'qml-mode
		 '(lambda ()
		    (require 'company-qml)
		    (eye/push-to-list 'company-qml company-backends)))
  )



(defun eye/qt5-help ()
  "Find Qt5 document."
  (interactive)
  (let ((url "http://doc.qt.io/qt-5/search-results.html?q="))
    (setq url (concat url (read-string "Query Qt5 document: " (eye/current-word))))
    (browse-url-firefox url)))


(defun qt-mode-style-setup ()
  (interactive)
  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                 "\\|protected slot\\|private\\|private slot"
                                 "\\)\\>")
        c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                 "\\|public slots\\|protected slots\\|private slots"
                                 "\\)\\>[ \t]*:"))
  (progn
    ;; modify the colour of slots to match public, private, etc ...
    (font-lock-add-keywords 'c++-mode
                            '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
    ;; make new font for rest of qt keywords
    (make-face 'qt-keywords-face)
    (set-face-foreground 'qt-keywords-face "DeepSkyBlue1")
    ;; qt keywords
    (font-lock-add-keywords 'c++-mode
                            '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
                            '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
                            '(("\\<Q[A-Z][A-Za-z]\\>" . 'qt-keywords-face)))
    ))

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'c-mode-common-hook
               ))
  (add-hook hook '(lambda () (qt-mode-style-setup))))



(provide 'init-qt)
  
