(add-to-list 'load-path "~/packages/smex")
(require 'smex)

;; modify smex so that typing a space will insert a hyphen ‘-’ like in normal M-x
;; @see https://www.emacswiki.org/emacs/Smex
(defadvice smex (around space-inserts-hyphen activate compile)
        (let ((ido-cannot-complete-command 
               `(lambda ()
                  (interactive)
                  (if (string= " " (this-command-keys))
                      (insert ?-)
                    (funcall ,ido-cannot-complete-command)))))
          ad-do-it))



;; this code the completion of ‘ffow’ shows:
;; { find-file-other-window | find-function-other-window}
;; 开启后xah-open-file-fast执行时出现variable void: smex-ido-cache的奇怪问题

;; (defadvice ido-set-matches-1 (around ido-smex-acronym-matches activate)
;;   "Filters ITEMS by setting acronynms first."
;;   (if (and (fboundp 'smex-already-running) (smex-already-running) (> (length ido-text) 1))

;;       ;; We use a hash table for the matches, <type> => <list of items>, where
;;       ;; <type> can be one of (e.g. `ido-text' is "ff"):
;;       ;; - strict: strict acronym match (i.e. "^f[^-]*-f[^-]*$");
;;       ;; - relaxed: for relaxed match (i.e. "^f[^-]*-f[^-]*");
;;       ;; - start: the text start with (i.e. "^ff.*");
;;       ;; - contains: the text contains (i.e. ".*ff.*");
;;       (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
;;             (matches (make-hash-table :test 'eq)))

;;         ;; Filtering
;;         (dolist (item items)
;;           (let ((key))
;;             (cond
;;              ;; strict match
;;              ((string-match (concat regex "[^-]*$") item)
;;               (setq key 'strict))

;;              ;; relaxed match
;;              ((string-match regex item)
;;               (setq key 'relaxed))

;;              ;; text that start with ido-text
;;              ((string-match (concat "^" ido-text) item)
;;               (setq key 'start))

;;              ;; text that contains ido-text
;;              ((string-match ido-text item)
;;               (setq key 'contains)))

;;             (when key
;;               ;; We have a winner! Update its list.
;;               (let ((list (gethash key matches ())))
;;                 (puthash key (push item list) matches)))))

;;         ;; Finally, we can order and return the results
;;         (setq ad-return-value (append (gethash 'strict matches)
;;                                       (gethash 'relaxed matches)
;;                                       (gethash 'start matches)
;;                                       (gethash 'contains matches))))

;;     ;; ...else, run the original ido-set-matches-1
;;     ad-do-it))





(eye-define-leader-key global-map "a" 'smex)




(provide 'init-smex)
