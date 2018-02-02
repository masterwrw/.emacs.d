;;; org brain configuration, require orgmode version 9
(require-package 'org-brain)
(require 'org-brain)
(setq org-brain-path "~/notebook/notes/brain")
;(eval-after-load 'evil
;  (evil-set-initial-state 'org-brain-visualize-mode 'emacs))

(setq org-id-track-globally t)
(setq org-id-locations-file "~/.emacs.d/.org-id-locations")
(setq org-brain-visualize-default-choices 'all)
(setq org-brain-title-max-length 64)
;; If org-brain is slow, set this!, if this value is t, the title can not contain slashes(/)
(setq org-brain-file-entries-use-title t)

(require-package 'uuidgen)
(require 'uuidgen)


;;; Not use capture, because will be add child but no new file
;; Need insert uuid
;(defun my-auto-insert-uuid ()
;    (replace-string "(uuid)" (uuidgen-4)))

;(push '("b" "Brain" plain (function org-brain-goto-end)
;	"* %i%?\n:PROPERTIES:\n:ID:       (uuid)\n:END:" :empty-lines 1)
;      org-capture-templates)
;(add-hook 'org-capture-prepare-finalize-hook 'my-auto-insert-uuid) //works

;(push '("b" "Add to brain" plain (function org-brain-goto-end)
;	"* %i%?" :empty-lines 1)
;      org-capture-templates)
;(add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)


(require-package 'org-cliplink)
(require 'org-cliplink)

(defun org-brain-cliplink-resource ()
  "Add a URL from the clipboard as an org-brain resource.
Suggest the URL title as a description for resource."
  (interactive)
  (let ((url (org-cliplink-clipboard-content)))
    (org-brain-add-resource
     url
     (org-cliplink-retrieve-title-synchronously url)
     t)))

(define-key org-brain-visualize-mode-map (kbd "L") #'org-brain-cliplink-resource)


(defhydra org-brain-hydra (:color pink :hint nil)
  "
_c_ Add child(advise)  _p_ New parent  _f_ Add friendship
_h_ New child  _C_ Remove child  _P_ Remove parent  _F_ Remove friendship
_d_ Delete entry  _l_ Add resource  _L_ Add link resource
_n_ Pin  _t_ Set title  _T_ Set tag  _r_ Random  _R_ Random circle
_j_ Goto next link _b_ Go back _v_ Open brain  _C-r_ Rename file
_k_ Goto previous link
"
  ("j" forward-button)
  ("k" backward-button)
  ("b" org-brain-visualize-back)
  ("h" org-brain-new-child)
  ("*" org-brain-new-child)
  ("c" org-brain-add-child)
  ("C" org-brain-remove-child)
  ("p" org-brain-add-parent)
  ("P" org-brain-remove-parent)
  ("f" org-brain-add-friendship)
  ("F" org-brain-remove-friendship)
  ("n" org-brain-pin)
  ("t" org-brain-set-title)
  ("T" org-brain-set-tags)
  ("d" org-brain-delete-entry)
  ("l" org-brain-visualize-add-resource)
  ("L" org-brain-cliplink-resource)
  ("C-y" org-brain-visualize-paste-resource)
  ("a" org-brain-visualize-attach)
  ("o" org-brain-goto-current)
  ("O" org-brain-goto)
  ("v" org-brain-visualize)
  ("r" org-brain-visualize-random)
  ("R" org-brain-visualize-wander)
  ("C-r" org-brain-rename-file)
  ("q" nil "quit" :color blue))



(provide 'init-org-brain)
