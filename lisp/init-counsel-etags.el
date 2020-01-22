

(defhydra hydra-c++ (:exit t)
  "
_f_:Find tag at point   _t_:Find other tag   _r_:Open recent tag
_a_:List all tag        _c_:Create tags      _l_:Load root tags
"
  ("SPC" nil "quit")
  ("f" counsel-etags-find-tag-at-point)
  ("t" counsel-etags-find-tag)
  ("r" counsel-etags-recent-tag)
  ("a" counsel-etags-list-tag)
  ("c" eye/create-ctags-file)
  ("l" eye/load-project-root-tags))





(provide 'init-counsel-etags)
