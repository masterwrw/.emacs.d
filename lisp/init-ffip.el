;;; init-ffip.el --- find file in project            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  owensys

;; Author: owensys <owensys at hotmail dot com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:



(provide 'init-ffip)
;;; init-ffip.el ends here

;; Windows平台必须设置，否则执行ffip会直占用CPU。
(when is-windows (setq ffip-find-executable "find"))

(if (executable-find ffip-find-executable)
    (progn
      (defhydra+ hydra-file (:exit t :idle 1.0)
	("e" ffip "ffip")))
  (warn "Can't find program \"find\", so you can not use command \"ffip\""))



(provide 'init-ffip)
;;; init-ffip.el ends here
