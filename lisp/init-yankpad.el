;;; init-yankpad.el --- Orgmode template system -> yankpad  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <soeye@SOEYE-WIN>
;; Keywords: lisp

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

(require-maybe 'yankpad)
;;如果手动更换orgmode9后，这句执行后出现Not a face: nil的奇怪问题，终端下ivy无法弹出来，如果是赋值为不带/的字符串，又不会出现问题
(eval-after-load 'yankpad
  (setq yankpad-file (concat locale-notebook-dir "/yankpad.org"))
  (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand))


(provide 'init-yankpad)
;;; init-yankpad.el ends here

