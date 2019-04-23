
(setq qt-dir "C:\\Qt\\Qt4.8.7\\bin")
(setq qtcreator-dir "C:\\Qt\\qtcreator-4.6.0\\bin")
(setq gcc-dir "C:\\Qt\\Qt4.8.7\\bin")
(setq vs-env "C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\VC\\vcvarsall.bat")

(defun eye/set-gcc-env ()
  (let (path)
    (setq path (concat "@echo off\r\n"
                       "set path=%path%;" qt-dir ";" gcc-dir ";" qtcreator-dir ";" "\r\n"))
    path))

(defun eye/set-vs-env ()
  (let (path)
    (setq path (concat "@echo off\r\n"
                       "call \"" vs-env "\"" "\r\n"))
    path))

(defun eye/get-directory ()
  (let ((dir (read-directory-name "Project Directory: ")))
    (if (not (file-exists-p dir))
        (mkdir dir))
    dir))

(defun eye/create-qt-gcc-build-script ()
  (interactive)
  (let (dir file script command)
    (setq dir (eye/get-directory))
    (setq file (concat dir build-script))
    (setq command (format "mingw32-make -w -f Makefile.Release -C %s" dir))
    (setq script (concat (eye/set-gcc-env) command))
    (f-write script 'gbk file)
    ))

(defun eye/create-qt-vs-build-script ()
  (interactive)
  (let (dir file script command projectfile)
    (setq projectfile (read-file-name "Project file:"))
    (setq dir (file-name-directory projectfile))
    (setq file (concat dir build-script))
    (setq command (format "devenv \"%s\" /build" projectfile))
    (setq script (concat (eye/set-vs-env) command))
    (f-write script 'gbk file)
    ))




(defun find-project-directory-recursive (x)
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p x) t
    (cd "../")
    (find-project-directory-recursive x)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))


(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  ;;(switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (find-project-directory-recursive build-script)
    (setq last-compilation-directory default-directory)))




(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile (concat "build.bat " (buffer-name (current-buffer)) )))
  ;;(switch-to-buffer-other-window "*compilation*")
  (delete-other-window)
  (switch-to-buffer "*compilation*"))

(defun real-make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile "make" ))
  (switch-to-buffer-other-window "*compilation*")
  (other-window 1))


(require-maybe 'smart-compile)
(with-eval-after-load 'smart-compile
  (setq smart-compile-option-string "-w -s -j4"))
