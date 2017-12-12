(defpackage oke.git
  (:use :cl :cl-annot)
  (:import-from
   :uiop
   :run-program
   :chdir
   :getcwd)
  (:import-from
   :oke.debug
   :debugp)
  (:import-from
   :oke.util
   :chomp))
(in-package :oke.git)

(annot:enable-annot-syntax)

@export
(defun git-setup-logs-dir (dir logs-repo)
  (let ((cwd (getcwd)))
    (chdir dir)
    (run-git "init" "--quiet")
    (run-git "remote" "add" "origin" logs-repo)
    (chdir cwd))
  (chdir dir))

@export
(defun git-current-remote ()
  (chomp (run-git-output "config" "remote.origin.url")))

@export
(defun run-git (&rest args)
  (git t args))

@export
(defun run-git-output (&rest args)
  (git 'string args))

(defun git (out args)
  (when (debugp)
    (format *error-output* ">>> RUN git ~{~A~^ ~}~%" args))
  (uiop:run-program (format nil "git ~{~A~^ ~}" args) :output out))
