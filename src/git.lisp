(defpackage oke.git
  (:use :cl :cl-annot)
  (:import-from
   :uiop
   :run-program
   :chdir
   :getcwd
   :subprocess-error-code)
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
(defun git-get-revision ()
  (handler-case (chomp (run-git-output "rev-parse" "HEAD"))
    (error (e)
      (when (debugp) (format *error-output* "=> ~A" (subprocess-error-code e)))
      "")))
@export
(defun run-git (&rest args)
  (git t nil args))

@export
(defun run-git-null (&rest args)
  (git nil nil args))

@export
(defun run-git-output (&rest args)
  (git 'string nil args))

(defun git (out in args)
  (when (debugp)
    (format *error-output* ">>> RUN git ~{~A~^ ~}~%" args))
  (uiop:run-program (format nil "git ~{~A~^ ~}" args)
                    :output out
                    :error-output t
                    :input in))
