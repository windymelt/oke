(defpackage oke
  (:use :cl :cl-ppcre :optima :oke.git :cl-annot :local-time)
  (:import-from
   :uiop
   :with-temporary-file
   :run-program
   :subprocess-error-code
   :copy-stream-to-stream
   :chdir
   :getenv
   :getcwd
   :pathname-directory-pathname
   :pathname-parent-directory-pathname
   :merge-pathnames*
   :directory-exists-p)
  (:import-from
   :oke.util
   :join
   :chomp))
(in-package :oke)

(annot:enable-annot-syntax)

(defconstant +version+ 0.0) ; TODO: read file

(defun load-proj-config ()
  '())

(defun get-logs-repo ()
  (or (getenv "OKE_LOGS_REPOSITORY")
      (error "OKE_LOGS_REPOSITORY not set")))

(defun build-log-filename (timestamp)
  (local-time:format-timestring nil timestamp
   :format '((:year 4) #\/ (:month 2) #\/ (:day 2) #\/ (:hour 2) (:min 2) (:sec 2) #\. (:usec 6) ".log")))

@export
(defun command-exec (cmdargs)
  (when (eq (length cmdargs) 0) (return-from command-exec 1))
  (destructuring-bind (repo-path proj-path proj-logs-dir)
      (init-project)
    (let* ((logs-repo (get-logs-repo))
           (git-revision (chomp (run-git-output "rev-parse" "HEAD")))
           (ret-code 0)
           (log-file (merge-pathnames* proj-logs-dir (build-log-filename (local-time:now))))
           (temp-file nil)
           (_ (format t "~A~%" log-file))
           (_ (ensure-directories-exist
               (pathname-directory-pathname log-file)))
           (_ (progn (handler-case (with-temporary-file
                                (:pathname p :stream s :keep t)
                              (setf temp-file p)
                              (run-program (join cmdargs) :output s))
                       (error (x) (setf ret-code (subprocess-error-code x))))
                     (with-open-file (s log-file :direction :output)
                       (format s +log-template+
                               cmdargs
                               (getenv "USER" #|getpath.getuser|#)
                               repo-path
                               proj-path
                               git-revision
                               +version+
                               ret-code)
                       (with-open-file (ts temp-file :direction :input)
                         (copy-stream-to-stream ts s)))
                     (chdir proj-logs-dir)))
           (current-remote
             (handler-case (git-current-remote)
               (error (e) (git-setup-logs-dir proj-logs-dir logs-repo))))
           (headline
             (format nil "~:[[failed]~;~]~{~A~^ ~}" (eq ret-code 0) cmdargs))
           (_ (handler-case
                  (run-git "ls-remote" "--exit-code" "origin" proj-path)
                (error (e) (unless (eq (subprocess-error-code e) 2)
                             (error e)))))
           (has-branch t))
      (run-git "add" "--force" log-file)
      (run-git "commit" "--quiet" "--message" headline)
      (when has-branch
        (run-git "pull" "--quiet" "--rebase" "origin" proj-path))
      (run-git "push" "--quiet" "origin" proj-path)
      ret-code)))

@export
(defun command-history (args)
  (destructuring-bind (repo-path proj-path proj-logs-dir) (init-proj)
    (if (eq (length args) 0)
        128
        (or (match args
              ((cons "show" rest)
               (chdir proj-logs-dir)
               (setf (getenv "GIT_EXTERNAL_DIFF") "sh -c \"cat $5\"")
               (run-git "show" "-pretty=format:" "--ext-diff" rest))              
              ((cons "pull" rest)
               (format "~A~%" proj-logs-dir)
               (unless (directory-exists-p proj-logs-dir)
                 (ensure-directories-exist proj-logs-dir)
                 (run-git "clone" (get-logs-repo) "-b" proj-path proj-logs-dir)
                 (chdir proj-logs-dir)
                 (run-git "pull" "origin" proj-path)))
              ((cons "fix" rest)
               (chdir (pathname-parent-directory-pathname project-logs-dir))
               (run-git "clone" proj-logs-dir "-b" proj-path proj-logs-dir))
              ((cons "git" rest)
               (chdir proj-logs-dir)
               (apply #'run-git rest)))
            (progn (if (not (eq (car args nil)))
                       (chdir proj-logs-dir)
                       (apply #'run-git
                              (concatenate 'list
                                           '("log" "--no-decorate" "--pretty=%h [%ad] (%an) %s")
                                           args))))))))

@export
(defun command-help ()
  (format *error-output* "oke exec COMMAND [ARGS...]
oke history [pull | show COMMIT | fix]
oke version")
  129)

@export
(defun command-version ()
  (format t "oke version ~A~%" +version+)
  0)

(defun init-project ()
  (format t "initializing~%")
  (let* ((repo-path (repo->repo-path (git-current-remote)))
         (proj-path (or (load-proj-config) repo-path #|TODO|#))
         (proj-logs-dir (pathname-directory-pathname (concatenate 'string *logs-dir* "/" proj-path "/"))))
    (list repo-path proj-path proj-logs-dir)))

(defun repo->repo-path (repo)
  (regex-replace
   "^[a-zA-Z0-9_]+@([a-zA-Z0-9._-]+):(.*)$"
   (regex-replace
    "^https?://(?:[^@]+@)?|^ssh://(?:[^@]+@)?|^git://|\\.git$"
    repo
    "")
   "\\1/\\2"))

(defconstant +log-template+
   "command: ~{~A~^ ~}
user: ~A
repoPath: ~A
projectPath: ~A
gitRevision: ~A
furoVersion: ~A
exitCode: ~A
---~%")

(defparameter *logs-dir* (or (uiop:getenv "OKE_LOGS_DIR") "~/.oke/logs"))
