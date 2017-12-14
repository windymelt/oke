(defpackage oke
  (:use :cl :cl-ppcre :optima :oke.git :cl-annot :local-time)
  (:import-from
   :uiop
   :with-temporary-file
   :run-program
   :subprocess-error-code
   :copy-stream-to-stream
   :getenv
   :getcwd
   :pathname-directory-pathname
   :pathname-parent-directory-pathname
   :merge-pathnames*
   :directory-exists-p
   :delete-directory-tree
   :delete-file-if-exists)
  (:import-from
   :oke.util
   :join
   :chomp)
  (:import-from
   :oke.debug
   :debugp))
(in-package :oke)

(annot:enable-annot-syntax)

(defconstant +version+ 0.0) ; TODO: read file

(defmacro chdir (to)
  `(progn
       (when (debugp) (format *error-output* ">>> CHDIR ~A~%" ,to))
       (uiop:chdir ,to)))

(defun load-proj-config ()
  '())

(defun get-logs-repo ()
  "Return environmental variable for [remote] repository path"
  (or (getenv "OKE_LOGS_REPOSITORY")
      (error "OKE_LOGS_REPOSITORY not set")))

(defun build-log-filename (timestamp)
  (local-time:format-timestring nil timestamp
   :format '((:year 4) #\/ (:month 2) #\/ (:day 2) #\/ (:hour 2) (:min 2) (:sec 2) #\. (:usec 6) ".log")))

(defun write-to-temp-file (cmdargs)
  (with-temporary-file (:pathname p :stream s :keep t)
    (multiple-value-bind (_ _ ret-code)
        (run-program (join cmdargs) :ignore-error-status t :output s :error s)
      (values p ret-code))))

@export
(defun command-exec (cmdargs)
  (when (eq (length cmdargs) 0) (return-from command-exec 1))
  (destructuring-bind (repo-path proj-path proj-logs-dir)
      (init-project)
    (let* ((log-file (merge-pathnames* (build-log-filename (local-time:now)) proj-logs-dir))
           (ret-code (progn
                       (multiple-value-bind (temp-path ret-code)
                           (write-to-temp-file cmdargs)
                         (ensure-directories-exist
                          (pathname-directory-pathname log-file))
                         (with-open-file (>s log-file :direction :output)
                           (format >s +log-template+
                                   cmdargs
                                   (getenv "USER" #|getpath.getuser|#)
                                   repo-path
                                   proj-path
                                   (git-get-revision)
                                   +version+
                                   ret-code)
                           (with-open-file (s> temp-path :direction :input)
                             (copy-stream-to-stream s> >s))
                           (delete-file-if-exists temp-path))
                         (chdir proj-logs-dir)
                         ret-code)))
           (current-remote
             (handler-case (git-current-remote)
               (error (e) (git-setup-logs-dir proj-logs-dir (get-logs-repo)))))
           (headline
             (format nil "~:[[failed]~;~]~{~A~^ ~}" (eq ret-code 0) cmdargs))
           (_ (run-git "checkout" "--quiet" "-B" proj-path))
           (has-branch (multiple-value-bind (_ _ ret-code)
                           (run-git-null "ls-remote" "--exit-code" "origin" proj-path)
                         (ccase ret-code
                           (0 t)
                           (2 nil)))))
      (run-git "add" "--force" log-file)
      (run-git "commit" "--quiet" "--message" (format nil "\"~A\"" headline) )
      (when has-branch
        (run-git "pull" "--quiet" "--rebase" "origin" proj-path))
      (run-git "push" "--quiet" "origin" proj-path)
      ret-code)))

@export
(defun command-history (args)
  (destructuring-bind (repo-path proj-path proj-logs-dir) (init-project)
    (if (eq (length args) 0)
        (command-help)
        (or (match args
              ((cons "show" rest)
               (chdir proj-logs-dir)
               (setf (getenv "GIT_EXTERNAL_DIFF") "sh -c \"cat $5\"")
               (apply #'run-git "show" "-pretty=format:" "--ext-diff" rest))
              ((cons "pull" rest)
               (unless (directory-exists-p proj-logs-dir)
                 (ensure-directories-exist proj-logs-dir)
                 (run-git "clone" (get-logs-repo) "-b" proj-path proj-logs-dir)
                 (chdir proj-logs-dir)
                 (run-git "pull" "origin" proj-path)))
              ((cons "fix" rest)
               (delete-directory-tree
                proj-logs-dir
                :validate #'(lambda (p) (yes-or-no-p "rm -rf ~A" p))
                :if-does-not-exist :ignore)
               (chdir (pathname-parent-directory-pathname proj-logs-dir))
               (run-git "clone" (get-logs-repo) "-b" proj-path proj-logs-dir))
              ((cons "git" rest)
               (chdir proj-logs-dir)
               (apply #'run-git rest)))
            (progn (if (not (eq (car args) nil))
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
  (when (debugp) (format *error-output* ">>> init-project~%"))
  (let* ((repo-path (handler-case (repo->repo-path (git-current-remote))
                      (error (e) "")))
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

(defparameter +log-template+
   "command: ~{~A~^ ~}
user: ~A
repoPath: ~A
projectPath: ~A
gitRevision: ~A
furoVersion: ~A
exitCode: ~A
---~%")

;;; where oke logs repository cloned to
(defparameter *logs-dir* (or (uiop:getenv "OKE_LOGS_DIR") "~/.oke/logs"))
