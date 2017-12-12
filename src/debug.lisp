(defpackage oke.debug
  (:use :cl :cl-annot)
  (:import-from
   :uiop
   :getenv))
(in-package :oke.debug)

(annot:enable-annot-syntax)

@export
(defun debugp ()
  (if (getenv "CLOS_DEBUG") t nil))
