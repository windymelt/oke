(defpackage oke.util
  (:use :cl :cl-annot))
  (in-package :oke.util)

(annot:enable-annot-syntax)

@export
(defmacro chomp ((&body body))
  `(string-right-trim '(#\Newline) ,body))

@export
(defmacro join (lis)
  `(format nil "~{~A~^ ~}" ,lis))
