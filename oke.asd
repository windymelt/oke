#|
  This file is a part of oke project.
  Copyright (c) 2017 Windymelt
|#

#|
  Author: Windymelt
|#

(defsystem "oke"
  :version "0.1.0"
  :author "Windymelt"
  :license ""
  :depends-on ("optima" "cl-annot" "cl-ppcre" "uiop" "local-time")
  :components ((:module "src"
                :components
                ((:file "oke"
                  :depends-on ("git" "util" "debug"))
                 (:file "git"
                  :depends-on ("util" "debug"))
                 (:file "util")
                 (:file "debug"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "oke-test"))))
  
