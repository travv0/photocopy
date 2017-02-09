;;;; photocopy.asd

(asdf:defsystem #:photocopy
  :description "INSERT PROJECT DESCRIPTION HERE"
  :author "Travis"
  :license "Modified BSD License"
  :serial t
  :depends-on (:parser.ini
               :cl-fad
               :trivial-types)
  :pathname "./"
  :components ((:file "app-utils")
               (:file "photocopy")))
