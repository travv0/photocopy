(defpackage :photocopy
            (:use :cl)
            (:use :photocopy.app-utils)
            (:export :-main))

(in-package :photocopy)

(defun -main (&optional args)
  (format t "~a~%" "I don't do much yet"))

