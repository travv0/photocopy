(defpackage :photocopy
  (:use :cl
        :parser.ini)
  (:use :photocopy.app-utils)
  (:export :-main))

(in-package :photocopy)

(defun -main (&optional args)
  (format t "~a~%" "I don't do much yet"))

(defun read-ini-to-string (file-path)
  (let ((string (make-array '(0) :element-type 'character
                                 :adjustable t
                                 :fill-pointer 0)))
    (with-open-file (f file-path)
      (loop for char = (read-char f nil :eof)
            until (eql char :eof) do (vector-push-extend char string)))
    string))

(defun normalize-line-endings (string)
  (remove #\ string))
