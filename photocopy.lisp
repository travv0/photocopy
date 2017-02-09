(defpackage :photocopy
  (:use :cl
        :cl-fad
        :parser.ini
        :trivial-types)
  (:use :photocopy.app-utils)
  (:export :-main))

(in-package :photocopy)

(defvar *paths* (make-hash-table :test 'equal)
  "Hash table of paths to save files to by badge number.")
(defvar *general-settings* (make-hash-table :test 'equal)
  "Hash table to hold general settings from ini file.")

(defun -main (&optional args)
  (let ((ini (parse (normalize-line-endings
                     (read-ini-to-string "test.ini"))
                    'list))
        (badge-number (second args)))
    (ini-section-to-directories (get-ini-section ini "VIEW-PATHS") *paths*)
    (ini-section-to-hash-table (get-ini-section ini "GENERAL") *general-settings*)))

(defun read-ini-to-string (file-path)
  "Read INI file at `file-path' to a string."
  (declare ((or pathname string) file-path))
  (let ((string (make-array '(0) :element-type 'character
                                 :adjustable t
                                 :fill-pointer 0)))
    (with-open-file (f file-path)
      (loop for char = (read-char f nil :eof)
            until (eql char :eof) do (vector-push-extend char string)))
    (the string string)))

(defun normalize-line-endings (string)
  "Remove carriage returns to normalize line endings to unix-style."
  (declare (string string))
  (remove #\Return string))

(defun get-ini-section (ini section-name)
  "Get section from `ini' that was returned by `parser.ini:parse'
with `section-name'."
  (declare ((proper-list property-list) ini)
           (string section-name))
  (loop for section in ini
        when (equal (getf section :name) (list section-name))
          return (the (proper-list (proper-list property-list))
                      (getf (getf section :section)
                            :section-option))))

(defun ini-section-to-hash-table (section table)
  "Populate `settings-table' hash table with keys and values from section."
  (declare ((proper-list (proper-list property-list)) section))
  (loop for setting in section
        do (let* ((setting-plist (first setting))
                  (key (first (getf setting-plist :name)))
                  (value (getf setting-plist :value)))
             (setf (gethash key table) value)))
  table)

(defun ini-section-to-directories (paths-section paths-table)
  "The same as `ini-section-to-hash-table', but ensures all values are
stored as pathname directories."
  (declare (hash-table paths-table)
           ((proper-list (proper-list property-list)) paths-section))
  (loop for badge-number
          being the hash-keys of (ini-section-to-hash-table paths-section
                                                            paths-table)
            using (hash-value path)
        do (setf (gethash badge-number paths-table)
                 (pathname-as-directory path)))
  paths-table)

(defun copy-files (from to)
  "Copy all files from `from' to `to'."
  (declare ((or pathname string) from)
           ((or pathname string) to))
  (let ((from (pathname-as-directory from))
        (to (pathname-as-directory to)))
    (ensure-directories-exist to)
    (walk-directory from
                    (lambda (file)
                      (copy-file file
                                 (merge-pathnames-as-file
                                  to
                                  (format nil "~a~@[.~a~]"
                                          (pathname-name file)
                                          (pathname-type file)))
                                 :overwrite t)))))
