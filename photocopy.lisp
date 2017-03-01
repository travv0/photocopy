(defpackage :photocopy
  (:use :cl
        :uiop/pathname
        :parser.ini
        :trivial-types
        :cl-utilities)
  (:use :photocopy.app-utils)
  (:export :-main))

(in-package :photocopy)

(defvar *device-ids* (make-hash-table :test 'equal)
  "Hash table of badge numbers by device ID.")
(defvar *settings* (make-hash-table :test 'equal)
  "Hash table to hold general settings from ini file.")

(defun -main (&optional args)
  (let ((ini (parse (normalize-line-endings
                     (read-ini-to-string "test.ini"))
                    'list))
        (badge-number (second args)))
    (values (ini-section-to-hash-table (get-ini-section ini "DEVICE-BADGE") *device-ids*)
            (ini-section-to-hash-table (get-ini-section ini "GENERAL") *settings*))))

(defmethod print-object ((object hash-table) stream)
  (format stream "#HASH{~{~{~s ~s~}~^,~%      ~}}"
          (loop for key being the hash-keys of object
                  using (hash-value value)
                collect (list key value))))

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
  "Populate `table' hash table with keys and values from `section'."
  (declare ((proper-list (proper-list property-list)) section))
  (loop for setting in section
        do (let* ((setting-plist (first setting))
                  (key (first (getf setting-plist :name)))
                  (value (getf setting-plist :value)))
             (setf (gethash key table) value)))
  table)

(defun copy-files (from to)
  "Copy all files from `from' to `to'."
  (declare ((or pathname string) from)
           ((or pathname string) to))
  (let ((from (ensure-directory-pathname from))
        (to (ensure-directory-pathname to)))
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

(defvar *whitespace-chars* '(#\Space #\Newline #\Backspace #\Tab
                             #\Linefeed #\Page #\Return #\Rubout))

(defun whitespacep (character)
  "Check if `character' is whitespace."
  (not (not (position character *whitespace-chars*))))

(defun trim-whitespace (string)
  (string-trim *whitespace-chars* string))

(defun retrieve-current-serial-numbers ()
  "Retrieve serial numbers of devices currently plugged in."
  (let ((command-results (with-output-to-string (s)
                           (sb-ext:run-program
                            "wmic"
                            '("logicaldisk" "get" "caption,volumeserialnumber")
                            :search t
                            :output s))))
    (rest (rest (cl-utilities:split-sequence-if
                 #'whitespacep
                 (trim-whitespace command-results)
                 :remove-empty-subseqs t)))))
