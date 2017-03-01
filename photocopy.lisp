(defpackage :photocopy
  (:use :cl
        :parser.ini
        :trivial-types
        :cl-utilities
        :cl-fad)
  (:use :photocopy.app-utils)
  (:export :-main))

(in-package :photocopy)

(defvar *device-ids* (make-hash-table :test 'equal)
  "Hash table of badge numbers by device ID.")
(defvar *settings* (make-hash-table :test 'equal)
  "Hash table to hold general settings from ini file.")

(defparameter *progress-bar-length* 50
  "Length of progress bars for file transfer.")

(defparameter *waiting-message* "Waiting for USB device...~%"
  "Message to show when waiting for a USB to be inserted.")

(defparameter *default-expiration-days* 14
  "Default value for expiration days when not provided in INI or if invalid.")
(defparameter *default-check-frequency* 5
  "Default value for check frequency when not provided in INI or if invalid.")

(defun -main (&optional args)
  (let ((ini (parse (normalize-line-endings
                     (read-ini-to-string (or (second args) "config.ini")))
                    'list)))
    (ini-section-to-hash-table (get-ini-section ini "DEVICE-BADGE") *device-ids*)
    (ini-section-to-hash-table (get-ini-section ini "GENERAL") *settings*)
    (let ((check-frequency (or (parse-integer
                                (gethash "checkFrequency" *settings*)
                                :junk-allowed t)
                               *default-check-frequency*)))
      (format t *waiting-message*)
      (loop
        (import-from-usb *device-ids*
                         (gethash "vault" *settings*)
                         (gethash "viewable" *settings*))
        (sleep check-frequency)))))

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

(defun copy-files (from to &optional location-description)
  "Copy all files from `from' to `to'."
  (declare ((or pathname string) from)
           ((or pathname string) to))
  (let ((from (uiop/pathname:ensure-directory-pathname from))
        (to (uiop/pathname:ensure-directory-pathname to)))
    (ensure-directories-exist to)
    (walk-directory from
                    (lambda (file)
                      (copy-file-with-progress
                       file
                       (merge-pathnames-as-file
                        to
                        (format nil "~a~@[.~a~]"
                                (pathname-name file)
                                (pathname-type file)))
                       location-description)))))

(defun copy-file-with-progress (from to &optional location-description)
  "Copy files from `from' to `to', hopefully with a nice progress bar."
  (declare (pathname from)
           (pathname to)
           ((or string null) location-description))
  (let ((buf-size 4096))
    (with-open-file (input-stream from
                                  :direction :input
                                  :element-type '(unsigned-byte 8))
      (with-open-file (output-stream to
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
        (let ((buf (make-array buf-size :element-type (stream-element-type input-stream)))
              (total-bytes (file-length input-stream)))
          (format t "Copying file ~a~@[ to ~a~]...~%"
                  (file-namestring from)
                  location-description)
          (loop for pos = (read-sequence buf input-stream)
                with progress = 0
                with progress-bar-size = 0
                while (plusp pos)
                do (progn
                     (incf progress pos)
                     (when (> (floor (* *progress-bar-length*
                                        (/ progress total-bytes)))
                              progress-bar-size)
                       (format t "=")
                       (finish-output)
                       (incf progress-bar-size))
                     (write-sequence buf output-stream :end pos))
                finally (when (< progress-bar-size *progress-bar-length*)
                          (format t
                                  "~v@{~A~:*~}"
                                  (- *progress-bar-length* progress-bar-size)
                                  "=")))
          (format t "~%"))))))

(defvar *whitespace-chars* '(#\Space #\Newline #\Backspace #\Tab
                             #\Linefeed #\Page #\Return #\Rubout))

(defun whitespacep (character)
  "Check if `character' is whitespace."
  (declare (character character))
  (not (not (position character *whitespace-chars*))))

(defun trim-whitespace (string)
  (declare (string string))
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

(defun check-serial-numbers (serial-number-list serial-number-table)
  "Checks serial numbers in `serial-number-list' and returns the badge number
and drive letter of the first one that's found in `serial-number-table'."
  (declare ((proper-list string) serial-number-list)
           (hash-table serial-number-table))
  (loop for drive-letter in serial-number-list by #'cddr
        for serial-number in (rest serial-number-list) by #'cddr
        do (when (nth-value 1 (gethash serial-number serial-number-table))
             (return (list :badge-number (gethash serial-number
                                                  serial-number-table)
                           :drive-letter drive-letter)))))

(defun copy-files-from-device (drive-letter destination &optional location-description)
  "Copy all files from `drive-letter' to `destination'."
  (declare ((or string pathname) drive-letter)
           ((or string pathname) destination)
           ((or string null) location-description))
  (let ((destination (uiop/pathname:ensure-directory-pathname destination)))
    (ensure-directories-exist destination)
    (copy-files (format nil "~a/"
                        (uiop/pathname:ensure-directory-pathname drive-letter))
                destination
                location-description)))

(defun import-from-usb (device-ids vault-path viewable-path)
  "Check USBs for relavant ones, and if one is found, copy the files
from it to the necessary places."
  (declare (hash-table device-ids)
           ((or string pathname) vault-path)
           ((or string pathname) viewable-path))
  (let ((device (check-serial-numbers (retrieve-current-serial-numbers)
                                      device-ids))
        (vault-path (uiop/pathname:ensure-directory-pathname vault-path))
        (viewable-path (uiop/pathname:ensure-directory-pathname viewable-path)))
    (when device
      (let ((full-vault-path (format nil "~a/~a"
                                     vault-path
                                     (getf device :badge-number)))
            (full-viewable-path (format nil "~a/~a"
                                        viewable-path
                                        (getf device :badge-number))))
        (copy-files-from-device
         (getf device :drive-letter)
         full-vault-path
         "vault")
        (copy-files full-vault-path full-viewable-path "viewable location")
        (format t "Files copied successfully, please remove USB and press Enter.~%")
        (read-line)
        (format t *waiting-message*)))))
