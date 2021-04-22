;;; Code for chapter 2 - A Simple Database

;;; Returns a plist representing a CD
(defun make-cd (title artist rating ripped)
    (list :title title :artist artist :rating rating :ripped ripped))


;;; Adds a record to the database
(defun add-record (cd)
    (push cd *db*))
