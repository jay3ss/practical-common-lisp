;;; Code for chapter 2 - A Simple Database

;;; Returns a plist representing a CD
(defun make-cd (title artist rating ripped)
    (list :title title :artist artist :rating rating :ripped ripped))


;;; Adds a record to the database
(defun add-record (cd)
    (push cd *db*))


;;; Displays the contents of the database in an easy-to-read format
(defun dump-db ()
    (dolist (cd *db*)
        (format t "~{a:~10t~a%~}~%" cd)))


;;; Prompts the user to enter input
(defun prompt-read (prompt)
    (format *query-io* "~a: " prompt)
    (force-output *query-io*)
    (read-line *query-io*))