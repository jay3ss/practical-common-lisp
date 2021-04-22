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


;;; Prompts the user to enter a CD
(defun prompt-for-cd ()
    (make-cd
        (prompt-read "Title")
        (prompt-read "Artist")
        (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
        (y-or-n-p "Ripped [y/n]: ")))


;;; Prompts the user to enter multiple CDs
(defun add-cds ()
    (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))
