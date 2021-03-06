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
        (format t "~{~a:~10t~a~%~}~%" cd)))


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


;;; Saves the database to a given file name (will silently overwrite an existing
;;; file)
(defun save-db (filename)
    (with-open-file (out filename
                     :direction :output
                     :if-exists :supersede)
        (with-standard-io-syntax
            (print *db* out))))


;;; Loads a database from a file (this will overwrite the contents of *db*)
(defun load-db (filename)
    (with-open-file (in filename)
        (with-standard-syntax
            (setf *db* (read in)))))


;;; General select function that takes a function that gives the selection
;;; criteria
(defun select (selector-fn)
    (remove-if-not selector-fn *db*))


;;; General where function
(defun where (&key title artist rating (ripped nil ripped-p))
    #'(lambda (cd)
        (and
            (if title    (equal (getf cd :title)  title)  t)
            (if artist   (equal (getf cd :artist) artist) t)
            (if rating   (equal (getf cd :rating) rating) t)
            (if ripped-p (equal (getf cd :ripped) ripped) t))))


;;; Update the database
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
    (setf *db*
        (mapcar
            #'(lambda (row)
                (when (funcall selector-fn row)
                    (if title    (equal (getf cd :title)  title))
                    (if artist   (equal (getf cd :artist) artist))
                    (if rating   (equal (getf cd :rating) rating))
                    (if ripped-p (equal (getf cd :ripped) ripped)))))))



;;; Remove a row fromt he database
(defun delete-rows (selector-fn)
    (setf *db* (remove-if selector-fn *db*)))
