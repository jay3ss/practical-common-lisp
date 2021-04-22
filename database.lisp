;;; Code for chapter 2 - A Simple Database

;;; Returns a plist representing a CD
(defun make-cd (title artist rating ripped)
    (list :title title :artist artist :rating rating :ripped ripped))
