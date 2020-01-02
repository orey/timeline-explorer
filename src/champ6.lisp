;=============================HEADER====================================
; champ4.lisp "Timeline explorer" v6
; Copyleft Olivier Rey 2019-2020
; This version provides a macro-based html toolkit
;=======================================================================

(defpackage "CHAMP"
            (:use "COMMON-LISP")
            (:nicknames "CHAMP")
            (:export "MAIN" "USAGE" "TAG1" "TAGN"))

(in-package champ)


(defparameter LAST-DIR-CHAR "/")
(defparameter PATTERN       "*.*")


(let ((myverbose nil))
  (defun setverbose ()
    (setf myverbose T))
  (defun unsetverbose ()
    (setf myverbose nil))
  (defun getverbose ()
    myverbose))


(let ((globalcounter 0)
      (intermediatecounter 0))
  (defun start-timecount ()
    (setf globalcounter (get-universal-time))
    (setf intermediatecounter (get-universal-time)))
  (defun timecount ()
    (let* ((temp (get-universal-time))
           (delta (- temp intermediatecounter)))
      (if (getverbose) (format t "Intermediate time spent: ~d seconds~%" delta))
      (setf intermediatecounter temp)
      delta))
  (defun stop-timecount ()
    (- (get-universal-time) globalcounter)))
      

(defun last-char (str)
  "Get the last char of a string."
  (subseq str (- (length str) 1)))


(defun folder-p (str)
  "Check the last char of the str passed and return true if it is '/'.
   Used for folders as namestrings."
  (if (string-equal (last-char str) LAST-DIR-CHAR) T NIL))


(defun check-folder (dir)
  "Check if it is a real folder"
  (if (not (probe-file dir)) NIL
    (folder-p (namestring (car (directory dir))))))


(defun get-good-folder-name (dir)
  (if (not (probe-file dir)) NIL
    (namestring (car (directory dir)))))


(defun files-in-dir (dir)
  "dir is a string."
  (remove-if 'folder-p (mapcar 'namestring (directory (concatenate 'string dir PATTERN)))))


(defun dirs-in-dir (dir)
  "dir is a string"
  (remove-if-not 'folder-p (mapcar 'namestring (directory (concatenate 'string dir PATTERN)))))


(defun find-files (dir)
  "Main recursive loop to find files"
  (let ((acc nil))
    (if (getverbose) (progn (format t "Before recursive loop~%") (timecount)))
    (labels ((find-files1 (acc1 dir1)
                          (setf acc1 (concatenate 'list acc1 (files-in-dir dir1)))
                          (dolist (d (dirs-in-dir dir1))
                            (setf acc1 (find-files1 acc1 d)))
                          acc1))
            (setf acc (find-files1 acc dir)))))


(defun get-file-seconds (f)
  "Get the age of file in seconds"
  (with-open-file (s f)
                  (file-write-date s)))


(defun build-map (acc)
  "Create the customized hashmap. As several files can have the same timestamp,
   it is not possible to use a struct with primary key."
  (let ((out nil))
    (dolist (f acc)
      (setf out (concatenate 'list out (list (cons (get-file-seconds f) (list f))))))
    out))


(defun list> (a b)
  (if (> (car a) (car b)) T nil))

;========================== html toolkit

;--- struct for complex tag
(defstuct ctag
  name attributes)


(defmacro tag1 (str tag content)
  ; Generate tag on one line
  `(format ,str "~&<~(~A~)>~A</~(~A~)>~%" ',tag ,content ',tag))


(defmacro tagn (str tag &rest body)
  ; Generate tag on several lines for readability
  `(format ,str "~&<~(~A~)>~%~&~A~%~&</~(~A~)>~%" ',tag ,@body ',tag))


(defmacro complextag (str tag)
  ; Generate tag on one line
  `(format ,str "<~(~A~)>~A</~(~A~)>" ',tag ,content ',tag))






(defun format-elem (elem strea)
  (let ((dt (multiple-value-list (decode-universal-time (car elem))))
        (pn (car (directory (cadr elem)))))
    (format strea "<p>~2,'0d-~2,'0d-~2,'0d - ~2,'0d:~2,'0d:~2,'0d | <a href=\"~A\" target=\"_new\">~A</a></p>"
            (sixth dt)
            (fifth dt)
            (fourth dt)
            (third dt)
            (second dt)
            (first dt)
            (cadr elem)
            (concatenate 'string (pathname-name pn) "." (pathname-type pn)))))


(defun format-date (d level strea sidebar)
  (cond ((eql level 1) ; change of year
         (progn
           (format strea "<a name=\"~d\" /><h1>Year ~2,'0d</h1>" (car d) (car d))
           (format strea "<a name=\"~d_~d\" /><h2>Month ~2,'0d</h2>" (car d) (second d) (second d))
           (format strea "<h3>Day ~2,'0d</h3>" (third d))
           (format sidebar "<b><a href=\"#~d\">Year ~2,'0d</a></b>" (car d) (car d))
           (format sidebar "<a href=\"#~d_~d\">Month ~2,'0d</a>" (car d) (second d) (second d))))
        ((eql level 2) ; change of month
         (progn
           (format strea "<a name=\"~d_~d\" /><h2>Month ~2,'0d</h2>" (car d) (second d) (second d))
           (format strea "<h3>Day ~2,'0d</h3>" (third d))
           (format sidebar "<a href=\"#~d_~d\">Month ~2,'0d</a>" (car d) (second d) (second d))))
        ((eql level 3) ; change of day
         (format strea "<h3>Day ~2,'0d</h3>" (third d)))))


(let ((mydate '(3000 1 1))
      (mystart T))
  (defun generate-section (elem strea sidebar)
    (let* ((dt (multiple-value-list (decode-universal-time (car elem))))
           (ddate (list (sixth dt) (fifth dt) (fourth dt))))
      (cond ((< (car ddate) (car mydate))
             (progn
               (setf mydate ddate)
               (setf mystart nil)
               (format-date mydate 1 strea sidebar)))
            ((and (eql (car ddate) (car mydate))
                  (< (second ddate) (second mydate)))
             (progn
               (setf mydate ddate)
               (setf mystart nil)
               (format-date mydate 2 strea sidebar)))
            ((and (eql (car ddate) (car mydate))
                  (eql (second ddate) (second mydate))
                  (< (third ddate) (third mydate)))
             (progn
               (setf mydate ddate)
               (setf mystart nil)
               (format-date mydate 3 strea sidebar)))
            (t
             (if mystart
                 (progn
                   (print "This should not happen. dt then ddate")
                   (print dt)
                   (print ddate))))))))


(defun format-header (strea)
  (format strea
          "<!DOCTYPE html><html lang=\"en\"> \
<head> \
<title>Champollion Explorer</title> \
<meta charset=\"utf-8\" /> \
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /> \
<link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\" /> \
</head> \
<body> \
<div class=\"content\">"))

(defun format-sidebar (strea)
  (format strea "</div><div class=\"sidenav\">"))


(defun format-footer (strea)
  (format strea "</div></body></html>"))


(defun generate-page (dir output)
  (let ((sorted-list (sort (build-map (find-files dir)) #'list>))
        (sidebar (make-string-output-stream)))
    (if (getverbose)
        (progn (format t "After sorting~%") (timecount)))
    (with-open-file (strea output
                           :direction :output
                           :if-exists :supersede)
                    (format-header strea)
                    (dolist (elem sorted-list)
                      (generate-section elem strea sidebar)
                      (format-elem elem strea))
                    (format-sidebar strea)
                    (format strea (get-output-stream-string sidebar))
                    (format-footer strea))))



(defun usage ()
  (format t "---~%Timeline Explorer v4 usage:~%")
  (format t " * (champ:main :dir \"/path/to/wherever\") <- Generated file will be \"index.html\"~%")
  (format t " * (champ:main :dir \"/path/to/wherever\ :output \"my-index.html\")~%---~%")
  (format t "Available commands in package: (champ:usage), (champ:main :dir XXX)~%---~%"))


(defun not-folder-error (dir)
  (format t "Error: ~A is not a directory.~%" dir)
  (usage)
  (abort))


(defun main (&key dir (output "index.html") (verbose nil))
  (let ((truedir (get-good-folder-name dir)))
      (progn
        (if verbose (setverbose))
        (start-timecount)
        (generate-page truedir output)
        (format t "~A generated in ~d seconds~%" output (stop-timecount)))))


; Executed when package is loaded
(usage)

