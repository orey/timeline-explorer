;=============================HEADER====================================
; champ3.lisp "Timeline explorer" v3
; Copyleft Olivier Rey 2019-2020
;=======================================================================

(defparameter LAST-DIR-CHAR "/")
(defparameter PATTERN       "*.*")


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


(defun files-in-dir (dir)
  "dir is a string."
  (remove-if 'folder-p (mapcar 'namestring (directory (concatenate 'string dir PATTERN)))))


(defun dirs-in-dir (dir)
  "dir is a string"
  (remove-if-not 'folder-p (mapcar 'namestring (directory (concatenate 'string dir PATTERN)))))

(defun find-files (dir)
  (if (not (check-folder dir))
      (print "Not a folder")
    (let ((acc nil))
      (labels ((find-files1 (acc1 dir1)
                            (setf acc1 (concatenate 'list acc1 (files-in-dir dir1)))
                            (dolist (d (dirs-in-dir dir1))
                              (setf acc1 (find-files1 acc1 d)))
                            acc1))
              (setf acc (find-files1 acc dir))))))


(defun get-file-seconds (f)
  (with-open-file (s f)
                  (file-write-date s)))

(defun build-map (acc)
  (let ((out nil))
    (dolist (f acc)
      (setf out (concatenate 'list out (list (cons (get-file-seconds f) (list f))))))
    out))


(defun list> (a b)
  (if (> (car a) (car b)) T nil))


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


(let ((mydate '(3000 1 1)))
  (defun generate-section (elem strea sidebar)
    (let* ((dt (multiple-value-list (decode-universal-time (car elem))))
           (ddate (list (sixth dt) (fifth dt) (fourth dt))))
      (cond ((< (car ddate) (car mydate))
             (progn
               (setf mydate ddate)
               (format-date mydate 1 strea sidebar)))
            ((and (eql (car ddate) (car mydate))
                  (< (second ddate) (second mydate)))
             (progn
               (setf mydate ddate)
               (format-date mydate 2 strea sidebar)))
            ((and (eql (car ddate) (car mydate))
                  (eql (second ddate) (second mydate))
                  (< (third ddate) (third mydate)))
             (progn
               (setf mydate ddate)
               (format-date mydate 3 strea sidebar)))))))


(defun format-header (strea)
  (format strea
          "<!DOCTYPE html><html lang=\"en\"> \
<head> \
<title>Champollion Explorer</title> \
<meta charset=\"utf-8\"> \
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"> \
<link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\"> \
</head> \
<body> \
<div class=\"content\">"))


(defun format-sidebar (strea)
  (format strea "</div><div class=\"sidenav\">"))


(defun format-footer (strea)
  (format strea "</div></body></html>"))


(defun generate-page (dir)
  (let ((sorted-list (sort (build-map (find-files dir)) #'list>))
        (sidebar (make-string-output-stream)))
    (with-open-file (strea "index.html"
                           :direction :output
                           :if-exists :supersede)
                    (format-header strea)
                    (dolist (elem sorted-list)
                      (generate-section elem strea sidebar)
                      (format-elem elem strea)
                      (princ "+"))
                    (format-sidebar strea)
                    (format strea (get-output-stream-string sidebar))
                    (format-footer strea))))


;======================================Test programs

(defun test1 ()
  (find-files "/home/olivier/Documents/github/"))

(defun test2 ()
  (find-files "/home/olivier/Documents/bloub/"))

(defun test3 ()
  (get-file-seconds "/home/olivier/Documents/github/toto.txt"))

(defun test4 ()
  (build-map (find-files "/home/olivier/Documents/github/")))

(defun test5 ()
  (format-header T)
  (print "------------------")
  (format-footer T))

(defun test6 ()
  (generate-page "/home/olivier/Documents/OREYBOX/Biblio/"))

(defun test-suite ()
  (test1)
  (test2)
  (test3)
  (test4)
  (test5)
  (test6))
