;=============================HEADER====================================
; champ.lisp "Timeline explorer" v 1.1
; Copyleft 1001nuits (http://www.1001nuits.org - August 2010
;=======================================================================
(defpackage "TIMELINE-EXPLORER"
            (:use "COMMON-LISP")
            (:nicknames "CHAMP")
            (:export "MAIN"))

(in-package timeline-explorer)

;=====================General constants =====================
(defparameter *symbols* (list 'get-file-info 'test-get-file-info
                      'remove-singletons 'test-remove-singletons) "General symbols of the file")
(defparameter *DIR-WC* "*/" "Wild card default for directories. Could be implementation specific")
(defparameter *DIR* "/")
(defparameter *FILE-WC* "*" "Wild card default for files.")
(defparameter *months* (vector "January" "February" "March" "April" "May" "June" 
                        "July" "August" "September" "October" "November" "December"))

;dictionary of fi-info instances
(defparameter *dict* nil)

(defun doc ()
    (dolist (elt *symbols*)
        (format t "Documentation for '~A': ~A~%" elt (documentation elt 'function))))

;=====================Get file info=====================
(defclass fi-info ()
    ((fi-univ :accessor univ
              :initarg :univ)
     (fi-name :accessor name
              :initarg :name)
     (fi-year :accessor year
              :initarg :year)
     (fi-month :accessor month
               :initarg :month) 
     (fi-day :accessor day
             :initarg :day)))

(defun get-file-info (f)
    "fi-finfo construct: gets file information and put them in a fi-info object"
    (let* ((n (with-open-file (s f) (file-write-date s)))
           (l (multiple-value-list (decode-universal-time n)))
           (day (fourth l))
           (month (fifth l))
           (year (sixth l)))
        (make-instance 'fi-info :univ n :name f :day day :month month :year year)))

(defmethod fi-more-recent? ((a fi-info) (b fi-info))
    "fi-finfo comparison operator"
    (if (> (univ a) (univ b)) t nil)) 

(defmethod format-link ((a fi-info))
    "fi-info link formatter"
    (let* ((fname (concatenate 'string (pathname-name (pathname (name a))) "." (pathname-type (pathname (name a)))))
           (fdir (subseq (name a) 0 (- (length (name a)) (length fname)))))
        (format nil "<a href=\"file://~A\" target=\"new\">~A</a> - <a href=\"file://~A\" target=\"new\">Folder</a>"
            (name a) fname fdir)))
            
(defmethod get-time ((a fi-info))
    "fi-finto time formatter"
    (let ((timeobj (multiple-value-list (decode-universal-time (univ a)))))
        (format nil "~2,'0d:~2,'0d:~2,'0d" (third timeobj) (second timeobj) (first timeobj))))

;===========================runfrom=======================
(defun runfrom (dir)
    "Get files in a directory and recurse on sub directories"
    (let ((files (directory (concatenate 'string dir *FILE-WC*))))
        (dolist (file files 'done-files)
            (push (get-file-info (namestring file)) *dict*)))
    (let ((subdirs (directory (concatenate 'string dir *DIR-WC*))))
        (dolist (subdir subdirs 'done-dirs)
            (runfrom (concatenate 'string (namestring subdir) *DIR*))))) ;recurse

;===========================formatting=======================
(defun html-format-dict (outputfile dict)
    "Prints a list of fi-info instances"
    (with-open-file (strea outputfile :direction :output
                                    :if-exists :supersede)
        (format strea "<html><head><title>Timeline Explorer</title></head>~%")
        (let ((tyear 0) (tmonth 0) (tday 0))
            (dolist (obj dict 'end)
                (let ( (tempyear (year obj))
                       (tempmonth (month obj))
                       (tempday (day obj))
                       (changeofyear nil)
                       (changeofmonth nil)
                       (changeofday nil))
                    (if (equal tyear 0)
                        (progn 
                            (setf tyear tempyear) ;init loop
                            (setf changeofyear t))
                        (if (not (equal tyear tempyear))
                            (progn (setf changeofyear t) (setf tyear tempyear))))
                    (if (equal tmonth 0)
                        (progn
                            (setf tmonth tempmonth) ;init loop
                            (setf changeofmonth t))
                        (if (not (equal tmonth tempmonth))
                            (progn (setf changeofmonth t) (setf tmonth tempmonth))))
                    (if (equal tday 0)
                        (progn
                            (setf tday tempday) ;init loop
                            (setf changeofday t))
                        (if (not (equal tday tempday))
                            (progn 
                                (setf changeofday t) (setf tday tempday))))
                    (if changeofyear (format strea "<hr><h1>Year ~d</h1>~%"
                            (year obj)))
                    (if changeofmonth (format strea "<hr><h2>~A ~d</h2>~%" 
                            (svref *months* (- (month obj) 1))(year obj)))
                    (if changeofday (format strea "<p><font color=\"blue\"><b><i>~d ~A ~d</i></b></font></p>~%" 
                            (day obj) (svref *months* (- (month obj) 1))(year obj)))
                    (format strea "<font size=\"-1\"><b>~A</b> - ~A<br></font>~%" 
                        (get-time obj)(format-link obj))
                    )))
        (format strea "</html>")))

;=====================main=====================
(defun champ-usage ()
    "Usage function"
    (format t "Timeline Explorer usage:~%  >(load \"champ.lisp\")(timeline-explorer:main \"output.html\" \"dir1/\" [...])~%")
    (format t "[...] can be other directories.~%Info: directories should end with '/' like '/home/user1/'")
    (abort))

(defun main(outputfilename &rest others)
    "Main entry point"
    (if (equal others nil) 
        (champ-usage)) 
    (if (not (equal (length *dict*) 0))
        (progn (setf *dict* nil)
            (format t "Info: memory has been cleaned up...~%")))
    (let ( (start (get-universal-time)) )
        (dolist (dirtemp others)
            (if (not (stringp dirtemp))
                (progn (format t "Invalid input parameter, string expected.~%") (champ-usage)))
            (runfrom dirtemp))
        (format t "~A records treated in ~A seconds~%" (length *dict*)(- (get-universal-time) start))
        (let ((temp (sort *dict* #'fi-more-recent?)))
            (format t "Sorting done. Writing output to file...~%")
            (html-format-dict outputfilename temp)
            (format t "Output written~%Total time: ~A seconds~%" (- (get-universal-time) start))))
    t)
 
