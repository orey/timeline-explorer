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

(defun test1 ()
  (find-files "/home/olivier/Documents/github/"))

(defun test2 ()
  (find-files "/home/olivier/Documents/bloub/"))

(defun get-file-seconds (f)
  (with-open-file (s f)
                  (file-write-date s)))

(defun test3 ()
  (get-file-seconds "/home/olivier/Documents/github/toto.txt"))

(defun build-map (acc)
  (let ((out nil))
    (dolist (f acc)
      (setf out (concatenate 'list out (list (cons (get-file-seconds f) (list f))))))
    out))
    
(defun test4 ()
  (build-map (find-files "/home/olivier/Documents/github/")))

(defun list> (a b)
  (if (> (car a) (car b)) T nil))

(defun generate-page (dir)
  (let ((sorted-list (sort (build-map (find-files dir)) #'list>)))
    (dolist (elem sorted-list)
      (print elem))))

(defun test5 ()
  (generate-page "/home/olivier/Documents/github/"))
