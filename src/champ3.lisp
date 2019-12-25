;=============================HEADER====================================
; champ3.lisp "Timeline explorer" v3
; Copyleft Olivier Rey 2019-2020
;=======================================================================

(defparameter LAST-DIR-CHAR "/")
(defparameter PATTERN "*.*")

(defun last-char (str)
  "Get the last char of a string"
  (subseq str (- (length str) 1)))

(defun folder-p (str)
  "Check the last char of the str passed and return true if it is /
   Used for folders as namestrings"
  (if (string-equal (last-char str) LAST-DIR-CHAR) T NIL))

(defun files-in-dir (dir)
  "dir is a string"
  (remove-if 'folder-p (mapcar 'namestring (directory (concatenate 'string dir PATTERN)))))

(defun dirs-in-dir (dir)
  "dir is a string"
  (remove-if-not 'folder-p (mapcar 'namestring (directory (concatenate 'string dir PATTERN)))))

(defvar acc '())

(defun find-files (dir)
  (setf acc (concatenate 'list acc (files-in-dir dir)))
  (dolist (d (dirs-in-dir dir))
    (find-files d)))

(defvar mymap (make-hash-table))

(defun record-date-in-map (f)
  (with-open-file (s f)
                  (setf (gethash (file-write-date s) mymap) f)))


(defun find-all-files (dir)
  (find-files dir)
  (print acc)
  (print (length acc))
  (dolist (x acc)
    (record-date-in-map x))
  (print mymap))
