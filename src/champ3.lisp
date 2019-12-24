;=============================HEADER====================================
; champ3.lisp "Timeline explorer" v3
; Copyleft Olivier Rey 2019-2020
;=======================================================================

(defparameter DIR-WC  "*/" "Wild card default for directories. Could be implementation specific")
(defparameter FILE-WC "*" "Wild card default for files.")

(defparameter LAST-DIR-CHAR "/")

;(defun list-files (dir)
; (directory (concatenate

(defun last-char (str)
  "Get the last char"
  (subseq str (- (length str) 1)))

(defun folder-p (str)
  "Check the last char of the str passed and return true if it is /"
  (if (string-equal (last-char str) LAST-DIR-CHAR) T NIL))

(defun filter-files (lst)
  "lst is a list of pathnames and not string, typically our from directory"
  (remove-if 'folder-p (mapcar 'namestring lst)))

(defun filter-folders (lst)
  "lst is a list of pathnames and not string, typically our from directory"
  (remove-if-not 'folder-p (mapcar 'namestring lst)))

(defun filter (lst)
  "lst is a list of pathnames and not string, typically our from directory"
  (values (remove-if-not 'folder-p (mapcar 'namestring lst))
          (remove-if     'folder-p (mapcar 'namestring lst))))

(defun explore-and-collect (dir acc)
  (if (null acc)
      (multiple-value-bind (dirs acc)
                           (filter (directory dir))
                           (dolist (d dirs)
                             (explore-and-collect d acc)))
      (multiple-value-bind (dirs files)
                           (filter (directory dir))
                           (progn
                             (setf acc (append acc files))
                             (dolist (d dirs)
                               (explore-and-collect d acc))))))
                           

 

