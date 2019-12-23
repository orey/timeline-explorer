;=============================HEADER====================================
; champ3.lisp "Timeline explorer" v3
; Copyleft Olivier Rey 2019-2020
;=======================================================================

(defparameter DIR-WC  "*/" "Wild card default for directories. Could be implementation specific")
(defparameter FILE-WC "*" "Wild card default for files.")

(defvar sampledir "/home/olivier/olivier2/Bibliotheque/IT/Lisp/")

(defun list-files (dir)
  (let ((pn (pathname dir)))
    (directory (concatenate 'string pn FILE-WC)))

 

