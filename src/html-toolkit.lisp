; Utilities coming from Paul Graham

(defmacro as (tag content)
  ; This macro is supposed to work with an overload of the
  ; default printing context. Otherwise, it prints to the console.
  `(format t "~&<~(~A~)>~A</~(~A~)>~%" ',tag ,content ',tag))


(defmacro with (tag &rest body)
  `(progn
     (format t "~&<~(~A~)>~%" ',tag)
     ,@body
     (format t "~&</~(~A~)>~%" ',tag)))


(defmacro with-div (style &rest body)
  `(progn
     (format t "~&<div class=\"~(~A~)\">~%" ',style)
     ,@body
     (format t "~&</div>~%")))

(defun html-file (base)
  (format nil "~(~A~).html" base))

(defun format-header ()
  (format t "<!DOCTYPE html><html lang=\"en\">~%"))

(defun format-meta (style)
  (format t "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />~%")
  (format t "<link rel=\"stylesheet\" type=\"text/css\" href=\"~(~A~).css\" />~%" style))

;(defmacro page (name title style &rest body)
;  (let ((ti (gensym))
;        `(with-open-file (*standard-output*
;                          (html-file ,name)
;                          :direction :output
;                          :if-exists :supersede)
;                         (let ((,ti ,title))
;                           (format-header)
;                           (with head
;                                 (as title ,ti)
;                                 (format-meta ,style))
;                           (with body
;                                 ,@body))))))
                                 
(defmacro page (name tit style &rest body)
  `(with-open-file (*standard-output*
                    (html-file ,name)
                    :direction :output
                    :if-exists :supersede)
                   (format-header)
                   (with head
                         (as title ,tit)
                         (format-meta ,style))
                   (with body
                         ,@body)))

(defun test1 ()
  (page 'toto "Champ oli" "style"
        (with-div "content"
                  (princ "rheu"))))



