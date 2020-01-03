; HTML utilities inspired by Paul Graham

(defmacro simple-tag (tag content)
  ; This macro is supposed to work with an overload of the
  ; default printing context. Otherwise, it prints to the console.
  `(format t "~&<~(~A~)>~A</~(~A~)>~%" ',tag ,content ',tag))


(defmacro with-tag (tag &rest body)
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


(defun format-html-header ()
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
                   (format-html-header)
                   (with-tag head
                         (simple-tag title ,tit)
                         (format-meta ,style))
                   (with-tag body
                         ,@body)))

(defun test1 ()
  (page 'toto "Champ oli" "style"
        (with-div "content"
                  (princ "rheu"))))

(defstruct elem
  timestamp fileloc)


(defun format-timestamp (ts)
  (format t "~2,'0d-~2,'0d-~2,'0d - ~2,'0d:~2,'0d:~2,'0d"
            (sixth ts)
            (fifth ts)
            (fourth ts)
            (third ts)
            (second ts)
            (first ts)))


(defun link-tag (name fileloc)
  (format t "~&<a href=\"~A\" target=\"_new\">~A</a>~%" fileloc name))








