;--- struct for complex tag
(defstruct ctag
  name attributes)

(defun test1 ()
  (let ((a (make-ctag :name "tag1"
                      :attributes '(("att1" "value1")("att2" "value2")))))
    a))

(defun ctag-to-html (tag)
  
