(in-package minara)

(defmacro with-package (package &rest rest)
  (let ((old-package (gensym)))
    `(let ((,old-package *package*)) 
       (setf *package* ,package)
       ,@rest
       (setf *package* ,old-package ))))

#|(defvar *temp-package-num* 0)

(defun disposable-package-with-package (base-package)
  "Create a disposable package that includes the symbols from base-package."
  (+1 *temp-package-num*)
  ('make-package (format nil "%%temp-%d" *temp-package-num*)
	 :use (list base-package)))

(defun eval-buffer-with-package (buffer package)
  "Eval buffer in a temporary package with the symbols exported from package."
  (with-package (disposable-package-with-package package)
    (let ((stream (make-buffer-stream (contents buffer))))
      (loop
       do (let ((form (read stream nil stream)))
	    (if (ne form stream)
		(eval form)
	      (return nil)))))))|#