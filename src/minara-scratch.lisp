

;; How does Slime handle source file positions?

;; Anyway, deftool can register these

*compile-file-truename* 
*load-truename*

(defmacro file-path-string ()
  *compile-file-truename*)





(defmacro with-package (package &rest rest)
  (with-gensyms (old-buffer)
    `(setf old-buffer *package*)
    `(setf *package* package)
    @,rest
    `(setf *package* old-package)))

(defvar *temp-package-num* 0)

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
	      (return nil)))))))





(defclass minara-window (glut:window)
  ()
  (:default-initargs :width 500 :height 500 :pos-x 100 :pos-y 100
                     :mode '(:single :rgb) :title "movelight.lisp"))


(defmethod glut:display-window :before ((w polys-window))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat))


(defmethod glut:display ((w movelight-window))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:with-pushed-matrix
    (glu:look-at 0 0 5 0 0 0 0 1 0)
    (gl:with-pushed-matrix
      (gl:rotate (slot-value w 'spin) 1 0 0)
      (gl:light :light0 :position #(0 0 1.5 1))
      (gl:translate 0 0 1.5)
      (gl:disable :lighting)
      (gl:color 0 1 1)
      (glut:wire-cube 0.1)
      (gl:enable :lighting))
    (glut:solid-torus 0.275 0.85 8 15))
  (gl:flush))

(defmethod glut:reshape ((w movelight-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 40 (/ width height) 1 20)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:mouse ((w movelight-window) button state x y)
  (declare (ignore x y))
  (when (and (eq button :left-button) (eq state :down))
    (with-slots (spin) w
      (setf spin (mod (+ spin 30) 360)))
    (glut:post-redisplay)))

(defmethod glut:keyboard ((w movelight-window) key x y)
  (declare (ignore x y))

(defmethod initialize-instance :after ((w varray-window) &key)
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :smooth)
  (setup-pointers))

(defmethod glut:display ((w varray-window))
  (gl:clear :color-buffer-bit)
  (ecase deref-method
    (draw-array
     (gl:draw-arrays :triangles 0 6))
    (array-element
     (gl:with-primitives :triangles
       (gl:array-element 2)
       (gl:array-element 3)
       (gl:array-element 5)))
    (draw-elements
     (gl:draw-elements :polygon 4 :unsigned-int '(0 1 3 4))))
  (gl:flush))

(defmethod glut:reshape ((w varray-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width 0 height))

(defun rb-movelight ()
  (glut:display-window (make-instance 'movelight-window)))

      ;; create a display list for each of 6 characters
      (gl:shade-model :flat)
      (let ((base (gl:gen-lists 128)))
        (gl:list-base base)
        (loop for char in (list a e p r s) do
              (gl:with-new-list ((+ base (char-code (car char))) :compile)
                (draw-letter (cdr char))))
        (gl:begin :line-strip)
             (loop for (x y what) in instructions do
                   (case what
                     (pt (gl:vertex x y))
                     (stroke (gl:vertex x y)
                             (gl:end)
                             (gl:begin :line-strip))
                     (end (gl:vertex x y)
                          (gl:end)
                          (gl:translate 8 0 0))))))