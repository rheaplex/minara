;; rendering.lisp : rendering hooks
;;
;; Copyright (c) 2004, 2007 Rob Myers, rob@robmyers.org
;;
;; This file is part of Minara.
;;
;; Minara is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Minara is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Minara.  If not, see <http://www.gnu.org/licenses/>.


(in-package minara-rendering)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Rendering Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Caches

(defun cache-make ()
  (gl:gen-lists 1))

(defun cache-dispose (cache)
  (gl:delete-lists cache 1))

(defun cache-draw (cache)
  (gl:call-list cache)
  (gl:flush))

(defun cache-record-begin (cache)
  (gl:new-list cache :compile-and-execute))

(defun cache-record-end (cache)
  (declare (ignore cache))
  (gl:flush)
  (gl:end-list))


;; Rendering
#|
  See also rendering.lisp .

  We use OpenGL to render code into the current GLUT window.

  Since we're doing beziers, we use a glu_tesselator and have a cache for the
  resulting points. There's only one cache at the moment, which needs
  making threadsafe...

  Only rgb colour is supported at this level. Any other colour should be done
  in Lisp.

  Only filled shapes are supported at this level. Any stroking should be
  implemented in Lisp. This will remove compatibility, conversion and
  pre-press headaches.
|#

(defvar *glu-tesselator* nil)

(defvar *path-started* nil)
(defvar *rendering-previous-x* nil)
(defvar *rendering-previous-y* nil)

;; The tesselator requires lots of vertices stored in memory that persists
;;  until tesselation is finished.
;; We allocate these as individual blocks
;;  and keep a list of them for deallocation.
;; Replace with a block-based system like the old C one,
;;  or with a grow-only pool of vertices.

;; Our vertices are single floats.

(defvar *vertices-to-deallocate* '())

(defun cache-vertex (&rest xyz)
  (let ((vertex (cffi:foreign-alloc :float :count 3 :initial-contents xyz)))
    (setf *vertices-to-deallocate* 
	  (push vertex *vertices-to-deallocate*))
    vertex))

(defun deallocate-vertices ()
  (dolist (vertex *vertices-to-deallocate*)
    (cffi:foreign-free vertex))
  (setf *vertices-to-deallocate* 
	'()))

(defun render-mask-begin ()
  (gl:clear :stencil-buffer-bit)
  (gl:stencil-func :always 1 1)
  (gl:stencil-op :replace :replace :replace))

(defun render-mask-end ()
  nil)

(defun render-masking-begin ()
  (gl:clear-stencil 0)
  (gl:enable :stencil-test))

(defun render-masking-end ()
  (gl:disable :stencil-test)
  (gl:clear :stencil-buffer-bit))

(cffi:defcallback tesselator-error-callback :void ((err :int ))
  (error (format t "Tesselation error: ~d" err)))

(cffi:defcallback tesselator-combine-callback :void 
  ((coords :pointer) (vertex-data :pointer)
   (weights :pointer) (data-out :pointer))
  (declare (ignore vertex-data) (ignore weights))
  (setf (cffi:mem-ref data-out :float 3)
        (cache-vertex (cffi:mem-aref coords :double 0)
			     (cffi:mem-aref coords :double 1)
			     (cffi:mem-aref coords :double 2))))

(defcvar )

(defun tesselator-startup ()
  (setf *glu-tesselator* (glu::new-tess))
  (glu::tess-callback *glu-tesselator* :glu-tess-vertex 
		      (get-var-pointer ))
  (glu::tess-callback *glu-tesselator* :glu-tess-begin #'gl:begin)
  (glu::tess-callback *glu-tesselator* :glu-tess-end #'gl:end)
  (glu::tess-callback *glu-tesselator* :glu-tess-error
                   (cffi:callback tesselator-error-callback))
  (glu::tess-callback *glu-tesselator* :glu-tess-combine
                   (cffi:callback tesselator-combine-callback)))

;; Note we have rotate/translate/scale as well as matrix concatenate.
;; This is because OpenGL and PS have these as optimised operations
;; so it makes sense to allow them to be called from code.
;; Graphics toolkits without them can simulate them.

(defun set-colour (r g b a)
  (gl:color r g b a))

(defun path-begin ()
  (glu::tess-begin-polygon *glu-tesselator* 0)
  (setf *path-started* nil)
  (setf *rendering-previous-x* 0.0)
  (setf *rendering-previous-y* 0.0))

(defun path-end ()
  (when *path-started*
    (glu::tess-end-contour *glu-tesselator*)
    (setf *path-started* nil))
  (glu::tess-end-polygon *glu-tesselator*))

(defun move-to (h v)
  (when *path-started*
    (glu::tess-end-contour *glu-tesselator*))
  (glu::tess-begin-contour *glu-tesselator*)
  (let ((coords (cache-vertex h v 0.0)))
    (glu::tess-vertex *glu-tesselator* coords coords))
  (setf *path-started* t)
  (setf *rendering-previous-x* h)
  (setf *rendering-previous-y* v))

(defun line-to (h v)
  (let ((coords (cache-vertex h v 0.0)))
    (glu::tess-vertex *glu-tesselator* coords coords))
  (setf *rendering-previous-x* h)
  (setf *rendering-previous-y* v))

;; This can be optimized to reduce repeated calculation
;; e.g (* t t) and (* t t t) and (* 3) and -

(defparameter %bezier-steps 20)

(defun curve-to (h1 v1 h2 v2 h3 v3)
  (loop for i from 0.0 to 1.0 by (/ 1.0 %bezier-steps)
     do (let* ((q1 (+ (* i i i -1.0) (* i i 3.0) (* i -3.0) 1.0))
               (q2 (+ (* i i i 3.0) (* i i -6.0) (* i 3.0)))
               (q3 (+ (* i i i -3.0) (* i i 3.0)))
               (q4 (* i i i))
               (qx (+ (* q1 *rendering-previous-x*) 
		      (* q2 h1) (* q3 h2) (* q4 h3)))
               (qy (+ (* q1 *rendering-previous-y*) 
		      (* q2 v1) (* q3 v2) (* q4 v3)))
               (coords (cache-vertex qx qy 0.0)))
          (glu::tess-vertex *glu-tesselator* coords coords)))
  (let ((coords (cache-vertex h3 h3 0.0)))
    (glu::tess-vertex *glu-tesselator* coords coords))
  (setf *rendering-previous-x* h3)
  (setf *rendering-previous-y* v3))

(defun push-matrix ()
  (gl:push-matrix))

(defun pop-matrix ()
  (gl:pop-matrix))

(defun concatenate-matrix (m11 m12 m21 m22 m31 m32)
  (gl:mult-matrix (vector m11 m12 0.0 0.0
			  m21 m22 0.0 0.0
			  m31 m32 1.0 0.0
			  0.0 0.0 0.0 1.0)))

(defun set-matrix (m11 m12 m21 m22 m31 m32)
  (gl:load-matrix (vector m11 m12 0.0 0.0
			  m21 m22 0.0 0.0
			  m31 m32 1.0 0.0
			  0.0 0.0 0.0 1.0)))

(defun identity-matrix ()
  (gl:load-identity))

(defun translate (sx sy)
  (gl:translate sx sy 0.0))

(defun rotate (sr)
  (gl:rotate sr 0.0 0.0 1.0))

(defun scale (sx sy)
  (gl:scale sx sy 0.0))

;; Initialize the tesselator

(tesselator-startup)