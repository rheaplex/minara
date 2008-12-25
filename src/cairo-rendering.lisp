;; cairo-rendering.lisp : rendering implementation using Cairo and OpenGL
;;
;; Copyright (c) 2008 Rob Myers, rob@robmyers.org
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
;; OpenGL utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-gl-for-rendering ()
  (gl:blend-func :src-alpha :one))

(defparameter *texture-rectangle* nil)

(defun make-texture-rectangle ()
  (setf *texture-rectangle* (gl:gen-lists 1))
  (gl:with-new-list (*texture-rectangle* :compile-and-execute)
    (gl:with-primitives :quads 
      (gl:tex-coord 0  0)
      (gl:Vertex -0.5 0.5 0)
    
      (gl:tex-coord 1  0)
      (gl:Vertex 0.5 0.5 0)
      
      (gl:tex-coord 1  1)
      (gl:Vertex 0.5 -0.5 0)
      
      (gl:tex-coord 0  1)
      (gl:Vertex -0.5 -0.5 0))))

(make-texture-rectangle)

(defun blit-texture (texture)
  (gl:active-texture 0)
  (gl:bind-texture :texture-2d texture)
;;  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (gl:call-list *texture-rectangle*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Rendering Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *matrix-stack* '()
  "Cairo doesn't have a matrix stack so we manage it in Lisp.")

;; Caches

(defclass render-cache ()
  ((width :accessor cache-width
	  :initarg :width)
   (height :accessor cache-height 
	   :initarg :height)
   (cairo-texture :accessor cache-cairo-texture 
		  :initarg :cairo-texture)
   (cairo-context :accessor cache-cairo-context 
		  :initarg :cairo-context)
   (opengl-texure-id :accessor cache-opengl-texture-id 
		     :initarg :opengl-texture-id)))

(defun make-cache (width height)
  (let ((texture (cairo:create-image-surface :argb32 width height)))
    (make-instance 'render-cache 
		   :width width
		   :height height
		   :cairo-texture texture
		   :cairo-context (cairo:create-context texture)
		   :opengl-texture-id (first (gl:gen-textures 1)))))

(defun cache-dispose (cache)
  (cairo:destroy (cache-cairo-context cache)) ;; Also disposes of the surface
  ;; dispose of the gl texture
  )

(defun cache-resize (cache width height)
  (if (and (= (cache-width cache) width)
	   (= (cache-height cache) height))
      cache
    (progn
      (cache-dispose cache)
      (make-cache width height))))

(defun cache-draw (cache)
  (blit-texture (cache-opengl-texture-id cache)))

(defun cache-record-begin (cache)
  (setf cairo:*context* (cache-cairo-context cache)))

(defun cache-record-end (cache)
  (setf cairo:*context* nil)
  (gl:bind-texture :texture-2d (cache-opengl-texture-id cache))
  (gl:tex-image-2d :texture-2d 0
		   :rgba (cache-width cache) (cache-height cache) 
		   0 :bgra :unsigned-byte 
		   (cairo:image-surface-get-data cache 4)))


;; Rendering
#|
  We use Cairo to render code into an OpenGL texture.
  Then we use OpenGL to blit the textures into the current GLUT window.

  Only rgb colour is supported at this level. Any other colour should be done
  in Lisp.

  Only filled shapes are supported at this level. Any lines or stroking should 
  be implemented in Lisp. This will remove compatibility, conversion and
  pre-press headaches.
|#

(defvar *path-started* nil)
(defvar *rendering-previous-x* nil)
(defvar *rendering-previous-y* nil)

(defun render-mask-begin ()
  nil)

(defun render-mask-end ()
  nil)

(defun render-masking-begin ()
  nil)

(defun render-masking-end ()
  nil)

;; Note we have rotate/translate/scale as well as matrix concatenate.
;; This is because they are often optimised operations
;; so it makes sense to allow them to be called from code.
;; Graphics toolkits without them can simulate them.

(defun set-colour (r g b a)
  (cairo:set-source-rgba r g b a))

(defun path-begin ()
  (setf *path-started* t)
  (setf *rendering-previous-x* 0.0)
  (setf *rendering-previous-y* 0.0))

(defun path-end ()
  (when *path-started*
    (cairo:close-path))
  (cairo:fill-path))

(defun move-to (h v)
  (when *path-started*
    (cairo:close-path))
  (cairo:move-to h v)
  (setf *path-started* t)
  (setf *rendering-previous-x* h)
  (setf *rendering-previous-y* v))

(defun line-to (h v)
  (cairo:line-to h v)
  (setf *rendering-previous-x* h)
  (setf *rendering-previous-y* v))

;; This can be optimized to reduce repeated calculation
;; e.g (* t t) and (* t t t) and (* 3) and -

(defparameter %bezier-steps 20)

(defun curve-to (h1 v1 h2 v2 h3 v3)
  (cairo:curve-to h1 v1 h2 v2 h3 v3)
  (setf *rendering-previous-x* h3)
  (setf *rendering-previous-y* v3))

(defun push-matrix ()
  (push (cairo:get-trans-matrix)
	*matrix-stack*))

(defun pop-matrix ()
  (cairo:set-trans-matrix (pop *matrix-stack*)))

(defun concatenate-matrix (m11 m12 m21 m22 m31 m32)
  (cairo:transform (cairo:make-trans-matrix m11 m12 m21 m22 m31 m32)))

(defun set-matrix (m11 m12 m21 m22 m31 m32)
  (cairo:set-trans-matrix (cairo:make-trans-matrix m11 m12 m21 m22 m31 m32)))

(defun identity-matrix ()
  (cairo:reset-trans-matrix))

(defun translate (sx sy)
  (cairo:translate sx sy))

(defun rotate (sr)
  (cairo:rotate sr))

(defun scale (sx sy)
  (cairo:scale sx sy))
