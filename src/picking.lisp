;; picking.lisp : minara scheme development file
;;
;; Copyright (c) 2004-2006, 2007 Rob Myers, rob@robmyers.org
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(in-package minara-picking)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Picking (object selection, highlighting, choosing, whatever)
;;
;; How does this work?
;;
;; Precis:
;; Count the shapes, hit-test the shapes, store the buffer positions that hit, 
;; find the s-expressions that contain those buffer positions.
;;
;; Details:
;; We install a rendering protocol that counts the number of 
;; occurrences of the (begin-path) operator. 
;; This allows us to identify which shape is being drawn.
;;
;; We then count the number of intersections between a ray
;; from the target point and the result of evaluating each drawing command.
;; When we get to the end of a path, if the number of intersections
;; are odd the point is inside the shape so we push the current path number 
;; onto a list.
;; This count indicates the number of the hit path. There may be more than one,
;; stored in Z-order.
;;
;; We then use the count to search the text for the relevent path description.
;;
;; This is slow, but we can cache a lot of the information and improve 
;; performance.
;;
;; Note that picking returns a list of every item under the picking point
;; from back to front rather than just the frontmost object. 
;; A normal "selection" tool can then disard everything apart from the topmost
;; object.
;;
;; Area-based selection will also be required and can be implemented similarly.
;; A point and a rectangle (or other shape eg pen-drawing based selection) are
;; just geometries to check for intersection or containment after all.
;;
;; This is all very single threaded. Attach the vars to the buffer being picked.
;;
;; And it's inefficient, having no optimization for bounding boxes for example
;; It is possible to generate, cache and update bounding boxes and other
;; optimizations (hashed to object counts) when editing the text, but this will
;; be done once the basic functionality is implemented.
;; Ideally we'd evaluate the buffer front-to-back. :-)
;; Nothing should be done or assumed to prevent the model of rebinding the 
;; drawing routines to the picking routines then evaluating the drawing buffer
;; from working.
;;
;; For picking inside of functions e.g. (define (square) moveto...) ... (square)
;; Rebind define to keep track of the call stack, or can we get the stack from
;; Guile?
;; Picking inside of functions is a TODO.
;;
;; Note that we will not be able to pick every imaginable piece of code, eg
;; random scribbles that do not have their seed in the main buffer won't pick,
;; and code from over the network may be problematic.
;; So provide guidelines for producing pickable code.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A picking hit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass picking-hit ()
  ((index :accessor picking-hit-index
	  :initarg :index)
   (from :accessor picking-hit-from
	  :initarg :from)
   (to :accessor picking-hit-to
	  :initarg :to)
   (transformation :accessor picking-hit-transformation
	  :initarg :transformation)))
		      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals
;; Used within a single pass through the picking routines
;; (so should be thread-local)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The buffer string chopped to the end of the last matched sexp
;; RENAME
(defvar *previous-buffer-string* "")

;; The picking point
(defvar pick-x nil)
(defvar pick-y nil)

;; Where the last drawing operation left the pen
(defvar previous-x nil)
(defvar previous-y nil)

;; Keep track of which colour we're currently using
;; (We'll need to keep track of any other fill methods as well,
;;  but we will never stroke, so we won't need to track that.)
(defvar current-colour nil)

;; Keep track of the current transformations
(defvar current-translate nil)
(defvar current-rotate nil)
(defvar current-scale nil)
(defvar transformation-stack (make-matrix-stack))
(defvar current-transform nil)

;; RENAME

;; Keep track of which polygon we're currently drawing
(defvar current-polygon 0)
;; Keep track of the last polygon so we can skip polys to speed up sexp matching
(defvar previous-polygon 0)

;; How many ray-line intersections there are with the current polygon
(defvar intersections 0)

;; RENAME

;; The list of polygons picked and their transforms. This will be back-to-front.
(defvar picked-polygons '())

;; Reset the picking state
(defun start-picking ()
  (setf pick-x nil)
  (setf pick-y nil)
  (setf previous-x nil)
  (setf previous-y nil)
  (setf current-colour 0)
  (setf current-polygon 0)
  (setf previous-polygon 0)
  (setf intersections 0)
  (setf current-rotate 0)
  (setf current-scale 0)
  (setf current-translate 0)
  (setf transformation-stack (make-matrix-stack))
  (setf picked-polygons '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Picking protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keep track of the colour
(defun set-colour (r g b a)
  (setf current-colour (+ current-colour 1)))

;; Keep track of the transforms
;; TODO: do it.

(defun push-matrix ()
    (setf transformation-stack 
	  (stack-push-matrix transformation-stack)))

(defun pop-matrix ()
    (setf transformation-stack 
	  (stack-pop-matrix transformation-stack)))

(defun concatenate-matrix (a b c d e f)
    (setf transformation-stack 
	  (stack-concatenate-matrix transformation-stack
				    (make-matrix a b c d e f))))

(defun set-matrix (a b c d e f)
    (setf transformation-stack 
	  (stack-set-matrix transformation-stack 
			    (make-matrix a b c d e f))))

(defun identity-matrix ()
    (setf transformation-stack 
	  (stack-set-matrix transformation-stack 
			    (identity-matrix))))

(defun translate  (x y)
    (setf transformation-stack 
	  (stack-concatenate-matrix transformation-stack 
				    (matrix-translate-make x y)))
  (setf current-translate (+ current-translate 1)))

(defun scale (x y)
    (setf transformation-stack 
	  (stack-concatenate-matrix transformation-stack 
				    (matrix-scale-make x y)))
  (setf current-scale (+ current-scale 1)))

(defun rotate (theta)
    (setf transformation-stack 
	  (stack-concatenate-matrix transformation-stack 
				    (matrix-rotate-make theta)))
  (setf current-rotate (+ current-rotate 1)))

(defun transform (x y)
    (matrix-point-transform x y (stack-current-matrix transformation-stack)))

;; Start a new pick pass
(defun path-begin ()
  (setf intersections 0)
  (setf current-polygon (+ current-polygon 1)))

;; Check the intersections. Even = inside, Odd = oustide
;; Store the colour and anything else in a list with the polygon number?
(defun path-end ()
  (if (and (oddp intersections)
           (not (= intersections
                   0)))
      (setf picked-polygons 
	    (cons 
	     (get-picked-path)
	     picked-polygons)))
  (setf intersections 0))

;; Keep track of the "previous" position
(defun move-to (xx yy)
  (multiple-value-bind (x y) (picking-transform xx yy)
    (setf previous-x x)
    (setf previous-y y)))

;; Where to send the ray -uh- line to. Oh, the horror! Fixme.

(defparameter %ray-x 65535.0)
  
;; Line segment hit test

(defun line-to (xx yy)
  (multiple-value-bind (x y) (picking-transform xx yy)
    (if (lines-intersect-vertices previous-x 
				  previous-y 
				  x 
				  y 
				  pick-x 
				  pick-y 
				  %ray-x 
				  pick-y)
	(setf intersections (+ intersections
			       1)))
    (setf previous-x x)
    (setf previous-y y)))

;; Curve hit test

(defun curve-to (xx1 yy1 xx2 yy2 xx3 yy3)
  (multiple-value-bind (x1 y1) (picking-transform xx1 yy1)
    (multiple-value-bind (x2 y2) (picking-transform xx2 yy2)
      (multiple-value-bind (x3 y3) (picking-transform xx3 yy3)
	(let ((count (line-bezier-intersection-count-vertices 
		      pick-x pick-y %ray-x pick-y
		      previous-x previous-y x1 y1 x2 y2 x3 y3)))
	  (setf previous-x x3)
	  (setf previous-y y3)
	  (setf intersections (+ intersections
				 count)))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer routines
;; Find the positions in the buffer that match the s-expression that was
;; evaluated to draw a particular shape.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Find the index of the start of the nth occurrence of a phrase
(defun nth-occurrence (buffer phrase nth)
  (nth-occurrence-aux buffer phrase nth 0 0))

(defun nth-occurrence-aux (buffer phrase target count position)
  ;; Terminal clause, return the value
  (if (or (= target count)
	  ;; Catch the error condition
	  (not position))
      position
      ;; Otherwise search forward
      (nth-occurrence-aux buffer phrase target (+ count 1)
			  ;; +1 so we don't re-match the same string
			  (+ (string-contains buffer phrase position) 1))))

;; Get an s-expression from the ( at the character index given to the closing )
(defun sexp-bounds (buffer start)
  (let ((end (sexp-bounds-aux buffer (+ start 1) 1)))
    (values start end)))

;; Recursively find the end of the s-expression
(defun sexp-bounds-aux (buffer current count)
  ;; Terminal clause, return the value
  (if (= count 0)
      current
      ;; Otherwise we get the current char and check it
      (let ((current-char (substring buffer current (+ current 1))))
	(cond
	  ((string= current-char "(")
	   (sexp-bounds-aux buffer (+ current 1) (+ count 1)))
	  ((string= current-char ")") 
	   (sexp-bounds-aux buffer (+ current 1) (- count 1)))
	  (else
	   (sexp-bounds-aux buffer (+ current 1) count))))))

;; Get an s-expression from the ) at the character index given to the opening (
(defun reverse-sexp-bounds (buffer start)
  (let ((end (sexp-bounds-aux buffer (- start 1) 1)))
    (values start end)))

;; Recursively find the beginning of the s-expression
(defun reverse-sexp-bounds-aux (buffer current count)
  ;; Terminal clause, return the value
  (if (= count 0)
      current
      ;; Otherwise we get the current char and check it
      (let ((current-char (substring buffer current (- current 1))))
	(cond
	  ((string= current-char ")")
	   (sexp-bounds-aux buffer (- current 1) (+ count 1)))
	  ((string= current-char "(") 
	   (sexp-bounds-aux buffer (- current 1) (- count 1)))
	  (otherwise
	   (sexp-bounds-aux buffer (- current 1) count))))))

;; Get the nth sexp starting with the given operator
(defun nth-sexp-bounds (buffer operator count)	  
  (let* ((op-with-bracket (string-append "(" operator))
	 (start (nth-occurrence buffer op-with-bracket count)))
    (sexp-bounds buffer start)))

;; Get the nth colour statement in the buffer
(defun get-nth-sexp (buffer-str func nth)
  (nth-sexp-bounds buffer-str func nth))
	
;; Get the nth path in the buffer
(defun get-nth-path (buffer nth)
  (let ((path-from (- (nth-occurrence buffer
				      "(path-begin)" nth)
		      1))
	;; 10 to move past "path-end"
	(path-to (+ (nth-occurrence buffer 
				    "(path-end)" nth) 10)))
    (values path-from path-to)))

(defun sexp-before (buffer-str pos)  
  (let ((sexp-start (string-rindex buffer-str #\( 0 pos)))
    (if sexp-start
	(sexp-bounds buffer-str sexp-start)
	nil)))

(defun sexp-after (buffer-str pos)
  (let ((sexp-start (string-index buffer-str #\( pos)))
    (if sexp-start
	(sexp-bounds buffer-str sexp-start)
	nil)))

(defun sexp-symbol-string (buffer-str sexp-pos)
  (if (string= (substring buffer-str sexp-pos (+ sexp-pos 1))
	       "(")
      (let ((symbol-end (or (string-index buffer-str #\space sexp-pos)
			    (string-index buffer-str #\) sexp-pos))))
	(if symbol-end
	    (substring buffer-str 
		       (+ sexp-pos 1 ) 
		       symbol-end)
	    nil))
      nil))

(defun get-picked-path ()
  (let* ((nnth (- current-polygon previous-polygon)))
    (let-values (((path-from path-to)
		  (get-nth-path previous-buffer-string nnth)))
		(setf previous-buffer-string (substring previous-buffer-string
							path-to))
		(setf previous-polygon current-polygon)
		(make-picking-hit current-polygon
				  path-from
				  path-to
				  (copy-tree (stack-current-matrix
					      transformation-stack))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Picking in the main window buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-picking-point (x y)
  ;; Translate?
  (setf pick-x x)
  (setf pick-y y))

(defun pick-paths (buf x y)
  (install-picking-rendering-protocol)
  (set-picking-point x y) ;; Translate?
  (let ((buffer-string (buffer-to-string buf)))
    (setf previous-buffer-string buffer-string)
    (eval-string buffer-string))
  (if (eq picked-polygons '())
      nil
      picked-polygons))

(defun pick-paths-window (win x y)
  (pick-paths (window-buffer-main win) x y))

(defun pick-path (buf x y)
  (let ((picks (pick-paths buf x y)))
    (if picks
	;; Return the range and transform
	(last picks)
	nil)))

(defun pick-path-window (win x y)
  (pick-path (window-buffer-main win) x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;; Horribly tied to first minara logo file version. Need better checks...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(test-section "picking: s-expressions")
;;(defvar buf (find-file "../minara.minara"))
;;(test 105 (car (get-nth-path buf 1)))
;;(test 5025 (cadr (get-nth-path buf 1)))
;;(test 1063 (car (get-nth-sexp buf "move-to" 3)))
;;(test 1088 (cadr (get-nth-sexp buf "move-to" 3)))

;;(test-section "picking: picking")
;;(defvar %pickbuf (make-gap-buffer))
;;(gb-insert-string! %pickbuf
	;;		     ";;minara file\n(set-colour 0.0 0.0 1.0)\n(path-begin)\n(move-to 10 10)\n(line-to 10 100)\n(line-to 100 10)\n(line-to 10 10)\n(path-end)\n(fill-path)\n")
;;(test 40 (begin
;;	   (install-picking-rendering-protocol)
;;	   (eval-string (gb->string %pickbuf))))