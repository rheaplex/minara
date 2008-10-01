;; geometry.lisp : minara scheme development file
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

(in-package minara)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematical functions for geometric calculations.
;; Particularly for hit-testing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Find the distance between the points x1,y1 and x2,y2

(defun distance-between-points (x1 y1 x2 y2)
  (abs (sqrt (+ (expt (- x2 
			 x1)
		      2.0)
		(expt (- y2
			 y1)
		      2.0)))))

;; Find polar angle of point px,py around point ox,oy

(defun angle-around-point (ox oy px py)
  (let ((x (- px
	      ox))
	  (y (- py
		oy)))
    (case x
      ((0.0)
       (cond 
	 ((< y 0.0) 
	  270.0)
	 ((> y 0.0)
	  90.0)
	 ((= y 0.0)
	  0.0)))
      (otherwise
       (let ((r (* (atan (/ y
			      x)) 
		     (/ 180.0
			3.14159))))
	   (if (< x 0.0)
	       (+ r
		  180.0)
	       r))))))

(defparameter degrees-to-radians
  (/ pi
     180.0))

(defun degrees-to-radians (degrees)
  (* degrees
     degrees-to-radians))

(defun rotate-point-around-point (x1 y1 x2 y2 theta)
  (let ((st (sin (degrees-to-radians theta)))
	(ct (cos (degrees-to-radians theta)))
	(x (- x2
	      x1))
	(y (- y2
	      y1)))
    (cons (+ x1 
	     (- (* x
		   ct)
		(* y
		   st)))
	  (+ y1 
	     (+ (* y
		   ct)
		(* x
		   st))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line-line intersection
;; http://astronomy.swin.edu.au/~pbourke/geometry/lineline2d/
;; Returns the t where the second line intersects the first line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lines-intersect-vertices (p1x p1y p2x p2y ;; First line 
				 p3x p3y p4x p4y);; Second line 
  (let ((denominator (- (* (- p4y p3y)
			   (- p2x p1x)) 
			(* (- p4x p3x)
			   (- p2y p1y)))))
    (if (= denominator 0.0)
	nil ;; Parallel lines
	(let ((ua (/ (- (* (- p4x p3x) 
			   (- p1y p3y))
			(* (- p4y p3y) 
			   (- p1x p3x)))
		     denominator))
	      (ub (/ (- (* (- p2x p1x) 
			   (- p1y p3y)) 
			(* (- p2y p1y) 
			   (- p1x p3x))) 
		     denominator)))
	  (if (and (>= ua 0.0)
		   (<= ua 1.0)
		   (>= ub 0.0)
		   (<= ub 1.0)) ;; Intersection (or not)
	      ua
	      nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass point ()
  ((x :initarg :x
      :initform 0.0
      :accessor point-x)
   (y :initarg :y
      :initform 0.0
      :accessor point-y)))

(defun add-point (p1 p2)
  (make-instance 'point 
		 :x (+ (point-x p1) (point-x p2))
		 :y(+ (point-y p1) (point-y p2))))

(defun divide-point (p d)
  (make-instance 'point :x (/ (point-x p) d) :y (/ (point-y p) d)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find out which side of an infinite line through p1 and p2 that p0 lies on.
;;   < 0 = left, > 0 = right, == 0 = exactly on.
;; From draw-something :-)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun point-line-side (p0 p2 p1)
  (- (* (- (point-x p1) (point-x p0)) (- (point-y p2) (point-y p0)))
     (* (- (point-x p2) (point-x p0)) (- (point-y p1) (point-y p0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beziers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Have a bezier class?

;; Evaluate the bezier at time t
;; Converted from the C in minara_rendering.c

(defun bezier-eval (h0 v0 h1 v1 h2 v2 h3 v3 time)
  (let ((q1 (+ (* time time time -1.0) (* time time 3) (* time -3.0) 1.0))
	(q2 (+ (* time time time 3.0) (* time time -6.0) (* time 3.0)))
	(q3 (+ (* time time time -3.0) (* time time 3.0)))
	(q4 (* time time time)))
    (let ((qx (+ (* q1 h0) (* q2 h1) (* q3 h2) (* q4 h3)))
	  (qy (+ (* q1 v0) (* q2 v1) (* q3 v2) (* q4 v3))))
      (values qx qy))))

;; Divide the bezier into two equal halves
;; Left first, then right. Two lists of vertex co-ordinates

#|(defun split-bezier (h0 v0 h1 v1 h2 v2 h3 v3)
  (let* ((p01 (/ (add-point h0 v0 h1 v1) 2.0))
	 (p12 (/ (add-point h1 v1 h2 v2) 2.0))
	 (p23 (/ (add-point h2 v2 h3 v2) 2.0))?
	 (p012 (/ (add-point (point-x p01) (point-y p01)
			     (point-x p12) (point-y p12)) 2.0))
	 (p123 (/ (add-point (point-x p12) (point-y p12)
			     (point-x p23) (point-y p23)) 2.0))
	 (p0123 (/ (add-point (point-x p012) (point-y p012)
			      (point-x p123) (point-y p123)) 2.0)))
    (list (list h0 v0 h01 v01 (point-x 012) (point-y 012) 
		(point-x 0123) (point-y 0123))
	  (list (point-x 0123) (point-y 0123) (point-x 123) (point-y 123)
		h23 v23 h3 v3))))|#

;; Decide the flatness of the bezier

;; Line-bezier intersection
;; Terrible. Almost as good as our bezier drawing
;; Replace with something less embarrasingly awful, 
;; recursive subdivision at least
;; And the name is bad, too

(defparameter *bez-eval-steps* 10)
(defparameter *bez-eval-step* (/ 1.0 *bez-eval-steps*))

;; Doesn't handle pathological cases

(defun line-bezier-intersection-count-vertices  (ax ay bx by h0 v0 h1 v1 h2 v2 
						 h3 v3)
  (let ((crossings '())
        (ph h0)
        (pv v0))
    ;; Step through the bezier at a very coarse resolution
    (loop for time from 0.0 to 1.0 by *bez-eval-step*
       ;; Return the count of intersections
       do (let* ((p (bezier-eval h0 v0 h1 v1 h2 v2 h3 v3 time))
		 (h (point-x p))
		 (v (point-y p))
		 (ti (lines-intersect-vertices ph pv h v
					       ax ay bx by)))
	    ;; Counting the number of intersections
	    ;; Ignoring intersections at 0.0 because 
	    ;; they are the same as the previous 1.0 intersection...
	    (if (and ti
		     (> ti 
			0.0))
		(let ((intersection (cons h v)))
		  ;; Avoid duplicating points from adjacent sections
		  ;; when the ray passes exactly through the point
		  (setf crossings (assoc intersection
					 crossings))))
	    (setf ph h)
	    (setf pv v))
       finally  (length crossings))))

  
;; Get the normal of the bezier at point t


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|(test-section "geometry: intersection")
(test 0.0 (lines-intersect-vertices 0.0 0.0 0.0 100.0 
				    0.0 0.0 100.0 0.0))
(test 0.5 (lines-intersect-vertices 0.0 0.0 0.0 100.0 
				    0.0 50.0 100.0 50.0))
(test 1.0 (lines-intersect-vertices 0.0 0.0 0.0 100.0 
				    0.0 100.0 100.0 100.0))
(test 0.5 (lines-intersect-vertices 0.0 0.0 100.0 100.0 
				    0.0 50.0 100.0 50.0))
(test nil (lines-intersect-vertices 0.0 0.0 100.0 100.0 
				    1000.0 1000.0 1000.0 1000.0))
(test 0 (line-bezier-intersection-count-vertices 20 0 80 0
						 0 0 0 100 100 100 100 0))
;; Aligned with end-point of a subdivision (given t step of 0.1)
(test 1 (line-bezier-intersection-count-vertices 50 0 50 150
						 0 0 0 100 100 100 100 0))
;; Not aligned with end-point of subdivision (given t step of 0.1)
(test 1 (line-bezier-intersection-count-vertices 52 0 52 150
						 0 0 0 100 100 100 100 0))
(test 2 (line-bezier-intersection-count-vertices 0 50 100 50
						 0 0 0 100 100 100 100 0))|#