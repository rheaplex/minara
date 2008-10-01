;; transformations.lisp : 2d geometric transformations for minara
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geometric transformations using matrices
;; Matrices are Postscript-style 6-element arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package minara)


(defclass matrix ()
  ((a :initarg :a
      :initform 1.0
      :accessor matrix-a) 
   (b :initarg :b
      :initform 0.0
      :accessor matrix-b) 
   (c :initarg :c
      :initform 0.0
      :accessor matrix-c) 
   (d :initarg :d
      :initform 1.0
      :accessor matrix-d) 
   (tx :initarg :t
      :initform 0.0
      :accessor matrix-tx) 
   (ty :initarg :ty
      :initform 0.0
      :accessor matrix-ty)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to generate transformation matrices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a  b  0
;; c  d  0
;; tx ty 1

;; (a b c d tx ty)

;; identity

;; 1  0  0
;; 0  1  0
;; 0  0  1

;; (1 0 0 0 1 0)

(defun make-matrix-identity ()
  (make-instance 'matrix))

;; scale

;; sx 0  0
;; 0  sy 0
;; 0  0  1

;; (sx 0 0 sy 0 0)

(defun make-matrix-scale (x y)
  (make-instance 'matrix :a x :b 0.0 :c 0.0 :d y :tx 0.0 :ty 0.0))

;; translate

;; 1  0  0
;; 0  1  0
;; tx ty 1

;; (1 0 0 1 tx ty)

(defun make-matrix-translate (x y)
  (make-instance 'matrix :a 1.0 :b 0.0 :c 0.0 :d 1.0 :tx x :ty y))

;; rotate

;; cos  sin 0
;; -sin cos 0
;; 0    0   1

;; (cos sin -sin cos 0 0)

(defun make-matrix-rotate (z)
  (let ((c (cos z))
	(s (sin z))
	(ns (- (sin z))))
    (make-instance 'matrix :a c :b s :c ns :d c :tx 0.0 :ty 0.0)))

;; to string

(defun matrix-to-string (matrix)
    (format nil "~a ~a ~a ~a ~a ~a" (matrix-a matrix) (matrix-b matrix)
	    (matrix-c matrix) (matrix-d matrix) (matrix-tx matrix) 
	    (matrix-ty matrix)))

(defun matrix-to-concatenate-string (matrix)
    (format nil "(concatenate-matrix ~a)" (matrix-to-string matrix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matrix Concatenation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; concatenate

;; Multiply the rows of A by the columns of B
;;(define a (matrix-translate-make 10 20))(define b (matrix-translate-make 200 100))(define c (matrix-concatenate a b)) c

;; This is going to be a bottleneck, so unroll it
;; Optimise so rather than first, second, third, we use car/cdr on each cdr

(defun matrix-concatenate-aux (a b)
  (let* ((aa (matrix-a a))
	 (ab (matrix-b a))
	 (ac (matrix-c a))
	 (ad (matrix-d a))
	 (ae (matrix-tx a))
	 (af (matrix-ty a))
	 (ba (matrix-a b))
	 (bb (matrix-b b))
	 (bc (matrix-c b))
	 (bd (matrix-d b))
	 (be (matrix-tx b))
	 (bf (matrix-ty b)))
    (make-instance 'matrix
		   :a (+ (* aa ba) (* ab bc)) ;;(* 0.0 btx)
		   :b (+ (* aa bb) (* ab bd)) ;;(* 0.0 b32)	
		   ;;(+ (* a11 b13) (* a12 b23) (* a13 b33))
		   :c (+ (* ac ba) (* ad bc)) ;;(*a23 b31)	
		   :d (+ (* ac bb) (* ad bd)) ;;(* a23 b32)
		   ;;(+ (* a21 b13) (* a22 b23) (* a23 b33))
		   :tx (+ (* ae ba) (* af bc) be) ;;(* a33 b31)
		   :ty (+ (* ae bb) (* af bd) bf)))) ;;(* a33 b32)
;;(+ (* a31 b13) (* a32 b23) (* a33 b33))

;; concatenaten
;; Concatenate a list of matrices

(defun matrix-concatenate (a &rest ms)
  (let ((product (matrix-concatenate-aux a (car ms)))
	(rest (cdr ms)))
    (if (not (consp rest))
	product
	(matrix-concatenate product (cdr ms)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying transformation matrices to objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; transform
;; Transform a point by a matrix
;; This is going to be a bottleneck, so unroll it
;; Optimise so rather than first, second, third, we use car/cdr on each cdr

(defun matrix-point-transform (x y m)
  (let* ((a (matrix-a m))
	 (b (matrix-b m))
	 (c (matrix-c m))
	 (d (matrix-d m))
	 (tx (matrix-tx m))
	 (ty (matrix-ty m))
	 (xx (+ (* x a) 
		(* y b)
		tx))
	 (yy (+ (* x c)
		(* y d)
		ty)))
    (values xx yy)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matrix stacks, pushing, popping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-matrix-stack ()
    (list (make-matrix-identity)))

(defun stack-set-matrix (matrix-stack matrix)
    (cons matrix (cdr matrix-stack)))

(defun stack-current-matrix (matrix-stack)
  (car matrix-stack))

(defun stack-concatenate-matrix (matrix-stack matrix)
  (stack-set-matrix matrix-stack
		    (matrix-concatenate matrix
					(stack-current-matrix matrix-stack))))

(defun stack-push-matrix (matrix-stack)
  (cons (copy-tree (stack-current-matrix matrix-stack))
	matrix-stack))

(defun stack-pop-matrix (matrix-stack)
  (cdr matrix-stack))

(defun get-translate-values (trans)
  (multiple-value-bind (x next) 
      (read-from-string trans nil 0.0)
    (values x (read-from-string trans nil 0.0 :start next))))