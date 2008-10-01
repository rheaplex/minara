;; test.lisp : minara minimal test harness
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
;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Store all the tests as they are defined
(defvar *tests* '())
;; The total number of tests run
(defvar *tests-run* 0)
;; And the total number of tests failed
(defvar *tests-failed* 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A single test assertion
(defmacro test-assert (comparison comment expected &rest code)
  `(setf *tests*
	 (cons
	  (lambda ()
	    (setf *tests-run* (+ *tests-run* 1))
	    (let ((result ,code))
	      (if (not (,comparison ,expected result))
		  (begin
		   (format t "~a: " ,comment)
		   (write @,code)
		   (format t "~%  Expected ~a, got ~a.~%" ,expected result)
		   (setf *tests-failed* (+ *tests-failed* 1))))))
	  *tests*)))

(defmacro test (expected code)
  `(test-assert equal? "Test unexpectedly failed" ,expected ,code))

(defmacro test-fail (expected code)
  `(test-assert (lambda (a b) (not (equal a b))) 
		"Test unexpectedly succeeded" ,expected ,code))

(defmacro test-section (name)
  `(setf *tests*
	 (cons
	  ,name
	  *tests*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-tests-aux (tests)
  (if (consp tests)
      (progn
       (if (stringp (car tests))
	   (format t "---->~a~%" (car tests))
	   (funcall (car tests)))
       (run-tests-aux (cdr tests)))))

(defun run-tests ()
  "Run all the tests"
  (format t "Running tests.~%")
  (run-tests-aux (reverse *tests*))
  (format t "Total tests passed: ~a/~a~%" 
	  (- *tests-run* *tests-failed*) 
	  *tests-run*))