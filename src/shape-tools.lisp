;; shape-tools.lisp : simple shape tools
;;
;; Copyright (c) 2004, 2007 Rob Myers, rob@robmyers.org
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


;; Trivial shape tools
;; Something to inspire more advanced tools

;; TODO - Keymap for polygon & star to increase/decrease number of sides
;;        and increase/decrease inner radius

;; TODO - Oval tool

;; TODO - Insert function call rather than code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Circle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *circle-tool-mouse-down* nil)
(defvar *circle-tool-mouse-down-x* nil)
(defvar *circle-tool-mouse-down-y* nil)

;; When the button is pressed, make and initialise the circle drawing buffer

(defun circle-mouse-down (window button x y)
  (let* ((circle-buffer (buffer-text (make-window-buffer window
                                                         "circle"))))
    (window-undo-stack-push window
                            circle-buffer)
    (setf circle-tool-mouse-down t)
    (setf circle-tool-mouse-down-x (window-view-x window
                                                  x))
    (setf circle-tool-mouse-down-y (window-view-y window
                                                  y))))

(defun circle-mouse-move (window raw-x raw-y)
  (if circle-tool-mouse-down
      (let* ((circle-buffer (window-buffer window
                                           "circle"))
             (x (window-view-x window
                               raw-x))
             (y (window-view-y window
                               raw-y))
             (radius (abs (distance-between-points circle-tool-mouse-down-x
                                                   circle-tool-mouse-down-y
                                                   x
                                                   y)))
             ;; http://www.whizkidtech.redprince.net/bezier/circle/
             (l (* radius
                   0.5522847498))
             (left (- circle-tool-mouse-down-x
                      radius))
             (right (+ circle-tool-mouse-down-x
                       radius))
             (top (+ circle-tool-mouse-down-y
                     radius))
             (bottom (- circle-tool-mouse-down-y
                        radius)))
        (buffer-erase circle-buffer)
        (write-current-colour circle-buffer)
        (buffer-insert-undoable circle-buffer
                                nil
                                "(path-begin)\n")
        (buffer-insert-undoable circle-buffer
                                nil
                                (format nil
                                        "(move-to ~f ~f)~%"
                                        left circle-tool-mouse-down-y))
        (buffer-insert-undoable circle-buffer
                                nil
                                (format nil
                                        "(curve-to ~f ~f ~f ~f ~f ~f)~%"
                                        left (+ circle-tool-mouse-down-y l)
                                        (- circle-tool-mouse-down-x l) top
                                        circle-tool-mouse-down-x top))
        (buffer-insert-undoable circle-buffer
                                nil
                                (format nil
                                        "(curve-to ~f ~f ~f ~f ~f ~f)~%"
                                        (+ circle-tool-mouse-down-x l) top
                                        right (+ circle-tool-mouse-down-y l)
                                        right circle-tool-mouse-down-y))
        (buffer-insert-undoable circle-buffer
                                nil
                                (format nil
                                        "(curve-to ~f ~f ~f ~f ~f ~f)~%"
                                        right (- circle-tool-mouse-down-y l)
                                        (+ circle-tool-mouse-down-x l) bottom
                                        circle-tool-mouse-down-x bottom))
        (buffer-insert-undoable circle-buffer
                                nil
                                (format nil
                                        "(curve-to ~f ~f ~f ~f ~f ~f)~%"
                                        (- circle-tool-mouse-down-x l) bottom
                                        left (- circle-tool-mouse-down-y l)
                                        left circle-tool-mouse-down-y))
        (buffer-insert-undoable circle-buffer
                                nil
                                "(path-end)\n")
        (buffer-undo-mark circle-buffer)
        (buffer-invalidate circle-buffer)
        (window-redraw window))))

;; When the mouse is released, add the circle  buffer to the main buffer
;; Redraw the main buffer, and release the pen drawing buffers

(defun circle-mouse-up (window button x y)
  (setf circle-tool-mouse-down nil)
  (let* ((buff (window-buffer
                window
                "circle"))
         (main-buff (window-buffer-main window)))
    (buffer-insert-undoable main-buff
                            nil
                            (buffer-to-string buff))
    (buffer-undo-mark main-buff)
    (window-undo-stack-pop window)
    ;; Clean up and redraw
    (remove-window-buffer window
                          "circle")
    (buffer-invalidate (window-buffer-main window))
    (window-redraw window)))

;; Install

(defun circle-tool-install ()
  (add-mouse-move-hook circle-mouse-move)
  (add-mouse-down-hook circle-mouse-down)
  (add-mouse-up-hook circle-mouse-up))

;; Uninstall

(defun circle-tool-uninstall ()
  (remove-mouse-move-hook circle-mouse-move)
  (remove-mouse-down-hook circle-mouse-down)
  (remove-mouse-up-hook circle-mouse-up))

;; Register

(install-tool circle-tool-install
              circle-tool-uninstall
              "Circle"
              "t" "c")

;; Oval

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Square
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar square-tool-mouse-down nil)
(defvar square-tool-mouse-down-x nil)
(defvar square-tool-mouse-down-y nil)

;; When the button is pressed, make and initialise the drawing buffer

(defun square-mouse-down (window button x y)
  (let* ((buff (buffer-text (make-window-buffer window
                                                "square"))))
    (window-undo-stack-push window
                            buff)
    (setf square-tool-mouse-down t)
    (setf square-tool-mouse-down-x (window-view-x window
                                                  x))
    (setf square-tool-mouse-down-y (window-view-y window
                                                  y))))

(defun square-mouse-move (window raw-x raw-y)
  (if square-tool-mouse-down
      (let* ((buff (window-buffer window
                                  "square"))
             (x (window-view-x window
                               raw-x))
             (y (window-view-y window
                               raw-y))
             (radius (distance-between-points square-tool-mouse-down-x
                                              square-tool-mouse-down-y
                                              x
                                              y))
             (left (- square-tool-mouse-down-x radius))
             (right (+ square-tool-mouse-down-x radius))
             (top (+ square-tool-mouse-down-y radius))
             (bottom (- square-tool-mouse-down-y radius)))
        (buffer-erase buff)
        (write-current-colour buff)
        (buffer-insert-undoable buff
                                nil
                                "(path-begin)\n")
        (buffer-insert-undoable buff
                                nil
                                (format nil
                                        "(move-to ~f ~f)~%"
                                        left bottom))
        (buffer-insert-undoable buff
                                nil
                                (format nil
                                        "(line-to ~f ~f)~%"
                                        left top))
        (buffer-insert-undoable buff
                                nil
                                (format nil
                                        "(line-to ~f ~f)~%"
                                        right top))
        (buffer-insert-undoable buff
                                nil
                                (format nil
                                        "(line-to ~f ~f)~%"
                                        right bottom))
        (buffer-insert-undoable buff
                                nil
                                (format nil
                                        "(line-to ~f ~f)~%"
                                        left bottom))
        (buffer-insert-undoable buff
                                nil
                                "(path-end)\n")
        (buffer-undo-mark buff)
        (buffer-invalidate buff)
        (window-redraw window))))

;; When the mouse is released, add the circle  buffer to the main buffer
;; Redraw the main buffer, and release the pen drawing buffers

(defun square-mouse-up (window button x y)
  (setf square-tool-mouse-down nil)
  (let* ((buff (window-buffer
                window
                "square"))
         (main-buff (window-buffer-main window)))
    (buffer-insert-undoable main-buff
                            nil
                            (buffer-to-string buff))
    (buffer-undo-mark main-buff)
    (window-undo-stack-pop window)
    ;; Clean up and redraw
    (remove-window-buffer window
                          "square")
    (buffer-invalidate (window-buffer-main window))
    (window-redraw window)))

;; Install

(defun square-tool-install ()
  (add-mouse-move-hook square-mouse-move)
  (add-mouse-down-hook square-mouse-down)
  (add-mouse-up-hook square-mouse-up))

;; Uninstall

(defun (square-tool-uninstall)
  (remove-mouse-move-hook square-mouse-move)
  (remove-mouse-down-hook square-mouse-down)
  (remove-mouse-up-hook square-mouse-up))

;; Register

(install-tool square-tool-install
              square-tool-uninstall
              "Square"
              "t" "s")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rectangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar star-number-points 6)
(defvar star-inner-radius-offset 0.5)

(defvar rectangle-tool-mouse-down nil)
(defvar rectangle-tool-mouse-down-x nil)
(defvar rectangle-tool-mouse-down-y nil)

;; When the button is pressed, make and initialise the drawing buffer

(defun rectangle-mouse-down (window button x y)
  (let* ((buff (buffer-text (make-window-buffer window
                                                "rectangle"))))
    (window-undo-stack-push window
                            buff)
    (setf rectangle-tool-mouse-down t)
    (setf rectangle-tool-mouse-down-x (window-view-x window
                                                     x))
    (setf rectangle-tool-mouse-down-y (window-view-y window
                                                     y))))

(defun rectangle-mouse-move (window raw-x raw-y)
  (if rectangle-tool-mouse-down
      (let* ((buff (window-buffer window
                                  "rectangle"))
             (x (window-view-x window
                               raw-x))
             (y (window-view-y window
                               raw-y)))
        (buffer-erase buff)
        (write-current-colour buff)
        (buffer-insert-undoable buff
                                nil
                                "(path-begin)\n")
        (buffer-insert-undoable buff
                                nil
                                (format nil
                                        "(move-to ~f ~f)~%"
                                        rectangle-tool-mouse-down-x
                                        rectangle-tool-mouse-down-y))
        (buffer-insert-undoable buff
                                nil
                                (format nil
                                        "(line-to ~f ~f)~%"
                                        x
                                        rectangle-tool-mouse-down-y))
        (buffer-insert-undoable buff
                                nil
                                (format nil
                                        "(line-to ~f ~f)~%"
                                        x
                                        y))
        (buffer-insert-undoable buff
                                nil
                                (format nil
                                        "(line-to ~f ~f)~%"
                                        rectangle-tool-mouse-down-x
                                        y))
        (buffer-insert-undoable buff
                                nil
                                (format nil
                                        "(line-to ~f ~f)~%"
                                        rectangle-tool-mouse-down-x
                                        rectangle-tool-mouse-down-y))
        (buffer-insert-undoable buff
                                nil
                                "(path-end)\n")
        (buffer-undo-mark buff)
        (buffer-invalidate buff)
        (window-redraw window))))

;; When the mouse is released, add the circle  buffer to the main buffer
;; Redraw the main buffer, and release the pen drawing buffers

(defun rectangle-mouse-up (window button x y)
  (setf rectangle-tool-mouse-down nil)
  (let* ((buff (window-buffer
                window
                "rectangle"))
         (main-buff (window-buffer-main window)))
    (buffer-insert-undoable main-buff
                            nil
                            (buffer-to-string buff))
    (buffer-undo-mark main-buff)
    (window-undo-stack-pop window)
    ;; Clean up and redraw
    (remove-window-buffer window
                          "rectangle")
    (buffer-invalidate (window-buffer-main window))
    (window-redraw window)))

;; Install

(defun rectangle-tool-install ()
  (add-mouse-move-hook rectangle-mouse-move)
  (add-mouse-down-hook rectangle-mouse-down)
  (add-mouse-up-hook rectangle-mouse-up))

;; Uninstall

(defun rectangle-tool-uninstall ()
  (remove-mouse-move-hook rectangle-mouse-move)
  (remove-mouse-down-hook rectangle-mouse-down)
  (remove-mouse-up-hook rectangle-mouse-up))

;; Register

(install-tool rectangle-tool-install
              rectangle-tool-uninstall
              "Rectangle"
              "t" "r")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Star
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar star-points 5)
(defvar star-inner-radius-fraction 0.5)

(defvar star-tool-mouse-down nil)
(defvar star-tool-mouse-down-x nil)
(defvar star-tool-mouse-down-y nil)

;; When the button is pressed, make and initialise the drawing buffer

(defun star-mouse-down (window button x y)
  (let* ((buff (buffer-text (make-window-buffer window
                                                "star"))))
    (window-undo-stack-push window
                            buff)
    (setf star-tool-mouse-down t)
    (setf star-tool-mouse-down-x (window-view-x window
                                                x))
    (setf star-tool-mouse-down-y (window-view-y window
                                                y))))

(defun star-draw-point (buff theta inner-angle-step inner-y outer-y)
  (let ((outer-point (rotate-point-around-point star-tool-mouse-down-x
                                                star-tool-mouse-down-y
                                                star-tool-mouse-down-x
                                                outer-y
                                                theta))
        (inner-point (rotate-point-around-point star-tool-mouse-down-x
                                                star-tool-mouse-down-y
                                                star-tool-mouse-down-x
                                                inner-y
                                                (- theta
                                                   inner-angle-step))))
    (buffer-insert-undoable buff
                            nil
                            (format nil
                                    "(line-to ~f ~f)~%"
                                    (car inner-point)
                                    (cdr inner-point)))
    (buffer-insert-undoable buff
                            nil
                            (format nil
                                          "(line-to ~f ~f)~%"
                                          (car outer-point)
                                          (cdr outer-point)))))

(defun star-draw (window x y)
  (let* ((buff (window-buffer window
                              "star"))
         (radius (/ (distance-between-points star-tool-mouse-down-x
                                             star-tool-mouse-down-y
                                             x
                                             y)
                    2.0))
         (inner-radius (/ radius
                          star-inner-radius-fraction))
         (angle-step (/ 360.0
                        star-points))
         (inner-angle-step (/ angle-step
                              2.0))
         (inner-y (+ star-tool-mouse-down-y
                     inner-radius))
         (outer-y (+ star-tool-mouse-down-y
                     radius))
         (first-point (rotate-point-around-point star-tool-mouse-down-x
                                                 star-tool-mouse-down-y
                                                 star-tool-mouse-down-x
                                                 outer-y
                                                 (- inner-angle-step))))
    (buffer-insert-undoable buff
                            nil
                            "(path-begin)\n")
    (buffer-insert-undoable buff
                            nil
                            (format nil
                                    "(move-to ~f ~f)~%"
                                    (car first-point)
                                    (cdr first-point)))
    (do ((theta inner-angle-step (+ theta angle-step)))
        ((> theta 360.0) nil)
      (star-draw-point buff theta inner-angle-step inner-y outer-y))
    (buffer-insert-undoable buff
                            nil
                            "(path-end)\n")))

(defun star-mouse-move (window raw-x raw-y)
  (if star-tool-mouse-down
      (let* ((buff (window-buffer window
                                  "star"))
             (x (window-view-x window
                               raw-x))
             (y (window-view-y window
                               raw-y)))
        (buffer-erase buff)
        (write-current-colour buff)
        (star-draw window
                   x
                   y)
        (buffer-undo-mark buff)
        (buffer-invalidate buff)
        (window-redraw window))))

;; When the mouse is released, add the circle  buffer to the main buffer
;; Redraw the main buffer, and release the pen drawing buffers

(defun star-mouse-up (window button x y)
  (setf star-tool-mouse-down nil)
  (let* ((buff (window-buffer
                window
                "star"))
         (main-buff (window-buffer-main window)))
    (buffer-insert-undoable main-buff
                            nil
                            (buffer-to-string buff))
    (buffer-undo-mark main-buff)
    (window-undo-stack-pop window)
    ;; Clean up and redraw
    (remove-window-buffer window
                          "star")
    (buffer-invalidate (window-buffer-main window))
    (window-redraw window)))

;; Install

(defun star-tool-install ()
  (add-mouse-move-hook star-mouse-move)
  (add-mouse-down-hook star-mouse-down)
  (add-mouse-up-hook star-mouse-up))

;; Uninstall

(defun star-tool-uninstall ()
  (remove-mouse-move-hook star-mouse-move)
  (remove-mouse-down-hook star-mouse-down)
  (remove-mouse-up-hook star-mouse-up))

;; Register

(install-tool star-tool-install
              star-tool-uninstall
              "Star"
              "t" "S")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polygon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar polygon-points 5)

(defvar polygon-tool-mouse-down nil)
(defvar polygon-tool-mouse-down-x nil)
(defvar polygon-tool-mouse-down-y nil)

;; When the button is pressed, make and initialise the drawing buffer

(defun polygon-mouse-down (window button x y)
  (let* ((buff (buffer-text (make-window-buffer window
                                                "polygon"))))
    (window-undo-stack-push window
                            buff)
    (setf polygon-tool-mouse-down t)
    (setf polygon-tool-mouse-down-x (window-view-x window
                                                   x))
    (setf polygon-tool-mouse-down-y (window-view-y window
                                                   y))))

(defun polygon-draw (window x y)
  (let* ((buff (window-buffer window
                              "polygon"))
         (radius (distance-between-points polygon-tool-mouse-down-x
                                          polygon-tool-mouse-down-y
                                          x
                                          y))
         (angle-step (/ 360.0
                        polygon-points))
         (outer-y (+ polygon-tool-mouse-down-y
                     radius)))
    (buffer-insert-undoable buff
                            nil
                            "(path-begin)\n")
    (buffer-insert-undoable buff
                            nil
                            (format nil
                                    "(move-to ~f ~f)~%"
                                    polygon-tool-mouse-down-x
                                    outer-y))
    (do ((theta angle-step (+ theta angle-step)))
        ((> theta 360.0) nil)
      (let ((outer-point (rotate-point-around-point polygon-tool-mouse-down-x
                                                    polygon-tool-mouse-down-y
                                                    polygon-tool-mouse-down-x
                                                    outer-y
                                                    theta)))
        (buffer-insert-undoable buff
                                nil
                                (format nil
                                        "(line-to ~f ~f)~%"
                                        (car outer-point)
                                        (cdr outer-point)))))
    (buffer-insert-undoable buff
                            nil
                            "(path-end)\n")))

(defun polygon-mouse-move (window raw-x raw-y)
  (if polygon-tool-mouse-down
      (let* ((buff (window-buffer window
                                  "polygon"))
             (x (window-view-x window
                               raw-x))
             (y (window-view-y window
                               raw-y)))
        (buffer-erase buff)
        (write-current-colour buff)
          (polygon-draw window
                        x
                        y)
          (buffer-undo-mark buff)
          (buffer-invalidate buff)
          (window-redraw window))))

;; When the mouse is released, add the circle  buffer to the main buffer
;; Redraw the main buffer, and release the pen drawing buffers

(defun polygon-mouse-up (window button x y)
  (setf polygon-tool-mouse-down nil)
  (let* ((buff (window-buffer
                window
                "polygon"))
         (main-buff (window-buffer-main window)))
    (buffer-insert-undoable main-buff
                            nil
                            (buffer-to-string buff))
    (buffer-undo-mark main-buff)
    (window-undo-stack-pop window)
    ;; Clean up and redraw
    (remove-window-buffer window
                          "polygon")
    (buffer-invalidate (window-buffer-main window))
    (window-redraw window)))

;; Install

(defun polygon-tool-install ()
  (add-mouse-move-hook polygon-mouse-move)
  (add-mouse-down-hook polygon-mouse-down)
  (add-mouse-up-hook polygon-mouse-up))

;; Uninstall

(defun polygon-tool-uninstall ()
  (remove-mouse-move-hook polygon-mouse-move)
  (remove-mouse-down-hook polygon-mouse-down)
  (remove-mouse-up-hook polygon-mouse-up))

;; Register

(install-tool polygon-tool-install
              polygon-tool-uninstall
              "Polygon"
              "t" "y")

