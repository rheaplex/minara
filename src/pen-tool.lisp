;; pen-tool.lisp : the pen tool
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is a simple scribble tool for testing during initial development.
;; It should be renamed "scribble" and replaced with a real pen tool.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar $path-end "(path-end)\n")
(defvar $path-end-length (string-length $path-end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keep track of the mouse button state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pen-tool-mouse-down nil)

;; When the button is pressed, make and initialise the pen drawing buffers

(defun begin-drawing (window x y)
  (let ((pen-buffer (make-window-buffer window
                                        "pen")))
    (window-undo-stack-push window
                            pen-buffer)
    (write-current-colour pen-buffer)
    (buffer-insert-undoable pen-buffer
                            nil
                            (format nil
                                    "(path-begin)~%(move-to ~a ~a)~%"
                                    (window-view-x window x)
                                    (window-view-y window y)))
    (buffer-insert-undoable pen-buffer
                            nil
                            $path-end)
    (buffer-undo-mark pen-buffer)))

(defun pen-mouse-down (win button x y)
  (begin-drawing win x y)
  (setf pen-tool-mouse-down t))

;; When the mouse is dragged, add the new co-ordinate to the drawing buffer
;; And redraw the pen drawing buffers

(defun draw-point (window x y)
  (let ((pen-buffer (window-buffer window
                                   "pen")))
    ;; Remove the previous path-end
    (buffer-delete-undoable pen-buffer
                            (- (buffer-end pen-buffer)
                               $path-end-length)
                            $path-end-length)
    (buffer-insert-undoable pen-buffer
                            nil
                            (format nil
                                    "(line-to ~a ~a)~%"
                                    (window-view-x window x)
                                    (window-view-y window y)))
    (buffer-insert-undoable pen-buffer
                            nil
                            $path-end)
    (buffer-undo-mark pen-buffer)
    (buffer-invalidate pen-buffer)
    (window-redraw window)))

(defun pen-mouse-move (win x y)
  (if pen-tool-mouse-down
      (draw-point win x y)))

;; When the mouse is released, add the pen drawing buffers to the main buffer
;; Redraw the main buffer, and release the pen drawing buffers

(defun end-drawing (window)
  (let ((buff (window-buffer window
                             "pen"))
        (main-buff (window-buffer-main window)))
    (buffer-insert-undoable main-buff
                            nil
                            (buffer-to-string buff))
    (buffer-undo-mark main-buff)
    (window-undo-stack-pop window)
    ;; Clean up and redraw
    (remove-window-buffer window
                          "pen")
    (buffer-invalidate (window-buffer-main window))
    (window-redraw window)))

(defun pen-mouse-up (win button x y)
  (end-drawing win)
  (setf pen-tool-mouse-down nil))

;; Install

(defun pen-tool-install ()
  (add-mouse-move-hook pen-mouse-move)
  (add-mouse-down-hook pen-mouse-down)
  (add-mouse-up-hook pen-mouse-up))

;; Uninstall

(defun pen-tool-uninstall ()
  (remove-mouse-move-hook pen-mouse-move)
  (remove-mouse-down-hook pen-mouse-down)
  (remove-mouse-up-hook pen-mouse-up))

;; Register

(install-tool pen-tool-install
              pen-tool-uninstall
              "Simple Pen"
              "t" "p")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polyline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO:
;; Show first click as point
;; Show second click as line
;; Then show other clicks as poly

(defun polyline-timestamp (window)
  (let ((buffer (window-buffer window
                               "pen")))
    (if buffer
        (buffer-variable buffer
                         "polyline-timestamp")
        nil)))

(defun update-polyline-timestamp (window)
  (let ((buffer (window-buffer window
                               "pen")))
    (if buffer
        (set-buffer-variabl buffer
                            "polyline-timestamp"
                            (get-internal-real-time)))))

(defun polyline-begin-drawing (win button x y)
  (begin-drawing win x y)
  (update-polyline-timestamp win))

(defun polyline-draw (win button x y)
  (draw-point win x y)
  (update-polyline-timestamp win))

(defun polyline-finish-drawing (win)
  (end-drawing win))

(defun polyline-mouse-up (win button x y)
  (let ((last-time (polyline-timestamp win)))
    (if (not last-time)
        (polyline-begin-drawing win button x y)
        (let* ((now (get-internal-real-time))
               (delta (- now
                         last-time)))
          ;; If it's not a double-click, then draw
          (if (> (/ delta
                    internal-time-units-per-second)
                 0.25)
              (polyline-draw win button x y)
              (polyline-finish-drawing win))))))

;; Install

(defun polyline-tool-install ()
  (add-mouse-up-hook polyline-mouse-up))

;; Uninstall

(defun polyline-tool-uninstall ()
  (let ((win (window-current)))
    (if (polyline-timestamp win)
        (polyline-finish-drawing win)))
  (remove-mouse-up-hook polyline-mouse-up))

;; Register

(install-tool polyline-tool-install
              polyline-tool-uninstall
              "Polyline"
              "t" "P")