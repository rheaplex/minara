;; view-tools.lisp : tools for viewing windows for minara
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

(in-package minara)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zoom limits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter $zoom-max-out 0.0625)
(defparameter $zoom-normal 1.0)
(defparameter $zoom-max-in 16.0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View buffer
;; The window buffer below the main buffer that gives the window view
;; view when evaluated.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun window-view-buffer (window)
  (window-buffer window '_view))

(defun window-view-buffer-current()
  (window-view-buffer (window-current)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Zoom

(defun buffer-scale (buffer)
  (buffer-variable buffer
                   "scale"))

(defun window-scale (window)
  (buffer-scale (window-view-buffer window)))

;; Pan

(defun buffer-scale-tx (buffer)
  (buffer-variable buffer
                   "scale-tx"))

(defun buffer-scale-ty (buffer)
  (buffer-variable buffer
                   "scale-ty"))

(defun window-scale-tx (window)
  (buffer-scale-tx (window-view-buffer window)))

(defun window-scale-ty (window)
  (buffer-scale-ty (window-view-buffer window)))

(defun buffer-tx (buffer)
  (buffer-variable buffer
                   "tx"))

(defun buffer-ty (buffer)
  (buffer-variable buffer
                   "ty"))

(defun set-buffer-tx (buffer tx)
  (set-buffer-variable buffer
                       "tx"
                       tx))

(defun set-buffer-ty (buffer ty)
  (set-buffer-variable buffer
                       "ty"
                       ty))

;; The temporary translate used by the pan tool.
;; Rename

(defun buffer-pan-tx (buffer)
  (buffer-variable buffer
                   "pan-tx"))

(defun buffer-pan-ty (buffer)
  (buffer-variable buffer
                   "pan-ty"))

(defun set-buffer-pan-tx (buffer tx)
  (set-buffer-variable buffer
                   "pan-tx"
                   tx))

(defun set-buffer-pan-ty (buffer ty)
  (set-buffer-variable buffer
                   "pan-ty"
                   ty))

;; Tilt

(defun buffer-rotation (buffer)
  (buffer-variable buffer
                   "angle"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating the view buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is called in the window constructor. Don't call it in your own code.

(defun window-view-buffer-make (window)
  (make-window-buffer window '_view)
  (let ((buffer (window-view-buffer window)))
    (set-buffer-variable buffer
                         "resize-ty"
                         0.0)
    (set-buffer-variable buffer
                         "scale"
                         $zoom-normal)
    (set-buffer-variable buffer
                         "scale-tx"
                         0.0)
    (set-buffer-variable buffer
                         "scale-ty"
                         0.0)
    (set-buffer-variable buffer
                         "tx"
                         0.0)
    (set-buffer-variable buffer
                         "ty"
                         0.0)
    (set-buffer-variable buffer
                         "pan-tx"
                         0.0)
    (set-buffer-variable buffer
                         "pan-ty"
                         0.0)
    (set-buffer-variable buffer
                         "angle"
                         0.0)))

(defun view-buffer-update (buffer)
  (let ((scale (buffer-scale buffer))
        (text (buffer-gap-buffer buffer)))
    (buffer-erase buffer)
    (flexichain:insert-sequence
     text
     (format nil
             "(translate 0 ~f) ;; Window resize offset~%"
             (* scale
                (buffer-variable buffer "resize-ty"))))
    (flexichain:insert-sequence
     text
     (format nil
             "(translate ~f ~f) ;; Scale translate~%"
             (buffer-scale-tx buffer)
             (buffer-scale-ty buffer)))
    (flexichain:insert-sequence
     text
     (format nil
             "(translate ~f ~f) ;; Pan translate~%"
             (* scale
                (buffer-tx buffer))
             (* scale
                (buffer-ty buffer))))
    (flexichain:insert-sequence
     text
     (format nil
             "(translate ~f ~f) ;; Pan tool temp translate~%"
             (buffer-pan-tx buffer)
             (buffer-pan-ty buffer)))
    (flexichain:insert-sequence
     text
     (format nil
             "(rotate ~f)~%"
             (buffer-rotation buffer)))
    (flexichain:insert-sequence
     text
     (format nil
             "(scale ~f ~f)"
             scale
             scale))
    ;;(format t "~A~%~%" (buffer-to-string text))
    (buffer-invalidate buffer)))

(defun window-view-update (window)
  (view-buffer-update (window-view-buffer window))
  (window-redraw window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zoom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scale factors and view translation factors.
;; If anyone can explain this to me I'd be very grateful... - Rob.

(defun next-zoom-out-level (current)
  (if (= current
         $zoom-max-out)
      nil
      (/ current
         2.0)))

(defun next-zoom-in-level (current)
  (if (= current
         $zoom-max-in)
      nil
      (* current
         2.0)))

(defun next-zoom-level (current in?)
  (if in?
      (next-zoom-in-level current)
      (next-zoom-out-level current)))

(defun buffer-zoom-update (buffer zoom)
  (set-buffer-variable buffer
                       "scale"
                       zoom))

(defun window-zoom-update (window zoom)
  (buffer-zoom-update (window-view-buffer window)
                      zoom)
  (let* ((buffer (window-view-buffer window))
         (scale (buffer-scale buffer)))
    (set-buffer-variable buffer
                         "scale-tx"
                         (/ (- (window-width window)
                               (* (window-width window)
                                  scale))
                            2.0))
    (set-buffer-variable buffer
                         "scale-ty"
                         (/ (- (window-height window)
                               (* (window-height window)
                                  scale))
                            2.0)))
  (window-view-update window))

(defun zoom (in)
  ;; Ultimately zoom & pan with same click here
  (let* ((window (window-current))
         (buffer (window-view-buffer window))
         (current-zoom (buffer-scale buffer))
         (zoom (next-zoom-level current-zoom
                                in)))
    (if zoom
        (window-zoom-update window
                            zoom))))

(defun zoom-default()
  (window-zoom-update (window-current)
                      $zoom-normal))

(defun zoom-in ()
  (zoom t))

(defun zoom-out ()
  (zoom nil))

(keymap-add-fun *global-keymap*
                #'zoom-in
                "i")

(keymap-add-fun *global-keymap*
                #'zoom-out
                "I")

(keymap-add-fun *global-keymap*
                #'zoom-default
                "Ai")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pan-zoom-factor (zoom)
  (/ 1.0 zoom))

(defun buffer-pan-zoom-factor (buffer)
  (pan-zoom-factor (buffer-scale buffer)))

(defun window-pan-zoom-factor (window)
  (buffer-pan-zoom-factor (window-view-buffer window)))

(defun window-tx (window)
  (buffer-tx (window-view-buffer window)))

(defun window-ty (window)
  (buffer-ty (window-view-buffer window)))

(defun set-window-tx (window tx)
  (set-buffer-tx (window-view-buffer window)
                  tx))

(defun set-window-ty (window ty)
  (set-buffer-ty (window-view-buffer window)
                  ty))

(defun set-window-transform (window)
  (let ((buffer (window-view-buffer window)))
    (set-buffer-tx buffer
                   (+ (/ (buffer-pan-tx buffer)
                         (buffer-scale buffer))
                      (buffer-tx buffer)))
    (set-buffer-ty buffer
                   (+ (/ (buffer-pan-ty buffer)
                         (buffer-scale buffer))
                      (buffer-ty buffer)))
    (set-buffer-pan-tx buffer 0.0)
    (set-buffer-pan-ty buffer 0.0)))

(defvar pan-tool-mouse-down nil)
(defvar pan-tool-mousedown-x nil)
(defvar pan-tool-mousedown-y nil)

(defun pan-mouse-down (window button x y)
  (declare (ignore window)
	   (ignore button))
  (setf pan-tool-mouse-down t)
  (setf pan-tool-mousedown-x
        x)
  (setf pan-tool-mousedown-y
        y))

(defun pan-mouse-move (window x y)
  (if pan-tool-mouse-down
      (let ((buffer (window-view-buffer window)))
        (set-buffer-pan-tx buffer(- x;;(window-view-x window x)
                                    pan-tool-mousedown-x))
        (set-buffer-pan-ty buffer
                           (- y;;(window-view-y window y)
                              pan-tool-mousedown-y))
        (window-view-update window))))

(defun pan-mouse-up (window button x y)
  (declare (ignore button)
	   (ignore x)
	   (ignore y))
  (setf pan-tool-mouse-down nil)
  (set-window-transform window)
  (window-view-update window))

(defun pan-default ()
  (let ((window (window-current)))
    (set-window-tx window
                   0.0)
    (set-window-ty window
                   0.0)
    (set-window-transform window)
    (window-view-update window)))

;; Install

(defun pan-tool-install ()
  (add-mouse-down-hook #'pan-mouse-down)
  (add-mouse-move-hook #'pan-mouse-move)
  (add-mouse-up-hook #'pan-mouse-up))

;; Uninstall

(defun pan-tool-uninstall ()
  (remove-mouse-down-hook #'pan-mouse-down)
  (remove-mouse-move-hook #'pan-mouse-move)
  (remove-mouse-up-hook #'pan-mouse-up))

;; Register

(install-tool #'pan-tool-install
              #'pan-tool-uninstall
              "Pan"
              "p")

(keymap-add-fun *global-keymap*
                #'pan-default
                "Ap")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset the view to the identity matrix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun view-panic ()
  (zoom-default)
  (pan-default))

(keymap-add-fun *global-keymap*
                #'view-panic
                "AP")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tilt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View/window co-ordinate conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun window-view-width (window)
  (/ (window-width window)
     (window-scale window)))

(defun window-view-height (window)
  (/ (window-height window)
     (window-scale window)))

(defun window-view-left (window)
  (- (/ (- (window-scale-tx window))
        (window-scale window))
     (window-tx window)))

(defun window-view-right (window)
  (+ (window-view-width window)
     (window-tx window)))

(defun window-view-bottom (window)
  (-  (/ (- (window-scale-ty window))
         (window-scale window))
      (window-ty window)))

(defun window-view-top (window)
  (+ (window-view-height window)
     (window-ty window)))

(defun window-view-x (window x)
  (+ (/ x
        (window-scale window))
     (window-view-left window)))

(defun window-view-y (window y)
  (+ (/ (- y
           (* (- (window-height window)
                 *window-height*)
              (window-scale window)))
        (window-scale window))
     (window-view-bottom window)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window resize offset
;; The window co-ordinate system starts at the bottom left
;; So when the window is resized, it shifts and the page moves
;; which doesn't look good, so we counteract that here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun window-resizing-buffer-make (window)
  (make-window-buffer window "resizing-buffer"))

(defun window-previous-height (window)
  (or (buffer-variable (window-view-buffer window)
                       "old-height")
      *window-height*))

(defun update-window-previous-height (window)
  (set-buffer-variable (window-view-buffer window)
                       "old-height"
                       (window-height window)))

(defun window-view-resize (window x y)
  (declare (ignore x)
	   (ignore y))
  (let* ((height (window-height window)))
    (if (not (= height
                -1))
        (let* ((buffer (window-view-buffer window))
               (old-ty (buffer-variable buffer
                                        "resize-ty")))
          (set-buffer-variable buffer
                               "resize-ty"
                               (+ old-ty
                                  (/ (- height
                                        (window-previous-height window))
                                     (window-scale window))))
          (window-view-update window)
          (update-window-previous-height window)))))

(add-resize-hook #'window-view-resize)
