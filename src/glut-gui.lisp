
(in-package minara)

;; Menus

(defvar *main-menu* (glut:create-menu (cffi:null-pointer)))

(cffi:defcallback minara-menu-select-callback :void ((value :int))
  (menu-select-hook value))

(defun minara-menu-make ()
  (let ((id (glut:create-menu (cffi:callback minara-menu-select-callback))))
    (glut:set-menu *main-menu*)
    id))

(defun minara-menu-install (menu-id title)
  (glut:set-menu *main-menu*)
  (glut:add-sub-menu title menu-id))

(defun minara-menu-add-entry (menu-id entry-name entry-id)
  (glut:set-menu menu-id)
  (glut:add-menu-entry entry-name entry-id)
  (glut:set-menu *main-menu*))

(defun minara-menu-remove-entry (id)
  (glut:remove-menu-item id))


;; Windows

;(defun window-for-id (id)
; (aref cl-glut::*id->window* id))

(defun window-current ()
  (glut:get-window))

(defun glut-window-set (win)
  (when (\= win 0)
    (glut:set-window win)))

(defun window-height (win)
 (glut:height win))

(defun window-width (win)
 (glut:width win))

;;(defun minara-window-make (width height))

(defmethod glut:display ((w window))
  (draw-hook w))

(defmethod glut:reshape ((w window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0.0 width 0.0 height)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (resize-hook w width height))

(defmethod glut:passive-motion ((w window) x y)
  (mouse-move-hook w x y))

(defmethod glut:motion ((w window) x y)
  (mouse-move-hook w x y))

(defmethod glut:keyboard ((w window) key x y)
  (let ((modifiers (glut:get-modifier-values)))
    (key-press-hook (string key) modifiers)))

(defmethod glut:keyboard-up ((w window) key x y)
  (let ((modifiers (glut:get-modifier-values)))
    (key-release-hook w (string key) modifiers)))

(defmethod glut:mouse ((w window) button state x y)
  (if (eq state :up)
      (mouse-up-hook w
		     (case button
		       (:left-button 1)
		       (:middle-button 2)
		       (otherwise 3)) ;; GLUT_RIGHT_BUTTON
		     x
		     y)
      (mouse-down-hook w
		       (case button
			 (:left-button 1)
			 (:middle-button 2)
			 (otherwise 3)) ;; GLUT_RIGHT_BUTTON
		       x
		       y)))

(defmethod glut:menu-state ((w window) id)
  (menu-select-hook w id))

(defun minara-window-invalidate (win)
  (glut:post-window-redisplay win))

(defun minara-window-set-title (win title)
  (setf (glut:title win)
	title))

(defun minara-window-draw-status (text)
  (gl:push-matrix)
  ;; De-hardcode all this!
  (gl:color 0.9 0.8 0.8)
  (gl:raster-pos 5.2 4.8)
  (loop for c across text
     do (glut:bitmap-character glut:+bitmap-helvetica-12+ (char-int c)))
  (gl:color 0.1 0.1 0.25)
  (gl:raster-pos 5.0 5.0)
  (loop for c across text
     do (glut:bitmap-character glut:+bitmap-helvetica-12+ (char-int c)))
  ;; End de-hardcode
  (gl:pop-matrix))

(defun minara-window-draw-begin (win)
  (declare (ignore win))
  (gl:shade-model :flat)
  (gl:disable :dither)
  (gl:disable :depth-test)
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (gl:clear :color-buffer-bit))

(defun minara-window-draw-end (win)
  (declare (ignore win))
  (gl:flush)
  (glut:swap-buffers))

(defgeneric display-window (glut:window))

(defmethod display-window ((win window))
  (glut:display-window win))