
(in-package minara)


;; Events

(defvar *quit-hook* nil
  "The current quit event scheme hook")

(defvar *resize-hook* nil
  "The current window resize event scheme hook")

(defvar *draw-hook* nil
  "The current window expose event scheme hook")

(defvar *mouse-button-down-hook* nil
  "The current mouse button event scheme hook")

(defvar *mouse-button-up-hook* nil
  "The current mouse button release event scheme hook")

(defvar *mouse-move-hook* nil
  "The current mouse moved event scheme hook")

(defvar *key-press-hook* nil
  "The current key pressed event scheme hook")

(defvar *key-release-hook* nil
  "The current key releaseded event scheme hook")

(defvar *menu-select-hook* nil
  "The current menu selected event scheme hook")


;; Menus

(defvar *main-menu* (glut:create-menu (cffi:null-pointer)))

(cffi:defcallback minara-menu-select-callback :void ((value :int))
  (funcall *menu-select-hook* value))

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

(defun window-for-id (id)
  (aref id cl-glut::*id->window*))

(defun window-current ()
  (window-for-id (glut:get-window)))

(defun glut-window-set (win)
  (when (\= win 0)
    (glut:set-window win)))

(defun window-height (win)
 (glut:height win))

(defun window-width (win)
 (glut:width win))

(defclass minara-glut-window (glut:window)
  ()
  (:default-initargs :width 500 :height 500 :pos-x 100 :pos-y 100
                     :mode '(:single :rgb) :title "minara"))

(defmethod initialize-instance :after ((w minara-glut-window) &key)
  w)

;;(defun minara-window-make (width height))

(defmethod glut:display ((w minara-glut-window))
  (funcall *draw-hook* w))

(defmethod glut:reshape ((w minara-glut-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0.0 width 0.0 height)
  (gl:matrix-mode :model-view)
  (gl:load-identity)
  (funcall *resize-hook* w width height))

(defmethod glut:passive-motion ((w minara-glut-window) x y)
  (funcall *mouse-move-hook* w x y))

(defmethod glut:motion ((w minara-glut-window) x y)
  (funcall *mouse-move-hook* w x y))

(defmethod glut:keyboard ((w minara-glut-window) key x y)
  (let ((modifiers (glut:get-modifier-values)))
    (funcall *key-press-hook* (coerce 'string key) modifiers)))

(defmethod glut:keyboard-up ((w minara-glut-window) key x y)
  (let ((modifiers (glut:get-modifier-values)))
    (funcall *key-release-hook* (coerce 'string key) modifiers)))

(defmethod glut:mouse ((w minara-glut-window) button state x y)
  (funcall (if (eq state :up)
               *mouse-button-up-hook*
               *mouse-button-down-hook*)
           w
           (case button
             (:left-button 1)
             (:middle-button 2)
             (otherwise 3)) ;; GLUT_RIGHT_BUTTON
           x
           y))

(defmethod glut:menu-state ((w minara-glut-window) id)
  (funcall *menu-select-hook* w id))

(defun minara-window-current ()
  (glut:get-window))

(defun minara-window-set (win)
  (glut:set-window win))

(defun minara-window-invalidate (win)
  (glut:post-window-redisplay win))

(defun minara-window-set-title (win title)
  (let ((old-win (minara-window-current)))
    (glut:set-window win)
    (glut:set-window-title title)
    (glut:set-window old-win)))

(defun minara-window-draw-status (win text)
  (let ((old-win (minara-window-current)))
    (glut:set-window win)
    (gl:push-matrix)
    ;; De-hardcode all this!
    (gl:color 0.9 0.8 0.8)
    (gl:raster-pos 5.2 4.8)
    (loop for c across text
       do (glut:bitmap-character :+bitmap-helvetica-12+ c))
    (gl:color 0.1 0.1 0.25)
    (gl:raster-pos 5.0 5.0)
    (loop for c across text
       do (glut:bitmap-character :+bitmap-helvetica-12+ c))
    ;; End de-hardcode
    (gl:pop-matrix)
    (glut:set-window old-win)))

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


;; Main program startup

;;(glut:main-loop)