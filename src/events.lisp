;; events.lisp : gui event handling
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
;; Basic GUI and application events
;;
;; We receive events from the window system via the C code, which may have
;; done some setting up of the environment for us.
;;
;; We keep a list of handlers for each event, and call each in turn.
;;
;; People really *shouldn't* replace these functions, instead they should
;; use them to add and remove event hooks within the system set up here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package minara)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calling event handlers with good error recovery and diagnostics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *event-stack* '())
			  
#|(defun call-with-backtrace (call-me)
  (funcall call-me))
;;  (setf event-stack (make-stack t))
;;  (catch t
;;    call-me
;;    event-error-handler))

(defun event-error-handler (&rest args)
  (if (= (length args) 5)
      (progn
	(error (cdr args))
	(display-backtrace event-stack
			   (current-error-port)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *quit-funs* '())

(defun quit-hook ()
  (dolist (fun *quit-funs*) 
      (funcall fun)))

(defun add-quit-hook (fun)
  (if (not (member fun 
		   *quit-funs*))
      (setf *quit-funs* 
	    (cons fun 
		  *quit-funs*))))

(defun remove-quit-hook (fun)
  (setf *quit-funs*
	(remove fun 
		*quit-funs*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resizing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *resize-funs* '())

(defun resize-hook (win width height)
  (dolist (fun *resize-funs*)
    (funcall fun 
	     win
	     width 
	     height)))

(defun add-resize-hook (fun)
  (if (not (member fun 
		   *resize-funs*))
      (setf *resize-funs* 
	    (cons fun 
		  *resize-funs*))))

(defun remove-resize-hook (fun)
  (setf *resize-funs*
	(remove fun 
		*resize-funs*)))

;; GLUT's window co-ords go down, OGL's go up.
;; So we need to allow for this

#|(defun update-window-dimensions (window width height)
  (set-window-width window 
		    width)
  (set-window-height window 
		     height))

(add-resize-hook #'update-window-dimensions)|#

(defun swizzle-y (win y)
  (- (cl-glut:height (window-for-id win))
     y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *draw-funs* '())

(defun draw-hook (win)
  (dolist (fun *draw-funs*)
      (funcall fun win)))

(defun add-draw-hook (fun)
  (if (not (member fun 
		   *draw-funs*))
      (setf *draw-funs* 
	    (cons fun 
		  *draw-funs*))))

(defun remove-draw-hook (fun)
  (setf *draw-funs*
	(remove fun 
		*draw-funs*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse down
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mouse-down-funs* '())

(defun mouse-down-hook (win button x y)
  (let ((yy (swizzle-y win
		       y)))
    (dolist (fun *mouse-down-funs*)
      (funcall fun 
	       win
	       button 
	       x 
	       yy))))

(defun add-mouse-down-hook (fun)
  (if (not (member fun 
		 *mouse-down-funs*))
      (setf *mouse-down-funs* 
	    (cons fun 
		  *mouse-down-funs*))))

(defun remove-mouse-down-hook (fun)
  (setf *mouse-down-funs*
	(remove fun 
		*mouse-down-funs*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mouse-up-funs* '())

(defun mouse-up-hook (win button x y)
  (let ((yy (swizzle-y win
		       y)))
    (dolist (fun *mouse-up-funs*) 
	(funcall fun 
		 win 
		 button 
		 x 
		 yy))))

(defun add-mouse-up-hook (fun)
  (if (not (member fun 
		   *mouse-up-funs*))
      (setf *mouse-up-funs* 
	    (cons fun 
		  *mouse-up-funs*))))

(defun remove-mouse-up-hook (fun)
  (setf *mouse-up-funs*
	(remove fun 
		*mouse-up-funs*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mouse-move-funs* '())

(defun mouse-move-hook  (win x y)
  (let ((yy (swizzle-y win
		       y)))
    (dolist (fun *mouse-move-funs*)
      (funcall fun 
	       win
	       x 
	       yy))))

(defun add-mouse-move-hook (fun)
  (if (not (member fun 
		   *mouse-move-funs*))
      (setf *mouse-move-funs* 
	    (cons fun 
		  *mouse-move-funs*))))

(defun remove-mouse-move-hook (fun)
  (setf *mouse-move-funs*
	(remove fun 
	      *mouse-move-funs*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key presses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *key-press-funs* '())

(defun key-press-hook (win key modifiers)
  (dolist (fun *key-press-funs*)
    (funcall fun 
	     win 
	     key 
	     modifiers)))

(defun add-key-press-hook (fun)
    (if (not (member fun 
		     *key-press-funs*))
	(setf *key-press-funs* 
	      (cons fun 
		    *key-press-funs*))))

(defun remove-key-press-hook (fun)
  (setf *key-press-funs* 
	(remove fun 
		*key-press-funs*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key releases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *key-release-funs* '())

(defun key-release-hook (win key modifiers)
  (dolist (fun *key-release-funs*)
      (funcall fun 
	       win 
	       key 
	       modifiers)))

(defun add-key-release-hook (fun)
  (if (not (member fun 
		   *key-release-funs*))
      (setf *key-release-funs* 
	    (cons fun 
		  *key-release-funs*))))

(defun remove-key-release-hook (fun)
  (setf *key-release-funs* 
	(remove fun 
		*key-release-funs*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu Selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *menu-select-funs* '())

(defun menu-select-hook (win menu-id)
  (dolist (fun *menu-select-funs*) 
    (funcall fun 
	     win
	     menu-id)))

(defun add-menu-select-hook (fun)
  (if (not (member fun 
		   *menu-select-funs*))
      (setf *menu-select-funs* 
	    (cons fun 
		  *menu-select-funs*))))

(defun remove-menu-select-hook (fun)
  (setf *menu-select-funs*
	(remove fun 
		*menu-select-funs*)))
