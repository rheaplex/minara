;; minibuffer.lisp : hacky Emacs-style minibuffers
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

;; Have an uneditable section or prefix string

(defclass minibuffer ()
    ((gap-buffer :accessor minibuffer-gap-buffer
                 :initarg :gap-buffer
                 :initform (make-instance 'standard-cursorchain))
     (window :accessor minibuffer-window
             :initarg :window)
     (return-callback :accessor minibuffer-return
                      :initarg :return)
     (cancel-callback :accessor minibuffer-cancel
                      :initarg :cancel)))

(defun minibuffer-length (mini)
    (flexichain:nb-elements (minibuffer-gap-buffer mini)))

(defun minibuffer-position (mini)
    (flexichain:cursor-pos (minibuffer-gap-buffer mini)))

(defun minibuffer-string (mini)
  (let ((content (minibuffer-gap-buffer mini)))
    (loop with result = (make-string (flexichain:nb-elements content))
       for source from 0 below (flexichain:nb-elements content)
       for dest upfrom 0
       do (setf (flexichain:cursor-pos content) 
		source)
       do (setf (aref result dest) 
		(flexichain:element< content))
       finally (return result))))

(defun minibuffer-redraw (mini)
  (let ((window (minibuffer-window mini)))
    (minara-window-draw-status window
                        (minibuffer-string mini))
    (window-redraw window)))

(defun minibuffer-insert (mini char)
  (flexichain:insert (minibuffer-gap-buffer mini)
          char)
  (minibuffer-redraw mini))

(defun minibuffer-delete (mini)
  (when (> 0 (minibuffer-position mini))
    (flexichain:delete* (minibuffer-gap-buffer mini)
             (1- (minibuffer-position mini))))
  (minibuffer-redraw mini))

(defun minibuffer-erase (mini)
  (setf (minibuffer-gap-buffer mini)
        (make-instance 'standard-cursorchain))
  (minibuffer-redraw mini))

(defun minibuffer-go-start (mini)
  (setf (flexichain:cursor-pos (minibuffer-gap-buffer mini))
        0)
  (minibuffer-redraw mini))

(defun minibuffer-go-end (mini)
  (setf (flexichain:cursor-pos (minibuffer-gap-buffer mini))
        (1- (flexichain:nb-elements (minibuffer-gap-buffer mini))))
  (minibuffer-redraw mini))

(defun minibuffer-go-forward (mini)
  (when (> (flexichain:nb-elements (minibuffer-gap-buffer mini))
           (flexichain:cursor-pos (minibuffer-gap-buffer mini)))
    (incf (flexichain:cursor-pos (minibuffer-gap-buffer mini))))
  (minibuffer-redraw mini))

(defun minibuffer-go-back (mini)
  (when (> 1 (flexichain:cursor-pos (minibuffer-gap-buffer mini)))
    (decf (flexichain:cursor-pos (minibuffer-gap-buffer mini))))
  (minibuffer-redraw mini))

(defun minibuffer-make (window initial-text ok cancel)
  (let ((mini (make-instance 'minibuffer
                             :window window
                             :ok ok
                             :cancel cancel)))
    (flexichain:insert-sequence (minibuffer-gap-buffer mini) initial-text)
    (minibuffer-go-end mini)
    mini))

;; Adding & removing to window

(defun window-minibuffer (window)
  (buffer-variable (window-buffer-main window)
                   '_minibuffer))

;; Will need fixing for multi-window operation
;; How should window & global event handlers interact?
;; Tools must ensure their buffers on mouse-down rather than create on install?

;; Key handler

(defun minibuffer-key-handler (mini k)
  
  (let ((key (aref k 0)))
    (case key
      ((GLUT_KEY_LEFT)
       (minibuffer-go-back mini))
      ((GLUT_KEY_RIGHT)
       (minibuffer-go-forward mini))
      ((GLUTKEY_UP)
       (minibuffer-go-end mini))
      ((GLUTKEY_DOWN)
       (minibuffer-go-start mini))
      ((#\del)
       (minibuffer-delete mini))
      ((#\return) ;; #\nl
       ;;       (window-remove-minibuffer (minibuffer-window mini))
       (funcall (minibuffer-return mini)))
      ((#\esc)
       ;;        (window-remove-minibuffer (minibuffer-window mini))
       (funcall (minibuffer-cancel mini)))
      (else
       (minibuffer-insert mini
                          key)))))

(defun window-minibuffer-key-handler (window key modifiers)
  (declare (ignore modifiers))
  (minibuffer-key-handler (window-minibuffer window)
                          key))

(defvar *previous-key-handlers* nil)

(defun install-minibuffer-key-handler ()
  (setf *key-release-funs* (list #'window-minibuffer-key-handler)))

(defun uninstall-minibuffer-key-handler ()
  (setf *key-release-funs* *previous-key-handlers*)
  (setf *previous-key-handlers* nil))

(defun window-add-minibuffer (window initial-text ok cancel)
  (set-buffer-variable (window-buffer-main window)
                       '_minibuffer
                       (minibuffer-make window
                                        initial-text
                                        ok
                                        cancel))
  (install-minibuffer-key-handler)
  (minibuffer-redraw (window-minibuffer window)))

(defun window-remove-minibuffer (window)
  (kill-buffer-variable (window-buffer-main window)
                        '_minibuffer)
  (uninstall-minibuffer-key-handler)
  (window-redraw window))



