;; undo.lisp : undo and redo support for minara
;;
;; Copyright (c) 2004,2007 Rob Myers, rob@robmyers.org
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
;; Undo/redo
;; Memoization-based.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package minara)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-level undo handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The undo class
;; A linked list node containing an undo and redo method for the action

(defclass undoer ()
    ((undo :initarg :undo
           :accessor undoer-undo)
     (redo :initarg :redo
           :accessor undoer-redo)
     (next :initarg :next
           :accessor undoer-next)
     (previous :initarg :previous
               :accessor undoer-previous)))

;; Get the buffer undo stack

(defun buffer-undo-stack (buffer)
  (buffer-variable buffer "undo-stack"))

(defun set-buffer-undo-stack (buffer new-stack)
  (set-buffer-variable buffer
                       "undo-stack"
                       new-stack))

;; Push the undo/redo methods & double link them into the undo list

(defun buffer-undo-stack-push (buffer undo redo)
  (let* ((current-undo (buffer-undo-stack buffer))
         (new-undo (make-instance 'undoer
				  :undo undo
				  :redo redo
				  :next current-undo
				  :previous nil)))
    (if (not (eq current-undo nil))
        (setf (undoer-next current-undo)
              new-undo))
    (set-buffer-undo-stack buffer
                           new-undo)))

;; Placed at the bottom of / before a series of events
;; When we undo to this, it makes the *next* item the current undo & stop
;; When we redo to it, it makes the *previous* item the current undo & stop

(defun buffer-undo-mark (buffer)
    (buffer-undo-stack-push buffer nil nil))

(defun create-buffer-undo-stack (buffer)
  (set-buffer-undo-stack buffer '())
  (buffer-undo-mark buffer) ;; Make sure there's a mark at the bottom
  (buffer-variable buffer '_undo-stack))

;; Add an undo stack to every buffer when it is made

(defmethod initialize-instance :after ((b buffer) &rest initargs &key)
  (declare (ignore initargs))
  (create-buffer-undo-stack b)
  b)

;; Undo the buffer until we hit the next mark or the end of the undo list

(defun buffer-undo-aux (buffer undos)
  (let ((undo (undoer-undo undos))
        (previous (undoer-previous undos)))
    (if undo ;; If we haven't hit the next mark down
        (progn
          (funcall undo) ;; Call the undo action
          (buffer-undo-aux buffer ;; Then recurse
                           previous))
        (progn ;; If there isn't an undo it's a mark
          (set-buffer-undo-stack buffer ;; So set current to mark
                                 undos)
          (buffer-invalidate buffer)))))

(defun buffer-undo (buffer)
  (let* ((undos (buffer-undo-stack buffer)) ;; Get mark at top of stack
         (previous (undoer-previous undos))) ;; Get previous undo (or nil)
    (if (not (eq previous nil)) ;; If we are not at the base of the undo stack
        (buffer-undo-aux buffer ;; Move past the mark and start undoing
                         previous))))

;; Redo the buffer until we hit the next mark or the beginning of the undo list

(defun buffer-redo-aux (buffer redos)
  (let ((redo (undoer-redo redos))
        (next (undoer-next redos)))
    (if redo ;; If we haven't hit the next mark up
        (progn
          (funcall redo) ;; Call the redo action
          (buffer-redo-aux buffer ;; Then recurse
                           next))
        (progn ;; If there isn't a redo it's a mark
          (set-buffer-undo-stack buffer ;; So set current to mark
                                 redos)
          (buffer-invalidate buffer)))))

(defun buffer-redo (buffer)
  (let* ((undos (buffer-undo-stack buffer)) ;; Get the redos
         (redos (undoer-next undos))) ;; Get redo after current undo
    (if (not (eq redos nil)) ;; If it exists
        (buffer-redo-aux buffer
                         redos))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer undoable actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set a variable in an undoable way

(defun buffer-variable-set-undoable (buffer variable-name value)
  (let ((old-value (buffer-variable buffer variable-name)))
    (buffer-undo-stack-push buffer
                            (lambda ()
                              (set-buffer-variable buffer
                                                   variable-name
                                                   old-value))
                            (lambda ()
                              (set-buffer-variable buffer
                                                   variable-name
                                                   value))))
  (set-buffer-variable buffer variable-name value))

;; Insert text into a buffer.

(defun buffer-insert-undoable (buffer pos text)
  (let ((position (or pos
                      (buffer-end (buffer-gap-buffer buffer))))
        (text-length (length text)))
    (buffer-undo-stack-push buffer
                            (lambda ()
                              (buffer-delete-no-undo buffer 
						     position 
						     text-length))
                            (lambda ()
                              (buffer-insert-no-undo buffer 
						     position
						     text)))
    (buffer-insert-no-undo buffer position text)))

;; Delete text from a buffer

(defun buffer-delete-undoable (buffer pos len)
  (let* ((position (or pos
		       (buffer-start (buffer-gap-buffer buffer))))
	 (text (buffer-range-to-string buffer position (+ position len))))
    (buffer-undo-stack-push buffer
                            (lambda ()
                              (buffer-insert-no-undo buffer position text))
                            (lambda ()
                              (buffer-delete-no-undo buffer position len)))
    (buffer-delete-no-undo buffer position len)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window-level undo handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set a buffer as the current undo target: user calls for undo will target it

(defun window-undo-stack-push (window buffer)
  (push buffer
        (window-undo-stack window)))

;; Set the undo target to the previous one (possibly none)

(defun window-undo-stack-pop (window)
  (pop (window-undo-stack window)))

;; Ask the window to get its current undo target to undo

(defun window-undo (window)
  (let ((current-undo-buffer (car (window-undo-stack window))))
    (if current-undo-buffer
        (buffer-undo current-undo-buffer))))

;; Ask the window to get its current undo target to redo

(defun window-redo (window)
  (let ((current-undo-buffer (car (window-undo-stack window))))
    (if current-undo-buffer
        (buffer-redo current-undo-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook into window creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-window :around ()
  (let ((win (call-next-method)))
    (window-undo-stack-push win
                            (window-buffer-main win))
    win))

(defmethod make-window-from-file :around (file-path)
  (let ((win (call-next-method)))
    (window-undo-stack-push win
                            (window-buffer-main win))
    win))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-window-undo ()
  (let ((win (window-current)))
    (window-undo win)
    (window-redraw win)))

(keymap-add-fun *global-keymap*
                #'call-window-undo
                "z")

(defun call-window-redo ()
  (let ((win (window-current)))
    (window-redo win)
    (window-redraw win)))

(keymap-add-fun *global-keymap*
                #'call-window-redo
                "Z")
