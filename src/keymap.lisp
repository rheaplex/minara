;; keymap.lisp : Keymaps
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
;; Keymaps
;; Inspired by, but different from, Emacs keymaps.
;; In particular keymaps fit within more general event handlers, rather than
;; the other way round. (If that statement is incorrect, please correct it).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package minara)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: Use Emacs-style nested keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window-system-specific constants
;; Here GLUT constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant GLUT_KEY_F1			1)
(defconstant GLUT_KEY_F2			2)
(defconstant GLUT_KEY_F3			3)
(defconstant GLUT_KEY_F4			4)
(defconstant GLUT_KEY_F5			5)
(defconstant GLUT_KEY_F6			6)
(defconstant GLUT_KEY_F7			7)
(defconstant GLUT_KEY_F8			8)
(defconstant GLUT_KEY_F9			9)
(defconstant GLUT_KEY_F10			10)
(defconstant GLUT_KEY_F11			11)
(defconstant GLUT_KEY_F12			12)
;; directional keys
(defconstant GLUT_KEY_LEFT			100)
(defconstant GLUT_KEY_UP			101)
(defconstant GLUT_KEY_RIGHT		        102)
(defconstant GLUT_KEY_DOWN			103)
(defconstant GLUT_KEY_PAGE_UP		        104)
(defconstant GLUT_KEY_PAGE_DOWN		        105)
(defconstant GLUT_KEY_HOME			106)
(defconstant GLUT_KEY_END			107)
(defconstant GLUT_KEY_INSERT		        108)

;; glutGetModifiers return mask.
(defconstant GLUT_ACTIVE_SHIFT               1)
(defconstant GLUT_ACTIVE_CTRL                2)
(defconstant GLUT_ACTIVE_ALT                 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making and getting keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make an empty keymap alist
(defun make-keymap ()
  (make-hash-table :size 31))

;; The one and only global keymap
(defvar *global-keymap* (make-keymap))

;; The current root keymap
(defvar *keymap-current-root* nil)

;; Check that the object is a keymap
(defun keymap-p (keymap)
  (hash-table-p keymap))

;; The current keymap
(defvar *keymap-current* *keymap-current-root*)

;; Reset the history
(defun reset-current-keymap ()
  (setf *keymap-current* *keymap-current-root*))

;; Set the root keymap
(defun keymap-current-root-set (keymap)
  (setf *keymap-current-root* keymap)
  (reset-current-keymap))

;; Reset the root keymap
(defun keymap-current-root-reset ()
  (setf *keymap-current-root* nil))

;; Set the current keymap
(defun keymap-current-set (keymap)
  (setf *keymap-current* keymap))

;; Add a possibly nested key fun to the keymap
;; A key is a basic key (aA1) prefixed with CA for control and/or alt

(defun keymap-add-fun-list (keymap fun keys)
  (let* ((key (car keys))
	 (rest-of-keys (cdr keys))
	 (keymap-entry-for-key (gethash key keymap t)))
    ;;(format #t "keys: ~a~%key: ~a~%keymap-for-key: ~a~%rest: ~a~%~%"
    ;;        keys key keymap-entry-for-key rest-of-keys)
    (cond
      ;; Last key? Insert in current keymap
      ((eq rest-of-keys nil)
       ;; Warn if rebinding key
       ;;(if (hash-ref keymap
       ;;              key)
       ;;    (format #t "Redefined key ~a in keymap ~a~%" key keymap))
       (setf (gethash key keymap) 
	     fun))
      ;; No keymap for key?
      ((eq keymap-entry-for-key t)
       ;; Create it
       (setf (gethash key keymap)
	     (make-keymap))
       ;; And recurse
       (keymap-add-fun-list (gethash key keymap)
			    fun
			    rest-of-keys))
      ;; Keymap exists but key is not last key?
      (t
       ;; If it's a key, replace with a keymap
       (if (functionp keymap-entry-for-key)
	   (setf (gethash key keymap)
		 (make-keymap)))
       ;; Just recurse, re-getting keymap in case we replaced key with keymap
       (keymap-add-fun-list (gethash key keymap)
			    fun
			    rest-of-keys)))))

(defun keymap-add-fun (keymap fun &rest keys)
  (keymap-add-fun-list keymap fun keys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching keypresses through keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Try to dispatch the key press in the keymap
(defun dispatch-keymap (keymap key)
  (let ((next-candidate (gethash key 
				 keymap)))
    (cond 
      ;; Keymap? Install as current keymap
      ((keymap-p next-candidate)
       (keymap-current-set next-candidate)
       t)
     ;; No match? Reset
     ((not next-candidate)
      (reset-current-keymap)
      nil)
     ;; Fun? Call
     (t ;;(fun? next-candidate)
      (funcall next-candidate)
      (reset-current-keymap)
      t))))

;; Try to dispatch the current keymap
(defun dispatch-key (key)
  (if (not (and *keymap-current* 
		(dispatch-keymap *keymap-current* 
				 key)))
      (if (not (dispatch-keymap *global-keymap* 
				key))
	  (format t 
		  "No match for key ~a in current or global keymap.~%" 
		  key))))

;; Our method to interface to the event system
;; Note that whilst getting shift alt and control is GLUT-dependent,
;; once we make the booleans it could be any windowing system
(defun key-dispatch-hook-method (win key modifiers)
  (declare (ignore win))
  (let (;;	(shift (= 1 (logand modifiers 
	;;	GLUT_ACTIVE_SHIFT)))
	(control (= 2 (logand modifiers 
			      GLUT_ACTIVE_CTRL)))
	(alt (= 4 (logand modifiers 
			  GLUT_ACTIVE_ALT))))
    ;; Shift is just the upper-case character
    ;;(if shift
    ;;(set! key (concatenate 'string "S" key)))
    (if control
	(setf key (concatenate 'string "C" key)))
    (if alt
	(setf key (concatenate 'string "A" key)))
    (dispatch-key key)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up the keymaps in the events system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install the cancel key into the global keymap
(keymap-add-fun *global-keymap* #'reset-current-keymap "Cg")

;; Hook into the event system
;;(add-key-release-hook #'key-dispatch-hook-method)


;; TEST

#|(defun dummy ()
  (write "hello"))

(defun dummy2 ()
  (write "hello 2"))

(defvar km (make-keymap))

(keymap-add-fun km #'dummy "a")
(keymap-add-fun km #'dummy2 "b" "c")
(keymap-current-root-set km)

(dispatch-key "a")|#