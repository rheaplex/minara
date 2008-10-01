;; buffer.lisp : buffers for minara
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
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package minara)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer
;; A buffer of text, particularly the Lisp code describing the drawing
;; instructions from a document or an overlay.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass buffer ()
  ((gap-buffer :accessor buffer-gap-buffer
             :initarg :content
             :initform (make-instance 'flexichain:standard-flexichain))
   (mark :accessor buffer-mark
	 :initform 0)
   (changed :accessor buffer-changed
            :initform nil)
   (cache :accessor buffer-cache
          :initarg :cache
          :initform nil)
   (variables :accessor buffer-variables
              :initarg :variables
              :initform (make-hash-table :size 31))))

(defun set-buffer-changed (buf)
  (setf (buffer-changed buf)
        t))

(defun clear-buffer-changed (buf)
  (setf (buffer-changed buf)
        nil))

(defun buffer-changed-p (buf)
  (eq (buffer-changed buf) t))

(defun buffer-erase (buf)
  (setf (buffer-gap-buffer buf)
        (make-instance 'flexichain:standard-flexichain)))

(defun buffer-end (buf)
  (flexichain:nb-elements (buffer-gap-buffer buf)))

(defun buffer-start (buf)
  (declare (ignore buf))
  0)

;; To string
;; Don't use for evaluation, use stream
(defun buffer-range-to-string (buffer begin end)
  (let ((content (buffer-gap-buffer buffer)))
    (loop with result = (make-string  (- end begin))
       for source from begin below end
       for dest upfrom 0
       do (setf (aref result dest) 
		(flexichain:element* content source))
       finally (return result))))

(defun buffer-to-string (buffer)
  (buffer-range-to-string buffer (buffer-start buffer) (buffer-end buffer)))

;; Incredibly slow. Replace with a buffer-stream system.
;; Evaluates in current package, make sure to set the package beforehand.

(defun evaluate-buffer (buffer package-name)
  (let ((old-package *package*))
    (setf *package* (find-package package-name))
    (with-input-from-string 
	(strstr (buffer-range-to-string buffer 
					(buffer-start buffer) 
					(buffer-end buffer)))
      (load strstr))
    (setf *package* old-package)))

;; Insert and delete outside of the undo system.
;; No, you really do not want these. See undo.

(defun buffer-insert-no-undo (buffer pos text)
  (setf (buffer-mark buffer) pos)
  (let ((buf (buffer-gap-buffer buffer)))
    (loop for char across text
       do (flexichain:insert* buf (buffer-mark buffer) char)
       do (incf (buffer-mark buffer)))))
  

(defun buffer-delete-no-undo (buffer pos count)
  (loop repeat count
     do (flexichain:delete* (buffer-gap-buffer buffer) pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer Variables
;; A buffer can have an arbitrary number of named variables set on it.
;; These will last until the buffer is disposed of, and are unaffected by event
;; handling, particularly redraws, unless the code inside the buffer affects
;; the variables when evaluated, which would be weird.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set buffer variable

(defun set-buffer-variable (buffer name value)
  (setf (gethash name (buffer-variables buffer))
        value))

;; Get buffer variable

(defun buffer-variable (buffer name)
  (gethash name (buffer-variables buffer)))

;; Get buffer variable, creating it if it doesn't exist

(defun ensure-buffer-variable (buffer name)
  (multiple-value-bind (val present)
      (gethash name (buffer-variables buffer))
    (if present
	val
	(set-buffer-variable buffer name nil))))

;; Remove buffer variable

(defun kill-buffer-variable (buffer name)
  (remhash name (buffer-variables buffer)))

;; Remove all buffer variables

(defun kill-all-buffer-variables (buffer)
  (setf (buffer-variables buffer)
        (make-instance 'hashtable)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer Drawing and Caching
;; Note that we draw lazily, only evaluating a buffer if it has been updated
;; since it was last cached.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So code located in the current buffer can get buffer variables, for example.

(defvar *current-buffer* nil)

(defun current-buffer ()
    *current-buffer*)

(defmacro with-buffer (buf &rest body)
  (let ((old-buf (gensym)))
    `(let ((,old-buf *current-buffer*))
       (setf *current-buffer* ,buf)
       ,@body
       (setf *current-buffer* ,old-buf))))

;; Redraw the buffer (just run the cache if no timestamp variation)

(defun draw-buffer (cb)
  (if (buffer-changed-p cb)
      ;; Just redraw the cache, the text hasn't changed
      (minara-rendering:cache-draw (buffer-cache cb))
      ;; Otherwise, generate the cache and update the cache timestamp
      (let ((c (buffer-cache cb)))
	(minara-rendering:cache-record-begin c)
	(with-buffer cb
	  (evaluate-buffer cb))
	(minara-rendering:cache-record-end c)
	(clear-buffer-changed cb))))

;; Flag the buffer to be drawn when the window next redraws

(defun buffer-invalidate (cb)
  (set-buffer-changed cb))

;; Loading and saving buffers

(defun buffer-file-path (buffer)
  (buffer-variable buffer '_file-path ))

(defun set-buffer-file-path (buffer file-path)
  (set-buffer-variable buffer '_file-path file-path))

(defun buffer-file-timestamp (buffer)
  (buffer-variable buffer '_file-timestamp ))

(defun set-buffer-file-timestamp (buffer file-path)
  (set-buffer-variable buffer '_file-timestamp (file-write-date file-path)))

;; This adds a trailing newline at end of file if one wasn't present

(defun read-buffer-from-file (buffer filename)
  (let ((content (buffer-gap-buffer buffer)))
    (with-open-file (file filename)
      (loop for line = (read-line file nil 'eof)
	 until (eq line 'eof)
	 do (loop for char across line
	       do (flexichain:push-end content char)
	       finally (flexichain:push-end content #\Newline)))))
  (setf (buffer-mark buffer) (buffer-end buffer))
  (set-buffer-file-path buffer filename)
  (set-buffer-file-timestamp buffer filename)
  buffer)

(defun make-buffer-from-file (filename)
  (read-buffer-from-file (make-instance 'buffer) filename))

(defun reload-buffer-file (buffer)
  (buffer-erase buffer)
  (read-buffer-from-file buffer (buffer-file-path buffer)))

(defun write-buffer-to-file (buffer filename)
  (with-open-file (file filename :direction :output :if-exists :supersede)
    (write-string (buffer-range-to-string buffer 
					  (buffer-start buffer) 
					  (buffer-end buffer))
		  file))
  (set-buffer-file-path buffer filename)
  (set-buffer-file-timestamp buffer filename))
