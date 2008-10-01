;; buffer-stream.lisp : buffer streams and file loading/saving for minara
;;
;; Derived from buffer-stream.lisp from Drei
;; http://common-lisp.net/cgi-bin/viewcvs.cgi/mcclim/Drei/buffer-streams.lisp
;;;  (c) copyright 2006-2007 by
;;;           Troels Henriksen (athas@sigkill.dk)
;;
;; Additions and alterations Copyright (c) 2007 Rob Myers, rob@robmyers.org
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


(in-package minara)

(defclass buffer-stream (fundamental-character-input-stream
                         fundamental-character-output-stream)
  ((%buffer :initarg :buffer
            :initform (error "A buffer must be provided")
            :reader buffer
            :documentation "The buffer from which this stream
will read data.")
   (%start-mark :initarg :start-mark
                :reader start-mark
                :documentation "A mark into the buffer of the
stream that indicates from which point on the stream will read
data from the buffer. By default, the beginning of the
buffer. This mark should not be changed.")
   (%end-mark :initarg :end-mark
              :reader end-mark
              :documentation "A mark into the buffer of the
stream that indicates the buffer position that the stream will
consider end-of-file. By default, the end of the buffer. This
mark should not be changed.")
   (%point :accessor point-of
           :documentation "A mark indicating the current position
in the buffer of the stream."))
  (:documentation "A bidirectional stream that performs I/O on an
underlying Drei buffer. Marks can be provided to let the stream
operate on only a specific section of the buffer."))

(defmethod initialize-instance :after
    ((stream buffer-stream) &key)
  (unless (slot-boundp stream '%start-mark)
    (setf (slot-value stream '%start-mark)
          (clone-mark (point-of (buffer stream)) :left))
    (flexichain:at-beginning-p (start-mark stream)))
  (unless (slot-boundp stream '%end-mark)
    (setf (slot-value stream '%end-mark)
          (clone-mark (start-mark stream) :right))
    (flexichain:at-end-p (end-mark stream)))
  (setf (point-of stream)
        (narrow-mark (clone-mark (start-mark stream) :right)
				(start-mark stream)
                     (end-mark stream))))

;;; Input methods.

(defmethod stream-read-char ((stream buffer-stream))
  (if (flexichain:at-end-p (point-of stream))
      :eof
      (prog1 (flexichain:element> (point-of stream))
        (forward-object (point-of stream)))))

(defmethod stream-unread-char ((stream buffer-stream) (char character))
  (unless (flexichain:at-beginning-p (point-of stream))
    (flexichain:element< (point-of stream))
    nil))

(defmethod stream-read-char-no-hang ((stream buffer-stream))
  (stream-read-char stream))

(defmethod stream-peek-char ((stream buffer-stream))
  (if (flexichain:at-end-p (point-of stream))
      :eof
      (flexichain:element> (point-of stream))))

(defmethod stream-listen ((stream buffer-stream))
  (not (flexichain:at-end-p (point-of stream))))

(defmethod stream-read-line ((stream buffer-stream))
  (let ((orig-offset (offset (point-of stream)))
        (end-of-line-offset (offset (end-of-line (point-of stream)))))
    (unless (flexichain:at-end-p (point-of stream))
      (forward-object (point-of stream)))
    (values (buffer-substring (buffer stream)
                              orig-offset
                              end-of-line-offset)
            (end-of-buffer-p (point-of stream)))))

(defmethod stream-clear-input ((stream buffer-stream))
  nil)

;;; Output methods.

(defmethod stream-write-char ((stream buffer-stream) char)
  (flexichain:insert (point-of stream) char))

(defmethod stream-line-column ((stream buffer-stream))
  (column-number (point-of stream)))

(defmethod stream-start-line-p ((stream buffer-stream))
  (or (eq (point-of stream)
	  (start-mark stream))
      (beginning-of-line-p (point-of stream))))

(defmethod stream-write-string ((stream buffer-stream) string
                                &optional (start 0) end)
  (flexichain:insert-sequence (point-of stream)
                   (subseq string start end)))

(defmethod stream-terpri ((stream buffer-stream))
  (flexichain:insert (point-of stream) #\Newline))

(defmethod stream-fresh-line ((stream buffer-stream))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)))

(defmethod stream-finish-output ((stream buffer-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-force-output ((stream buffer-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-clear-output ((stream buffer-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-advance-to-column ((stream buffer-stream) (column integer))
  (call-next-method))

(defmethod interactive-stream-p ((stream buffer-stream))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-flexichain-stream (&key (buffer (current-buffer))
                           (start-mark nil start-mark-p)
                           (end-mark nil end-mark-p))
  (apply #'make-instance 'buffer-stream
         :buffer buffer
         (append (when start-mark-p
                   (list :start-mark start-mark))
                 (when end-mark-p
                   (list :end-mark end-mark)))))

(defun make-buffer-stream (buffer)
  (make-instance 'buffer-stream
                 :buffer buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer from stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-buffer-file-timestamp (buffer timestamp)
  (set-buffer-variable buffer '_t timestamp))

(defun buffer-file-timestamp (buffer)
  (buffer-variable buffer '_t))

;; The file path for a buffer than has been loaded from file

(defun set-buffer-file-path (buf filename)
  (set-buffer-variable buf '_filename filename))

(defun buffer-file-path (buf)
  (buffer-variable buf '_filename))

(defun load-buffer-from-file (buffer file-path)
  (with-open-file
   (file-stream file-path)
   (let ((buffer-stream (make-buffer-stream (buffer-content buffer))))
     (loop while (stream-listen file-stream)
           do (write-line buffer-stream (read-line file-stream)))))
  (set-buffer-file-timestamp buffer (file-write-date file-path))
  (set-buffer-file-path buffer file-path)
  (set-buffer-changed buffer)
  buffer)

(defun make-buffer-from-file (file-path)
  (load-buffer-from-file (make-instance 'buffer) file-path))

;; Reload the buffer from file.

(defun buffer-file-reload (buf)
  (buffer-delete-undoable buf nil nil)
  (load-buffer-from-file buf (buffer-file-path buf))
  (buffer-undo-mark buf)
  (buffer-invalidate buf))
