;; window.lisp : windows for minara
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
;; Windows (frames)
;; Documents are attached to a single window at the moment.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package minara)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *window-width* 600)
(defparameter *window-height* 400)
(defparameter *untitled-window-name* "Untitled")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The list of windows

(defvar *windows* (make-hash-table :size 31))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The window record

(defclass window (glut:window)
  ((buffers :initarg :buffers
            :accessor window-buffers
	    :initform '())
   (status :initarg :status
           :accessor window-status
	   :initform "")
   (undo-stack :initarg :undo-stack
               :accessor window-undo-stack
	       :initform '()))
  (:default-initargs :width *window-width* :height *window-height*
                     :pos-x 100 :pos-y 100
                     :mode '(:single :rgb) :title *untitled-window-name*))

;; Set the title

(defun set-window-title (window title)
  (glut:set-window-title window title))

;; Utility constructor

#|(define (%make-window)
  ;; Window must be made before any display lists (MacOSX)!
  (let ((window
         (really-make-window
          (window-make $window-width $window-height)
          '()
          -1
          -1
          ""
          '())))
    (hash-create-handle! *windows* (window-id window) window)
    ;; Redraw timestamp
    (initialise-timestamp! window)
c
    ;; Buffers *under* the main buffer, created bottom to top
    ;; Ask before adding anything here
    (window-view-buffer-make window)

    ;; Return the window
    window))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We assume that assoc lists will keep their order

;; Get window buffer

(defun window-buffer (window name)
  (cdr (assoc name (window-buffers window))))

;; Get main window buffer

(defun window-buffer-main (window)
  (window-buffer window '_main))

;; Add window buffer
;; We add buffers to the end of the list so we draw newer buffers last
;; This means doing our own assoc-set equivalent

(defun add-window-buffer (window buffer-name buffer)
  (if (not (window-buffer window buffer-name))
      (setf (window-buffers window)
	    (append (list (cons buffer-name buffer))
		    (window-buffers window)))))

(defun make-window-buffer (window buffer-name)
  (let ((buf (make-instance 'buffer)))
    (add-window-buffer window buffer-name buf)
    buf))

(defun ensure-window-buffer (window buffer-name)
  (let ((maybe-buffer (window-buffer window buffer-name)))
    (or maybe-buffer
        (make-window-buffer window buffer-name))))

;; Remove window buffer

(defun remove-window-buffer (window name)
  (setf (window-buffers window)
        (remove-if (lambda (candidate) (eq (car candidate) name))
                   (window-buffers window))))

;; Get the main buffer path

(defun window-buffer-path (window)
    (buffer-file-path (window-buffer-main window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Title
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get the window filename

(defun window-name-base (win)
  (let ((buf-path (buffer-file-path (window-buffer-main win))))
    (if (not buf-path)
        *untitled-window-name*
        (subseq buf-path 
		0
		(- (length buf-path) 
		   (length ".minara"))))))

;; Set a window title bar information

(defun set-window-title-info (win info)
  (minara-window-set-title (concatenate 'string (window-name-base win)
                                 "[" info "]")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Flag a window as needing redrawing

(defun window-redraw  (win)
  (minara-window-invalidate (glut:id win)))

;; Draw the window, rebuilding buffer caches as needed

(defun window-draw (cb)
  ;; Check timestamps, short circuiting on the first earlier than the window
  ;; If the window cache is more recent than the buffer timestamps
  ;;  (install-window-rendering-protocol)
  ;;  (minara-rendering:push-matrix)
  (minara-rendering:rendering-begin)
  (dolist (buf-cons (window-buffers cb))
    (let ((buf (cdr buf-cons)))
      (format t "caching ~a~%" (buffer-cache buf))
      (minara-rendering:cache-record-begin (buffer-cache buf))
      (evaluate-buffer buf "MINARA-RENDERING")
      (minara-rendering:cache-record-end (buffer-cache buf))
      (clear-buffer-changed buf)))
  (minara-rendering:rendering-end))

;; Draw or redraw a window's buffers/caches

(defgeneric window-redraw-event (window &optional prefix))

(defmethod window-redraw-event (window &optional (prefix ""))
  (minara-window-draw-begin window)
  (window-draw window)
  ;; Should be set status, like set title. But needs faking in GLUT...
  (minara-window-draw-status (concatenate 'string
					  prefix
					  (window-status window)))
  (minara-window-draw-end window))

(add-draw-hook #'window-redraw-event)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window construction & loading buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Public constructors

(defgeneric make-window ())

(defmethod make-window ()
  (let ((win (make-instance 'window)))
    ;; Main buffer
    (make-window-buffer win '_main)
    ;; Set title
    (set-window-title win *untitled-window-name*)
    win))

(defgeneric make-window-from-file (file-path))

(defmethod make-window-from-file (file-path)
  (let* ((win (make-instance 'window)))
    ;; Main buffer from file
    (add-window-buffer win '_main (make-buffer-from-file file-path))
    ;; Set title
    ;;    (set-window-title win (window-name-base win))
    win))


;; Reload the document buffer

(defun reload-window-buffer (window)
  (let ((main (window-buffer-main window)))
    (when (< (buffer-file-timestamp main)
             (file-write-date (window-buffer-path window)))
      (reload-buffer-file main)
      (window-redraw window))))
;;(window-status-temporary window
;;                         "Buffer changed, not reloaded!"
;;                 2))))

(defun reload-current-window ()
  (reload-window-buffer (window-current)))

(keymap-add-fun *global-keymap* #'reload-current-window "x" "r")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Find whether a buffer has ever been saved

(defun buffer-has-file-p (buf)
  (not (eq (buffer-file-path buf) nil)))

;; Find whether a buffer has changed

(defun buffer-changed-since-load-p (buf)
  (if (not (buffer-file-path buf))
      nil
      (buffer-changed buf)))

;; Get the file path for a window

;; Save a window buffer, updating the timestamp

(defun save-window (win)
  (let* ((file-buffer (buffer-gap-buffer (window-buffer-main win)))
         (file-path (buffer-file-path file-buffer)))
    (if (not file-path)
        (error "Trying to save buffer with no associated file")
        (if (buffer-changed-since-load-p file-buffer)
            (let ((backup-path (concatenate 'string file-path "~")))
              (rename-file file-path backup-path)
              (write-buffer-to-file file-buffer file-path))))))

;; Save the current frontmost window

(defun save-current-window ()
  (save-window (window-current)))

;; Register keys for saving a window

(keymap-add-fun *global-keymap* #'save-current-window "x" "s")

;; Close a window safely, prompting user and saving if changed

;(define (close-window-event win)
;  (save-window win))

;; Ask the user if they want to close the window, and if so whether to save

;(define (prompt-user-if-changed win)
  ;; IMPLEMENT ME
;  'just-close)

;; Close the frontmost window

;(define (close-current-window)
;  (let ((current-window ((window-for-id (window-current)))))
;    (case (prompt-user-if-changed)
;      ('save-and-close (save-window current-window)
;                      (close-window current-window))
;      ('just-close
;       (close-window current-window)))))

;; Register keys for closing a window

;(keymap-add-fun %global-keymap close-current-window "x" "c")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window External editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Edit the window's main buffer in an external editor

(defparameter *default-external-edit-command*
    "/usr/bin/open -a /Applications/TextEdit.app")

(defvar *external-edit-command* *default-external-edit-command*)

(defun external-edit-file (path)
  #|(system (concatenate 'string *external-edit-command*
                       " "
                       path
                       " &"))|#
  (format t "Should open ~a in Emacs.~%" path))

(defun external-edit-window (window)
  ;; TODO:  Warn user if unsaved!!!!!
  (save-window window)
  (external-edit-file (window-buffer-path window)))

;; Edit the current frontmost window

(defun external-edit-current-window ()
  (external-edit-window (window-current)))

;; Register keys for editing a window

(keymap-add-fun *global-keymap* #'external-edit-current-window "x" "e")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window status line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the status temporarily
;; Needs threads, so fix later
;; Or convert to installing/uninstalling mouse event handler

(defun window-status-temporary (window status duration)
  (declare (ignore window) (ignore status) (ignore duration)))

#|  (let ((old-status (window-status window)))
    (set-window-status window
                       status)
    (window-redraw (window-id window))
    (begin-thread
     (lambda ()
       (sleep duration)
       (if (string= status
                    (window-status window))
           (begin
            (set-window-status! window
                                old-status)
            (window-redraw (window-id window))))))))|#

