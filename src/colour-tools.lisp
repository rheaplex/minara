;; colour-tools.lisp : minara colour picking
;;
;; Copyright (c) 2007 Rob Myers, rob@robmyers.org
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

;; Colour support

(defvar current-colour "(set-colour 0.0 0.0 0.0 0.0)")

(defun write-current-colour (buff)
  (buffer-insert-undoable buff
                          nil
                          (format nil
                                  "~a~%"
                                  current-colour)))

;; RGB

(defvar rgb-minor-amount 0.01)
(defvar rgb-major-amount 0.1)

(defvar rgb-current-r 0.0)
(defvar rgb-current-g 0.0)
(defvar rgb-current-b 0.0)
(defvar rgb-current-a 0.0)

(defvar rgb-current-component 'r)

(defun rgb-current-string ()
  (format nil
          "(set-colour ~f ~f ~f ~f)"
          rgb-current-r
          rgb-current-g
          rgb-current-b
          rgb-current-a))

(defun set-rgb-current-colour ()
  (setf current-colour
        (rgb-current-string)))

#|(defun rgb-set-component (component to)
  (cond
    ((> to
        1.0)
     (rgb-set-current-component
                   1.0))
    ((< to
        0.0)
     (rgb-set-current-component
                   0.0))
    (t
     (rgb-set-current-component
                   to))))|#

(defun rgb-current-component ()
  (case rgb-current-component
    ('r rgb-current-r)
    ('g rgb-current-g)
    ('b rgb-current-b)
    ('a rgb-current-a)))

(defun rgb-set-current-component (to)
  (case %rgb-current-component
    ('r (setf rgb-current-r to))
    ('g (setf rgb-current-g to))
    ('b (setf rgb-current-b to))
    ('a (setf rgb-current-a to))))

(defun rgb-current-status-update (window)
  (minara-window-draw-status window
			     (format nil
				     "- Red: ~f Green: ~f Blue: ~f Alpha: ~f"
				     rgb-current-r
				     rgb-current-g
				     rgb-current-b
				     rgb-current-a)))

(defun rgb-current-buffer-make (window)
  (make-window-buffer window
                      "rgb"))

(defun rgb-current-buffer-destroy (window)
  (remove-window-buffer window
                        "rgb"))

(defun rgb-current-buffer (window)
  (window-buffer window
                 "rgb"))

(defun rgb-current-sample-update (window)
  (let ((buff (rgb-current-buffer window)))
    (buffer-erase buff)
    (buffer-insert-undoable buff
                            nil
                            (rgb-current-string))
    (buffer-insert-undoable buff
                            nil
                            "(push-matrix)\n(identity-matrix)\n")
    (buffer-insert-undoable buff
                            nil
                            "(path-begin)\n(move-to 10 40)\n(line-to 10 90)\n (line-to 60 90)\n(line-to 60 40)\n(path-end)\n")
    (buffer-insert-undoable buff
                            nil
                            "(pop-matrix)\n")
    (buffer-undo-mark buff)
    (buffer-invalidate buff)))

(defun rgb-current-refresh()
  (let ((window (window-current))) ;; Update for multi-window
    (rgb-current-status-update window)
    (rgb-current-sample-update window)
    (window-redraw window)))

(defun rgb-update-current-component (to)
  (rgb-set-current-component to)
  (rgb-current-refresh))

(defun rgb-add-current-component (to)
  (rgb-set-current-component (+ (rgb-current-component)
                                to)))

;; The tool's keymap

(defvar %rgb-keymap (make-keymap))

;; Install/uninstall the tool

(defun rgb-current-tool-install ()
  (let ((window (window-current)))
    (rgb-current-buffer-make window)
    (keymap-current-root-set %rgb-keymap)
    (rgb-current-refresh)))

(defun rgb-current-tool-uninstall ()
  (let ((window (window-current)))
    (set-rgb-current-colour)
    (keymap-current-root-reset)
    (set-window-status window
                       "")
    (rgb-current-buffer-destroy window)
    (window-redraw window)))

;; Key presses

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (setf rgb-current-component 'r))
                "r")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (setf rgb-current-component 'g))
                "g")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (setf rgb-current-component 'b))
                "b")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (set rgb-current-component 'a))
                "a")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-add-current-component rgb-minor-amount))
                "=")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-add-current-component rgb-major-amount))
                "+")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-add-current-component (- rgb-minor-amount)))
                "-")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-add-current-component (- rgb-major-amount)))
                "_")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-update-current-component 0.0))
                "0")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-update-current-component 0.1))
                "1")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-update-current-component 0.2))
                "2")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-update-current-component 0.3))
                "3")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-update-current-component 0.4))
                "4")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-update-current-component 0.5))
                "5")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-update-current-component 0.6))
                "6")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-update-current-component 0.7))
                "7")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-update-current-component 0.8))
                "8")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-update-current-component 0.9))
                "9")

(keymap-add-fun %rgb-keymap
                (lambda ()
                  (rgb-update-current-component 1.0))
                "!")

(keymap-add-fun %rgb-keymap
                #'rgb-current-tool-uninstall
                "q")


;; Register

(install-tool #'rgb-current-tool-install
              #'rgb-current-tool-uninstall
              "RGB"
              "c" "r")