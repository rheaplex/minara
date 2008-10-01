;; tool.lisp : tool handling
;;
;; Copyright (c) 2004,2007 Rob Myers, rob@robmyers.org
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tools
;; A tool is a set of event handlers and routines to install/uninstall them,
;; hooked up to the main keymap and the tool menu.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install a tool
;; This gives access to the tool from the menu and keyboard,
;; And installs an uninstall function (setup can be done by install-fun)
;; The install has to add the event handlers, the uninstall has to remove them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Handle a tool stopping being the current tool

(defvar *remove-current-tool-hook* nil)

;; Keep track of the current tool name

(defvar *current-tool-name* "")

(defun set-current-tool-name (name)
  (setf *current-tool-name* name)
  (window-redraw (window-current)))

;; Draw the status with the tool name after the main redraw

(defmethod window-redraw-event :around (window &optional (prefix ""))
  (declare (ignore prefix))
  (call-next-method window 
		    (concatenate 'string 
				 *current-tool-name*
				 " ")))

(defun remove-current-tool ()
  (if *remove-current-tool-hook*
      (funcall *remove-current-tool-hook*))
  (setf *remove-current-tool-hook* nil)
  (setf *current-tool-name* "")
  ;;(install-window-rendering-protocol)
  (window-redraw (window-current)))

(defun install-tool (install-fun uninstall-fun menu-name &rest key-combo)
  (let ((install-fun-with-boilerplate
	 (lambda () 
	   (remove-current-tool)
	   (set-current-tool-name menu-name)
	   (setf *remove-current-tool-hook* uninstall-fun)
	   (funcall install-fun))))
    (menu-callback-add menu-name install-fun-with-boilerplate)
    (funcall #'keymap-add-fun 
	     *global-keymap* 
	     install-fun-with-boilerplate 
	     key-combo)))
