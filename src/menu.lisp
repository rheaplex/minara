;; menu.lisp : contextual menus for minara
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


(in-package minara)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus
;; We use GLUT contextual menus at the moment. This is terrible. Nothing should
;; be done that relies on this, or that would prevent proper pull-down menus
;; being used in future.
;;
;; Menu handlers receive the window id they're called on as their parameter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The global menu / menu item id count 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *menu-item-id* 1024)

(defun menu-id-make ()
  (let ((id *menu-item-id*))
    (setf *menu-item-id* (+ *menu-item-id* 1))
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Callbacks for menu items
;; So when the window system passes us a "menu selected" event, we look up the
;; menu item id here and call the callback registered under that id.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar menu-callbacks '())

(defun menu-callback-add (id callback)
  (setf menu-callbacks
	(acons id callback menu-callbacks)))

(defun menu-callback-remove (id)
  (setf menu-callbacks
	(remove-if (lambda (target) (eq target (car id)))
		   menu-callbacks)))

(defun menu-callback (id)
  (assoc id menu-callbacks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level menus
;; We only have 1-level deep menus at the moment, no nested submenus.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun menu-make-toplevel (title)
  (list (minara-menu-make) title))

(defun menu-name (menu)
  (cadr menu))

(defun menu-id (menu)
  (car menu))

(defun menu-install-toplevel (menu)
  (minara-menu-install (menu-id menu) (menu-name menu)))


;; Remove a menu
;; Unimplemented

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a menu item.
;; This installs a callback that will be called when this item is selected.

(defun menu-make-entry (menu title callback)
  (let ((item-id (menu-id-make)))
    (minara-menu-add-entry (menu-id menu) title item-id)
    (menu-callback-add item-id callback)
    item-id))

;; Remove a menu item

(defun menu-delete-entry (menu-id)
  (minara-menu-remove-entry menu-id)
  (menu-callback-remove menu-id))


;; Change a menu state

;; Get a menu state

;; Enable a menu item

;; Disble a menu item


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu event handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The standard menu event handler
;; Hooked into the main event system in events.scm
;; Don't replace.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun default-menu-handler (window-id menu-id)
  (let ((handler (cdr (menu-callback menu-id))))
    (if handler
	(funcall handler window-id))))
  
(defun install-default-menu-handler ()
  (add-menu-select-hook #'default-menu-handler))

;; Just install the default menu handler to start with
(install-default-menu-handler)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define (pen-dummy window-id)
;   (write "called pen dummy"))

; (define (colour-dummy window-id)
;   (write "called colour dummy"))

; (define (select-dummy window-id)
;   (write "called select dummy"))

; (define menoo (menu-make-toplevel "tools"))
; (menu-make-entry menoo "pen" pen-dummy)
; (menu-make-entry menoo "colour" colour-dummy)
; (menu-make-entry menoo "select" select-dummy)
; (menu-install-toplevel menoo)