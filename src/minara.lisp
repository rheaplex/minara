;; minara.lisp : setup and startup
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

;; Load the splash screen if required
;; This is a GLUT wart: if we started without a window open we'd quit

(defun load-splash-screen ()
  (display-window (make-window-from-file "../minara.minara")))
	 

;; Main entry point / startup.

(defun minara ()
 (load-splash-screen))