;; package.lisp : the minara packages
;;
;; Copyright (c) 2007 Rob Myers, rob@robmyers.org
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


(in-package cl-user)

(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (defpackage minara
	(:documentation
	 "Minara public API.")
	(:use :cl)
	(:export :minara
		 ;; Transformations
		 :matrix :make-matrix-identity :make-matrix-scale 
		 :make-matrix-translate :make-matrix-rotate :matrix-to-string
		 :matrix-concatenate :matrix-point-transform
		 ;; Keymaps
		 :make-keymap :keymap-add-fun :keymap-current-set 
		 :reset-current-keymap 
		 ;; Buffers
		 :buffer :buffer-variable :set-buffer-variable 
		 :ensure-buffer-variable :kill-buffer-variable
		 :kill-all-buffer-variables :buffer-invalidate
		 :make-buffer-from-file :write-buffer-to-file 
		 :reload-buffer-file
		 ;; Windows
		 :window :window-buffer :window-buffer-main
		 :make-window-buffer :ensure-window-buffer :remove-window-buffer
		 :window-buffer-path :set-window-title-info :window-redraw
		 :make-window :make-window-from-file :reload-window-buffer
		 :reload-current-window :buffer-has-file-p :save-window
		 :save-current-window
		 ;; Minibuffer
		 :minibuffer :minibuffer-string :minibuffer-insert 
		 :minibuffer-delete :minibuffer-erase :minibuffer-go-start
		 :minibufer-go-end :minibuffer-go-forward :minibuffer-go-back
		 :window-minibuffer :window-add-minibuffer 
		 :window-remove-minibuffer
		 ;; Undo
		 :buffer-undo-mark :buffer-variable-set-undoable 
		 :buffer-insert-undoable :buffer-delete-undoable
		 :window-undo-stack-push :window-undo-stack-pop
		 ;; Menus
		 :menu-make-toplevel :menu-make-entry :menu-delete-entry 
		 :menu-install-toplevel
		 ;; Geometry
		 :distance-between-points :angle-around-point 
		 :degrees-to-radians :rotate-point-around-point 
		 :lines-intersect-vertices 
		 :point :add-point :divide-point :point-line-side
		 :bezier-eval :line-bezier-intersection-count-vertices
		 ;; Tools
		 :install-tool)))

(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (defpackage minara-rendering
	(:documentation
	 "Minara rendering API.")
	(:use :cl)
	(:export :set-colour :path-begin :path-end :move-to :line-to :curve-to 
		 :push-matrix :pop-matrix :concatenate-matrix :set-matrix 
		 :identity-matrix :translate :rotate :scale
		 :cache-draw :cache-record-begin :cache-record-end)))

(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (defpackage minara-picking
	(:documentation
	 "Minara picking API.")
	(:use :cl :minara)
	(:export :set-colour :path-begin :path-end :move-to :line-to :curve-to 
		 :push-matrix :pop-matrix :concatenate-matrix :set-matrix 
		 :identity-matrix :translate :rotate :scale)))

