(defpackage :minara
            (:use :cl :asdf))

(in-package :minara)

(asdf:defsystem :minara
                :name "minara"
                :version "0.1"
                :depends-on (:flexichain :cl-cairo2 :cl-opengl :cl-glu :cl-glut)
                :serial t
                :components
                ((:file "packages")
                 (:file "test")
		 (:file "glut-gui")
                 (:file "transformations")
                 (:file "cairo-rendering")
                 (:file "events")
                 (:file "keymap")
                 (:file "buffer")
		 (:file "evaluation")
		 (:file "window")
		 (:file "minibuffer")
                 (:file "undo")
                 ;;(:file "buffer-stream")
		 (:file "command-line")
		 (:file "menu")
		 (:file "geometry")
		 ;;(:file "picking")
		 (:file "tool")
		 (:file "minara")
                 ))


;;(load "view-tools.lisp")
;;; (load "colour-tools.lisp")

;;-
;;; (load "pen-tools.lisp")
;;;(load "shape-tools.lisp")