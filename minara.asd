(defpackage :minara.system
            (:use :cl :asdf))

;;(in-package :climacs.system)

(asdf:defsystem :minara
                :name "minara"
                :version "0.1"
                :depends-on (:flexichain :cl-opengl :cl-glu :cl-glut)
                :serial t
                :components
                ((:file "package")
                 (:file "test")
                 (:file "transformations")
                 (:file "rendering")
                 (:file "events")
                 (:file "keymap")
                 (:file "buffer")
                 (:file "undo")
                 (:file "buffer-stream")
                 (:file "window")
		 (:file "command-line")
		 (:file "menu")
		 (:file "geometry")
		 (:file "picking")
		 (:file "tool")
                 ))

;; Insert checks for libraries here