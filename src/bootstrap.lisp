(progn
  (require 'asdf)
  
  (setf asdf:*central-registry*
	'(*default-pathname-defaults*
	  #p"/home/robmyers/Documents/hacking/cl-minara/"))
  
  (asdf:operate 'asdf:load-op 'flexichain)
  
  (asdf:operate 'asdf:load-op 'cffi)
  
  (asdf:operate 'asdf:load-op 'cl-opengl)
  (asdf:operate 'asdf:load-op 'cl-glu)
  (asdf:operate 'asdf:load-op 'cl-glut)
  
  ;;(asdf:operate 'asdf:load-op 'minara)

  (load "packages.lisp")
  
  (load "test.lisp")
  (load "glut-gui.lisp")
  (load "transformations.lisp")
  (load "glut-rendering.lisp")
  (load "events.lisp")
  (load "keymap.lisp")
  (load "buffer.lisp")
  (load "evaluation.lisp")
  (load "window.lisp")
  (load "minibuffer.lisp")
  (load "undo.lisp")
  (load "command-line.lisp")
  (load "menu.lisp")
  (load "geometry.lisp")
  ;;  (load "picking.lisp") ;;-
  (load "tool.lisp")
  (load "minara.lisp"))



;;(load "view-tools.lisp")
;;; (load "colour-tools.lisp")

;;-
;;; (load "pen-tools.lisp")
;;;(load "shape-tools.lisp")

