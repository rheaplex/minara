(require 'asdf)

;; This will need modifying if launched from a script in the parent directory
;; It should handle both eventualities, checking for last part of truename . .
(mapc #'(lambda (asd)
	  (pushnew (make-pathname :directory 
				  (pathname-directory asd))
		   asdf:*central-registry*
		   :test #'equal))
      (directory (merge-pathnames #p"*/*.asd" (truename "../lib/"))))

(asdf:operate 'asdf:load-op 'flexichain)

;;(asdf:operate 'asdf:load-op 'cffi)

(asdf:operate 'asdf:load-op 'cl-opengl)
(asdf:operate 'asdf:load-op 'cl-glu)
(asdf:operate 'asdf:load-op 'cl-glut)

;; check for libcairo.so, warn user to install cairo-dev if missing
(asdf:operate 'asdf:load-op 'cl-cairo2)

(asdf:operate 'asdf:load-op 'minara)

(minara::minara)

;; Make sure you have a Lisp installed
;; sudo apt-get install sbcl

;; To install cl-cairo
;; sudo apt-get install libcairo2-dev
;; (asdf-install::install 'cl-utilities)
;; (asdf-install::install 'cl-colors)
;; (asdf-install::install 'cl-cairo2)

;; Most of the libraries Minara uses are included,
;; This will change as they become asdf-installable
;; (or cl-cairo will be included if neccessary).