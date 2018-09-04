(asdf:defsystem #:scheme2llvm
  :depends-on (#:scheme2common-lisp
	       #:kaleidoscope256)
  :serial t
  :components
  ((:file "scheme2llvm")
   (:file "other")))
