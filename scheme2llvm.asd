(asdf:defsystem #:scheme2llvm
  :depends-on (#:scheme2common-lisp)
  :serial t
  :components
  ((:file "scheme2llvm.lisp")))
