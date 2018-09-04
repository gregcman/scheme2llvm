(in-package #:scheme2llvm)

(defun read-ir-into-module (expression module)
  (declare (optimize (debug 3)))
  (let ((code 
	 (with-output-to-string (stream)
	   (let ((*standard-output* stream))
	     (compiler (list expression))))))
    (let ((memory-buffer
	   (cffi:with-foreign-string (foreign-string code)
	     (cffi:with-foreign-string (name "a Memory Buffer")
	       (llvm::-create-memory-buffer-with-memory-range-copy
		foreign-string
		(length code)
		name)))))
      (cffi:with-foreign-object (foo :pointer 1)	
	(llvm::-parse-i-r-in-context (llvm::-get-global-context)
				     memory-buffer
				     module
				     foo)
	(k-shared::with-llvm-message (ptr) (cffi:mem-ref foo :pointer)
	  (princ (cffi:foreign-string-to-lisp ptr))))     
      (llvm::-dispose-memory-buffer memory-buffer))))


(defclass module ()
  ((module :accessor module.module
	   :initarg :module)
   (function-pass-manager :accessor module.function-pass-manager
			  :initarg :function-pass-manager)))
(defun make-module (&optional name)
  (multiple-value-bind (module function-pass-manager)
      (%initialize-module-and-pass-manager name)
    (make-instance 'module
		   :module
		   module
		   :function-pass-manager
		   function-pass-manager
		   )))

(defun dispose-module (module)
  (llvm::-dispose-pass-manager (module.function-pass-manager module))
  (llvm::-dispose-module (module.module module))
  (values))

(defmacro with-module ((&optional (name-form "a module")) name &body body)
  `(let ((,name (make-module ,name-form)))
     (unwind-protect (progn ,@body)
       (dispose-module ,name))))

(defun %initialize-module-and-pass-manager (&optional (string "module name"))
  "return (values [NEW MODULE] [NEW FUNCTION PASS MANAGER])"
  (let ((module (cffi:with-foreign-string (str string)
		  (llvm::-module-create-with-name str))))
    (values
     module
     (let ((fpm (llvm::-create-function-pass-manager-for-module module)))
       (llvm::-add-promote-memory-to-register-pass fpm)
       (llvm::-add-instruction-combining-pass fpm)
       (llvm::-add-reassociate-pass fpm)
       (llvm::-add-g-v-n-pass fpm)
       (llvm::-add-c-f-g-simplification-pass fpm)
       (llvm::-initialize-function-pass-manager fpm)
       fpm))))


(defun bar (expression)
  (k-shared::with-kaleidoscope-jit
    (let ((module-object (make-module "wow")))
      (unwind-protect
	   (with-slots (module) module-object
	     (read-ir-into-module expression module)
	     (k-shared::dump-module module))
	(dispose-module module-object)))))


;;;;lf == (codegen ast)
(defun %bar (module &optional (name "main")) ;;;Whats lf?
  "Evalutate main"
  (declare (optimize (speed 0) (debug 3)))
  (let ((module-abnormal? (verify-llvm-module module)))
    (when module-abnormal?
      (let ((handle (k-shared::kaleidoscope-add-module module)))
	(unwind-protect
	     (let ((expr-symbol
		    (cffi:with-foreign-string (str name)
		      (k-shared::kaleidoscope-find-symbol str))))
	       (when (cffi:null-pointer-p expr-symbol)
		 (error 'k-shared::kaleidoscope-error :message "function not found"))
	       (let ((ptr (k-shared::kaleidoscope-get-symbol-address expr-symbol)))
		 (if (= 0 ptr)
		     (error 'k-shared::kaleidoscope-error :message "function no body???")
		     (let ((result
			    (cffi:foreign-funcall-pointer
			     (cffi:make-pointer ptr) 
			     ()
			     (:pointer :int32)
			     (cffi:null-pointer)
			     (:pointer (:pointer :int8))
			     (cffi:null-pointer)
			     :int64)))
		       (format t "~%Evaluated to ~fD0~%"
			       result)))))
	  (k-shared::kaleidoscope-remove-module handle))))))
