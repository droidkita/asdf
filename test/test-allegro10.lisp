(defpackage :asdf-test
  (:use :common-lisp)
  (:export
   #:asym #:acall #:asymval
   #:*test-directory* #:*asdf-directory* #:*build-directory* #:*implementation*
   #:deftest #:is #:signals #:errors #:with-expected-failure
   #:assert-compare #:assert-equal #:assert-pathname-equal #:assert-pathnames-equal
   #:hash-table->alist
   #:load-asdf #:maybe-compile-asdf
   #:load-asdf-lisp #:compile-asdf #:load-asdf-fasl
   #:compile-load-asdf #:compile-load-asdf-upgrade
   #:load-asdf-system #:clean-load-asdf-system
   #:register-directory #:load-test-system
   #:with-test #:test-asdf #:debug-asdf
   #:run-test-script #:interactive-test #:test-load-systems
   #:verbose #:exit-lisp
   #:assert-compare
   #:assert-equal
   #:leave-test #:def-test-system
   #:action-name #:in-plan-p
   #:test-source #:test-fasl #:resolve-output #:output-location
   #:quietly #:join-namestrings))

(in-package :asdf-test)

;;; UPDATE THIS AS NEEDED...
(defparameter *asdf-directory*
  (truename "home:lisp;asdf;"))

(defvar *trace-symbols*
  `(;; If you want to trace some stuff while debugging ASDF,
    ;; here's a nice place to say what.
    ;; These string designators will be interned in ASDF after it is loaded.

    ;;#+ecl ,@'( :perform :input-files :output-files :compile-file* :compile-file-pathname* :load*)
    :initialize-source-registry
    :compute-source-registry
    :flatten-source-registry
    ))

(defun make-sub-pathname (&rest keys &key defaults &allow-other-keys)
  (merge-pathnames (apply 'make-pathname keys) defaults))
(defparameter *uiop-directory*
  (truename (make-sub-pathname :directory '(:relative "uiop") :defaults *asdf-directory*)))
(defparameter *build-directory*
  (make-sub-pathname :directory '(:relative "build") :defaults *asdf-directory*))
(defparameter *implementation*
  (or #+allegro
      (ecase excl:*current-case-mode*
        (:case-sensitive-lower :mlisp)
        (:case-insensitive-upper :alisp))
      #+armedbear :abcl
      #+(or clasp ecl) (or #+ecl-bytecmp :ecl_bytecodes :ecl)
      #+clisp :clisp
      #+clozure :ccl
      #+cmu :cmucl
      #+corman :cormanlisp
      #+digitool :mcl
      #+gcl :gcl
      #+lispworks :lispworks
      #+mkcl :mkcl
      #+sbcl :sbcl
      #+scl :scl
      #+xcl :xcl))
(defparameter *early-fasl-directory*
  (make-sub-pathname :directory `(:relative "fasls" ,(string-downcase *implementation*))
                     :defaults *build-directory*))

(defun asdf-name (&optional tag)
  (format nil "asdf~@[-~A~]" tag))
(defun asdf-lisp (&optional tag)
  (make-pathname :name (asdf-name tag) :type "lisp" :defaults *build-directory*))
(defun debug-lisp ()
  (make-sub-pathname :directory '(:relative "contrib") :name "debug" :type "lisp" :defaults *uiop-directory*))
(defun early-compile-file-pathname (file)
  (compile-file-pathname
   (make-pathname :name (pathname-name file) :type "lisp" :defaults *early-fasl-directory*)))
(defun asdf-fasl (&optional tag)
  (early-compile-file-pathname (asdf-lisp tag)))

;;; Survival utilities
(defun asym (name &optional package errorp)
  (let* ((pname (or package :asdf))
         (package (find-package pname)))
    (if package
        (or (find-symbol (string name) package)
            (when errorp (error "Can't find symbol ~A in ~A" name pname)))
        (when errorp (error "Can't find package ~A" pname)))))
(defun acall (name &rest args)
  (apply (apply 'asym (if (consp name) name (list name))) args))
(defun asymval (name &optional package)
  (symbol-value (asym name package)))
(defsetf asymval (name &optional package) (new-value)
  (let ((sym (gensym "SYM")))
    `(let ((,sym (asym ,name ,package)))
       (if ,sym
           (setf (symbol-value ,sym) ,new-value)
           (error "NIHIL EX NIHILO")))))


(defun configure-asdf ()
  (format t "Configuring ASDF~%")
  (setf *debug-asdf* t)
  ;; (when (asym :getenvp)
  ;;   (format t "Enabling debugging~%")
  ;;   (setf *debug-asdf* (or *debug-asdf* (acall :getenvp "DEBUG_ASDF_TEST"))))
  (when *trace-symbols*
    (format t "Tracing~{ ~A~}~%" *trace-symbols*)
    (eval `(trace ,@(loop :for s :in *trace-symbols* :collect (asym s)))))
  ;; this seems to be enough to trigger the bug...
  (if (asym :initialize-source-registry)
      (acall :initialize-source-registry
           `(:source-registry :ignore-inherited-configuration))
    (error "Can't find ASDF:INITIALIZE-SOURCE-REGISTRY"))
  ;; (when (asym :initialize-output-translations)
  ;;   (acall :initialize-output-translations
  ;;          `(:output-translations
  ;;            (,(acall :wilden *asdf-directory*) ,(acall :wilden (resolve-output "asdf/")))
  ;;            (t ,(acall :wilden (resolve-output "root")))
  ;;            :ignore-inherited-configuration)))
  ;; (when (asym :*central-registry*)
  ;;   (set (asym :*central-registry*) `(,*test-directory*)))
  ;; (format t "Being a bit verbose~%")
  ;; (when (asym :*asdf-verbose*) (setf (asymval :*asdf-verbose*) t))
  ;; (when (asym :*verbose-out*) (setf (asymval :*verbose-out*) *standard-output*))
  ;; (when (and (asym :locate-system) (asym :pathname-directory-pathname) (asym :pathname-equal))
  ;;   (format t "Comparing directories~%")
  ;;   (acall :call-with-asdf-cache
  ;;            #'(lambda ()
  ;;                (let ((x (acall :pathname-directory-pathname (nth-value 2 (acall :locate-system :test-asdf)))))
  ;;                  (assert-pathname-equal-helper ;; not always EQUAL (!)
  ;;                   '*test-directory* *test-directory*
  ;;                   '(:pathname-directory-pathname (nth-value 2 (:locate-system :test-asdf))) x)
  ;;                  (unless (equal *test-directory* x)
  ;;                    (format t "Interestingly, while *test-directory* has components~% ~S~%~
  ;;                ASDF finds the ASDs in~% ~S~%Using the latter.~%"
  ;;                            (pathname-components *test-directory*)
  ;;                            (pathname-components x)))
  ;;                  (setf *test-directory* x)))))
  t)

(defun compile-asdf (&optional tag verbose upgradep)
  (let* ((alisp (asdf-lisp tag))
         (afasl (asdf-fasl tag))
         (tmp (make-pathname :name "asdf-tmp" :defaults afasl)))
    (ensure-directories-exist afasl)
    (multiple-value-bind (result warnings-p failure-p)
        (compile-file alisp :output-file tmp :verbose verbose :print verbose)
      (flet ((bad (key)
               (when result (ignore-errors (delete-file result)))
               key)
             (good (key)
               (when (probe-file afasl) (delete-file afasl))
               (rename-file tmp afasl)
               key))
        (cond
          ((null result)
           (bad :no-output))
          (failure-p
           (or
            #+clisp (good :expected-full-warnings)
            (bad :unexpected-full-warnings)))
          (warnings-p
           (or
            ;; CLISP has full warnings for method redefinition in eval-when.
            ;; CMUCL: ?
            ;; ECL 11.1.1 has spurious warnings, same with XCL 0.0.0.291.
            ;; SCL has no warning but still raises the warningp flag since 2.20.15 (?)
            #+(or clasp clisp cmu ecl scl xcl) (good :expected-style-warnings)
            (and upgradep (good :unexpected-style-warnings))
            (bad :unexpected-style-warnings)))
          (t (good :success)))))))

(defun compile-asdf (&optional tag verbose upgradep)
  (let* ((alisp (asdf-lisp tag))
         (afasl (asdf-fasl tag))
         (tmp (make-pathname :name "asdf-tmp" :defaults afasl)))
    (ensure-directories-exist afasl)
    (multiple-value-bind (result warnings-p failure-p)
        (compile-file alisp :output-file tmp :verbose verbose :print verbose)
      (flet ((bad (key)
               (when result (ignore-errors (delete-file result)))
               key)
             (good (key)
               (when (probe-file afasl) (delete-file afasl))
               (rename-file tmp afasl)
               key))
        (cond
          ((null result)
           (bad :no-output))
          (failure-p
           (or
            #+clisp (good :expected-full-warnings)
            (bad :unexpected-full-warnings)))
          (warnings-p
           (or
            ;; CLISP has full warnings for method redefinition in eval-when.
            ;; CMUCL: ?
            ;; ECL 11.1.1 has spurious warnings, same with XCL 0.0.0.291.
            ;; SCL has no warning but still raises the warningp flag since 2.20.15 (?)
            #+(or clasp clisp cmu ecl scl xcl) (good :expected-style-warnings)
            (and upgradep (good :unexpected-style-warnings))
            (bad :unexpected-style-warnings)))
          (t (good :success)))))))



(defun load-asdf-fasl (&optional tag)
  (load (asdf-fasl tag)))


(defun load-asdf (&optional tag)
  (load-asdf-fasl tag)
  (configure-asdf))

(compile-asdf)
(load-asdf)
