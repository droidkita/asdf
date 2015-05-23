(uiop/package:define-package :uiop/version
  (:recycle :uiop/version :uiop/utility :asdf)
  (:use :uiop/common-lisp :uiop/package :uiop/utility)
  (:export
   #:*uiop-version*
   #:parse-version #:unparse-version #:version< #:version<= ;; version support, initially in uiop/utility
   #:next-version
   #:version-obsolete-status #:with-obsolete-status)) ;; obsolescence control
(in-package :uiop/version)

(with-upgradability ()
  (defparameter *uiop-version* "3.1.7.27")

  (defun unparse-version (version-list)
    "From a parsed version (a list of natural numbers), compute the version string"
    (format nil "~{~D~^.~}" version-list))

  (defun parse-version (version-string &optional on-error)
    "Parse a VERSION-STRING as a series of natural numbers separated by dots.
Return a (non-null) list of integers if the string is valid;
otherwise return NIL.

When invalid, ON-ERROR is called as per CALL-FUNCTION before to return NIL,
with format arguments explaining why the version is invalid.
ON-ERROR is also called if the version is not canonical
in that it doesn't print back to itself, but the list is returned anyway."
    (block nil
      (unless (stringp version-string)
        (call-function on-error "~S: ~S is not a string" 'parse-version version-string)
        (return))
      (unless (loop :for prev = nil :then c :for c :across version-string
                    :always (or (digit-char-p c)
                                (and (eql c #\.) prev (not (eql prev #\.))))
                    :finally (return (and c (digit-char-p c))))
        (call-function on-error "~S: ~S doesn't follow asdf version numbering convention"
                       'parse-version version-string)
        (return))
      (let* ((version-list
               (mapcar #'parse-integer (split-string version-string :separator ".")))
             (normalized-version (unparse-version version-list)))
        (unless (equal version-string normalized-version)
          (call-function on-error "~S: ~S contains leading zeros" 'parse-version version-string))
        version-list)))

  (defun next-version (version)
    "When VERSION is not nil, it is a string, then parse it as a version, compute the next version
and return it as a string."
    (when version
      (let ((version-list (parse-version version)))
        (incf (car (last version-list)))
        (unparse-version version-list))))

  (defun version< (version1 version2)
    "Given two version strings, return T if the second is strictly newer"
    (let ((v1 (parse-version version1 nil))
          (v2 (parse-version version2 nil)))
      (lexicographic< '< v1 v2)))

  (defun version<= (version1 version2)
    "Given two version strings, return T if the second is newer or the same"
    (not (version< version2 version1))))


(with-upgradability ()
  (define-condition obsolete-function-condition (condition)
    ((name :initarg :name :reader function-name)))
  (define-condition obsolete-function-style-warning (obsolete-function-condition style-warning) ())
  (define-condition obsolete-function-warning (obsolete-function-condition warning) ())
  (define-condition obsolete-function-error (obsolete-function-condition error) ())

  (defun obsolete-function-condition-kind (type)
    (ecase type
      ((obsolete-function-style-warning) :style-warning)
      ((obsolete-function-warning) :warning)
      ((obsolete-function-error) :error)))

  (defmethod print-object ((c obsolete-function-condition) stream)
    (let ((name (function-name c)))
      (cond
        (*print-readably*
         (let ((fmt "#.(make-condition '~S :name ~S)")
               (args (list (type-of c) name)))
           (if *read-eval*
               (apply 'format stream fmt args)
               (error "Can't print ~?" fmt args))))
        (*print-escape*
         (print-unreadable-object (c stream :type t) (format stream ":name ~S" name)))
        (t
         (let ((*package* (find-package :cl)))
           (format stream "~A: Using obsolete function ~S -- please update your code to use a newer API"
                   (obsolete-function-condition-kind (type-of c)) name))))))

  (defun notify-obsolete-function (status name)
    (ecase status
      ((nil) nil)
      ((:style-warning) (style-warn 'obsolete-function-style-warning :name name))
      ((:warning) (warn 'obsolete-function-warning :name name))
      ((:error) (cerror "USE FUNCTION ANYWAY" 'obsolete-function-error :name name))))

  (defun version-obsolete-status (version &key (style-warning nil)
                                            (warning (next-version style-warning))
                                            (error (next-version warning))
                                            (delete (next-version error)))
    (cond
      ((version<= delete version) :delete)
      ((version<= error version) :error)
      ((version<= warning version) :warning)
      ((version<= style-warning version) :style-warning)))

  (defmacro with-obsolete-status ((status) &body definitions)
    (let ((status (eval status)))
      (check-type status (member nil :style-warning :warning :error :delete))
      (when (eq status :delete)
        (error "Function~P ~{~S~^ ~} should have been deleted"
               (length definitions) (mapcar 'second definitions)))
      (labels ((instrument (name head body whole)
                 (if status
                     (let ((notifiedp
                            (intern (format nil "*~A-~A-~A-~A*"
                                            :obsolete-function status name :notified-p))))
                       (multiple-value-bind (remaining-forms declarations doc-string)
                           (parse-body body :documentation t :whole whole)
                         `(progn
                            (defparameter ,notifiedp nil)
                            ;; tell some implementations to use the compiler-macro
                            (declaim (inline ,name))
                            (define-compiler-macro ,name (&whole form &rest args)
                              (declare (ignore args))
                              (notify-obsolete-function ,status ',name)
                              form)
                            (,@head ,@(when doc-string (list doc-string)) ,@declarations
                                    (unless ,notifiedp
                                      (setf ,notifiedp t)
                                      (notify-obsolete-function ,status ',name))
                                    ,@remaining-forms))))
                     `(progn
                        (eval-when (:compile-toplevel :load-toplevel :execute)
                          (setf (compiler-macro-function ',name) nil))
                        (declaim (notinline ,name))
                        (,@head ,@body)))))
        `(progn
           ,@(loop :for form :in definitions :collect
               (ecase (first form)
                 ((progn)
                  form) ;; escape with-obsolete-status processing
                 ((defun)
                  (instrument (second form) (subseq form 0 3) (subseq form 3) form))
                 ((defmethod)
                  (let ((body-start (if (listp (third form)) 3 4)))
                    (instrument (second form)
                                (subseq form 0 body-start)
                                (subseq form body-start)
                                form)))
                 ((defgeneric)
                  `(progn
                     ,form
                     ,(let ((name (second form))
                            (args (third form)))
                           (instrument name `(defmethod ,name :before ,args) '() form)))))))))))
