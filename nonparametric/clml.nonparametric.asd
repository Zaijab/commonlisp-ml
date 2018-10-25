#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.nonparametric-environment (:use :common-lisp :asdf))
(in-package :clml.nonparametric-environment)

(defun call-with--environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))


(asdf:defsystem :clml.nonparametric
  :description "CLML Nonparametric Library"
  :pathname "src/"
  :serial t
  :around-compile call-with--environment
  :depends-on (
               :clml.hjs
               )
  :components ((:file "package")
               (:file "statistics")
               (:file "gamma")
               (:file "dpm")
               (:file "multi-dpm")
               (:file "hdp-lda")
               (:file "hdp")
               (:file "hdp-hmm-class")
               (:file "hdp-hmm")
               (:file "gauss-hmm")
               (:file "sticky-hdp-hmm")
               (:file "blocked-hdp-hmm")
               (:file "ihmm")
               (:file "ftm")
               (:file "dpm-interface")
               ))
