;;; Compiled by f2cl version:
;;; ("$Id: f2cl1.l,v 1.209 2008/09/11 14:59:55 rtoy Exp $"
;;;  "$Id: f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Rel $"
;;;  "$Id: f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Rel $"
;;;  "$Id: f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Rel $"
;;;  "$Id: f2cl5.l,v 1.197 2008/09/11 15:03:25 rtoy Exp $"
;;;  "$Id: f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "$Id: macros.l,v 1.106 2008/09/15 15:27:36 rtoy Exp $")

;;; Using Lisp International Allegro CL Enterprise Edition 8.1 [64-bit Linux (x86-64)] (Oct 7, 2008 17:13)
;;;
;;; Options: ((:prune-labels nil) (:auto-save t)
;;;           (:relaxed-array-decls t) (:coerce-assigns :as-needed)
;;;           (:array-type ':array) (:array-slicing t)
;;;           (:declare-common nil) (:float-format double-float))

(in-package :clml.lapack)


(defun dlamrg (n1 n2 a dtrd1 dtrd2 indx)
  (declare (type (array double-float (*)) a)
   (type (array f2cl-lib:integer4 (*)) indx)
   (type (f2cl-lib:integer4) dtrd2 dtrd1 n2 n1))
  (f2cl-lib:with-multi-array-data ((indx f2cl-lib:integer4 indx-%data%
                                    indx-%offset%)
                                   (a double-float a-%data%
                                    a-%offset%))
    (prog ((i 0) (ind1 0) (ind2 0) (n1sv 0) (n2sv 0))
          (declare (type (f2cl-lib:integer4) n2sv n1sv ind2 ind1 i))
          (setf n1sv n1)
          (setf n2sv n2)
          (cond ((> dtrd1 0) (setf ind1 1)) (t (setf ind1 n1)))
          (cond ((> dtrd2 0) (setf ind2 (f2cl-lib:int-add 1 n1)))
                (t (setf ind2 (f2cl-lib:int-add n1 n2))))
          (setf i 1)
     label10 (cond ((and (> n1sv 0) (> n2sv 0))
                    (cond ((<= (f2cl-lib:fref a (ind1) ((1 *)))
                               (f2cl-lib:fref a (ind2) ((1 *))))
                           (setf (f2cl-lib:fref indx-%data% (i) ((1 *))
                                                indx-%offset%)
                                 ind1)
                           (setf i (f2cl-lib:int-add i 1))
                           (setf ind1 (f2cl-lib:int-add ind1 dtrd1))
                           (setf n1sv (f2cl-lib:int-sub n1sv 1)))
                          (t
                           (setf (f2cl-lib:fref indx-%data% (i) ((1 *))
                                                indx-%offset%)
                                 ind2)
                           (setf i (f2cl-lib:int-add i 1))
                           (setf ind2 (f2cl-lib:int-add ind2 dtrd2))
                           (setf n2sv (f2cl-lib:int-sub n2sv 1))))
                    (go label10)))
          (cond ((= n1sv 0)
                 (f2cl-lib:fdo (n1sv 1 (f2cl-lib:int-add n1sv 1))
                               ((> n1sv n2sv) nil)
                               (tagbody
                                   (setf (f2cl-lib:fref indx-%data% (i)
                                                        ((1 *))
                                                        indx-%offset%)
                                         ind2)
                                   (setf i (f2cl-lib:int-add i 1))
                                   (setf ind2
                                         (f2cl-lib:int-add ind2 dtrd2))
                                 label20)))
                (t
                 (f2cl-lib:fdo (n2sv 1 (f2cl-lib:int-add n2sv 1))
                               ((> n2sv n1sv) nil)
                               (tagbody
                                   (setf (f2cl-lib:fref indx-%data% (i)
                                                        ((1 *))
                                                        indx-%offset%)
                                         ind1)
                                   (setf i (f2cl-lib:int-add i 1))
                                   (setf ind1
                                         (f2cl-lib:int-add ind1 dtrd1))
                                 label30))))
          (go end_label)
     end_label (return (values nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamrg
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        fortran-to-lisp::integer4
                                                        (*)))
          :return-values '(nil nil nil nil nil nil)
          :calls 'nil)))

