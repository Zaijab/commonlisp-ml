;;;

(defpackage :f2cl-lib
  (:use :cl)
  (:documentation "The package holding all symbols used by the Fortran to Lisp library")
  (:nicknames :fortran-to-lisp-library)
  (:export
   ;; Constants
   #:%false% #:%true%
   ;; User-settable runtime options
   #:*check-array-bounds*
   ;; Types
   #:integer4 #:integer2 #:integer1 #:real8 #:real4 #:complex8 #:complex16
   #:array-double-float #:array-single-float #:array-integer4 #:array-strings
   #:logical
   ;; Macros
   #:fref #:fset #:with-array-data
   #:with-multi-array-data
   #:f2cl-init-string #:fref-string #:fset-string #:f2cl-set-string
   #:f2cl-// #:fstring-/= #:fstring-= #:fstring-> #:fstring->= #:fstring-< #:fstring-<=
   #:fortran_comment #:fdo #:f2cl/ #:arithmetic-if #:computed-goto
   #:assigned-goto
   #:fformat
   #:data-implied-do
   #:int-add #:int-sub #:int-mul
   ;; Utilities
   #:array-slice #:array-initialize
   ;; Intrinsic functions
   #:abs #:acos #:aimag #:dimag #:aint #:alog #:alog10 #:amax0 #:amax1
   #:amin1 #:amod #:anint #:asin #:atan #:atan2
   #:cabs #:cexp #:fchar #:clog #:cmplx #:dcmplx #:conjg #:ccos
   #:csin #:csqrt #:zsqrt #:dabs #:dacos #:dasin
   #:datan #:datan2 #:dble #:dcos #:dcosh #:dexp #:dim
   #:dint #:dlog #:dlog10 #:dmax1 #:dmin1 #:dmod
   #:dnint #:dprod #:dsign #:dsin #:dsinh #:dsqrt #:dtan
   #:dtanh #:ffloat #:iabs #:ichar #:idim #:idint
   #:idnint #:ifix #:index #:int #:isign #:le #:len
   #:lge #:lgt #:flog #:log10 #:lt #:max #:max0
   #:max1 #:min0 #:min1 #:nint #:freal
   #:sign #:sngl #:fsqrt
   #:cdabs #:dconjg
   ;; other functions
   #:d1mach #:r1mach #:i1mach

   #:F2CL-COPY-SEQ
   #:MAKE-COMPATIBLE-SEQ
   #:*STOP-SIGNALS-ERROR-P*
   #:DFLOAT
   #:LEN_TRIM
   ))


