;; k-nearest-neighbors algorithm

(in-package :clml.nearest-search.k-nn-new)

(defclass k-nn-estimator ()
  ((vecs :initarg :vecs :accessor vecs)
   (vec-labels :initarg :vec-labels :accessor vec-labels)
   (vec-profiles :initarg :vec-profiles :accessor vec-profiles)
   (vec-weight :initarg :vec-weight :accessor vec-weight)
   (mins :initarg :mins :accessor mins)
   (maxs :initarg :maxs :accessor maxs)
   (target :initarg :target :accessor target)
   (teachears :initarg :teachers :accessor teachers)
   (esttype :initarg :esttype :accessor esttype)
   (k :initarg :k :accessor k)
   (distance :initarg :distance :accessor distance)
   (nearest-search :initarg :nn-search :accessor k-nn-search)
   (k-points :initarg :k-points :accessor k-points)
   (k-dis    :initarg :k-dis :accessor k-dis))
  (:documentation "- accessor:
  - vec :           data for learning
  - vec-labels :    explanatories
  - vec-profiles :  type infomation for each explanatories
  - vec-weight :    weight for each explanatories
  - mins :          minimum value for each explanatories
  - maxs :          maximum value for each explanatories
  - target :        objective variable
  - teachers :      values of target
  - k :             value of parameter k
  - distance :      ") )

(defstruct k-nn-point data signal weight)

(defgeneric estimator-properties (est &key)
  (:documentation "- return: <list>, property list
- arguments:
  - estimator : <k-nn-estimator>
  - verbose: nil | t, default is nil
- comment:
  Get k-nn-estimator's properties. If /verbose/ is t, all /accessor/ of k-nn-estimator would be extracted.
"))
(defmethod estimator-properties ((est k-nn-estimator) &key verbose)
  (with-accessors ((vecs vecs)
                   (explanatories vec-labels)
                   (profs vec-profiles)
                   (weight vec-weight)
                   (mins mins)
                   (maxs maxs)
                   (target target)
                   (teachers teachers)
                   (esttype esttype)
                   (k k)
                   (dist distance)) est
    (if verbose
        `(:k ,k :target ,target :explanatories ,explanatories :distance ,dist
             :mins ,mins :maxs ,maxs :vec-weight ,weight
             :vecs ,vecs :vec-profiles ,profs :teachers ,teachers :esttype ,esttype)
      `(:k ,k :target ,target :explanatories ,explanatories :distance ,dist
           :mins ,mins :maxs ,maxs :vec-weight ,weight))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(inline make-dir update-tmp update-tmp-w
                     decide average choice-type
                     calc-classify-category-distance)))
(defun make-dir (name)
  #+allegro (excl:make-directory name)
  #-allegro (ensure-directories-exist
                                (if (and  (>  (length name) 0) (char= #\/  (elt (reverse name) 0))) name (concatenate 'string name #(#\Space))))
  )

(defun update-tmp-w (tmp-d tmp-l label dis tmp-w weight)
  (let ((l label)
        (d dis)
        (w weight)
        (flag nil))                     ; pushing out flag
    (do-vecs ((_ tmp-d :setf-var td)
              (__ tmp-l :setf-var tl)
              (___ tmp-w :setf-var tw))
      #-sbcl (declare (ignorable _ __ ___))
      (when (or flag (< dis td))
        (setf flag t)
        (rotatef d td)
        (rotatef l tl)
        (rotatef w tw))
      (unless l ;; if no next-label, update stop.
        (return)))))
(defun update-tmp (tmp-d tmp-l label dis)
  (declare (optimize speed (safety 0) (debug 0))
           (type dvec tmp-d)
           (type (simple-array t (*)) tmp-l))
  (let ((dis (if (typep dis 'double-float)
                 dis
               (float dis 0d0))))
    (declare (type double-float dis))
    (let ((l label)
          (d dis)
          (flag nil))                   ; pushing out flag
      (declare (type double-float d))
      (do-vecs ((_ tmp-d :setf-var td :type double-float)
                (__ tmp-l :setf-var tl :type t))
        #-sbcl (declare (ignorable _ __))
        (when (or flag (< dis td))
          (setf flag t)
          (rotatef d td)
          (rotatef l tl))
        (unless l                       ; if no next-label, update stop.
          (return))))))

(defun decide (k-data)
  (let ((alist nil))
    (loop for p across k-data
        for l = (k-nn-point-signal p)
        for w = (or (k-nn-point-weight p) 1) do
      (let ((found (assoc l alist :test #'string=)))
        (if found
            (incf (cdr found) w)
          (push (cons l w) alist))))
    (caar (stable-sort alist #'> :key #'cdr))))

(defun average (k-data k)
  (let ((ans 0d0))
    (loop for p across k-data
        for l = (k-nn-point-signal p)
        for w = (or (k-nn-point-weight p) 1d0) do
          (incf ans (* w (/ l k))))
    ans))
(defun choice-type (teachers)
  (etypecase (svref teachers 0)
    ((or string symbol) :classify)
    (number :linear)))

(defun calc-classify-category-distance (x y profiles finalizer)
  (if (string= x y)
      0
    (let ((cx (gethash x (cdr profiles))))
      (if cx
          (let ((cy (gethash y (cdr profiles)))
                (ans 0))
            (maphash #'(lambda (k v)
                         (declare (ignore k))
                         (incf ans
                               (funcall finalizer
                                        (- (/ (gethash x v 0) cx)
                                           (/ (gethash y v 0) cy))))) (car profiles))
            ans)
        1))))

;; data-driven distance function
(defun knn-manhattan (x-vec y-vec profiles)
  (declare (optimize speed (safety 0) (debug 0))
           (type (simple-array t (*)) x-vec y-vec profiles))
  (let ((ans 0))
    (do-vecs ((x x-vec)
              (y y-vec)
              (p profiles))
      (incf ans (case p
                  (:numeric (abs (- x y)))
                  (:delta (if (string= x y)
                              0
                            1))
                  (t (calc-classify-category-distance x y p #'abs)))))
    ans))

(defun knn-double-manhattan (x-vec y-vec profiles)
  (declare (optimize speed (safety 0) (debug 0))
           (type dvec x-vec y-vec)
           (ignore profiles))
  (let ((ans 0.0d0))
    (declare (type double-float ans))
    (do-vecs ((x x-vec :type double-float)
              (y y-vec :type double-float))
      (incf ans (abs (- (the double-float x)
                        (the double-float y)))))
    ans))

(defun knn-euclid (x-vec y-vec profiles)
  (declare (optimize speed (safety 0) (debug 0))
           (type (simple-array t (*)) x-vec y-vec profiles))
  (let ((ans 0))
    (do-vecs ((x x-vec)
              (y y-vec)
              (p profiles))
      (incf ans (case p
                  (:numeric (expt (- x y) 2))
                  (:delta (if (string= x y)
                              0
                            1))
                  (t (calc-classify-category-distance x y p #'(lambda (z) (expt z 2)))))))
    ans))

(defun knn-double-euclid (x-vec y-vec profiles)
  (declare (optimize speed (safety 0) (debug 0))
           (type dvec x-vec y-vec)
           (ignore profiles))
  (let ((ans 0.0d0))
    (declare (type double-float ans))
    (do-vecs ((x x-vec :type double-float)
              (y y-vec :type double-float))
      (incf ans (expt (- (the double-float x)
                         (the double-float y)) 2)))
    ans))

#+ignore
(defun knn-double-euclid (x-vec y-vec profiles)
  (declare (optimize speed (safety 0) (debug 0))
           (type (simple-array t (*)) x-vec y-vec profiles)
           (ignore profiles))
  (let ((ans 0.0d0))
    (declare (type double-float ans))
    (do-vecs ((x x-vec :type t)
              (y y-vec :type t))
      (incf ans (expt (- (the double-float x)
                         (the double-float y)) 2)))
    ans))

#||
;;; dump or load estimator without compiler
(defmethod make-load-form ((self k-nn-estimator) &optional environment)
  (declare (ignorable environment))
  `(make-instance ',(class-name (class-of self))
                  :vecs ',(vecs self)
                  :vec-labels ',(vec-labels self)
                  :vec-profiles ',(vec-profiles self)
                  :vec-weight ',(vec-weight self)
                  :mins ',(mins self)
                  :maxs ',(maxs self)
                  :target ',(target self)
                  :teachers ',(teachers self)
                  :esttype ',(esttype self)
                  :k ',(k self)
                  :distance ',(distance self)))
(defun dump-knn-est (est outdir)
  (make-dir outdir)
  (let ((*loading-est* est)
        (*package* (find-package :learn.k-nn))
        (outfile (format nil "~A/estimator" outdir)))
    (with-open-file (out outfile :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (format out "~&(setf *loading-est* ~s)~%" (make-load-form est))))))
(defun load-dumped-est (indir)
  (let ((*loading-est* nil)
        (*package* (find-package :learn.k-nn)))
    (load (format nil "~A/estimator" indir))
    *loading-est*))

;;; dump or load estimator with compiler
(defmethod make-load-form ((self k-nn-estimator) &optional environment)
  (make-load-form-saving-slots self
                               :slot-names '(vecs vec-labels vec-weight vec-profiles
                                                  teachears k distance mins maxs)
                               :environment environment))
(defun dump-knn-est (est outdir)
  (make-dir outdir)
  (let ((*loading-est* est)
        (*package* (find-package :learn.k-nn))
        (outfile (format nil "~A/tmp.cl" outdir)))
    (with-open-file (out outfile :direction :output :if-exists :supersede)
      (format out "(setf *loading-est*  #.*loading-est*)"))
    (compile-file outfile :output-file (format nil "~A/estimator" outdir))))
(defun load-dumped-est (indir)
  (let ((*loading-est* nil)
        (*package* (find-package :learn.k-nn)))
    (load (format nil "~A/estimator" indir))
    *loading-est*))
||#

(defclass knn-exp ()
  ((vec :initarg :vec :accessor vec)
   (ested :initform nil :accessor ested)))


;;; main function
(defun estimate (data est-type search k k-points k-dis)
  (let ((k-data (find-nearest-k search (make-k-nn-point :data (vec data)) k k-points k-dis)))
    (setf (ested data) (case est-type
                         (:classify (decide k-data))
                         (:linear (average k-data k))))))

(defun make-class-weight (initarg teachers)
  (assert (and (listp initarg) (every #'consp initarg)))
  (map 'vector #'(lambda (x) (cdr (assoc x initarg :test #'string=))) teachers))
(defun make-data-weight (initarg unsp-data)
  (etypecase initarg
    ((or string symbol) (choice-a-dimension initarg unsp-data))
    (vector initarg)
    (list (coerce initarg 'vector))))

(defun make-vector-profile (vecs teachers esttype)
  (let* ((dim (length (svref vecs 0)))
         (ans (make-array dim :element-type t :initial-element nil)))
    (dotimes (index dim)
      (etypecase (aref (svref vecs 0) index)
        (number (setf (svref ans index) :numeric))
        ((or string symbol)
         (setf (svref ans index)
           (if (eq esttype :classify)
               (classify-category-distance-helper vecs teachers index)
             :delta)))))
    ans))
(defun classify-category-distance-helper (vecs teachers index)
  (let ((ansj (make-hash-table :test #'equal))
        (ansx (make-hash-table :test #'equal)))
    (do-vecs ((ve vecs)
              (te teachers))
      (let ((found (gethash te ansj)))
        (unless found
          (setf found (setf (gethash te ansj) (make-hash-table :test #'equal))))
        (incf (gethash (aref ve index) found 0))
        (incf (gethash (aref ve index) ansx  0))))
    (cons ansj ansx)))

(defun normalize-vecs (vecs)
  (let ((mins (copy-seq (svref vecs 0))))
    (do-vecs ((_ mins :setf-var s))
      #-sbcl (declare (ignore _))
      (unless (numberp s)
        (setf s :category)))
    (let ((maxs (copy-seq mins)))
      (do-vecs ((v vecs))
        (do-vecs ((x v)
                  (_ mins :setf-var s)
                  (__ maxs :setf-var b))
          #-sbcl (declare (ignorable _ __))
          (unless (eq s :category)
            (setf s (min s x))
            (setf b (max b x)))))
      (do-vecs ((v vecs))
        (do-vecs ((_ v :setf-var x)
                  (s mins)
                  (b maxs))
          #-sbcl (declare (ignorable _))
          (unless (or (eq s :category) (= s b))
            (setf x (/ (- x s) (- b s))))))
      (values mins maxs))))

(defun move-vecs-by-minmax (points mins maxs)
  (do-vecs ((p points))
    (let ((v (vec p)))
      (do-vecs ((_ v :setf-var x)
                (s mins)
                (b maxs))
        #-sbcl (declare (ignorable _))
        (unless (or (eq s :category) (= s b))
          (setf x (/ (- x s) (- b s))))))))

(defun k-nn-analyze (learning-data k target explanatories
                     &key (distance :euclid) target-type
                          use-weight weight-init normalize (nns-type :naive) nns-args)
  "                     &key (distance :euclid) target-type
                          use-weight weight-init normalize)
- return: <k-nn-estimator>
- arguments:
  - learning-data : <unspecialized-dataset>
  - k : <integer>
  - target : <string>
  - explanatories : <list string> | :all
  - distance : :euclid | :manhattan | a function object
    - distance now can be a function object, it will be regarded as
      a user-specified distance function.
    - A distance function must accept 3 arguments: vector_1, vector_2
      and profiles. profiles is a list whose elements are
      either :numeric or :delta,
      :numeric indicates the dimension is of numeric values and :delta
      indicates the dimension is of categorical values. It's totally
      fine to ignore profiles if users know exactly what their data is
      about.
  - target-type : :numeric | :category | nil
    - :numeric means regression analysis
    - :category means classification analysis
    - when nil, the target type will be automatically determined by
      the type of data.
  - use-weight : nil | :class | :data
  - weight-init :
    - if use-weight is :class, it's an assoc-list of form ((class-name . weight) ...)
    - if use-weight is :data, then a vector of weight, a list of
      weight or a column name of input data are allowable. When a
      column name is passed in, the element in the column is treated
      as weight.
  - normalize : t | nil"
  (assert (member target-type '(nil :numeric :category)))
  (unless (eq nns-type :naive)
    (assert (member distance '(:double-euclid :double-manhattan))))
  (when (eq explanatories :all)
    (setq explanatories (remove target
                                (map 'list #'dimension-name
                                     (dataset-dimensions
                                      learning-data))
                                :test #'string-equal)))
  (let* ((teachers (choice-a-dimension target learning-data))
         (esttype (if target-type
                      (ecase target-type
                        (:numeric :linear)
                        (:category :classify))
                      (choice-type teachers)))
         (vecs (if (member distance '(:double-euclid :double-manhattan))
                   (map 'vector #'(lambda (x) (coerce x 'dvec)) (choice-dimensions explanatories learning-data))
                 (choice-dimensions explanatories learning-data)))
         (vecs-tmp (map 'vector
                     #'(lambda (vec) (make-instance 'knn-exp :vec vec))
                     vecs))
         (disf (case distance
                 (:euclid #'knn-euclid)
                 (:double-euclid #'knn-double-euclid)
                 (:manhattan #'knn-manhattan)
                 (:double-manhattan #'knn-double-manhattan)
                 (otherwise
                    (check-type distance (or function symbol))
                    distance)))
         (vec-weight (ecase use-weight
                       (:class (assert (eq esttype :classify))
                               (make-class-weight weight-init teachers))
                       (:data (make-data-weight weight-init learning-data))
                       ((nil) nil)))
         (vec-profiles (make-vector-profile vecs teachers esttype))
         (mins nil)
         (maxs nil)
         (k-points (make-array k))
         (k-dis (make-dvec k most-positive-double-float)))
    (when normalize
      (setf (values mins maxs) (normalize-vecs vecs)))
    ;; initialize nearest-search
    (let* ((input-data (if use-weight
                           (map 'vector #'(lambda (x s w) (make-k-nn-point :data x :signal s :weight w))
                                vecs teachers vec-weight)
                         (map 'vector #'(lambda (x s) (make-k-nn-point :data x :signal s)) vecs teachers)))
           (nns-class (case nns-type
                        (:naive 'naive-nearest-search)
                        (:kd-tree 'kd-tree-search)
                        (:m-tree  'm-tree-search)
                        (:lsh (ecase distance
                                (:double-euclid 'euclid-locality-sensitive-hashing)
                                (:double-manhattan 'manhattan-locality-sensitive-hashing)))
                        (otherwise
                         (check-type nns-type symbol)
                         nns-type)))
           (search (apply #'make-instance nns-class :input-data input-data :input-key #'k-nn-point-data
                          :distance #'(lambda (x y) (funcall disf x y vec-profiles))
                          nns-args))
           (estimator
            (make-instance 'k-nn-estimator
              :k k :distance distance :target target
              :vecs vecs :vec-weight vec-weight :vec-labels explanatories
              :vec-profiles vec-profiles :teachers teachers :esttype esttype :mins mins :maxs maxs
              :nn-search search :k-points k-points :k-dis k-dis)))
      (do-vecs ((v vecs-tmp))
        (estimate v esttype search k k-points k-dis))
      (when (eq esttype :classify)
        (format t "~&Number of self-misjudgement : ~A~%"
                (let ((count 0))
                  (do-vecs ((org-id teachers)
                            (ested-vec vecs-tmp))
                    (unless (string-equal org-id (ested ested-vec))
                      (incf count)))
                  count)))
      estimator)))

#+ignore
(make-unspecialized-dataset
 (append (map 'list #'dimension-name
              (dataset-dimensions learning-data))
         '("ested"))
 (loop for vec across (dataset-points learning-data)
     for ested-vec across vecs-tmp
     collect (concatenate 'vector vec `(,(ested ested-vec)))
     into result
     finally (return (coerce result 'vector))))

(defun k-nn-estimate (estimator in-data)
  "- return: <unspecialized-dataset>, estimated result\\
  The column named 'estimated-*' (* is the name of target parameter) is appended to 1st column of /in-data/.
- arguments:
  - estimator : <k-nn-estimator>
  - in-data :  <unspecialized-dataset> data to be estimated."
  (let* ((est-type (esttype estimator))
         (vecs (if (member (distance estimator) '(:double-euclid :double-manhattan))
                   (map 'vector #'(lambda (x) (coerce x 'dvec)) (choice-dimensions (vec-labels estimator) in-data))
                 (choice-dimensions (vec-labels estimator) in-data)))
         (target-vecs
          (map 'vector
            #'(lambda (vec) (make-instance 'knn-exp :vec vec))
            vecs))
         (k (k estimator))
         (search (k-nn-search estimator))
         (k-points (k-points estimator))
         (k-dis (k-dis estimator))
         (mins (mins estimator))
         (maxs (maxs estimator)))
    (when (and mins maxs)
      (move-vecs-by-minmax target-vecs mins maxs))
    (do-vecs ((v target-vecs))
      (estimate v est-type search k k-points k-dis))
    (make-unspecialized-dataset
     (append
      `(,(format nil "estimated-~A" (target estimator)))
      (map 'list #'dimension-name
           (dataset-dimensions in-data)))
     (loop for vec across (dataset-points in-data)
         for ested-vec across target-vecs
         collect (concatenate 'vector `(,(ested ested-vec)) vec)
         into result
         finally (return (coerce result 'vector))))))

#+ignore
(defun k-nn-analyze-dump-file
    (infile outfileest outfilesummary k target explanatories
     &key (distance :euclid) (file-type :sexp)
          use-weight weight-init normalize
          (external-format :default) (csv-type-spec '(string double-float)))
  (multiple-value-bind (est outdata)
      (k-nn-analyze (read-data-from-file
                     infile
                     :type file-type :csv-type-spec csv-type-spec
                     :csv-header-p t :external-format external-format)
                    k target explanatories
                    :distance distance
                    :use-weight use-weight :weight-init weight-init
                    :normalize normalize)
    (dump-knn-est est outfileest)
    (labeled-data-out outdata outfilesummary :external-format external-format)
    (values est outdata)))
#+ignore
(defun k-nn-estimate-dump-file (estimator-path in-data-file outfile &key (external-format :default))
  (labeled-data-out
   (k-nn-estimate (load-dumped-est estimator-path)
                  (read-data-from-file in-data-file :external-format external-format))
   outfile :external-format external-format))


