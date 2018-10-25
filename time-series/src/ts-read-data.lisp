;-*- coding: utf-8 -*-
(in-package :clml.time-series.read-data)

(defclass time-series-dataset (specialized-dataset)
  ((frequency :initarg :frequency
              :accessor ts-freq
              :initform nil
              :type number)
   (start :initarg :start :accessor ts-start :initform nil)
   (end :initarg :end :accessor ts-end :initform nil)
   (ts-type :initarg :ts-type :accessor ts-type :initform nil)
   (ts-points :initarg :ts-points :accessor ts-points :initform nil)
   (time-label-name :initarg :time-label-name
                    :accessor time-label-name :initform nil))
  (:documentation
   "- accessor
  - ts-points : vector of ts-point
  - ts-freq : number of observed values per a cycle
  - ts-start : time for the first observed value, ts-point is represented as list of time and freq. Please refer to the sample usage.
  - ts-end : time for the last observed value, the form is same as ts-start.

The dataset for time-series data. Values are specialized in numeric"))


(defgeneric time-series-data (d &key)
  (:documentation "- return: <time-series-dataset>
- arguments:
  - d          : <unspecialized-dataset>
  - start      : <list integer integer> | integer, specify the start time, integer larger than 1 or a list of integer of such kind. e.g. (1861 3)
                 Where the second integer is the starting frequency value.
  - end        : <list integer integer> | integer, specify the end time, format same as start. When unspecified, all the lines will be read in.
  - frequency  : integer >= 1, specify the frequency
  - range      : :all | <list integer>, indices of columns used in the result, start from 0, e.g. '(0 1 3 4)
  - except     : <list integer>, the opposite of :range, indices of columns which will be excluded from the result, start from 0. e.g. '(2)
  - time-label : integer, index of column which represents the labels of time series data points, no labels when not specified.
"))
(defmethod time-series-data ((d unspecialized-dataset)
                             &key (start 1) end (frequency 1)
                                  (ts-type :constant)
                                  (range :all) except
                                  time-label)

  (with-accessors ((dims dataset-dimensions)
                   (pts dataset-points)) d
  (assert (or (integerp start) (integerp (car start))))
  (assert (or (null end) (integerp end) (integerp (car end))))

  (when (integerp start)
    (setq start (list start 1)))
  (when (integerp end)
    (setq end (list end 1)))
  (assert (every #'(lambda (num) (>= num 1))
                 `(,(first start) ,(second start) ,frequency)))

  (let* ((total-size (length dims))
         (range1 (if (eq range :all)
                     (loop for i below total-size collect i)
                   range))
         (range (sort (set-difference range1 except) #'<))

         (column-names
          (loop for index in range
              collect (dimension-name (aref dims index))))

         (size (length column-names))
         (time-labels (when time-label
           (map 'vector (lambda (p) (declare (simple-vector p))
                                (format nil "~A" (svref p time-label))) pts)))

         (time-label-name (when time-label
                            (dimension-name (svref dims time-label))))

         (data
          (map 'vector
            (lambda (p)
              (declare (simple-vector p))
              (let* ((sp (make-dvec size)))
                (declare (type dvec sp))
                (loop
                    for i in range
                    for j from 0
                    as p-val = (svref p i)
                    do (setf (aref sp j)
                         (if (na-p p-val) +nan+ (coerce p-val 'double-float)))
                    finally (return sp))))
            pts)))

    (case ts-type
      (:constant
       (make-constant-time-series-data
        column-names data
        :start start :end end :freq frequency :time-labels time-labels
        :time-label-name time-label-name))
      (t nil)))))

(defstruct (ts-point (:conc-name ts-p-)
                     (:constructor %make-ts-point (time freq label pos))
                     (:copier copy-ts-point)
             )
  "- accessor
  - ts-p-time : n-th period of the data point, integer larger than 1.
  - ts-p-freq : n-th of the period of the data point, integer larget than 1
  - ts-p-label : name of the data point. e.g. \"2009/jan/5th\"
  - ts-p-pos : coordinate of the data point"
  (time -1 :type fixnum)
  (freq -1 :type fixnum)
  (label "" :type string)
  (pos #() :type dvec))

(defun make-ts-point (time freq label pos)
  (let ((pos (coerce pos 'dvec)))
    (check-type time fixnum)
    (check-type freq fixnum)
    (check-type pos dvec)
    (%make-ts-point time freq label pos)))

(defmethod print-object ((d time-series-dataset) stream)
  (with-accessors ((start ts-start)
                   (end ts-end)
                   (freq ts-freq)
                   (points ts-points)
                   (label time-label-name)) d
    (call-next-method)
    (format stream "~&FREQUENCY:  ~A~%" freq)
    (format stream "~&START:      ~A~%" start)
    (format stream "~&END:        ~A~%" end)
    (format stream "~&POINTS:     ~A~%" (length points))
    (when label
    (format stream "~&TIME-LABEL: ~A~%" label))
    ))

(defun tf-incl (tf-list num &key (freq 1))
  (multiple-value-bind (shou mod)
      (floor (+ num (1- (second tf-list))) freq)
    `(,(+ (first tf-list) shou) ,(1+ mod))))

(defun tf-gap (tf1 tf2 &key (freq 1))
  (+ (* freq (- (first tf2) (first tf1)))
     (- (second tf2) (second tf1))))

(defun make-constant-time-series-data (all-column-names data
                                       &key (start '(1 1)) end
                                            (freq 1) time-labels
                                            time-label-name)
  " Create time-series-dataset from a vector of samples.

- return: <time-series-dataset>
- arguments:
  - all-column-names : List of column names
  - data             : Vector of dvec '(simple-array double-float (*)) containg samples
  - start            : ~optional~ <list integer integer> | specify the start time (>1), and initial frequency, default (1 1)
  - end              : ~optional~ <list integer integer> | integer, specify the end time and frequwncy, format same as start. When unspecified, all the lines will be read in.
  - freq             : ~optional~ integer >= 1, specify the frequency
  - time-lables      : ~optional~ <array string (length data)>, array containing string time lables should be same length as data.
  - time-label       : point label for the time lables in the timeseries dataset
"
  (assert (> (length data) 0))
  (assert (> (length all-column-names) 0))
  (assert (= (length all-column-names) (length (aref data 0))))
  (assert (every #'(lambda (num)
                     (and (plusp num) (integerp num)))
                 `(,freq ,(first start) ,(second start))))
  (check-type data simple-vector)
  (check-type (aref data 0) dvec)
  (unless time-labels
    (setq time-labels (make-array (list (length data)) :initial-element ""
                                  :element-type 'string)))
  (assert (= (length data) (length time-labels)))
  (let ((dimensions
         (make-array (list (length all-column-names)) :element-type 'dimension
                     :initial-element (make-dimension "" :numeric 0))
          )
        (ts-len (length data)))

    (loop
        for n in all-column-names
        for i from 0
       for d = (make-dimension n :numeric i)
        do (setf (aref dimensions i) d))
    (when (null end)
      (setq end (tf-incl start (1- ts-len) :freq freq)))
    (let* ((len (tf-gap start end :freq freq))
           (ts-points
            (coerce
             (loop for i from 0 to len
                 as j = (mod i ts-len)
                 as tf = (tf-incl start i :freq freq)
                 collect (make-ts-point (first tf) (second tf)
                                        (svref time-labels j)
                                        (svref data j)))
             'vector)))
      (make-instance 'time-series-dataset
        :dimensions dimensions :ts-points ts-points :frequency freq
        :start start :end end :time-label-name time-label-name
        :ts-type :constant))))

(defgeneric copy-ts (d))
(defmethod copy-ts ((d time-series-dataset))
  (make-instance 'time-series-dataset
    :dimensions (map 'vector (lambda (dim) (copy-dimension dim)) (dataset-dimensions d))
    :time-label-name (time-label-name d)
    :ts-points (map 'vector #'copy-ts-point (ts-points d))
    :frequency (ts-freq d) :start (ts-start d)
    :end (ts-end d) :ts-type (ts-type d)))

(defgeneric ts-cleaning (d &key)
  (:documentation "- return: <time-series-dataset>
- arguments:
  - d : <time-series-dataset>
  - interp-types-alist   : a-list (key: column name, datum: interpolation(:zero :min :max :mean :median :mode :spline)) | nil\\
  - outlier-types-alist  : a-list (key: column name, datum: outlier-verification(:std-dev :mean-dev :user :smirnov-grubbs :freq)) | nil\\
  - outlier-values-alist : a-list (key: outlier-verification datum: the value according to outlier-verification) | nil
- comment:
  Same as /dataset-cleaning/ in read-data package."))
(defmethod ts-cleaning ((d time-series-dataset)
                        &key interp-types-alist
                             outlier-types-alist
                             outlier-values-alist)
  (let ((names (map 'list #'dimension-name (dataset-dimensions d))))
    (multiple-value-bind (interp-types outlier-types outlier-values)
        (clml.hjs.read-data::convert-cleaning-alist-to-list
         names
         interp-types-alist
         outlier-types-alist
         outlier-values-alist)
      (let* ((d (copy-ts d))
             (pts
              (clean-points (map 'vector #'ts-p-pos (ts-points d))
                            interp-types outlier-types outlier-values))
             (points (ts-points d)))
        (when pts
          (do-vec (vec pts :type dvec :index-var i :return d)
            (setf (ts-p-pos (svref points i)) vec)))))))

(defmethod choice-dimensions (names (data time-series-dataset))
  (with-accessors ((dims dataset-dimensions)
                   (pts ts-points)) data
    (let* ((poses (loop for name in names
                      collect (position name dims :key #'dimension-name :test #'string=)))
           (types (mapcar (lambda (pos) (dimension-type (aref dims pos))) poses))
           (type (cond ((every (lambda (ty) (eq ty :numeric)) types) :numeric)
                       ((every (lambda (ty) (eq ty :category)) types) :category)
                       (t t))))
      (when poses
        (loop for pt across pts
            as new-pt = (mapcar (lambda (pos) (aref pt pos)) poses)
            collect (case type
                      (:numeric (coerce new-pt 'dvec))
                      (:category (coerce new-pt 'vector)) ;;
                      (t (coerce new-pt 'vector))) into result
            finally (return (coerce result 'vector)))))))

(defmethod choice-a-dimension (name (data time-series-dataset))
  (multiple-value-bind (pos type)
      (loop for pos from 0
          for dim across (dataset-dimensions data)
          when (string= (dimension-name dim) name)
          return (values pos (dimension-type dim)))
    (when pos
      (loop for vec across (ts-points data)
            collect (aref (ts-p-pos vec) pos) into result
          finally (return (ecase type
                            (:numeric (coerce result 'dvec))
                            (:category (coerce result 'vector)) ;;
                            ))))))

