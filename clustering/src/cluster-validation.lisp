#||
(let ((*read-default-float-format* 'double-float))
  (load "defsystem.cl") (excl:load-system :machine-learning :compile t))


||#


(in-package :clml.clustering.cluster-validation)

(defvar *workspace* nil
  "*workspace* | validation target, the result of k-means clustering")

(defun default-init-workspace ()
  (progn (setf clml.clustering.cluster-validation:*workspace*
               (clml.hjs.k-means:k-means
                10
                (clml.hjs.read-data:pick-and-specialize-data
                 (clml.hjs.read-data:read-data-from-file
                  (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/norm-interp-feature.sexp")) :except '(0)
                  :data-types (make-list 12 :initial-element :numeric))))nil))
(defdoublefunc v-diff-sum^2 (dvec dvec))

(defun v-diff-sum^2 (x y)
  (declare (type dvec x y))

  (let ((result 0.0d0))
    #-ccl (declare (type (double-float 0.0) result))
    (do-vecs ((ex x :type double-float)
              (ey y :type double-float))
      (let ((diff (- ex ey)))
        (incf result (* diff diff))))
    result))


(defparameter *distance* :euclid) ; :manhattan :euclid or :cosine
(defun set-distance (method)
  (assert
      (or (eq method :euclid) (eq method :manhattan) (eq method :cosine)))
  (setf *distance* method))


#-(and)
(defun-speedy d (x y)
  (euclid-distance x y))
#-(and)
(defmacro d (x y)
  `(locally
       (declare (optimize speed (safety 0) (debug 0) (:explain :inlining)))
     (vml::sse3-euclid-indirect (length ,x) ,x ,y)))
; TODO
; add manhattan and cosine distance calculation of sse3

#+(and)
(defmacro d (x y)
  `(ecase *distance*
     (:euclid (euclid-distance ,x ,y))
     (:manhattan (manhattan-distance ,x ,y))
     (:cosine (cosine-distance ,x ,y))))

(defun foo (x y)
  (d x y))


(defdoublefunc d-func (dvec dvec))
(defun-speedy d-func (x y)
  (declare (optimize speed (safety 0) (debug 0))
           (type (simple-array double-float) x y))
  (d x y))




#-(and)
(defun-speedy p-d (x y)
  (let ((xn (p-id x)) (yn (p-id y)))
    (when (> xn yn)
      (let ((tmp xn))
        (setf xn yn
              yn tmp)))
    (p-internal-d (+ (* yn (length (pw-points *workspace*))) xn))))

(defun-speedy p-d (x y)
  (d (p-pos x) (p-pos y)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest args)
    (intern
     (with-standard-io-syntax
     (format nil "~{~A~}" args))))

  (defun memo-table-symbol (name)
    (let ((*package* (symbol-package name)))
      (symbolicate '% name '-%memo-table))))

(defmacro do-memo-update (table params gen-value)
  (with-unique-names (key)
    `(let ((,key
            ,(cond ((not (cdr params))
                    (first params))
                   ((not (cddr params))
                    `(cons ,(first params) ,(second params)))
                   (t `(make-array ,(length params))))))
       (declare (dynamic-extent ,key))
       ,(when (cddr params)
              `(setf ,@(loop for p in params
                             for i from 0
                             collect `(aref ,key ,i)
                             collect p)))
       (gethash-or-set ,key ,table ,gen-value))))

(defmacro defmemo (defun name lambda-list &body body)
  (let ((table-name (memo-table-symbol name))
        (internal (symbolicate '% name '%-calculate))
        (args lambda-list))
    `(progn
       (defvar ,table-name)
       (,defun ,internal ,lambda-list ,@body)
       (,defun ,name ,lambda-list
         (do-memo-update ,table-name ,args (,internal ,@args))))))

(defmacro with-memo ((&rest memos) &body body)
  `(let ,(loop for m in memos collect `(,(memo-table-symbol m) (make-hash-table :test 'equal)))
     ,@body))

(defdoublefunc ^2 (double-float))
(defun-speedy ^2 (x)
  (* x x))

(defun-speedy d-taxi (x y)
  "Manhattan distance, taxicab metric, L1 distance or rectilinear distance"
  (loop for a across x
        for b across y
        summing (abs (- a b))))

(defun-speedy d-euclid (x y)
  "Euclidean distance, straight line distance"
  (sqrt
   (loop for a across x
         for b across y
         summing (^2 (- a b)))))


(defun-speedy d-chebyshev (x y)
  (loop for a across x
        for b across y
        maximizing (abs (- a b))))

(defmemo defun-speedy p-internal-d (coded-points)
  (multiple-value-bind (yn xn)
      (floor coded-points (length (pw-points *workspace*)))
    (let ((x (elt (pw-points *workspace*) xn))
          (y (elt (pw-points *workspace*) yn)))
      (d (p-pos x) (p-pos y)))))




(defun-speedy d-1000 ()
  (let ((x (make-dvec 10)) (y (make-dvec 10)))
    (loop repeat 10000 do (d x y))))

(defun-speedy nop ())
(defun-speedy nop-100 ()
  (loop repeat 100 do (nop)))



#-(and)
(defun-speedy d (x y)
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((n (length x)))
    (let ((tmp (make-array 1000 :element-type '(unsigned-byte 8))))
      (declare (dynamic-extent tmp))
      (vml::|%cffi-foreign-function/VDSUB| n x y tmp)
      (vml::|%cffi-foreign-function/CBLAS_DNRM2| n tmp 1))))






(defun-speedy second-closest-cluster (p)
  (declare (optimize speed))
  (let ((clusters (pw-clusters *workspace*)))
    (let (closest next-closest (c-d most-positive-double-float) (n-c-d most-positive-double-float))
      (iter (for c in-sequence clusters)
            (for distance = (d (c-center c) (p-pos p)))
            (when (> c-d distance)
              (when (> n-c-d c-d)
                (setf next-closest closest
                      n-c-d c-d))
              (setf closest c
                    c-d distance))
            (when (and (not (eq c closest)) (> n-c-d distance))
              (setf next-closest c
                    n-c-d distance)))
      ; (assert (eq closest (p-owner p)))
      next-closest)))


(defun p-two-closest-clusters (p)
  (values (p-owner p) (second-closest-cluster p)))


(defun silhouette-width (p)
  (flet ((average-dissimularity (c)
           (loop for p1 in (c-points c)
               summing (p-d p1 p) into d
               counting t into n
               finally (return (/ d n)))))
    (multiple-value-bind (closest next-closest)
        (p-two-closest-clusters p)
      (let ((a (average-dissimularity closest))
            (b (average-dissimularity next-closest)))
        (/ (- b a) (max a b))))))


(defun silhouette (cluster)
  (loop for p in (c-points cluster)
      counting t into n
      summing (silhouette-width p) into s
      finally (return (/ s n))))


(defun intracluster-complete-diameter (c)
  (loop for p in (c-points c)
      maximizing
        (loop for q in (c-points c)
            unless (eq p q)
            maximizing (p-d p q))))

(defun intracluster-average-diameter (c)
  (loop for p in (c-points c)
      counting t into n
      summing
        (loop for q in (c-points c)
            unless (eq p q)
            summing (p-d p q)) into d
      finally (return (/ d (* n (1- n))))))

(defun c-centroid (c)
  (c-center c))

(defun intracluster-centroid-diameter (c)
  (let* ((points (c-points c))
         (n (length points))
         (centroid (c-centroid c)))
    (* 2
       (/ (loop for p in points
              summing (d (p-pos p) centroid))
          n))))


(defun intercluster-single-linkage (c0 c1)
  (let ((c0-points (c-points c0))
        (c1-points (c-points c1)))
    (iter (for x in-sequence c0-points)
          (minimizing
           (loop for y in c1-points
               minimizing (p-d x y))))))

(defun intercluster-complete-linkage (c0 c1)
  (let ((c0-points (c-points c0))
        (c1-points (c-points c1)))
    (iter (for x in-sequence c0-points)
          (maximizing
           (loop for y in c1-points
               maximizing (p-d x y))))))

(defun intercluster-average-linkage (c0 c1)
  (let ((c0-points (c-points c0))
        (c1-points (c-points c1)))
    (let ((c0-n (length c0-points))
          (c1-n (length c1-points)))
      (/
       (loop for x in c0-points
           summing
             (loop for y in c1-points
                 summing (p-d x y)))
       c0-n c1-n))))

(defun intercluster-centroid-linkage (c0 c1)
  (d (c-centroid c0) (c-centroid c1)))

(defun intercluster-average-to-centroids-linkage (c0 c1)
  (let ((c0-points (c-points c0))
        (c1-points (c-points c1)))
    (let ((c0-n (length c0-points))
          (c1-n (length c1-points)))
      (/
       (flet ((sum-to-c (centroid points)
                (loop for x in points
                    summing (d (p-pos x) centroid))))
         (+
          (sum-to-c (c-centroid c0) c1-points)
          (sum-to-c (c-centroid c1) c0-points)))
       (+ c0-n c1-n)))))


(defun intercluster-hausdorff-linkage (c0 c1)
  (let ((c0-points (c-points c0))
        (c1-points (c-points c1)))
    (flet ((max-min-d (xps yps)
      (iter (for x in-sequence xps)
            (maximizing
             (iter (for y in-sequence yps)
                   (minimizing (p-d x y)))))))
      (max (max-min-d c0-points c1-points) (max-min-d c1-points c0-points)))))


(defun intercluster-d (c0 c1 &key (method :centroid))
  (ecase method
    (:centroid (intercluster-centroid-linkage c0 c1))
    (:single (intercluster-single-linkage c0 c1))
    (:complete (intercluster-complete-linkage c0 c1))
    (:average (intercluster-average-linkage c0 c1))
    (:average-to-centroids (intercluster-average-to-centroids-linkage
                            c0 c1))
    (:hausdorff (intercluster-hausdorff-linkage c0 c1))))

(defun intracluster-diameter (c &key (method :centroid))
  (ecase method
    (:centroid (intracluster-centroid-diameter c))
    (:complete (intracluster-complete-diameter c))
    (:average (intracluster-average-diameter c))))


(defun ssw ()
  (iter (for c in-sequence (pw-clusters *workspace*))
        (let ((mu (c-center c)))
          (summing
           (loop
               for p in (c-points c)
               summing (v-diff-sum^2 (p-pos p) mu))))))

(defun ssb ()
  (let ((mu (centroid)))
    (iter (for c in-sequence (pw-clusters *workspace*))
          (summing (* (c-size c) (v-diff-sum^2 (c-center c) mu))))))
(defun sst ()
  (let ((mu (centroid)))
    (iter (for p in-sequence (pw-points *workspace*))
          (summing (v-diff-sum^2 (p-pos p) mu)))))




(defun make-zero-dvec ()
  (let ((v (copy-seq (p-pos (elt (pw-points *workspace*) 0)))))
    (fill-vec v 0d0)
    v))


(defun calinski (&optional (*workspace* *workspace*))
  "- return: <number> cluster validity index"
  (let ((n (length (pw-points *workspace*)))
        (k (length (pw-clusters *workspace*))))
    (/ (* (ssb)  (- (1- n) k)) (* (ssw) (1- k)))))

(defun hartigan (&optional (*workspace* *workspace*))
  (- (log (ssb)) (log (ssw))))

(defun ball-and-hall (&optional (*workspace* *workspace*))
  (let ((k (length (pw-clusters *workspace*))))
    (/ (ssw) k)))

(defun dunn-index (&key (*workspace* *workspace*)
                        (distance :euclid)
                        (intercluster :centroid)
                        (intracluster :centroid))
  (set-distance distance)
  (let ((clusters (pw-clusters *workspace*)))
    (/
     (iter (for c0 in-sequence clusters)
           (minimizing (iter (for c1 in-sequence clusters)
                             (unless (eq c0 c1)
                               (minimizing
                                (intercluster-d c0 c1
                                                :method intercluster))))))
     (iter (for c in-sequence clusters)
           (maximizing (intracluster-diameter
                        c
                        :method intracluster))))))

(defun davies-bouldin-index (&key (*workspace* *workspace*)
                                  (distance :euclid)
                                  (intercluster :centroid)
                                  (intracluster :centroid))
  (set-distance distance)
  (let* ((clusters (pw-clusters *workspace*))
         (cids (map 'list #'(lambda (c)
                              (intracluster-diameter
                               c
                               :method intracluster)) clusters))
         (c (length clusters)))
    (/
     (iter (for c0 in-sequence clusters)
           (for c0id in-sequence cids)
           (summing
            (iter (for c1 in-sequence clusters)
                  (for c1id in-sequence cids)
                  (unless (eq c0 c1)
                    (maximizing (/ (+ c1id c0id)
                                   (intercluster-d
                                    c0 c1
                                    :method intercluster)))))))
     c)))

(defun global-silhouette-value (&key (*workspace* *workspace*)
                                     (distance :euclid))
  (set-distance distance)
  (iter (for c in-sequence (pw-clusters *workspace*))
        (summing (silhouette c) into s)
        (counting t into n)
        (finally (return (/ s n)))))

(defun centroid ()
  (let ((c (make-zero-dvec))
        (n 0)
        (pw-points (pw-points *workspace*
                              )))
    (do-vec (p pw-points ) ;(p pw-points :type #'p-point)
      (v+ c (p-pos p) c)
      (incf n))
    (v-scale c (the double-float (/ 1d0 (coerce n 'double-float))) c)
    c))
#|

|#

