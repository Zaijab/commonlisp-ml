
(in-package :clml.test)

(defun eppy (x y) (epsilon> x y -1.d-7))

(defparameter sample-ts nil)
(defparameter exchange nil)
(define-test test-ts-anomaly-detection-data
    (assert
       (setf sample-ts
             (time-series-data
              (read-data-from-file
               (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/traffic-balance.csv")
               :type :csv :csv-type-spec (cons 'string
                                               (make-list 6 :initial-element 'double-float))
               :external-format :utf-8)
              :frequency 12 :except '(0) :time-label 0)
             exchange
             (time-series-data
              (read-data-from-file
               (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/exchange.csv")
               :type :csv :csv-type-spec (cons 'string
                                               (make-list 10 :initial-element 'double-float))
               :external-format :utf-8)
              :except '(0) :time-label 0)))
  (assert-equality #'= 1015 (length (ts-points sample-ts)))
  (assert-equality #'= 753 (length (ts-points exchange)))
  )

(define-test test-ts-anomaly-detection-snn
    (when (not (or sample-ts exchange)) (test-ts-anomaly-detection-data))
    (mapc (lambda (a1 a2)
              (assert-equality #'equal (car a2) (car a1))
              (assert-equality #'epsilon>
                               (coerce (cdr a2) 'single-float) (coerce (cdr a1) 'single-float) ))
            (let ((target-snn (make-snn (sub-ts exchange :start 1 :end 150) 3))
                  (reference-snn (make-snn (sub-ts exchange :start 600 :end 700) 3)))
              (e-scores target-snn reference-snn))
            '(("AUD/USD" . 0.47406298323897705d0) ("CAD/USD" . 0.5240011355714634d0)
              ("CHF/USD" . 0.5325785438502517d0) ("EUR/USD" . 0.731769158687747d0)
              ("GBP/USD" . 0.596827444239165d0) ("HKD/USD" . 0.5766733684269696d0)
              ("JPY/USD" . 0.5117506042665696d0) ("KRW/USD" . 0.5198055610159624d0)
              ("MXN/USD" . 0.7027828954312578d0) ("NZD/USD" . 0.2842836687583187d0)))
  )

(define-test test-ts-anomaly-detection-periodic-detector
  (when (not (or sample-ts exchange)) (test-ts-anomaly-detection-data))
  (mapc (lambda (p1 p2)
          (assert-equality #'epsilon> (getf p2 :score) (getf p1 :score))
          (mapc (lambda (v1 v2) (assert-equality #'epsilon> (coerce v2 'single-float) (coerce v1 'single-float)))
                (getf p2 :local-scores) (getf p1 :local-scores)))
        (loop with detector = (make-periodic-detector
                               (sub-ts sample-ts :start '(1 1) :end '(2 12)))
              for p across (ts-points (sub-ts sample-ts :start '(3 1) :end '(3 12)))
              collect (funcall detector (ts-p-pos p)))
        '((:SCORE 0.15980001156818346d0 :LOCAL-SCORES (-0.011247495797210605d0 0.04067641708837213d0 0.07657475988236122d0 0.026173388386296143d0 -0.001005722797717759d0 -0.13117336322290166d0))
          (:SCORE 0.16606559269099325d0 :LOCAL-SCORES (-0.04404576382434579d0 0.08836079938698248d0 0.06427181525186569d0 0.008060984870295258d0 6.037724071195098d-5 -0.11672432427082227d0))
          (:SCORE 0.0835963350476519d0 :LOCAL-SCORES (0.02860344056963936d0 0.02049834345000817d0 0.018558627759386243d0 0.005805395166900154d0 -1.7563302955435247d-4 -0.07329208280202894d0))
          (:SCORE 0.10895276517361178d0 :LOCAL-SCORES (0.06171796944486013d0 0.02627577908981959d0 -0.0013938026860552477d0 7.108933807211727d-4 -0.0015292225676566903d0 -0.08581498358943485d0))
          (:SCORE 0.14372822478142372d0 :LOCAL-SCORES (0.019119719424318164d0 0.06530386435337952d0 -0.03223066630047898d0 0.05779465755012304d0 -0.0021226015789952857d0 -0.10789806554381363d0))
          (:SCORE 0.1214316386275602d0 :LOCAL-SCORES (0.08180945936566704d0 -0.01666669357385849d0 0.01789677418744477d0 -0.08623381474472612d0 -5.783555512765765d-4 0.003743461124108086d0))
          (:SCORE 0.16328621183435152d0 :LOCAL-SCORES (0.09252923344792947d0 0.04206473653695766d0 0.03524081165133149d0 -0.10442527700870255d0 -6.866050459105892d-4 -0.06471611713622019d0))
          (:SCORE 0.17165824330218574d0 :LOCAL-SCORES (0.1124055553487212d0 -0.04483642919806279d0 0.06943579226133692d0 -0.08609866163195316d0 -1.3815655640593742d-4 -0.05081348776600684d0))
          (:SCORE 0.14705276128118872d0 :LOCAL-SCORES (0.03176665855145954d0 -0.05169044126068538d0 0.11199895677113193d0 -0.020881754613730465d0 -0.0013360512015534781d0 -0.06969391195126472d0))
          (:SCORE 0.1753941034019109d0 :LOCAL-SCORES (0.0926869320817864d0 -0.04500698002481467d0 0.08111355541737571d0 -0.010867820410934509d0 -0.0027675310185543865d0 -0.11509576770374046d0))
          (:SCORE 0.21949653755912735d0 :LOCAL-SCORES (0.045993703368709546d0 -0.009282656742070803d0 0.09337478957559686d0 0.05588545026517181d0 -0.0016903984620666593d0 -0.18442976781751605d0))
          (:SCORE 0.12925093469918736d0 :LOCAL-SCORES (-0.011727190219218097d0 -0.08255653999587756d0 0.09714760754195113d0 -0.01394679381635095d0 3.227553008842132d-4 0.010885745599589552d0))))
  )



(define-test test-ts-anomaly-detection-eec-detector
  (when (not (or sample-ts exchange)) (test-ts-anomaly-detection-data))
  (mapc (lambda (p1 p2)
          (assert-equality #'epsilon> (getf p2 :score) (getf p1 :score))
          (mapc (lambda (v1 v2) (assert-equality #'epsilon> v2 v1))
                (getf p2 :local-scores) (getf p1 :local-scores)))
        (loop with detector = (make-eec-detector
                               (sub-ts exchange :start 1 :end 60) 20)
              for p across (ts-points (sub-ts exchange :start 60 :end 70))
              collect (funcall detector (ts-p-pos p)))
        '((:SCORE 2.700571112024573d0 :LOCAL-SCORES
           (-3.7189814823543945d0 1.0326461685226247d0 -0.09199334202340251d0 -1.5304334860393167d0 1.6336817412409927d0 0.09973192007442783d0 -1.7705007982055647d0 -1.3659133055436354d0 1.6229166989275772d0
            -2.456418564898763d0))
          (:SCORE 2.2333558257821577d0 :LOCAL-SCORES
           (-3.905638387254389d0 1.0111353552477693d0 -0.16180107817711298d0 -0.06211424245500806d0 2.444035892878855d0 -0.7941221366494797d0 -2.0601881585490758d0 -0.6032554617242315d0 1.3644194991066583d0
            -2.94095956222471d0))
          (:SCORE 1.9868164604264957d0 :LOCAL-SCORES
           (-4.071453905957172d0 0.09987314488820478d0 -0.5124850991763434d0 0.3572466274370432d0 1.985594397643084d0 -1.2627672914256596d0 -2.0286025799206437d0 -2.0180011854462823d0 1.0031799987968517d0
            -3.349034884667727d0))
          (:SCORE 1.99119158115065d0 :LOCAL-SCORES
           (-4.21295552995317d0 3.6696601922048d0 0.13498367839300002d0 2.202025796055173d0 1.5652235278554427d0 -1.5185993444794728d0 -1.9951097435792884d0 -2.141676229907566d0 0.536949673309007d0
            0.13587904258754527d0))
          (:SCORE 1.655330278980456d0 :LOCAL-SCORES
           (-3.940751233076124d0 1.4944533102503788d0 -1.134801399167889d0 1.0953740695897256d0 0.8538413750781987d0 -2.6483828385806047d0 -1.9833372992457443d0 -2.1457229135357965d0 -0.25535073809135234d0
            -1.1228770376956778d0))
          (:SCORE 1.6026376553309072d0 :LOCAL-SCORES
           (-0.034554670356311185d0 1.2292838508330988d0 1.132721967732395d0 -0.7371812412223815d0 -1.2217525313170159d0 -3.7170161170631384d0 -0.8394971355287675d0 -2.309275510777308d0 -0.6893891878271913d0
            -1.2247368414257422d0))
          (:SCORE 1.4921358653856052d0 :LOCAL-SCORES
           (-1.1119582168928317d0 0.13109381389384833d0 0.03822852402739136d0 -1.2567269843174933d0 -1.0016538526115792d0 -3.7378375887102315d0 0.0018749768626725657d0 -2.1904933121802066d0 -1.0031674527371155d0
            -1.8580823578222343d0))
          (:SCORE 1.834987095608023d0 :LOCAL-SCORES
           (-2.411063158982719d0 -0.9462790230517837d0 -0.5412882072844031d0 -1.8686452258034443d0 -2.4080116434386505d0 -4.2224169886297185d0 -0.19950597770025008d0 -2.1142292908200604d0 0.49105626655832846d0
            -1.4030218415732563d0))
          (:SCORE 1.0321828011949825d0 :LOCAL-SCORES
           (-3.2832950290358296d0 -1.7201312662081096d0 -0.806431510082311d0 -0.49749735373008097d0 -2.3879869063190085d0 -4.243481779019334d0 -1.1894302963419576d0 -2.5038090216601767d0 -0.1556970436113533d0
            -1.4378596777323336d0))
          (:SCORE 0.5533902042593536d0 :LOCAL-SCORES
           (-3.7083233694175766d0 -1.6133834329235863d0 -0.01938368944029429d0 -0.6476096999243521d0 0.03650134747649691d0 -3.3240586306405393d0 -1.8620675130088626d0 -1.7836998046168742d0 -0.875130410874981d0
            -1.9750969929005304d0))
          (:SCORE 0.42468864104772863d0 :LOCAL-SCORES
           -3.485188610580603d0 (-1.756294648228343d0 0.7021257432151065d0 -1.7970368760900062d0 8.50911984460571d0 -4.0375303534574725d0 -2.0344472681961383d0 -2.380519783436971d0 -0.8423336118905549d0
                                 -3.4975080967910612d0))))
  )

(define-test test-ts-anomaly-detection-db-detector
  (when (not (or sample-ts exchange)) (test-ts-anomaly-detection-data))
  (mapc (lambda (v1 v2) (assert-equality #'epsilon> v2 v1))
        (loop with detector = (make-db-detector
                               (sub-ts sample-ts :start '(1 1) :end '(2 12)))
              for p across (ts-points (sub-ts sample-ts :start '(3 1) :end '(3 12)))
              collect (funcall detector (ts-p-pos p)))
        '(7.689004308083502d-4 8.690742068634405d-4 0.0014640360422599752d0
          9.645504419952822d-4 0.002189430044882701d0 0.0022804402419548397d0
          8.653971028227403d-4 0.0021245846566718685d0 0.0021297890535286745d0
          0.003035579690776613d0 0.0010429131136164838d0 0.0017684154782838801d0))
  )

(defun ad ()
    (let (sample-ts exchange)
      (assert
       (setf sample-ts
         (time-series-data
          (read-data-from-file
           (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/traffic-balance.csv")
           :type :csv :csv-type-spec (cons 'string
                                           (make-list 6 :initial-element 'double-float))
           ):external-format :utf-8
          :frequency 12 :except '(0) :time-label 0)
         exchange
         (time-series-data
          (read-data-from-file
           (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/exchange.csv")
           :type :csv :csv-type-spec (cons 'string
                                           (make-list 10 :initial-element 'double-float))
           :external-format :utf-8)
          :except '(0) :time-label 0)))
      (assert-equality #'= 1015 (length (ts-points sample-ts)))
      (assert-equality #'= 753 (length (ts-points exchange)))

      (mapc (lambda (p1 p2)
              (assert-equality #'epsilon> (getf p2 :score) (getf p1 :score))
              (mapc (lambda (v1 v2) (assert-equality #'epsilon> (coerce v2 'single-float) (coerce v1 'single-float)))
                    (getf p2 :local-scores) (getf p1 :local-scores)))
            (loop with detector = (make-periodic-detector
                                   (sub-ts sample-ts :start '(1 1) :end '(2 12)))
                for p across (ts-points (sub-ts sample-ts :start '(3 1) :end '(3 12)))
                  collect (funcall detector (ts-p-pos p)))
            '((:SCORE 0.15980001156818346d0 :LOCAL-SCORES (-0.011247495797210605d0 0.04067641708837213d0 0.07657475988236122d0 0.026173388386296143d0 -0.001005722797717759d0 -0.13117336322290166d0))
              (:SCORE 0.16606559269099325d0 :LOCAL-SCORES (-0.04404576382434579d0 0.08836079938698248d0 0.06427181525186569d0 0.008060984870295258d0 6.037724071195098d-5 -0.11672432427082227d0))
              (:SCORE 0.0835963350476519d0 :LOCAL-SCORES (0.02860344056963936d0 0.02049834345000817d0 0.018558627759386243d0 0.005805395166900154d0 -1.7563302955435247d-4 -0.07329208280202894d0))
              (:SCORE 0.10895276517361178d0 :LOCAL-SCORES (0.06171796944486013d0 0.02627577908981959d0 -0.0013938026860552477d0 7.108933807211727d-4 -0.0015292225676566903d0 -0.08581498358943485d0))
              (:SCORE 0.14372822478142372d0 :LOCAL-SCORES (0.019119719424318164d0 0.06530386435337952d0 -0.03223066630047898d0 0.05779465755012304d0 -0.0021226015789952857d0 -0.10789806554381363d0))
              (:SCORE 0.1214316386275602d0 :LOCAL-SCORES (0.08180945936566704d0 -0.01666669357385849d0 0.01789677418744477d0 -0.08623381474472612d0 -5.783555512765765d-4 0.003743461124108086d0))
              (:SCORE 0.16328621183435152d0 :LOCAL-SCORES (0.09252923344792947d0 0.04206473653695766d0 0.03524081165133149d0 -0.10442527700870255d0 -6.866050459105892d-4 -0.06471611713622019d0))
              (:SCORE 0.17165824330218574d0 :LOCAL-SCORES (0.1124055553487212d0 -0.04483642919806279d0 0.06943579226133692d0 -0.08609866163195316d0 -1.3815655640593742d-4 -0.05081348776600684d0))
              (:SCORE 0.14705276128118872d0 :LOCAL-SCORES (0.03176665855145954d0 -0.05169044126068538d0 0.11199895677113193d0 -0.020881754613730465d0 -0.0013360512015534781d0 -0.06969391195126472d0))
              (:SCORE 0.1753941034019109d0 :LOCAL-SCORES (0.0926869320817864d0 -0.04500698002481467d0 0.08111355541737571d0 -0.010867820410934509d0 -0.0027675310185543865d0 -0.11509576770374046d0))
              (:SCORE 0.21949653755912735d0 :LOCAL-SCORES (0.045993703368709546d0 -0.009282656742070803d0 0.09337478957559686d0 0.05588545026517181d0 -0.0016903984620666593d0 -0.18442976781751605d0))
              (:SCORE 0.12925093469918736d0 :LOCAL-SCORES (-0.011727190219218097d0 -0.08255653999587756d0 0.09714760754195113d0 -0.01394679381635095d0 3.227553008842132d-4 0.010885745599589552d0))))

      (mapc (lambda (a1 a2)
              (assert-equality #'equal (car a2) (car a1))
              (assert-equality #'epsilon>
                               (coerce (cdr a2) 'single-float) (coerce (cdr a1) 'single-float) ))
            (let ((target-snn (make-snn (sub-ts exchange :start 1 :end 150) 3))
                  (reference-snn (make-snn (sub-ts exchange :start 600 :end 700) 3)))
              (e-scores target-snn reference-snn))
            '(("AUD/USD" . 0.47406298323897705d0) ("CAD/USD" . 0.5240011355714634d0)
              ("CHF/USD" . 0.5325785438502517d0) ("EUR/USD" . 0.731769158687747d0)
              ("GBP/USD" . 0.596827444239165d0) ("HKD/USD" . 0.5766733684269696d0)
              ("JPY/USD" . 0.5117506042665696d0) ("KRW/USD" . 0.5198055610159624d0)
              ("MXN/USD" . 0.7027828954312578d0) ("NZD/USD" . 0.2842836687583187d0)))

      (mapc (lambda (p1 p2)
              (assert-equality #'epsilon> (getf p2 :score) (getf p1 :score))
              (mapc (lambda (v1 v2) (assert-equality #'epsilon> v2 v1))
                    (getf p2 :local-scores) (getf p1 :local-scores)))
            (loop with detector = (make-eec-detector
                                   (sub-ts exchange :start 1 :end 60) 20)
                for p across (ts-points (sub-ts exchange :start 60 :end 70))
                collect (funcall detector (ts-p-pos p)))
            '((:SCORE 2.700571112024573d0 :LOCAL-SCORES
                      (-3.7189814823543945d0 1.0326461685226247d0 -0.09199334202340251d0 -1.5304334860393167d0 1.6336817412409927d0 0.09973192007442783d0 -1.7705007982055647d0 -1.3659133055436354d0 1.6229166989275772d0
                                             -2.456418564898763d0))
              (:SCORE 2.2333558257821577d0 :LOCAL-SCORES
               (-3.905638387254389d0 1.0111353552477693d0 -0.16180107817711298d0 -0.06211424245500806d0 2.444035892878855d0 -0.7941221366494797d0 -2.0601881585490758d0 -0.6032554617242315d0 1.3644194991066583d0
                -2.94095956222471d0))
              (:SCORE 1.9868164604264957d0 :LOCAL-SCORES
               (-4.071453905957172d0 0.09987314488820478d0 -0.5124850991763434d0 0.3572466274370432d0 1.985594397643084d0 -1.2627672914256596d0 -2.0286025799206437d0 -2.0180011854462823d0 1.0031799987968517d0
                -3.349034884667727d0))
              (:SCORE 1.99119158115065d0 :LOCAL-SCORES
               (-4.21295552995317d0 3.6696601922048d0 0.13498367839300002d0 2.202025796055173d0 1.5652235278554427d0 -1.5185993444794728d0 -1.9951097435792884d0 -2.141676229907566d0 0.536949673309007d0
                0.13587904258754527d0))
              (:SCORE 1.655330278980456d0 :LOCAL-SCORES
               (-3.940751233076124d0 1.4944533102503788d0 -1.134801399167889d0 1.0953740695897256d0 0.8538413750781987d0 -2.6483828385806047d0 -1.9833372992457443d0 -2.1457229135357965d0 -0.25535073809135234d0
                -1.1228770376956778d0))
              (:SCORE 1.6026376553309072d0 :LOCAL-SCORES
               (-0.034554670356311185d0 1.2292838508330988d0 1.132721967732395d0 -0.7371812412223815d0 -1.2217525313170159d0 -3.7170161170631384d0 -0.8394971355287675d0 -2.309275510777308d0 -0.6893891878271913d0
                -1.2247368414257422d0))
              (:SCORE 1.4921358653856052d0 :LOCAL-SCORES
               (-1.1119582168928317d0 0.13109381389384833d0 0.03822852402739136d0 -1.2567269843174933d0 -1.0016538526115792d0 -3.7378375887102315d0 0.0018749768626725657d0 -2.1904933121802066d0 -1.0031674527371155d0
                -1.8580823578222343d0))
              (:SCORE 1.834987095608023d0 :LOCAL-SCORES
               (-2.411063158982719d0 -0.9462790230517837d0 -0.5412882072844031d0 -1.8686452258034443d0 -2.4080116434386505d0 -4.2224169886297185d0 -0.19950597770025008d0 -2.1142292908200604d0 0.49105626655832846d0
                -1.4030218415732563d0))
              (:SCORE 1.0321828011949825d0 :LOCAL-SCORES
               (-3.2832950290358296d0 -1.7201312662081096d0 -0.806431510082311d0 -0.49749735373008097d0 -2.3879869063190085d0 -4.243481779019334d0 -1.1894302963419576d0 -2.5038090216601767d0 -0.1556970436113533d0
                -1.43785967773u23336d0))
              (:SCORE 0.5533902042593536d0 :LOCAL-SCORES
               (-3.7083233694175766d0 -1.6133834329235863d0 -0.01938368944029429d0 -0.6476096999243521d0 0.03650134747649691d0 -3.3240586306405393d0 -1.8620675130088626d0 -1.7836998046168742d0 -0.875130410874981d0
                -1.9750969929005304d0))
              (:SCORE 0.42468864104772863d0 :LOCAL-SCORES
               (-3.485188610580603d0 -1.756294648228343d0 0.7021257432151065d0 -1.7970368760900062d0 8.50911984460571d0 -4.0375303534574725d0 -2.0344472681961383d0 -2.380519783436971d0 -0.8423336118905549d0
                -3.4975080967910612d0))))
      (print "-------")
        (mapc (lambda (v1 v2) (assert-equality #'epsilon> v2 v1))
            (loop with detector = (make-db-detector
                                   (sub-ts sample-ts :start '(1 1) :end '(2 12)))
                for p across (ts-points (sub-ts sample-ts :start '(3 1) :end '(3 12)))
                  do (print "da")
                  collect (funcall detector (ts-p-pos p)))
            '(7.689004308083502d-4 8.690742068634405d-4 0.0014640360422599752d0
              9.645504419952822d-4 0.002189430044882701d0 0.0022804402419548397d0
              8.653971028227403d-4 0.0021245846566718685d0 0.0021297890535286745d0
              0.003035579690776613d0 0.0010429131136164838d0 0.0017684154782838801d0))
      ))
