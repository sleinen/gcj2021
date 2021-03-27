;;; Google Code Jam 2021, Qualification Round, Problem 4: Median Sort

(defvar *debug-oracle*)
(setq *debug-oracle* t)
(setq *debug-oracle* nil)
(defvar *debug-stat*)
(setq *debug-stat* t)
(setq *debug-stat* nil)

(defun solve (&optional setno)
  (if setno
      (let ((p (sb-ext:run-program "/usr/local/bin/python3" (list "local_testing_tool.py" (format nil "~D" setno))
                                   :input :stream :output :stream :wait nil)))
        (assert p)
        (unwind-protect
             (solve-with-streams
              (sb-ext:process-output p)
              (sb-ext:process-input p))
          (sb-ext:process-close p)))
      (solve-with-streams *standard-input* *standard-output*)))

(defun solve-with-streams (i o)
  (let ((ncase (read i))
        (N (read i))
        (Q (read i)))
    (catch 'solve-exit
      (dotimes (caseno ncase)
        (solve-case caseno N Q i o)))))

(defstruct (query-engine
            (:conc-name qe-))
  (c1 nil)
  (c2 nil)
  (in nil)
  (out nil)
  (qc 0))

(defun make-cache ()
  (make-hash-table :test #'equal))

(defun iota (N)
  (let ((result (make-array (list N))))
    (dotimes (k N result)
      (setf (aref result k) (+ k 1)))))

(defun solve-case (caseno N Q in out)
  (let ((c1 (make-cache))
        (c2 (make-cache))
        (cand (iota N)))
    (let ((qe (make-query-engine :in in :out out :c1 c1 :c2 c2)))
      (let ((solution (bisort-list cand qe)))
        (format out "~{~^ ~D~}~%" (coerce solution 'list))
        (when *debug-oracle*
          (format *trace-output* "SUBMITTING SOLUTION: ~{~^ ~D~}~%" (coerce solution 'list)))
        (when *debug-stat*
          (format *trace-output* "  stats: qc ~D c1 ~D c2 ~D~%"
                  (qe-qc qe)
                  (hash-table-count (qe-c1 qe))
                  (hash-table-count (qe-c2 qe))))
        (finish-output out)
        (let ((response (read in)))
          (assert (= response 1)))))))

(defun bisort-list (cand qe)
  (if (<= (length cand) 2)              ;always bisorted
      cand
      (multiple-value-bind (a b qe)
          (find-extremes cand qe)
        (let ((cand (remove-if #'(lambda (x) (or (= x a) (= x b))) cand)))
          (let ((qe2 (make-query-engine :in (qe-in qe)
                                       :out (qe-out qe)
                                       :c1 (qe-c2 qe)
                                       :c2 (forget-extremes (qe-c2 qe) a b))))
            (let ((result (bisort-list-in-context a cand b qe2)))
              (incf (qe-qc qe) (qe-qc qe2))
              result))))))

(defun bisort-list-in-context (a cand b qe)
  (let ((sorted (bisort-list cand qe)))
    (let ((ax (aref sorted 0))
          (bx (aref sorted (1- (length sorted)))))
      (if (well-ordered4 a ax bx b qe)
          (concatenate 'simple-vector (vector a) sorted (vector b))
          (concatenate 'simple-vector (vector a) (nreverse sorted) (vector b))))))

(defun well-ordered4 (a ax bx b qe)
  (if (gethash (sort (list a ax bx) #'<) (qe-c1 qe))
      (= (query-median-3 a ax bx qe) ax)
      (= (query-median-3 ax bx b qe) bx))
  #+FOO
  (let ((c1 (qe-c1 qe)))
    (if (gethash (sort (list a ax bx) #'<) c1)
        (query-median-3 a ax bx qe)
        (query-median-3 ax bx b qe))))

(defun forget-extremes (c a b)
  (declare (ignore a b))
  c)

(defun query-median-3 (a b c qe)
  (let ((query (sort (list a b c) #'<)) ;; optimization: sort arguments.
        ;; The order of arguments doesn't influence the result.
        ;; This saves us some queries.

        (c1 (qe-c1 qe)) (c2 (qe-c2 qe)))
    (or (gethash query c1)
        (setf (gethash query c1)
              (setf (gethash query c2)
                    (progn
                      (format (qe-out qe) "~D ~D ~D~%" a b c)
                      (when *debug-oracle*
                        (format *trace-output* "median? ~D ~D ~D~%" a b c))
                      (finish-output (qe-out qe))
                      (incf (qe-qc qe))
                      (let ((result (read (qe-in qe))))
                        (assert (/= result -1))
                        result)))))))

(defun find-extremes (cand qe)
  (let ((cand (prune-medians cand qe))
        (c1 (qe-c1 qe))
        (c2 (qe-c2 qe)))
    (do ((n (length cand) (length cand))
         (k 0 (mod (+ k 1) (length cand))))
        ((<= n 2) (values (aref cand 0) (aref cand 1) qe))
      (let ((median (query-median-3
                     (aref cand k)
                     (aref cand (mod (+ k 1) (length cand)))
                     (aref cand (mod (+ k 2) (length cand)))
                     qe)))
        (setq cand (remove median cand))))))

(defun prune-medians (cand qe)
  (let ((c1 (qe-c1 qe)))
    (maphash #'(lambda (key value)
                 (let ((a (first key)) (b (second key)) (c (third key)))
                   (if (and (find a cand) (find b cand) (find c cand))
                       (setq cand (remove value cand)))))
             c1)
    cand))

(solve)
