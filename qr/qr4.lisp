;;; Google Code Jam 2021, Qualification Round, Problem 4: Median Sort

(defvar *debug-oracle*)
(setq *debug-oracle* t)
(setq *debug-oracle* nil)

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
    (let ((solution (bisort-list cand in out c1 c2)))
      (format out "~{~^ ~D~}~%" (coerce solution 'list))
      (when *debug-oracle*
        (format *trace-output* "SUBMITTING SOLUTION: ~{~^ ~D~}~%" (coerce solution 'list)))
      (finish-output out)
      (let ((response (read in)))
        (assert (= response 1))))))

(defun bisort-list (cand in out c1 c2)
  (if (<= (length cand) 2)              ;always bisorted
      cand
      (multiple-value-bind (a b c1 c2)
          (find-extremes cand in out c1 c2)
        (let ((cand (remove-if #'(lambda (x) (or (= x a) (= x b))) cand)))
          (bisort-list-in-context a cand b in out c2 (forget-extremes c2 a b))))))

(defun bisort-list-in-context (a cand b in out c1 c2)
  (let ((sorted (bisort-list cand in out c1 c2)))
    (let ((ax (aref sorted 0))
          (bx (aref sorted (1- (length sorted)))))
      (let ((median (query-median a ax bx in out c1 c2)))
        (cond ((= median a) (error "This can't happen"))
              ((= median ax) (concatenate 'simple-vector (vector a) sorted (vector b)))
              ((= median bx) (concatenate 'simple-vector (vector a) (nreverse sorted) (vector b))))))))

(defun forget-extremes (c a b)
  (declare (ignore a b))
  c)

(defun query-median (a b c in out c1 c2)
  (let ((query (list a b c)))
    (or (gethash query c1)
        (setf (gethash query c1)
              (setf (gethash query c2)
                    (progn
                      (format out "~D ~D ~D~%" a b c)
                      (when *debug-oracle*
                        (format *trace-output* "median? ~D ~D ~D~%" a b c))
                      (finish-output out)
                      (let ((result (read in)))
                        (assert (/= result -1))
                        result)))))))

(defun find-extremes (cand in out c1 c2)
  (let ((cand (prune-medians cand c1 c2)))
    (do ((n (length cand) (length cand))
         (k 0 (mod (+ k 1) (length cand))))
        ((<= n 2) (values (aref cand 0) (aref cand 1) c1 c2))
      (let ((median (query-median
                     (aref cand k)
                     (aref cand (mod (+ k 1) (length cand)))
                     (aref cand (mod (+ k 2) (length cand))) in out c1 c2)))
        (setq cand (remove median cand))))))

(defun prune-medians (cand c1 c2)
  (declare (ignore c2))
  (maphash #'(lambda (key value)
               (let ((a (first key)) (b (second key)) (c (third key)))
                 (if (and (find a cand) (find b cand) (find c cand))
                     (setq cand (remove value cand)))))
           c1)
  cand)

(solve)
