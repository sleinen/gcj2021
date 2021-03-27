;;; Google Code Jam 2021, Qualification Round, Problem 5: Cheating Detection

(defun solve (&optional (in *standard-input*))
  (let ((ncases (read in))
        (p (read in)))
    (declare (ignore p))
    (dotimes (caseno ncases)
      (format t "Case #~D: " (+ caseno 1))
      (solve-case in))))

(defun solve-case (in &key (np 100) (nq 10000))
  (let ((answers (make-array (list np nq) :element-type 'bit)))
    (dotimes (pk np)
      (let ((pa (read-line in)))
        (assert (= (length pa) nq))
        (dotimes (qk nq)
          (setf (aref answers pk qk)
                (parse-integer pa :start qk :end (+ qk 1))))))
    (let ((q-rank (make-array (list nq))))
      (dotimes (qk nq)
        (setf (aref q-rank qk)
              (let ((count 0))
                (dotimes (pk np (cons qk count))
                  (incf count (aref answers pk qk))))))
      (setq q-rank (map 'simple-vector #'car (sort q-rank #'< :key #'cdr)))
      (let ((players (make-array (list np))))
        (dotimes (pk np)
          (setf (aref players pk)
                (let ((pa (make-array (list nq) :element-type 'bit)))
                  (dotimes (qk nq pa)
                    (setf (aref pa qk)
                          (aref answers pk (aref q-rank qk)))))))
        ;;
        ;; players contains, for each player, the correct-response
        ;; bits ordered by estimated strength of question.
        ;;
        (let ((ps (make-array (list np)))
              (high-deviation (make-array (list np))))
          (dotimes (pk np)
            (let ((ct-all (count 1 (aref players pk)))
                  (ct-high (count 1 (aref players pk) :start 0 :end (/ (length (aref players pk)) 2)))
                  (ct-low (count 1 (aref players pk) :start (/ (length (aref players pk)) 2))))
              ;;(warn "player ~D: overall ~5,3F low ~5,3F high ~5,3F" pk ct-all ct-low ct-high)
              (let ((es-all (estimated-strength (/ (+ 0d0 ct-all) nq) -3d0 3d0)))
                (let ((exp-all (expected-correct es-all -3d0 3d0))
                      (exp-high (expected-correct es-all 0d0 3d0))
                      (exp-low (expected-correct es-all -3d0 0d0)))
                  #+foo
                  (warn "player ~D: overall ~6,3F exp-low ~5,3F low ~5,3F exp-high ~5,3F high ~5,3F"
                        pk es-all
                        exp-low (/ (* 1d0 ct-low) (/ nq 2d0))
                        exp-high (/ (* 1d0 ct-high) (/ nq 2d0)))
                  #+bar
                  (warn "player ~D: overall ~6,3F low-delta ~5,3F high-delta ~5,3F"
                        pk es-all
                        (- (/ (* 1d0 ct-low) (/ nq 2d0)) exp-low)
                        (- (/ (* 1d0 ct-high) (/ nq 2d0)) exp-high))
                  (setf (aref ps pk) es-all)
                  (setf (aref high-deviation pk) (- (/ (* 1d0 ct-high) (/ nq 2d0)) exp-high))))))
          (let ((max-high-dev (reduce #'max high-deviation)))
            (let ((maxpos (position max-high-dev high-deviation)))
              (format t "~D~%" (+ maxpos 1)))))))))

(defun f (x) (/ 1d0 (+ 1d0 (exp (- x)))))

(defun expected-correct (ps qmin qmax)
  (let ((samples 1000)
        (psum 0))
    (dotimes (s samples (/ psum samples))
      (let ((q (+ qmin (* s (/ (- qmax qmin 0d0) samples)))))
        (incf psum (f (- ps q)))))))

(defvar *est-cache* (make-hash-table :test #'equal))

(defparameter ec-gran 1000)

(defun estimated-strength (p qmin qmax)
  "Compute estimated player strength from

     P (ratio of correct questions
     Qmin (easiest question strength)
     Qmax (hardest question strength)

  The assumption is that question strength are uniformly distributed
  between these points."

  (let ((x (gethash (cons qmin qmax) *est-cache*)))
    (unless x
      (setf (gethash (cons qmin qmax) *est-cache*)
            (setq x (make-array (list ec-gran))))
      (dotimes (k ec-gran)
        (let* ((pmin (+ -3d0 (* (/ 6d0 ec-gran) k)))

               (pmax (+ pmin (/ 6d0 ec-gran))))
          ;;(warn "~6,3F - ~6,3F" pmin pmax)
          (setf (aref x k)
                (expected-correct (/ (+ pmin pmax) 2d0) qmin qmax)))))
    ;(warn "~S" x)
    (let ((pos (or (position p x :test #'<) (1- (length x)))))
      (let* ((pmin (+ -3d0 (* (/ 6d0 ec-gran) pos)))
             (pmax (+ pmin (/ 6d0 ec-gran))))
        (/ (+ pmin pmax) 2d0)))))

(solve)
