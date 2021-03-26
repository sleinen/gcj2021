;;; Google Code Jam 2021, Qualification Round, Problem 1: Reversort

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (read in))
    (format t "Case #~D: " (+ caseno 1))
    (solve-case in)))

(defun solve-case (in)
  (let ((n (read in)) (cost (read in)))
    (let ((solution (construct-case n cost)))
      (if solution
          (format t "~{~A~^ ~}~%" (mapcar #'1+ (coerce solution 'list)))
          (format t "IMPOSSIBLE~%")))))

(defun construct-case (n cost)
  (construct-case-0 n (+ (- cost n) 1)))

(defun construct-case-0 (n cost)
  (and (>= cost 0) (< cost (/ (* n (- n 1)) 2))
       (construct-case-1 n cost))
  )

(defun construct-case-1 (n cost)
  (let ((positions (make-array (list n) :initial-element 0)))
    (do ((k 0 (+ k 1)))
        ((or (= cost 0) (>= k (- n 1))))
      (let ((pos-k (min cost (- n k 1))))
        (setf (aref positions k) pos-k)
        (decf cost pos-k)))
    (let ((values (make-array (list n))))
      (dotimes (k n)
        (setf (aref values k) k))
      (do ((k (- n 1) (- k 1)))
          ((< k 0))
        (let ((pos-k (aref positions k)))
          (unless (zerop pos-k)
            (setf (subseq values k (+ k pos-k 1))
                  (reverse (subseq values k (+ k pos-k 1)))))))
      values)))

(defun cost (M)
  (do ((i 0 (+ i 1))
       (cost 0))
      ((>= i (- (length M) 1)) cost)
    (let ((min (reduce #'min M :start i)))
      (let ((minpos (position min M :start i)))
        (incf cost (+ (- minpos i) 1))
        (setf (subseq M i (+ minpos 1))
              (reverse (subseq M i (+ minpos 1))))))))

;;; Exploratory programming time...
;;;
;;; What is the range of possible cost values for sequences of length N?
;;;
;;; We can go through all N! lists and just collect the different values.
;;;
(defun map-permut (n fn)
  (let ((iota (make-array n)))
    (dotimes (k (length iota))
      (setf (aref iota k) k))
    (map-permut-1 #() iota fn)))

(defun map-permut-1 (prefix rest fn)
  (if (zerop (length rest))
      (funcall fn prefix)
      (dotimes (pos (length rest))
        (map-permut-1 (concatenate 'simple-vector prefix (vector (aref rest pos)))
                      (concatenate 'simple-vector
                                   (subseq rest 0 pos)
                                   (subseq rest (+ pos 1)))
                      fn))))

(defun result-set (n)
  (let ((result '()))
    (map-permut n #'(lambda (x) (pushnew (cost x) result)))
    (let ((min (reduce #'min result))
          (max (reduce #'max result)))
      (do ((x min (+ x 1)))
          ((> x max))
        (assert (member x result)))
      (values min max))))

;;; It turns out that the minimum is always N-1,
;;;
;;; and the maximum, maxcost(N) is
;;;
;;;     0 for N=0,
;;;     maxcost(N-1)+N for N > 0.
;;;
;;; For the moment, we "just assume" that for all values from
;;; mincost(N) to maxcost(N) (inclusive), there is an ordering that
;;; has that cost.  Hopefully this will become clear once we can find
;;; a construction!

(solve)
