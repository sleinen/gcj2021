;;; Google Code Jam 2021, Qualification Round, Problem 2: Moons and Umbrellas

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (read in))
    (format t "Case #~D: " (+ caseno 1))
    (solve-case in)))

(defun solve-case (in)
  (let ((x (read in)) (y (read in)) (seq (read-line in)))
    ;(warn "x ~D y ~D seq ~S" x y seq)
    (let ((cost (min-cost x y seq)))
      (format t "~D~%" cost))))

(defun min-cost (x y seq)
  (let ((first (char seq 0)))
    (min-cost-1 x y 1 seq (ecase first ((#\C #\?) 0) ((#\J) nil)) (ecase first ((#\J #\?) 0) ((#\C) nil)))))

(defun minq+ (cs x js y)
  (if cs
      (if js (min (+ cs x) (+ js y))
          (+ cs x))
      (+ js y)))

(defun min-cost-1 (x y k seq cs js)
  (if (>= k (length seq))
      (minq+ cs 0 js 0)
      (ecase (char seq k)
        ((#\C) (min-cost-1 x y (+ k 1) seq (minq+ cs 0 js y) nil))
        ((#\J) (min-cost-1 x y (+ k 1) seq nil (minq+ cs x js 0)))
        ((#\?) (min-cost-1 x y (+ k 1) seq (minq+ cs 0 js y) (minq+ cs x js 0))))))

(solve)
