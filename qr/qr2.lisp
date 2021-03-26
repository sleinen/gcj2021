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
  (let ((cost 0)
        (state #\?))
    (dotimes (k (length seq) cost)
      (ecase (aref seq k)
        ((#\?))
        ((#\C)
         (ecase state
           ((#\C))
           ((#\J) (incf cost y))
           ((#\?)))
         (setq state #\C))
        ((#\J)
         (ecase state
           ((#\C) (incf cost x))
           ((#\J))
           ((#\?)))
         (setq state #\J))))))

(solve)
