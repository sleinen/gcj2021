;;; Google Code Jam 2021, Qualification Round, Problem 1: Reversort

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (read in))
    (format t "Case #~D: " (+ caseno 1))
    (solve-case in)))

(defun solve-case (in)
  (let ((N (read in)))
    (let ((M (make-array (list N))))
      ;; read array
      (dotimes (i N)
        (setf (aref M i) (read in)))
      (let ((cost
              (do ((i 0 (+ i 1))
                   (cost 0))
                  ((>= i (- (length M) 1)) cost)
                (let ((min (reduce #'min M :start i)))
                  (let ((minpos (position min M :start i)))
                    (incf cost (+ (- minpos i) 1))
                    (setf (subseq M i (+ minpos 1))
                          (reverse (subseq M i (+ minpos 1)))))))))
        (format t "~D~%" cost)))))

(solve)
