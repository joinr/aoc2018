(defpackage :aocalgo
  (:use :common-lisp))
(in-package :aocalgo)

(load "observations.lisp")

(defun bounds (state)
  (loop for e in state
        as pos = (first e)
        as x = (first pos)
        as y = (second pos)
        minimizing x into x-min
        minimizing y into y-min
        maximizing x into x-max
        maximizing y into y-max
        finally
        (return (list x-min y-min x-max y-max))))

(defun y-height (state)
  (destructuring-bind (x-min y-min x-max y-max) (bounds state)
    (declare (ignore x-min x-max))
    (- y-max y-min)))

(defun evolve-point (point &optional (delta 1))
  (destructuring-bind ((x y) (vx vy)) point
    (declare (optimize (speed 3))
             (fixnum x) (fixnum y)
             (fixnum vx) (fixnum vy)
             (fixnum delta))
    (list
     (list (the fixnum (+ x (* delta  vx)))
           (the fixnum (+ y (* delta vy))))
     (list vx vy))))

(defun evolve-state (state &optional (delta 1))
  (mapcar (lambda (p) (evolve-point  p delta)) state))


(defun deriv (f x)
  (declare (function f))
  (- (funcall f x) (funcall f (+ 1 x))))

(defun height-at-time-t (x)
  (y-height (evolve-state *observations* x)))

(defun dumb-range (l r)
  (loop for i from l to (- r 1) collect i))

(defun render (state])
  (let [occupied (into {}
                       (for [[p v] state] [p "#"]))
    [[xmin ymin] [xmax ymax]] (bounds state)]
    (doseq [y (range ymin ymax)]
           (doseq [x (range xmin xmax)]
                  (print (get occupied [x y] ".")))
           (println ""))))

(defun render (state)
  (let ((occupied (let ((ht (make-hash-table :test 'equal)))
                    (loop for (p . v) in state
                          do  (setf (gethash p ht) "#")
                          finally (return ht)))))
    (destructuring-bind (x-min y-min x-max y-max) (bounds state)
      (dolist (y (dumb-range y-min (1+ y-max)))       
        (format t "~{~a~}~%" (mapcar (lambda (x)  (gethash (list x y) occupied ".")) (dumb-range x-min (1+ x-max))))
        )
      )))


(time
 (let ((best-time  (floor (funcall height-at-time-t 0)
                          (deriv height-at-time-t 0))))
   ;; We will reach the best time this many steps after the start, so
   ;; This solves the problem in 4 calls total to evolve-state,
   ;; one of which could be elided if we really cared.
   (render (evolve-state *observations* best-time))))
