(in-package :cl-user)

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
    (- y-max y-min)))

(defun evolve-point (point)
  (destructuring-bind ((x y) (vx vy)) point
    (declare (optimize (speed 3))
             (fixnum x) (fixnum y)
             (fixnum vx) (fixnum vy))
    (list
     (list (the fixnum (+ x vx))
           (the fixnum (+ y vy)))
     (list vx vy))))

(defun evolve-state (state)
  (mapcar #'evolve-point state))

(defun smallest-state (start)
  (loop
    as a = start then b
    as b = (evolve-state a) then (evolve-state a)
    as y-a = (y-height a)
    as y-b = (y-height b)
    while (<= y-b y-a)
    finally (return a)))

(time (smallest-state *observations*))

;; To compute this:
;; Evaluation took:
;;   0.378 seconds of real time
;;   0.380000 seconds of total run time (0.380000 user, 0.000000 system)
;;   [ Run times consist of 0.032 seconds GC time, and 0.348 seconds non-GC time. ]
;;   100.53% CPU
;;   377,405,830 processor cycles
;;   369,786,496 bytes consed
