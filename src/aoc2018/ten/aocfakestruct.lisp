(defpackage :aocopt
  (:use :common-lisp))
(in-package :aocopt)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(load "observations.lisp")

(defstruct longcell
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (vx 0 :type fixnum)
  (vy 0 :type fixnum))

(defstruct extrema
  (x-min most-positive-fixnum :type fixnum)
  (x-max most-negative-fixnum :type fixnum)
  (y-min most-positive-fixnum :type fixnum)
  (y-max most-negative-fixnum :type fixnum))


;;from https://stackoverflow.com/questions/45550984/can-sbcl-arrays-have-typed-arrays-as-elements
;;bummer!
;;Limitations of Common Lisp arrays

;; Thus Common Lisp does not have optimized storage layout for arrays of structures, vectors, CLOS objects,
;; etc. Since there is a pointer for each element stored, access always needs an indirection and there is
;; nothing that will guarantee that these objects are stored in linear order in memory.
;; Stored in linear order are the pointers to them in the array.

(defun cell-array (xs)
  (make-array (length xs) :element-type 'longcell :initial-contents xs))

;; (defun observations->cells (xs)
;;   (cell-array (mapcar (lambda (p)
;;                         (destructuring-bind ((x y) (vx vy)) p
;;                           (make-longcell :x x :y y :vx vx :vy vy)))
;;                       xs)))

(defun observations->cells (xs)
  (mapcar (lambda (p)
            (destructuring-bind ((x y) (vx vy)) p
              (make-longcell :x x :y y :vx vx :vy vy)))
          xs))


;;simple-vector unhappy...

(defparameter cells
  (observations->cells *observations*))

;;try this using structs....
;;about 15ms at 10k iterations.
(defun bounds (state)
  (loop for e in state
        as x   = (longcell-x e)
        as y   = (longcell-y e)
        minimizing x into x-min
        minimizing y into y-min
        maximizing x into x-max
        maximizing y into y-max
        finally
        (return
          (make-extrema :x-min x-min :x-max  x-max :y-min y-min :y-max y-max)
                                        ;(list x-min  y-min x-max y-max )
          )))

(defun update-bounds (acc cl)
  (declare (extrema acc)
           (longcell cl))
  (with-slots (x-min x-max y-min y-max) acc
    (with-slots (x y) cl
      (make-extrema :x-min (min x-min x)
                    :x-max (max x-max x) 
                    :y-min (min y-min y)
                    :y-max (max y-max y)))))

;;slower persistent version.  ~6 times slower.
;;90ms A 10k iterations
(defun bounds-reduce (state)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (reduce #'update-bounds
          state :initial-value (make-extrema)))

;;minor performance savings, gets us down to 0.125 sometimes...
;; (defun y-height (state)
;;   (destructuring-bind (x-min y-min x-max y-max) (bounds state)
;;     (declare (ignore x-min x-max)
;;              (fixnum y-max)
;;              (fixnum y-min))
;;     (the fixnum  (- y-max y-min))))

(defun y-height (state)
  (let ((bnds (bounds state)))
    (with-slots (y-min y-max) (the extrema bnds)
      (declare (fixnum y-max)
               (fixnum y-min))
      (the fixnum  (- y-max y-min)))))

;;using structs gets us to 0.140 seconds@
;;slightly beating clojure record implementation.
(defun evolve-point (point)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (longcell point))
  (with-slots (x y vx vy) point    
    (make-longcell
     :x (the fixnum (+ x vx))
     :y (the fixnum (+ y vy))
     :vx vx
     :vy vy )))

(defun smallest-state (start)
  (loop
    as a = start then b
    as b = (evolve-state a) then (evolve-state a)
    as y-a = (y-height a)
    as y-b = (y-height b)
    while (<= y-b y-a)
    finally (return a)))
