(ns aoc2018.ten.parallel
  (:require [aoc2018.ten :as ten]
            [aoc2018.ten.opt :as opt]
            ))

;;parallel bounds computation.
(defn bounds [^longs state from bound]
  (let [bound (long bound)
        x     (aget state 0)
        y     (aget state 1)]
    (loop [idx (long from)
           xmin x
           xmax x
           ymin y
           ymax y]
      (if (== idx bound)
        [xmin xmax ymin ymax]
        (let [x (aget state  idx)
              y (aget state (unchecked-inc idx))]
          (recur (unchecked-add idx 4)
                 (min xmin x)
                 (max xmax x)
                 (min ymin y)
                 (max ymax y)))))))

(defn parallel-bounds [^longs state]
  (let [buckets (

;;parallel evolution.
(defn evolve-state!
  ([^longs xs ^longs ys]
   (let [bound (count xs)]
     (loop [idx 0]
       (if (== idx bound)
         ys
         (let [y-idx (unchecked-inc idx)]
           (do (aset ys idx
                     (unchecked-add (aget xs idx) (aget xs (unchecked-add idx 2)))) ;;x
               (aset ys y-idx
                     (unchecked-add (aget xs y-idx) (aget xs (unchecked-add y-idx 2)))) ;;y
               (recur (unchecked-add idx 4))))))))
  ([xs] (evolve-state! xs xs)))
