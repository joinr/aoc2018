(ns aoc2018.ten.opt
  (:require [aoc2018.ten :as ten]))

;;recode the implementation to use an array
;;packing the [[x y] [vx vy]] into
;;[x y vx vy x1 y1 vx1 vy1 ...]
(defn observation-array [xs]
  (->> (for [[[x y] [vx vy]] xs]
         [x y vx vy])
       (apply concat)
       (long-array)))

(defn bounds [^longs state]
  (let [bound (count state)
        x     (aget state 0)
        y     (aget state 1)]
    (loop [idx 0
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

;;changed to a flat vector instead of nested [] [] 
(defn y-height [state]
  (let [[xmin xmax ymin ymax] (bounds state)]
    (- ymax ymin)))

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

(defn shortest-state-loop  
  [^longs xs]
  (let [^longs xs (aclone ^longs xs)
        initb (evolve-state! (aclone ^longs xs))]
    (loop [a        xs
           height-a (y-height a)
           b        initb
           height-b (y-height initb)]
      (if (> height-b height-a)
        a
        (let [nxt (evolve-state! b a)]
          (recur b height-b nxt (y-height nxt)))
        ))))


;;coerce our answer back into something we can compare,
;;a nested vector of the form [[[x y] [vx vy]] ...] 
(defn array->obs [^longs xs]
  (vec (for [[x y vx vy] (partition 4 xs)]
         [[x y] [vx vy]])))

(comment
  (time
   (shortest-state-loop
    (observation-array aoc2018.ten/observations)))
;;"Elapsed time: 19.728715 msecs"
  )


;;looks correct!
(defn result= []
  (= (-> aoc2018.ten/observations
         observation-array
         shortest-state-loop
         array->obs)
     (ten/shortest-state-loop ten/observations)))

;;aoc2018.ten.opt> (result=)
;;true
