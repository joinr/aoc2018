(ns aoc2018.ten.faster
  (:require [aoc2018.ten :as ten]))

;;The majority of optimizations here are
;;going to unroll our destructuring,
;;instead using type-hints on the vector
;;for clojure.lang.Indexed, an then
;;use direct method access
;;(via the .nth form on the hinted
;; object) to generate more efficient
;;bytecode and save on function calls.

;;We also use unchecked-math...
;;but I put in unchecked-add|subtract
;;variants for simplicity.

;;unroll using
;;~36-38ms vs 50ms @ 10^6 iterations.
(defn evolve-point [^clojure.lang.Indexed pv]
  (let [^clojure.lang.Indexed p  (.nth pv 0)
        ^clojure.lang.Indexed v  (.nth pv 1)
        x  (.nth p 0)
        y  (.nth p 1)
        vx (.nth v 0)
        vy (.nth v 1)]
    [[(unchecked-add  ^long x ^long vx)
      (unchecked-add  ^long y ^long vy)]
     [vx vy]]))

;;134 ms vs 192ms @ 10k iterations
(defn bounds [state]
  (reduce (fn [^clojure.lang.Indexed acc ^clojure.lang.Indexed pv]
            (let [^clojure.lang.Indexed p (.nth pv 0)
                  x (.nth p 0)
                  y (.nth p 1)
                  xmin  (.nth acc 0)
                  xmax  (.nth acc 1)
                  ymin  (.nth acc 2)
                  ymax  (.nth acc 3)
                  ]
              [(min xmin x)
               (max xmax x)
               (min ymin y)
               (max ymax y)]))
          (let [[x y] (ffirst state)]
            [x x y y]) (rest state)))

;;neglibile improvement
(defn y-height [^clojure.lang.Indexed state]
  (let [^clojure.lang.Indexed bnds (bounds state)
        ymin (.nth bnds 2)
        ymax (.nth bnds 3)]
  (unchecked-subtract ^long ymax ^long ymin)))


;;same
(def evolve-state #(into [] (map evolve-point) %))
  
;;same
;;no longer hold onto the head.
(defn state-seq
  "state-seq is an infinite sequence of observations,
       each separated by 1 second's worth of evolution."
  [xs]
  (iterate evolve-state xs))


;;new, we consolidate the map and predicate
;;functions into a useful predicate.
(defn before-growth [^clojure.lang.ISeq ab]
  (let [a        (.first ab)
        height-a (y-height a)
        b        (.first (.next ab))
        height-b (y-height b)]
    (when (> height-b height-a) a)))

(defn shortest-state
  "The idea here is to filter through successive pairs of evolved states,
      looking for the first pair where the second element is 'less compact'
      than the first one."
  [xs]
  (->> (state-seq xs)       
       (partition 2 1)
       (some before-growth)))

(defn shortest-state-reduce
  "This is a reduce based variant that's even
   simpler than loop/recur.  It will terminate
   early as well."
  [xs]
  (reduce (fn [[a height-a]  _]
            (let [b        (evolve-state a)
                  height-b (y-height b)]
              (if (> height-b height-a)
                (reduced a) ;;a was smaller.
                [b height-b])))
          [xs (y-height xs)]
          (range)))
             

(defn shortest-state-loop  
  "We eschew the intermediate seqs and conform closer to the CL
       loop-based implementation."
  [xs]
  (let [initb (evolve-state xs)]
    (loop [a        xs
           height-a (y-height a)
           b        initb
           height-b (y-height initb)]
      (if (<= height-b height-a)
        (let [nxt (evolve-state b)]
          (recur b height-b nxt (y-height nxt)))
        a))))



;;This is now as fast as the
;;looped, non-hinted version from before.
(time
 (shortest-state ten/observations))
;;"Elapsed time: 442.000849 msecs"


;;This is now in the ballpark of CL
;;on my hardware (CL is at 238 to 280)

;;Clojure comes in around 283 to 296
(time
 (shortest-state-loop ten/observations))
;;"Elapsed time: 287.974153 msecs"


;;The reduce-based version is about
;;as good as loop (typical)
(time 
 (shortest-state-reduce ten/observations))
;;"Elapsed time: 293.412182 msecs"


;;EXPERIMENTS / MICROBENCHMARKS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Just some microbenchmarks to see
;;if allocating intermediate vectors
;;is costly, and if there are other
;;options for optimization:

(comment 
;;Let's avoid intermediate vectors...
;;14ms...
(time
 (let [x 1 y 2]
   (dotimes [i 1000000] [x y] )))


;;13 ms
(defn pair [x y]
  (.cons  (clojure.lang.PersistentList. x ) y ))

(time
 (let [x 1 y 2] (dotimes [i 1000000] (pair x y))))


;;for the record, if we use structural types
;;with direct field access, we get dis...
(defrecord position [x y])

;;9.6 ms, better!
(time
 (let [x 1 y 2]
   (dotimes [i 1000000] (position. x y))))
)


;;166 ms
#_(defn bounds [state]
  (transduce (drop 1)
             (completing
              (fn [^clojure.lang.Indexed acc ^clojure.lang.Indexed pv]
                (let [^clojure.lang.Indexed p (.nth pv 0)
                      x (.nth p 0)
                      y (.nth p 1)
                      xmin  (.nth acc 0)
                      xmax  (.nth acc 1)
                      ymin  (.nth acc 2)
                      ymax  (.nth acc 3)
                      ]
                  [(min xmin x)
                   (max xmax x)
                   (min ymin y)
                   (max ymax y)])))
              (let [[x y] (ffirst state)]
                [x x y y]) state))


;;slowe than reduce!
#_(defn bounds-looped [^clojure.lang.PersistentVector state]
  (let [bound  (count state)]
    (loop [xmin   Long/MAX_VALUE
           xmax   Long/MIN_VALUE
           ymin   Long/MAX_VALUE
           ymax   Long/MIN_VALUE
           chunk  (chunk-first (seq state))
           chunks (chunk-rest (seq state))
           n      0
           idx    0]
      (cond (== n bound) [xmin xmax ymin ymax]
            (< idx 32)             
            (let [^clojure.lang.Indexed pv (.nth ^clojure.lang.ArrayChunk chunk idx)
                  ^clojure.lang.Indexed p  (.nth pv 0)
                  x (.nth p 0)
                  y (.nth p 1)]
              (recur 
               (min xmin x)
               (max xmax x)
               (min ymin y)
               (max ymax y)
               chunk
               chunks
               (unchecked-inc n)
               (unchecked-inc idx)))
            :else
            (recur xmin
                   xmax
                   ymin
                   ymax
                   (chunk-first chunks)
                   (chunk-rest  chunks)
                   n
                   0)))))
    
;;no benefits really.
#_(defn evolve-state2 [xs]
  (persistent!
   (reduce (fn [^clojure.lang.PersistentVector$TransientVector acc x]
             (.conj acc (evolve-point x)))
           (transient xs) xs)))
