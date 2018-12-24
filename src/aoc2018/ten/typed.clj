(ns aoc2018.ten.typed
  (:require [aoc2018.ten :as ten]))

;;In this series of optimizations, we
;;move away from using vectors, and
;;start using structural types.
;;Specifically, we define records
;;with typed fields (primitive longs)
;;and use them to store our observations.

;;Similar to the faster namespace,
;;we use direct field access to
;;quickly read the type, as well as
;;using the constructor method
;;(vs. the more idiomatic (->longcell ...)
;;function clojure provides for us)
;;for maximum speed.

;;This solution is still entirely
;;persistent, only now instead of
;;allocating a 32-wide object array
;;for every vector (including the small
;;2-elements nested vectors), we
;;now allocate an object with 4
;;primitive longs.
;;Reading the fields happens in constant
;;time, and is computed via offset,
;;rather than through multiple
;;calls to nth.

;;Another major optimization is
;;using a similar record for
;;our bounds type, extrema,
;;which similarly provides
;;significant speed of construction
;;and field access.

;;For clarity (and performance),
;;we move the evolve-point function
;;into a protocol, which we can
;;inline in our record type to
;;get even more speed.  We also
;;invoke the inlined method directly,
;;eschewing protocol dispatch time for
;;a slight speed boost.

(defprotocol IEvolve
  (evolve-point [this]))

;;so, custom types are faster...
(defrecord longcell [^long x ^long y ^long vx ^long vy]
  IEvolve
  (evolve-point [this] (longcell. (unchecked-add x vx)
                                  (unchecked-add y vy)
                                  vx
                                  vy)))

(defn observations->cells [xs]
  (vec 
   (for [[[^long x ^long y] [^long vx ^long vy]] xs]
     (->longcell x y vx vy))))

;;our aggregate bounds accumulator.
(defrecord extrema [^long xmin ^long xmax ^long ymin ^long ymax])

;;40 ms vs 135ms @ 10k iterations
;;Note: we can get around 
(defn bounds [state]
  (reduce (fn [^extrema acc ^longcell pv]
            (let [x     (.-x pv)
                  y     (.-y pv)
                  xmin  (.-xmin acc)
                  xmax  (.-xmax acc)
                  ymin  (.-ymin acc)
                  ymax  (.-ymax acc)
                  ]
              (extrema. (min xmin x)
                        (max xmax x)
                        (min ymin y)
                        (max ymax y))))
          (let [^longcell p (first state)
                x (.-x p)
                y (.-y p)]
            (extrema. x x y y)) (rest state)))

;;neglibile improvement
(defn y-height [^clojure.lang.Indexed state]
  (let [^extrema bnds (bounds state)]
    (unchecked-subtract
     ^long (.-ymax bnds)
     ^long (.-ymin bnds))))

;;slight modification to use the inlined method.
(def evolve-state
  #(into [] (map (fn [^longcell c]
                   (.evolve-point c))) %))

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

(defn cells->observations [xs]
  (vec (for [{:keys [x y vx vy]} xs]
         [[x y] [vx vy]])))


(comment
  (let [xs (observations->cells ten/observations)]
    ;;seq version is faster now than hinted
    ;;vector versions.
    (time (do (shortest-state xs) nil))
    ;;"Elapsed time: 153.765653 msecs"

    ;;reduce version is on par with loop still.
    (time (do (shortest-state-reduce xs) nil))
    ;;"Elapsed time: 110.033865 msecs"


    ;;loop version with records only 5x slower than mutable
    ;;array version.
    (time (do (shortest-state-loop xs) nil))
    ;;"Elapsed time: 105.229515 msecs"
))


;;Experiments and observations:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;this is still in the 140ms range for some reason....
#_(defn bounds [state]
  (reduce (fn [^extrema acc ^clojure.lang.Indexed pv]
            (let [^clojure.lang.Indexed p (.nth pv 0)
                  x (.nth p 0)
                  y (.nth p 1)
                  xmin  (.-xmin acc)
                  xmax  (.-xmax acc)
                  ymin  (.-ymin acc)
                  ymax  (.-ymax acc)
                  ]
              (extrema. (min xmin x)
                        (max xmax x)
                        (min ymin y)
                        (max ymax y))))
          (let [[x y]  (ffirst state)]
            (extrema. x x y y)) (rest state)))
