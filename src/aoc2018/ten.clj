(ns aoc2018.ten)
    
(set! *print-length* 10)
(set! *warn-on-reflection* true)

;;parsing code and stuff you left out, I improvised to
;;conform to your solution...
(def num-re #"-?[0-9]+")
(defn p-and-v [ln]
  (let [[x y vx vy] (->> ln
                         (re-seq num-re)
                         (map #(Long/parseLong %)))]
    [[x y] [vx vy]]))

;;we get 314 points in my test set.
;;returning as a vector of vectors....maybe not
;;so hot for numerics.
(def observations
  (with-open [rdr (clojure.java.io/reader "day-10-input.txt")]
    (into [] (map p-and-v) (line-seq rdr))))

;;one pass over the data instead of 4, using reduce.
(defn bounds [state]
  (reduce (fn [[xmin xmax ymin ymax] [[x y] _]]
            [(min xmin x)
             (max xmax x)
             (min ymin y)
             (max ymax y)])
          (let [[x y] (ffirst state)]
            [x x y y]) (rest state)))

;;changed to a flat vector instead of nested [] [] 
(defn y-height [state]
  (let [[xmin xmax ymin ymax] (bounds state)]
    (- ymax ymin)))

(defn evolve-point [[p v]]
  (let [[x y]   p
        [vx vy] v]
    [[(+ x vx) (+ y vy)] [vx vy]]))

;;changed to mapv to keep vector representation...
;;using into, which engages a transient vector behind the scenes
;;and avoids an intermediate sequence creation using the
;;map transducer.
(def evolve-state #(into [] (map evolve-point) %))

;;no longer hold onto the head.
(defn state-seq
  "state-seq is an infinite sequence of observations,
       each separated by 1 second's worth of evolution."
  [xs]
  (iterate evolve-state xs))

(defn shortest-state
  "The idea here is to filter through successive pairs of evolved states,
      looking for the first pair where the second element is 'less compact'
      than the first one."
  [xs]
  (->> (state-seq xs)       
       (partition 2 1)
       (map (fn [[a b]]
              [[(y-height a) a] [(y-height b) b]]))
       ;; Each entry here looks like the arg to no-longer-shrinking?
       (map (fn no-longer-shrinking? [[[height-a a] [height-b _]]]
              ;; Once B is taller than A, A is our smallest picture.
              (when (> height-b height-a) a)))
       (drop-while nil?)
       first))

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

(time
 (shortest-state observations))
;;"Elapsed time: 663.924808 msecs"

(time
 (shortest-state-loop observations))
;;"Elapsed time: 429.183943 msecs"
