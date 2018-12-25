;;from reddit post submitted by u/Alain_Picard
;; Final solution; I'm _really_ done with this problem now!  :-)
(ns aoc2018.ten.algo #_advent-of-code-2018.day-10-best
  (:require #_[advent-of-code-2018.utils :refer [lines]]
            [clojure.string :as s]
            [clojure.set :as set]))

(def parser-re #"<([\s-]*?\d+),([\s-]*?\d+)>.*?<([\s-]*?\d+),([\s-]*?\d+)>")

(defn p-and-v
  "Return the position and velocity embedded in string S."
  [s]
  (let [[match & entries] (re-find parser-re s)
        [x y vx vy] (map #(Integer/parseInt (s/trim %)) entries)]
    [[x y] [vx vy]]))

(def observations
  (with-open [rdr (clojure.java.io/reader "day-10-input.txt")]
    (map p-and-v (doall (line-seq rdr)))))

(defn bounds [state]
  (reduce (fn [[[xmin ymin] [xmax ymax]] [[x y] _]]
            [[(min xmin x) (min ymin y)]
             [(max xmax x) (max ymax y)]])
          [[Integer/MAX_VALUE Integer/MAX_VALUE] [Integer/MIN_VALUE Integer/MIN_VALUE]]
          state))

(defn ranges [state]
  (let [[[xmin ymin] [xmax ymax]] (bounds state)]
    [(- xmax xmin) (- ymax ymin)]))

(defn y-height [state]
  (let [[[xmin ymin] [xmax ymax]] (bounds state)]
    (- ymax ymin)))

(use 'clojure.pprint)
(defn render [state]
  (let [occupied (into {}
                       (for [[p v] state] [p "#"]))
        [[xmin ymin] [xmax ymax]] (bounds state)]
    (doseq [y (range ymin (inc ymax))]
      ;(doseq [x (range xmin (inc xmax))]
      (println (clojure.string/join 
                 (map #(get occupied [% y] ".") (range xmin (inc xmax))))))))

(defn render [state]
  (let [occupied (into {}
                       (for [[p v] state] [p "#"]))
        [[xmin ymin] [xmax ymax]] (bounds state)]
    (println 
     (clojure.string/join \newline
       (for [y (range ymin (inc ymax))]
         (->> (range xmin (inc xmax))
              (map #(get occupied [% y] "."))
              clojure.string/join
              ))))))
  
(defn evolve-point
  ([x] (evolve-point 1 x))
  ([delta [^ints p ^ints v]]
   (let [[x y] p
         [vx vy] v]
     [[(+ x (* delta vx)) (+ y (* delta vy))]
      [vx vy]])))

(defn evolve-state
  ([state] (evolve-state state 1))
  ([state delta]
   (map (partial evolve-point delta) state)))

(defn deriv [f x]
  (- (f x) (f (+ 1 x))))

(def height-at-time-t #(y-height (evolve-state observations %)))

(deriv height-at-time-t 0) ; 10
(deriv height-at-time-t 100) ; 10 ; of course, because the velocities can never change

(time
 (let [best-time (quot (height-at-time-t 0)
                       (deriv height-at-time-t 0))]
   ;; We will reach the best time this many steps after the start, so
   ;; This solves the problem in 4 calls total to evolve-state,
   ;; one of which could be elided if we really cared.
   (render (evolve-state observations best-time))))
