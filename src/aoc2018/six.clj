(ns aoc2018.six)

;; --- Day 6: Chronal Coordinates ---

;; The device on your wrist beeps several times, and once again you feel like you're falling.

;; "Situation critical," the device announces. "Destination
;; indeterminate. Chronal interference detected. Please specify new
;; target coordinates."

;; The device then produces a list of coordinates (your puzzle
;; input). Are they places it thinks are safe or dangerous? It
;; recommends you check manual page 729. The Elves did not give you a
;; manual.

;; If they're dangerous, maybe you can minimize the danger by finding
;; the coordinate that gives the largest distance from the other
;; points.

;; Using only the Manhattan distance, determine the area around each
;; coordinate by counting the number of integer X,Y locations that are
;; closest to that coordinate (and aren't tied in distance to any
;; other coordinate).

;; Your goal is to find the size of the largest area that isn't
;; infinite. For example, consider the following list of coordinates:

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; If we name these coordinates A through F, we can draw them on a
;; grid, putting 0,0 at the top left:

;; ..........
;; .A........
;; ..........
;; ........C.
;; ...D......
;; .....E....
;; .B........
;; ..........
;; ..........
;; ........F.

;; This view is partial - the actual grid extends infinitely in all
;; directions. Using the Manhattan distance, each location's closest
;; coordinate can be determined, shown here in lowercase:

;; aaaaa.cccc
;; aAaaa.cccc
;; aaaddecccc
;; aadddeccCc
;; ..dDdeeccc
;; bb.deEeecc
;; bBb.eeee..
;; bbb.eeefff
;; bbb.eeffff
;; bbb.ffffFf

;; Locations shown as . are equally far from two or more coordinates,
;; and so they don't count as being closest to any.

;; In this example, the areas of coordinates A, B, C, and F are
;; infinite - while not shown here, their areas extend forever outside
;; the visible grid. However, the areas of coordinates D and E are
;; finite: D is closest to 9 locations, and E is closest to 17 (both
;; including the coordinate's location itself). Therefore, in this
;; example, the size of the largest area is 17.

;; What is the size of the largest area that isn't infinite?

(defn read-coords
  ([]  (->> (slurp "day-6-input.txt")
            clojure.string/split-lines
            (map #(clojure.edn/read-string (str "[" % "]")))
            )))

(defn bounds [xs]
  (reduce (fn [[mn mx] n] [(min n mn) (max n mx)])
          [Long/MAX_VALUE Long/MIN_VALUE] xs))

(def coords (read-coords))

;;the "good" way to do this is
;;to implement something like Fortune's algorithm,
;;compute the voronoi cells, and use them to
;;determine the cell with the minimum area....

;;I'm going to go the dumber, bad way.

;;Compute a distance matrix.
;;We know the bounds of the region
;;will necessarily define extreme points
;;(in each cardinal direction) that
;;cannot be surrounded by other points.
;;(Infinite areas).
;;we can clip our exploration to
;;a bounding box containing these
;;points...


;;Then, we compute manhattan distances
;;for every coordinate in the
;;region (gonna be laborious).

;;once we'ce got nearest points,
;;count the regions to
;;determine which site has the
;;largest area (in pixels).

(defn bounds [xs]
  (reduce (fn [[xmin xmax ymin ymax] [x y]]
            [(min xmin x)
             (max xmax x)
             (min ymin y)
             (max ymax y)])
          [Long/MAX_VALUE
           Long/MIN_VALUE
           Long/MAX_VALUE
           Long/MIN_VALUE] xs))

(defn extrema [xs]
  (let [[xmin xmax ymin ymax] (bounds xs)]
    (filter (fn [[x y]]
              (or (== x xmin)
                  (== x xmax)
                  (== y ymin)
                  (== y ymax)))
            xs)))

;;for convenience...
(defn manhattan [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))))


(def sites
  (into {}
        (for [[idx xy] (map-indexed vector coords)]
          [idx xy])))

;;this is an invalid hueristic.
;;it misses points on a non-linear boundary...

;;we know these can't be areas with finite
;;regions...
(defn extreme-points [sites]
  (let [extreme? (set (extrema (vals sites)))]
    (map first (filter (fn [[k p]]
                         (extreme? p)) sites))))

(defn distance-from
  ([sites site xy]
   (-> site sites (manhattan xy))))

;;this is a lame way to establish known infinite areas.
(defn boundary-points [xmin xmax ymin ymax]
  (concat 
   (for [x [xmin (inc xmax)]
         y (range ymin (inc ymax))]
     [x y])
   (for [x (range xmin (inc xmax))
         y [ymin (inc ymax)]]
     [x y ])))

(defn interior-points [xmin xmax ymin ymax]
  (for [x (range (inc xmin) xmax)
        y (range (inc ymin) ymax)]
    [x y]))

(defn closest [p sites]
  (let [ds (->> (into [] (map (fn [[site coord]]
                                [(manhattan coord p) site]))
                      (seq sites))
                (sort-by first))]
    (if (== (ffirst ds) (first (second ds)))
      [-1 nil]                    
      [p (first ds)])))

;;we can collect infinite regions here if we're smart.
(defn distances [sites]
  (let [[xmin xmax ymin ymax] (bounds (vals sites))
        infinites (atom #{})]
    (-> (concat 
         (for [p (interior-points xmin xmax ymin ymax)]
           (closest p sites))
         (for [p (boundary-points xmin xmax ymin ymax)]
           (let [res (closest p sites)
                 [_ [_ node]] res]
                   (do (when node
                         (swap! infinites conj node))
                       res))))
        vec
        (vary-meta assoc :infinites (deref infinites)))))


(defn maximal-areas [sites]
  (let [ds      (->> sites distances)
        finite? (-> ds meta :infinites  complement)]
    (->> ds
         (map (comp second second))
         frequencies 
         (sort-by (comp - val))
         (filter (fn [[site _]]
                   (and site
                        (finite? site)))))))
         
(def sample-data
  {:a [1 1]
   :b [1 6]
   :c [8 3]
   :d [3 4]
   :e [5 5]
   :f [8  9]})

;; --- Part Two ---

;; On the other hand, if the coordinates are safe, maybe the best you
;; can do is try to find a region near as many coordinates as
;; possible.

;; For example, suppose you want the sum of the Manhattan distance to
;; all of the coordinates to be less than 32. For each location, add
;; up the distances to all of the given coordinates; if the total of
;; those distances is less than 32, that location is within the
;; desired region. Using the same coordinates as above, the resulting
;; region looks like this:

;; ..........
;; .A........
;; ..........
;; ...###..C.
;; ..#D###...
;; ..###E#...
;; .B.###....
;; ..........
;; ..........
;; ........F.

;; In particular, consider the highlighted location 4,3 located at the
;; top middle of the region. Its calculation is as follows, where
;; abs() is the absolute value function:

;;     Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;;     Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;;     Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;;     Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;;     Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;;     Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;;     Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; Because the total distance to all coordinates (30) is less than 32,
;; the location is within the region.

;; This region, which also includes coordinates D and E, has a total
;; size of 16.

;; Your actual region will need to be much larger than this example,
;; though, instead including all locations with a total distance of
;; less than 10000.

;; What is the size of the region containing all locations which have
;; a total distance to all given coordinates of less than 10000?

