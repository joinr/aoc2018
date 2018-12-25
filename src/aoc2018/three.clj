(ns aoc2018.three)
;; --- Day 3: No Matter How You Slice It ---

;; The Elves managed to locate the chimney-squeeze prototype fabric
;; for Santa's suit (thanks to someone who helpfully wrote its box IDs
;; on the wall of the warehouse in the middle of the
;; night). Unfortunately, anomalies are still affecting them - nobody
;; can even agree on how to cut the fabric.

;; The whole piece of fabric they're working on is a very large square
;; - at least 1000 inches on each side.

;; Each Elf has made a claim about which area of fabric would be ideal
;; for Santa's suit. All claims have an ID and consist of a single
;; rectangle with edges parallel to the edges of the fabric. Each
;; claim's rectangle is defined as follows:

;;     The number of inches between the left edge of the fabric and the left edge of the rectangle.
;;     The number of inches between the top edge of the fabric and the top edge of the rectangle.
;;     The width of the rectangle in inches.
;;     The height of the rectangle in inches.

;; A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a
;; rectangle 3 inches from the left edge, 2 inches from the top edge,
;; 5 inches wide, and 4 inches tall. Visually, it claims the square
;; inches of fabric represented by # (and ignores the square inches of
;; fabric represented by .) in the diagram below:

;; ...........
;; ...........
;; ...#####...
;; ...#####...
;; ...#####...
;; ...#####...
;; ...........
;; ...........
;; ...........

;; The problem is that many of the claims overlap, causing two or more
;; claims to cover part of the same areas. For example, consider the
;; following claims:

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; Visually, these claim the following areas:

;; ........
;; ...2222.
;; ...2222.
;; .11XX22.
;; .11XX22.
;; .111133.
;; .111133.
;; ........

;; The four square inches marked with X are claimed by both 1 and
;; 2. (Claim 3, while adjacent to the others, does not overlap either
;; of them.)

;; If the Elves all proceed with their own plans, none of them will
;; have enough fabric. How many square inches of fabric are within two
;; or more claims?

(defn parse-claim [ln]
  (let [[id _ xy wh] (clojure.string/split ln #" ")
        [x y] (map clojure.edn/read-string (re-seq #"[0-9]+" xy) )
        [w h] (map clojure.edn/read-string (re-seq #"[0-9]+" wh) )
        ]
    {:id id :x x :y y :w w :h h}))

(def claims
  (->> (slurp "day-3-input.txt")
       clojure.string/split-lines
       (mapv parse-claim)))


;;brute force...
;;just render all the claims.
;;keep stats on overlapping regions.
;;read back overlapping regions after rendering.

;;better than brute force,
;;use a containment hierarchy.
;;compute overlap using vector representation.

;;if two regions overlap, create a combined node with combined bounds.

;;let's go bf*...

(defn ^longs canvas [w h]
  (long-array (* w h)))

(defn get-coord [^longs canvas x y]
  (aget canvas (unchecked-add x (unchecked-multiply 1000 y))))

(defn set-coord [^longs canvas x y ^long v]
  (aset canvas (unchecked-add x (unchecked-multiply 1000 y)) v))

(defn ^longs render [^longs canvas claim]
  (let [{:keys [x y w h]} claim
        ybound (+ y h)
        xbound (+ x w)]
    (loop [y y]
      (if (== y ybound)
        canvas        
        (do (loop [x x]
              (when (< x xbound)
                (do (set-coord canvas x y (unchecked-inc (get-coord canvas x y)))
                    (recur (unchecked-inc x)))))                  
            (recur (unchecked-inc y)))))))

(defn render-claims
  ([cnv xs]
   (reduce render cnv xs))
  ([xs] (render-claims (canvas 1000 1000) xs)))

(defn overlaps [^longs cnv]
  (areduce cnv idx acc 0 (if (>= (aget cnv idx) 2)
                           (unchecked-inc acc)
                           acc)))

(defn intersects [l1 l2  r1 r2]
  (or (and (>= r1 l1)
           (<= r1 l2))
      (and (>= r2 l1)
           (<= r2 l2))))

(defn intersects-x [l r]
  (if (<= (:x l ) (:x r))
    (intersects (:x l) (+ (:x l) (:w l))
                (:x r) (+ (:x r) (:w r)))
    (intersects-x r l)))

(defn intersects-y [l r]
  (if (<= (:y l ) (:y r))
    (intersects (:y l) (+ (:y l) (:h l))
                (:y r) (+ (:y r) (:h r)))
    (intersects-y r l)))


(defn intersects-2d [l r]
  (and (intersects-x l r)
       (intersects-y l r)))


(defn collide [l xs]
  (reduce (fn [acc r]
            (if (= (:id l) (:id r))
              acc
              (if (intersects-2d l r)
                (reduced [l r])
                acc))) nil xs))

(defn islands [xs]
  (for [l xs
        :when (nil? (collide l xs))]
    l))
                            
        
