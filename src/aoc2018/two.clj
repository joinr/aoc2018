(ns aoc2018.two)

;; Late at night, you sneak to the warehouse - who knows what kinds of
;; paradoxes you could cause if you were discovered - and use your
;; fancy wrist device to quickly scan every box and produce a list of
;; the likely candidates (your puzzle input).

;; To make sure you didn't miss any, you scan the likely candidate
;; boxes again, counting the number that have an ID containing exactly
;; two of any letter and then separately counting those with exactly
;; three of any letter. You can multiply those two counts together to
;; get a rudimentary checksum and compare it to what your device
;; predicts.

;; For example, if you see the following box IDs:

;;     abcdef contains no letters that appear exactly two or three times.
;;     bababc contains two a and three b, so it counts for both.
;;     abbcde contains two b, but no letter appears exactly three times.
;;     abcccd contains three c, but no letter appears exactly two times.
;;     aabcdd contains two a and two d, but it only counts once.
;;     abcdee contains two e.
;;     ababab contains three a and three b, but it only counts once.

;; Of these box IDs, four of them contain a letter which appears
;; exactly twice, and three of them contain a letter which appears
;; exactly three times. Multiplying these together produces a checksum
;; of 4 * 3 = 12.

;; What is the checksum for your list of box IDs?

(def ids
  (->> (slurp "day-2-input.txt")
       clojure.string/split-lines))

(defn classes [id]
  (->> (frequencies id)
       (reduce (fn [[found remaining :as acc] [_ n]]                 
                 (if (remaining n)
                   (let [nxt (disj remaining n)
                         found (conj found n)]
                     (if (seq nxt)
                       [found  nxt]
                       (reduced [found nxt])))
                   acc)) [[] #{2 3}])
       first))

(defn checksum [xs]
  (->> xs
       (mapcat classes)
       frequencies
       vals
       (reduce *)))
       

;; --- Part Two ---

;; Confident that your list of box IDs is complete, you're ready to
;; find the boxes full of prototype fabric.

;; The boxes will have IDs which differ by exactly one character at
;; the same position in both strings. For example, given the following
;; box IDs:

;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; The IDs abcde and axcye are close, but they differ by two
;; characters (the second and fourth).

;;However, the IDs fghij and fguij differ by exactly one character,
;;the third (h and u). Those must be the correct boxes.

;; What letters are common between the two correct box IDs? (In the
;; example above, this is found by removing the differing character
;; from either ID, producing fgij.)

(defn neighbors [x]
  (for [idx (range 1 (inc (count x)))]
    (str (subs x 0 (dec idx)) "*" (subs x idx))))

(defn trawl [xs]
  (->> xs 
       (reduce (fn [parents child]
                 (if (:found parents)
                   (reduced parents)
                   (reduce (fn [parents parent]
                             (if-let [ps (get parents parent)]
                               (reduced (assoc parents :found {:parent parent
                                                               :children (conj ps child)} ))
                               (assoc parents parent [child])))
                           parents (neighbors child))))
               {})
       :found))

;;do a radix sort to perform comparisons of things that are naturally
;;closer, improves the odds of finding common parents earlier.
(defn nearest [xs]
  (-> xs sort trawl))
