(ns mini.playground
  (:require
   [clojure.string :as str]))


;; Day 1

(defn parse-input [input]
  (map (comp #(map (comp int parse-double) %)  #(str/split % #"   ")) (str/split-lines input)))

(defn pair-dist [[a b]]
  (abs (- a b)))

;; Part 1
(->> (slurp "input/1.txt")
     (parse-input)
     (apply map vector)
     (map sort)
     (apply map vector)
     (map pair-dist)
     (apply +))


;; Part 2
(apply + (let [[left right]  (apply map vector (parse-input (slurp "input/1.txt"))) freqs (frequencies right)]
           (map (fn [k]
                  (* k (or (get freqs k) 0))) (distinct left))))

;; Day 2

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(map (comp int parse-double) (str/split % #" ")))))


(defn sign [x] (compare x 0))

(defn all-equal [xs]
  (every? #(= % (first xs)) xs))

(defn pred [x] (<= 1 (abs x) 3))


(defn diffs [xs]
  (->> xs (partition 2 1) (map #(apply - %))))

(def solve (comp #(and (every? pred %) (all-equal (map sign %))) diffs))

;; Part 1

(->> (slurp "input/2.txt")
     (parse-input)
     (map (comp #(if % 1 0) solve))
     (apply +))

;; Part 2
;; Very simple way of doing this

(defn remove-ith [i lst]
  (concat (take i lst) (drop (inc i) lst)))

(defn generate [lst]
  (map #(remove-ith %1 lst) (range (count lst))))

(defn solve2 [lst]
  (some solve (generate lst)))


(->> (slurp "input2.txt")
     (parse-input)
     (map (comp #(if % 1 0) solve2))
     (apply +))


(defn solve [xs]
  (->> (drop 1 xs) (map parse-double) (apply *)))


(defn parse [s]
  (re-seq #"mul\((\d+),(\d+)\)" s))

;; Day 3

;; Part 1

(->> (slurp "input/3.txt") (parse) (map solve) (apply +))

;; Part 2

(defn pad [s]
  (str "do()" s "don't()"))

(defn preprocess [s]
  (->>
   (str/replace s #"\s+" "x") ; The . does not match white space, so remove these first. Replace with 'x' so that we don't accidentally create a do() or a don't().
   (pad)
   (re-seq #"do\(\).*?don't\(\)")
   (apply str)))

(->> (slurp "input/3.txt") (preprocess) (parse) (map solve) (apply +) (long))



;; Day 4

;; Part 1

;; Idea: Consider each row, column and diagonal in the input. Go thru each of these forwards and backwards,
;; and count up every occurence of XMAS. 

(defn transpose [xs]
  (vec (apply map vector xs)))

(defn rotate [xs]
  (vec (reverse (transpose xs))))

(defn diagonal [xs]
  (mapv #(get-in xs [% %]) (range (count xs))))

(defn upper-diagonals [xs]
  (let [n (count xs)]
    (vec (for [i (range (dec n))]
           (vec (for [j (range (inc i))]
                  (get-in xs [j (- i j)])))))))

(defn all-diagonals* [xs]
  (->> [(rotate xs) (rotate (transpose xs))]
       (mapcat upper-diagonals)
       (concat [(diagonal xs)])
       (vec)))

(defn all-diagonals [xs]
  (concat (all-diagonals* xs) (all-diagonals* (rotate xs))))

(defn generate-strings [s]
  (let [cols (apply mapv vector (str/split-lines s)) rows (transpose cols)]
    (println cols)
    (map #(apply str %) (concat rows cols (all-diagonals rows)))))


(defn xmas-occurences* [s]
  (->> (re-seq #"XMAS" s)
       (count)))

(defn xmas-occurences [s]
  (->> [s (str/reverse s)]
       (map xmas-occurences*)
       (apply +)))


(apply + (map xmas-occurences (generate-strings (slurp "input/4.txt"))))

;; Part 2

(defn select-indices [xs idxs]
  (map #(get-in xs %) idxs))

(defn map-matrix [f mat]
  (map-indexed
   (fn [i row] (map-indexed (fn [j v] (f [i j] v)) row)) mat))


(defn inside? [i j n]
  (and (< 0 i n) (< 0 j n)))

(defn is-valid-cross? [i j mat]
  (and (inside? i j (count mat)) (= (get-in mat [i j]) \A)
       (let [a (select-indices mat [[(dec i) (dec j)] [(inc i) (inc j)]])
             b (select-indices mat [[(dec i) (inc j)] [(inc i) (dec j)]])]
         (= #{\M \S} (set a) (set b)))))

(let [mat (apply mapv vector (str/split-lines (slurp "input/4.txt")))]
  (->> mat
       (map-matrix (fn [[i j] _] (is-valid-cross? i j mat)))
       (flatten)
       (map #(if % 1 0))
       (apply +)))