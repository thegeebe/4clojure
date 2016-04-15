(ns fourclojure.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn seqreverse
  "Reverse a sequence without using reverse"
  [seq]
  (reduce conj () seq))

(defn sumitallup
  "Sum a sequence of numbers"
  [seq]
  (partial apply +))

(defn fibonacci
  "Generate the first n fibonacci numbers"
  [n]
  (loop [accum '(1 1) i 3]
    (if (> i n)
      accum
      (recur (concat accum (list (apply + (take-last 2 accum)))) (inc i)))
  )
)

(defn palindrome
  "detect palindromes"
  [seq]
  (= (reverse (reverse seq)) (reverse seq))
)

(defn flatSeq
  "flatten without flatten"
  [n]
  (remove sequential? (tree-seq sequential? seq n)))

(defn getcaps
  "filter only the capital letters"
  [s]
  (clojure.string/replace s #"(?![A-Z])." ""))

(defn compress-seq
  "Remove consecutive duplicates"
  [s]
  (reverse (reduce (fn [accum x]
    (if (not= (first accum) x)
      (cons x accum)
      accum))
    '()
    s
  ))
)

(defn pack-seq
  "Pack consecutive duplicates into sublists"
  [s]
  (reverse (reduce (fn [accum x]
    (if (= (ffirst accum) x)
      (cons (cons x (first accum)) (rest accum))
      (cons (list x) accum)
    )) '() s))
)

(comment "Duplicate sequence")
#(mapcat (fn [x] (list x x)) %)

(comment "Replicate a sequence")
(fn [x y] (mapcat (fn [e] (repeat y e)) x))

(comment "Implement range")
(fn [x y] (take (- y x) (iterate inc x)))

(comment "Max Value")
(fn [& body] (reduce (fn [x y] (if (> x y) x y)) body))

(comment "Interleave")
(fn [a b] (flatten
  (for [i (range (min (count a) (count b)))]
    (list (a i) (b i))
  ))
)
