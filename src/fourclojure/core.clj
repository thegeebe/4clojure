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
