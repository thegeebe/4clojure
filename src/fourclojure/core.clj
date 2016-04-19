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

(comment "Drop every nth item")
(fn [coll n] (mapcat rest (partition n n [] (cons 0 coll))))

(comment "Factorial")
(fn [x] (reduce * (range x 0 -1)))

(comment "Reverse interleave")
(fn [s n] (partition (/ (count s) n) (apply interleave (partition n s))))

(comment "Rotate Sequence")
#(take (count %2) (nthnext (cycle %2) (mod %1 (count %2))))

(comment "46. Flipping out")
(fn [f] (fn [x y] (f y x)))

(comment "49. Split-at")
#(vector (take %1 %2) (drop %1 %2))

(comment "50. Split by type")
#(vals (group-by type %))

(comment "54. Longest Increasting Sub-seq")
(defn sub-seq
  [s]
  (apply max-key count
    (cons '()
      (filter (fn [x] (> (count x) 1))
        (map reverse (reduce (fn [accum x]
          (if (or (nil? (ffirst accum))
            (>= (ffirst accum) x))
            (cons (list x) accum)
            (cons (cons x (first accum)) (rest accum))
          )) '() s)
        )
      )
    )
  )
)

(comment "Partition a Sequence")
(fn [n s]
  (loop [i (/ (count s) n) se s accum '()]
    (if (< i 1)
      (reverse accum)
      (recur (dec i)
        (drop n se)
        (cons (take n se) accum)
      )
    )))

(comment "55. Count Occurances")
#(reduce-kv (fn [m k v] (assoc m k (count v))) {} (group-by identity %))

(comment "56. Get Distinct")
#(reverse (reduce (fn [accum y]
  (if-not (some (partial = y) accum)
    (cons y accum)
    accum)
) '() %))

(comment "58. Function Composition")
(defn comp-cust
  "compose functions"
  [& funcs]
  (fn [& args]
    (first (reduce (fn [x y] (cons (apply y x) '())) args (reverse funcs)))
  )
)

(comment "59. Juxt")
(defn juxt-cust
  "juxtapose functions"
  [& funcs]
  (fn [& args]
    (map (fn [f] (apply f args)) funcs)
  )
)

(defn tester
  ([seq] (tester 1 seq))
  ([n seq] (lazy-seq (cons (reduce + (take n seq)) (tester (inc n) seq))))
)


(comment "60. Seq reductions")
(defn seq-reduce
  ([f coll] (seq-reduce f nil coll 1))
  ([f val coll] (seq-reduce f val coll 0))
  ([f val coll n] (lazy-seq
    (cons
      (if (nil? val)
        (reduce f (take n coll))
        (reduce f val (take n coll))
      )
      (if (nil? (nth coll n nil))
        '()
        (seq-reduce f val coll (inc n)))))
      )
)

(comment "62. Iterate")
(fn iter [f x] (lazy-seq (cons x (iter f (f x)))))

(comment "63. Group-by")
(defn my-group [f x]
  (reduce-kv (fn [m1 k1 v1] (assoc m1 k1 (reverse v1))) {}
    (reduce (fn [m v]
      (update m (f v) conj v)) {} x)))

(comment "4Clojure doesn't have update")
(fn my-group [f x]
  (reduce-kv (fn [m1 k1 v1] (assoc m1 k1 (reverse v1))) {}
    (reduce (fn [m v]
      (update-in m (vector (f v)) conj v)) {} x)))
