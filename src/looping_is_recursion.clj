(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

;power tests
(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

;last-element tests
(last-element [])      ;=> nil
(last-element [1 2 3]) ;=> 3
(last-element [2 5])   ;=> 5

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not (= (first seq1) (first seq2))) false
    :else (recur (rest seq1) (rest seq2))))

;seq= tests
(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false
(seq= [1 2 3] [1 2 3 nil])

(defn find-first-index [pred a-seq]
  (loop [n 0
         loop-seq a-seq]
    (cond
      (empty? loop-seq) nil
      (pred (first loop-seq)) n
      :else (recur (inc n) (rest loop-seq)))))

;find-first-index tests
(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
(find-first-index zero? [1 1 3 7 2])                          ;=> nil
(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
(find-first-index nil? [])                                    ;=> nil

(defn avg [a-seq]
  (loop [loop-seq a-seq
         n 0
         acc 0]
    (if
      (empty? loop-seq) (/ acc n)
      (recur (rest loop-seq) (inc n) (+ acc (first loop-seq))))))

;avg tests
(avg [1 2 3])   ;=> 2
(avg [0 0 0 4]) ;=> 1
(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5

(defn parity [a-seq]
  (loop [loop-seq a-seq
         acc-set #{}]
    (if
      (empty? loop-seq) acc-set
      (recur (rest loop-seq) (if
                               (contains? acc-set (first loop-seq)) (disj acc-set (first loop-seq))
                               (conj acc-set (first loop-seq)))))))

;parity tests
(parity [:a :b :c])           ;=> #{:a :b :c}
(parity [:a :b :c :a])        ;=> #{:b :c}
(parity [1 1 2 1 2 3 1 2 3 4]) ;=> #{2 4}

(defn fast-fibo [n]
  (loop [countdown n
         current 1
         previous 0]
    (if
      (= countdown 0) previous
      (recur (dec countdown) (+ current previous) current))))

;fast-fib tests
(fast-fibo 0) ;=> 0
(fast-fibo 1) ;=> 1
(fast-fibo 2) ;=> 1
(fast-fibo 3) ;=> 2
(fast-fibo 4) ;=> 3
(fast-fibo 5) ;=> 5
(fast-fibo 6) ;=> 8

(defn cut-at-repetition [a-seq]
  (loop [check-set #{}
         loop-seq a-seq
         acc-vec []]
    (cond
      (empty? loop-seq) acc-vec
      (contains? check-set (first loop-seq)) acc-vec
      :else (recur (conj check-set (first loop-seq)) (rest loop-seq) (conj acc-vec (first loop-seq))))))


(cut-at-repetition [1 1 1 1 1])
;=> [1] doesn't have to be a vector, a sequence is fine too
(cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
;=> [:cat :dog :house :milk 1]
(cut-at-repetition [0 1 2 3 4 5])
;=> [0 1 2 3 4 5]

