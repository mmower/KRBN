(ns rbn)

(defn or-oper [a b] (or a b))
(defn and-oper [a b] (and a b))
(defn xor-oper [a b] (or (and a (not b)) (and (not a) b)))

(defn random-oper []
  (rand-nth [or-oper and-oper xor-oper]))

(defn random-state []
  (rand-nth [true false]))

(defn potential-connections
  [n size exc]
  (let [pool (shuffle (filter #(not= exc %1) (range size)))]
    (take n pool)))

(defn make-cell-func [size]
  (fn [n]
    {:idx n
     :state (random-state)
     :connections (potential-connections 2 size n)
     :oper (random-oper)}))

(defn make-network [dim]
  (let [size (* dim dim)
        make-cell (make-cell-func size)]
    (map make-cell (range size))))

(defn evolve-cell
  [cell network]
  (let [connections (map #(nth network %1) (:connections cell))
        operands (map :state connections)
        new-state (apply (:oper cell) operands)]
        (assoc cell :state new-state)))

(defn evolve-network
  [network]
  (map #(evolve-cell %1 network) network))

(def first-gen-net (make-network 8))

(def second-gen-net (evolve-network first-gen-net))
