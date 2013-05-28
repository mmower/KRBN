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

(defn make-network [dim]
  (let [size (* dim dim)]
    {
     :generation 1
     :cells (map (fn [n] {
                          :idx n
                          :state (random-state)
                          :connections (potential-connections 2 size n)
                          :oper (random-oper)
                          }) (range size))
    }
    ))

(defn next-cell-state
  [cell cells]
  (let [connections (map #(nth cells %1) (:connections cell))
        operands (map :state connections)
        new-state (apply (:oper cell) operands)]
        (assoc cell :state new-state)))

(defn evolve-network
  [network]
  (let [{:keys [generation cells]} network]
    {
     :generation (inc generation)
     :cells (map #(next-cell-state %1 cells) cells)
    }))


(def first-gen-net (make-network 8))

(def second-gen-net (evolve-network first-gen-net))

(println second-gen-net)