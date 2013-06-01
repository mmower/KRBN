(ns rbn)

(defn or-oper [a b] (or a b))
(defn and-oper [a b] (and a b))
(defn xor-oper [a b] (or (and a (not b)) (and (not a) b)))

(def rules [[or-oper and-oper xor-oper]
            [or-oper and-oper]])

(defn random-oper [rule]
  (rand-nth (nth rules rule)))

(defn random-state
  ([]
  (rand-nth [true false]))
  ([p]
  (<= (/ (inc (rand-int 100)) 100) p)))

(defn potential-connections
  [n size exc]
  (let [pool (shuffle (filter #(not= exc %1) (range size)))]
    (take n pool)))

(defn make-network [dim p rule]
  (let [size (* dim dim)]
    {
     :generation 1
     :dim dim
     :cells (map (fn [n] {
                          :index n
                          :state (random-state p)
                          :connections (potential-connections 2 size n)
                          :oper (random-oper rule)
                          }) (range size))
    }
    ))

(defn next-cell-state
  "Given a cell and the list of all cells, determine the next state of the given cell"
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

(defn network-generation
  [network]
  (:generation network))