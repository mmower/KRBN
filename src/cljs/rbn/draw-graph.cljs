(ns graph-drawing
  (:require [rbn :as rbn]))

(defn cell-x
  [dim index]
  (mod index dim))

(defn cell-y
  [dim index]
  (int (/ index dim)))

(defn cell-rect
  "Given a network representation and cell index return the cell rectangle coordinates"
  [paper network index]
  (let [width (.-width paper)
        height (.-height paper)
        {:keys [dim size]} network
        w (/ width dim)
        h (/ height dim)
        x (* w (cell-x dim index))
        y (* h (cell-y dim index))]
    { :w w
      :h h
      :x x
      :y y }
    ))

(defn paper-rect
  [paper network cell]
  (let [{:keys [x y w h]} (cell-rect paper network (:index cell))
        shape (.rect paper x y w h 1)]
    (do
      (.attr shape "stroke" "#000000")
      shape)))

(defn make-network-representation
  [paper network]
  (do
    {:paper paper
     :network network
     :rects (doall (map #(paper-rect paper network %1) (:cells network)))}))

(defn update-network-representation
  [network representation]
  (let [cells (:cells network)]
    (doall (for [{:keys [index,state]} cells]
             (let [shape (nth (:rects representation) index)]
               (if state
                 (.attr shape "fill" "#555555")
                 (.attr shape "fill" "#FFFFFF")))))))
