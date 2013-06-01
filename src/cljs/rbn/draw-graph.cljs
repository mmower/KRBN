(ns graph-drawing
  (:require [rbn :as rbn]))

(defn cell-x
  [dim index]
  (mod index dim))

(defn cell-y
  [dim index]
  (int (/ index dim)))

(defn cell-centre
  [paper network index]
  (let [width (.-width paper)
        height (.-height paper)
        {:keys [dim size]} network
        w (/ width dim)
        h (/ height dim)
        hw (/ w 2)
        hh (/ h 2)
        x (+ (* w (cell-x dim index)) hw)
        y (+ (* h (cell-y dim index)) hh)]
    {:x x
     :y y}))

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


(defn draw-node
  [paper network index]
  (let [{:keys [x y]} (cell-centre paper network index)
        shape (.circle paper x y 1)]
    (.attr shape "fill" "#444444")
    shape))

(defn draw-nodes
  [paper network]
  (doall (for [{:keys [index]} (:cells network)]
    (draw-node paper network index))))

(defn draw-cell-connection
  [paper network cell connected-index]
  (let [fc (cell-centre paper network (:index cell))
        tc (cell-centre paper network connected-index)
        fx (:x fc)
        fy (:y fc)
        tx (:x tc)
        ty (:y tc)
        line (.path paper (str "M" fx " " fy "L" tx " " ty))]
    (.attr line "stroke" "#BBBBBB")))

(defn draw-cell-connections
  [paper network cell]
  (doall (for [connection (:connections cell)]
    (draw-cell-connection paper network cell connection))))

(defn draw-connections
  [paper network]
  (doall (for [cell (:cells network)]
           (draw-cell-connections paper network cell))))

(defn paper-rect
  [paper network cell]
  (let [{:keys [index]} cell
        {:keys [x y w h]} (cell-rect paper network index)
        shape (.rect paper x y w h 1)]
    (do
      (.attr shape "stroke" "#DDDDDD")
      shape)))

(defn make-network-representation
  [paper network]
  (do
    {:paper paper
     :network network
     :rects (doall (map #(paper-rect paper network %1) (:cells network)))}))

(def oper-color {
                 rbn/and-oper "#FF0000"
                 rbn/or-oper "#00FF00"
                 rbn/xor-oper "#0000FF"})

(def oper-color-mono {
                 rbn/and-oper "#666666"
                 rbn/or-oper "#888888"
                 rbn/xor-oper "#AAAAAA"})

(defn update-network-representation
  [network representation]
  (let [cells (:cells network)]
    (doall (for [{:keys [index state oper]} cells]
             (let [shape (nth (:rects representation) index)]
               (if state
                 (.attr shape "fill" (get oper-color-mono oper "#555555"))
                 (.attr shape "fill" "#FFFFFF")))))))
