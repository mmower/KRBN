(ns hello-clojurescript)

(defn make-network
  [dim]
  (map #( { :state true, :operator xor} ) (range (* dim dim))))

(def sample-network (make-network 16))

(defn handle-click []
  (js/alert (clojure.string/join sample-network)))

(defn element-with-id
  [id]
  (.getElementById js/document id))

(.addEventListener (element-with-id "clickable") "click" handle-click)
