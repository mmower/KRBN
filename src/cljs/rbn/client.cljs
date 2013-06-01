(ns rbn)

(defn handle-click []
  (js/alert "Hello Clojurescript!"))

(defn element-with-id
  [id]
  (.getElementById js/document id))

(.addEventListener (element-with-id "clickable") "click" handle-click)

