(ns structures.graph
  (:import clojure.lang.PersistentQueue))

;; {:a [:b :c], :b [:a :c], :c [:a :d], :d [:c]}

(defn new-paths [graph path node visited]
  (filter #(not (visited (first %)))
          (map #(cons % path) (graph node))))

(defn bfs [graph queue done? visited]
  (prn queue)
  (if (seq queue)
    (let [path (first queue)
          node (first path)
          new-visited (conj visited node)]
      (if (done? node)
        (reverse path)
        (recur graph
               (concat (rest queue)
                       (new-paths graph path node new-visited))
               done?
               new-visited)))
    nil))

(defn shortest-path [graph start end]
  (bfs graph (conj PersistentQueue/EMPTY (list start)) #(= %1 end) #{}))