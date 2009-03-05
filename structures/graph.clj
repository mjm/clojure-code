(ns structures.graph
  (:use clojure.set)
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

;; Debug Graph:
(comment
  (def g (-> empty-el-graph
             (add-edge :a :b 5)
             (add-edge :a :d 2)
             (add-edge :b :c 3)
             (add-edge :b :d 1)
             (add-edge :c :d 1))))

(defstruct el-graph :edges :vertices)

(def empty-el-graph (struct el-graph #{} #{}))

(defn edge [from to weight]
  [from to weight])

(defn add-edge
  ([graph] graph)
  ([graph edge]
     (apply add-edge graph edge))
  ([graph from to weight]
     (assoc graph
       :edges (union (graph :edges) #{(edge from to weight)
                                      (edge to from weight)})
       :vertices (union (graph :vertices) #{from to}))))

(defn edges-to-check [graph used]
  (filter (fn [[u v w]]
            (and (contains? used u)
                 (not (contains? used v))))
          (graph :edges)))

(defn next-edge [graph used]
  (if-let [edges (seq (edges-to-check graph used))]
    (apply min-key (fn [[u v w]] w) edges)))

(defn mst [graph start]
  (loop [tree empty-el-graph
         used #{start}]
    (if (= used (graph :vertices))
      tree
      (let [new-tree (add-edge tree (next-edge graph used))]
        (recur new-tree (tree :vertices))))))