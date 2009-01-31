(ns structures.avl-tree)

(defn height [tree]
  (if (nil? tree)
    0
    (inc (max (height (:left tree))
              (height (:right tree))))))

(defn balance [tree]
  (- (height (:left tree))
     (height (:right tree))))

(defn tree
  ([data] {:data data})
  ([data left right] {:data data
                      :left left
                      :right right}))

(defn- rotate-left [tree]
  (assoc (:right tree)
    :left (assoc tree
            :right (:left (:right tree)))))

(defn- rotate-right [tree]
  (assoc (:left tree)
    :right (assoc tree
             :left (:right (:left tree)))))

(defn- rotate-left-right [tree]
  (rotate-right
   (assoc tree
     :left (rotate-left (:left tree)))))

(defn- rotate-right-left [tree]
  (rotate-left
   (assoc tree
     :right (rotate-right (:right tree)))))

(defn- balanced [tree]
  (let [bal (balance tree)]
    (cond (> bal 1)
          (if (> (balance (:left tree)) 0)
            (rotate-right tree)
            (rotate-left-right tree))
          (< bal -1)
          (if (< (balance (:right tree)) 0)
            (rotate-left tree)
            (rotate-right-left tree))
          true tree)))

(defn insert [t val]
  (if (nil? t)
    (tree val)
    (balanced
     (cond (< val (:data t))
           (assoc t :left (insert (:left t) val))
           (> val (:data t))
           (assoc t :right (insert (:right t) val))
           true t))))

(defn- predecessor [t]
  (loop [n (:left t)]
    (if (:right n)
      (recur (:right n))
      n)))

(def delete)

(defn- delete-here [t val]
  (cond (nil? (:left t))
        (:right t)
        (nil? (:right t))
        (:left t)
        true
        (let [p (predecessor t)]
          (assoc (assoc t :data (:data p))
            :left (delete (:left t) (:data p))))))

(defn delete [t val]
  (if (nil? t)
    nil ; fail silently
    (balanced
     (cond (< val (:data t))
           (assoc t :left (delete (:left t) val))
           (> val (:data t))
           (assoc t :right (delete (:right t) val))
           true
           (delete-here t val)))))

(defn tree-with [& data]
  (loop [t nil
         d data]
    (if (empty? d)
      t
      (recur (insert t (first d)) (rest d)))))

(defn print-tree [tree]
  (if (nil? tree)
    "//"
    (str "(" (:data tree)
         " " (print-tree (:left tree))
         " " (print-tree (:right tree))
         ")")))