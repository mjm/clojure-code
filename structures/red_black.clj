(ns structures.red-black)

(defstruct tree :data :red :left :right)
(defn node
  ([data] (struct tree data true nil nil))
  ([data red?] (struct tree data red? nil nil)))
(def data (accessor tree :data))
(def left (accessor tree :left))
(def right (accessor tree :right))

(defn direction [t val]
  (cond (< val (data t)) :left
        (> val (data t)) :right))

(defn opposite [dir]
  (cond (= dir :left) :right
        (= dir :right) :left))

;;; This is probably unnecessary
(defn red? [t]
  (and t (:red t)))

(defn single-rotate [t dir]
  (let [opp (opposite dir)]
    (assoc (opp t)
      :red false
      dir (assoc t
            opp (dir (opp t))
            :red true))))

(defn double-rotate [t dir]
  (let [opp (opposite dir)]
    (single-rotate (assoc t opp (single-rotate (opp t) opp)) dir)))

(defn rebalance [t dir]
  (or (if (red? (dir t))
        (if (red? ((opposite dir) t))
          (assoc t
            :red true
            :left (assoc (left t) :red false)
            :right (assoc (right t) :red false))
          (cond (red? (dir (dir t)))
                (single-rotate t (opposite dir))
                (red? ((opposite dir) (dir t)))
                (double-rotate t (opposite dir)))))
      t))

(defn- insert-r [t val]
  (if (nil? t)
    (node val)
    (if-let [dir (direction t val)]
      (rebalance (assoc t dir (insert-r (dir t) val)) dir)
      t)))

(defn insert [t val]
  "Insert on the root of the tree."
  (assoc (insert-r t val)
    :red false))

(defn- red-violation? [t]
  "Cannot have two joined red nodes in a tree"
  (and (red? t)
       (or (red? (left t))
           (red? (right t)))))

(defn- bst-violation? [t]
  (or (and (left t) (>= (data (left t)) (data t)))
      (and (right t) (<= (data (right t)) (data t)))))

(defn- black-violation? [lh rh]
  (not (= lh rh)))

(defn valid? [t]
  (or (nil? t)
      (and (not (red-violation? t))
           (valid? ))))

(defn check [t]
  (if (nil? t)
    1
    (do
      (assert (not (red-violation? t)))
      (assert (not (bst-violation? t)))
      (let [lh (check (left t))
            rh (check (right t))]
        (assert (not (black-violation? lh rh)))
        (if (red? t) lh (inc lh))))))

(defn tree-with [& data]
  (loop [t nil
         d data]
    (if (empty? d)
      t
      (recur (insert t (first d)) (rest d)))))

(defn print-tree [t]
  (if (nil? t)
    "//"
    (str "(" (:data t)
         (if (red? t) "R" "B")
         (if (or (left t) (right t))
           (str " " (print-tree (:left t))
                " " (print-tree (:right t))))
         ")")))
