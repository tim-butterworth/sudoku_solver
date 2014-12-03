(ns sudoku-solver.core)

(defn check-verticals [puzzle]
  (doseq [i (range (count puzzle))]))

(defn pop-one-lvl [lst]
  (reduce 
   (fn [accume n] 
     (if (sequential? n) 
       (reduce 
        (fn [accume v] (conj accume v)) 
        accume n) 
       (conj accume n))) [] lst))

(defn x-fun [n]
  (fn [val] [n val]))
(defn y-fun [n]
  (fn [val] [val n]))

(defn linear-partition [fun min max]
  (map fun (range min (inc max))))
(defn full-linear-partition [fun]
  (linear-partition fun 1 9))
(defn x-partition [n]
  (full-linear-partition (x-fun n)))
(defn y-partition [n]
  (full-linear-partition (y-fun n)))

(defn v-h-partitions [partitioner]
  (map 
   (fn [n] (full-linear-partition (partitioner n))) 
   (range 1 (inc 9))))

(defn verticals []
  (v-h-partitions x-fun))
(defn horizontals []
  (v-h-partitions (fn [n] (y-fun n))))

(defn inject [accume basis c] 
  (reduce 
   (fn [accume n] (conj accume [c n]))
   accume
   basis))

(defn self-cross [basis] 
  (reduce 
   (fn [accume n] (inject accume basis n))
   []
   basis))

(defn next-box [x1 y1 x2 y2 max] 
  (loop [x1 x1 y1 y1 x2 x2 y2 y2 accume [[x1 y1 x2 y2]]]
    (if )
    (recur )))

(defn square-partition [x1 y1 x2 y2]
  (pop-one-lvl 
   (map 
    (fn [n] 
      (linear-partition (x-fun n) y1 y2)) 
   (range x1 (inc x2)))))

(def boxes 
  (zipmap 
   (self-cross [1 4 7]) 
   (self-cross [3 6 9])))

(def box-partitions 
  (map (fn [n] (let [e (boxes n)] 
                 (apply square-partition (flatten [n e])))) 
       (keys boxes)))

(def partitions 
  (pop-one-lvl 
   [(horizontals) 
    (verticals) 
    box-partitions]))

(def entries
  (reduce 
   (fn [accume n] 
     (reduce (fn [ccm m] (conj ccm m)) accume n)) 
   #{} 
   partitions))

(defn merge-into [k basis partition]
  (let [valset (basis k)]
    (assoc basis k 
           (reduce (fn [accume n] (if (contains? valset n)
                                    accume
                                    (conj accume n))) valset partition))))

(defn sub-reduce [basis partition]
  (reduce (fn [accume n] (merge-into n accume partition)) basis partition))

(def influence-sets 
  (let [basis (reduce (fn [accume n] (assoc accume n #{})) {} entries)]
    (reduce (fn [accume n] (sub-reduce accume n)) basis partitions)))

;for printing stuff
(defn print-set [set grd] 
  (map 
   (fn [row] 
     (reduce 
      (fn [accume n] (if (contains? set n) (str accume " x ") (str accume " o "))) " "row)) grd))

(def grid (map (fn [n] (map (fn [nn] [n nn]) (range 1 (inc 9)))) (range 1 (inc 9))))

(defn initial-board [entries]
  (let [options (set (range 1 (inc 9)))] 
    (reduce (fn [accume n] (assoc accume n options)) {} entries)))

(defn apply-influence [board point]
  (let [influence (influence-sets point)
        val (first (board point))]
    (reduce 
     (fn [accume n] 
       (if (not (= n point))
         (assoc accume n (disj (board n) val))
         accume)) 
     board 
     influence)))

(defn make-selection [board point val]
  (assoc board point #{val}))
