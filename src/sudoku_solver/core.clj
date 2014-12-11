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
(defn print-board [board grd] 
  (map 
   (fn [row] 
     (reduce 
      (fn [accume n] 
        (let [entry (board n)]
          (if (= 1 (count entry)) 
            (str accume " " (first entry) " ") 
            (if (= 0 (count entry))
              (str accume " x ")
              (str accume " _ "))
            ))) " " row)) 
   grd))

(def grid (map (fn [n] (map (fn [nn] [n nn]) (range 1 (inc 9)))) (range 1 (inc 9))))

(defn initial-board [entries]
  (let [options (set (range 1 (inc 9)))] 
    (reduce (fn [accume n] (assoc accume n options)) {} entries)))

(defn make-selection [board point val]
  (let [s (board point)]
    (assoc board point 
           (reduce (fn [accume n] 
                     (if (= n val) 
                       accume
                       (disj accume n))) s s))))

(defn gather-changed [sub-set board val]
  (reduce 
   (fn [accume n] (let [current (board n)]
                    (if (contains? current val)
                      (assoc accume n (disj current val))
                      accume)))
   {}
   sub-set))

(defn next-influence-set [remainder updated used]
  (reduce (fn [accume n] 
            (if (and
                 (not (contains? used n))
                 (= 1 (count (updated n))))
              (conj accume n)
              accume))
          remainder
          (keys updated)))

(defn next-board [board updated]
  (reduce (fn [accume n] 
            (assoc accume n (updated n))) 
          board 
          (keys updated)))

(defn apply-influence [changed board]
  (loop [influencers changed used #{} accume board]
    (if (empty? influencers)
      accume
      (let [influence (first influencers)
            val (first (accume influence))
            remainder (rest influencers)
            influence-set (disj (influence-sets influence) influence)
            updated-used (conj used influence)
            updated (gather-changed influence-set accume val)]
        (recur 
         (next-influence-set remainder updated updated-used) ;influencers 
         updated-used                                        ;used
         (next-board accume updated))))))                    ;board

(defn update-board [board point val]
  (apply-influence [point] (make-selection board point val)))

(defn get-move [board]
  (loop [options entries]
    (if (empty? options)
      {}
      (let [point (first options)
            picks (board point)]
        (if (< 1 (count picks))
          {point picks}
          (recur (rest options)))))))

(defn breadth-first-move [boards]
  (reduce (fn [accume board] 
            (let [moves (get-move board)
                  point (first (keys moves))
                  picks (moves point)]
              (reduce (fn [sub-accume val]
                        (conj sub-accume (update-board board point val)))
                      accume
                      picks))) 
          #{} 
          boards))

(defn filter-invalid [boards]
  (filter (fn [board]
            (reduce (fn [accume n]
                      (and accume (not (= 0 (count (board n))))))
                    true
                    entries)) 
          boards))

(defn solve [board]
  (loop [possible-boards #{board} solutions #{}]
    (if (empty? possible-boards)
      solutions
      (let [moved (breadth-first-move possible-boards)
            valid-move (filter-invalid moved)
            boards (partition-solved valid-move)]
        (recur (boards :remainder) (merge solution (boards :solved)))))))

(defn populate-with-example [initial-data]
  (loop [vals (keys initial-data) accume (initial-board entries)]
    (if (empty? vals)
      accume
      (let [k (first vals)]
       (recur (rest vals) (update-board accume k (initial-data k)))))))

(defn pretty-print-board [board]
  (println (clojure.string/join "\n" (print-board board grid))))

(def example
{[2 2] 8
 [2 3] 2
 [3 2] 4
 [2 4] 3
 [1 5] 6
 [1 9] 8
 [2 5] 7
 [3 6] 9
 [3 7] 3
 [3 8] 7
 [4 1] 7
 [5 1] 4
 [6 1] 8
 [6 2] 2
 [4 3] 9
 [5 3] 3
 [5 4] 2
 [5 6] 7
 [5 7] 9
 [6 7] 5
 [4 8] 8
 [4 9] 4
 [5 9] 1
 [6 9] 7
 [7 2] 1
 [9 1] 9
 [7 3] 4
 [7 4] 7
 [8 5] 3
 [9 5] 1
 [8 6] 4
 [7 8] 5})

