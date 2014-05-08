(ns mese-client.util)

(defn map-to-values [fun map]
  (into {} (for [[k v] map] [k (fun v)])))

(defn in? [seq elm]
  (some #(= % elm) seq))

(defn seq-in-seq? [subseq seq]
  (->> subseq
       (map (partial in? seq))
       (reduce #(and %1 %2))))
