(ns p-p-p-pokerface)

(def numeric-value-of {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (numeric-value-of rank)
      )))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        full-house [2 3]]
    (= (sort (vals (frequencies ranks))) full-house)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        two-pairs [1 2 2]]
    (cond
      (four-of-a-kind? hand) true
      (= (sort (vals (frequencies ranks))) two-pairs) true
      :else false)))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
