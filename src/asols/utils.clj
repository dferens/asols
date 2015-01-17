(ns asols.utils)


(defn convert-base
  [num to-base]
  (if (zero? num)
    (list 0)
    (loop [num num
           digits ()]
      (if (pos? num)
        (recur (quot num to-base)
               (conj digits (mod num to-base)))
        digits))))