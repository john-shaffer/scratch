(defn unwrap [x]
  (if (and (sequential? x) (= 1 (count x)) (not (string? x)))
    (unwrap (first x))
    x))

(defn collapse [x]
  (prn x)
  (if (or (string? x) (not (sequential? x)))
    x
    (if (= 1 (count x))
      (let [y (unwrap x)]
        (if (keyword? y)
          [y]
          y))
      (let [z (first x)]
        (if (keyword? z)
          (let [more (unwrap (mapv collapse (rest x)))]
            (if (keyword? (first more))
              [z more]
              (apply vector z more)))
          (mapv collapse x))))))

(use 'clojure.test)

(is (= [:div] (collapse [[:div]])))
(is (= [[:div] [:div]] (collapse [[:div] [:div]])))
(is (= [:div [:div] [:div]] (collapse [:div [[:div] [:div]]])))
(is (= [:div [:div] "s"] (collapse [:div [[:div] "s"]])))
(is (= [:div [:div "s" [:div]]] (collapse [:div [:div "s" [:div]]])))
