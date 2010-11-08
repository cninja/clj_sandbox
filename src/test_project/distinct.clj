					;Is this really that much faster?

(defn distinct3
  [coll]
  (lazy-seq
   ((fn step [[f :as xs] seen]
      (when-let [s (seq xs)]
	 (if (contains? seen f)
	   (recur (rest xs) seen)
	   (cons f (step (rest xs) (conj seen f))))))
    coll #{})))

(def l (take 1000000 (cycle [:a :b :c :a :b :d :d :b :e :a :f])))
(do
  (println "start")
  (time (dotimes [n 1000000] (distinct3 l)))
  (time (dotimes [n 1000000] (distinct l)))
  (time (dotimes [n 1000000] (distinct3 l)))
  (time (dotimes [n 1000000] (distinct l)))
  (time (dotimes [n 1000000] (distinct3 l)))
  (time (dotimes [n 1000000] (distinct l)))
  (time (dotimes [n 1000000] (distinct3 l)))
  (time (dotimes [n 1000000] (distinct l)))
  (time (dotimes [n 1000000] (distinct3 l)))
  (time (dotimes [n 1000000] (distinct l)))

  )
(println (distinct3 l))
