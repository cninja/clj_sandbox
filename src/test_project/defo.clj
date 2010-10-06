(ns test-project.defo)
(use 'clojure.contrib.def)
(defmacro defo
  [fn-name & fn-tail]
  (letfn [(add-if [sym def] `(~sym (if (nil? ~sym) ~def ~sym)))

	  (extract-symbol
	   [arg]
	   (if (seq? arg)
	     (first arg)
	     (if (vector? arg)
	       (vec (map extract-symbol arg))
	       arg)))

	  (extract-default
	   [arg]
	   (if (seq? arg)
	     (add-if (first arg) (second arg))
	     (if (vector? arg)
	       (mapcat extract-default arg))))

	  (has-default?
	   [arg]
	   (or (seq? arg)
	       (and (vector? arg)
		    (some has-default? arg))))

	  (parse-args
	   ([args norm-args]
	      (if args
		(let [f (first args)]
		  (if (or (has-default? f)
			  (and (not (coll? f)) (= (name f) "&" )))
		    (parse-args (next args) norm-args [(extract-symbol f)] (vec (extract-default f)))
		    (recur (next args) (conj norm-args f))))
		[norm-args [] []]))
	   ([args norm-args rest-args rest-defaults]
	      (if args
		(recur (next args) norm-args
		       (conj rest-args (extract-symbol (first args)))
		       (concat rest-defaults (extract-default (first args))))
		[norm-args rest-args rest-defaults])))
	  ]
    (let [[fn-name [args & body]] (name-with-attributes fn-name fn-tail)
	  [norm-args rest-args rest-defaults] (parse-args args [])]
      `(defn ~fn-name
	 [~@norm-args & rest#]
	 (let [[~@rest-args] rest#
	       ~@rest-defaults]
	   ~@body)))))
