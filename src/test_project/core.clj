(ns test-project.core)
(use 'test-project.defo)

;P01
(defn p01
  [l]
  (if (next l)
    (recur (next l))
    (first l)))
(p01 '(:a :b :c :d))

;P02
(defn p02
  [l]
  (if (next (next l))
    (recur (next l))
    (first l)))
(p02 '(:a :b :c :d))

:P03
(defn p03
  [l i]
  (if (= i 1)
    (first l)
    (recur (next l) (- i 1))))
(p03 '(:a :b :c :d :e) 3)

:P04
(defn p04
  [coll]
  (if (seq coll)
    (+ 1 (p04 (next coll)))
    0))
(p04 '(:a :c :d :e))
  
:P05
(defn p05
  "Reverse a list"
  [coll & revcoll]
  (if (seq coll)
    (recur (next coll) (conj revcoll (first coll)))
    revcoll))
(p05 '(:a :b :c :d :e))

:P06
(defn p06
  "Find out whether a list is a palendrome"
  [coll]
  (= coll (reverse coll)))
(p06 '(:a :b :c :d :e))
(p06 '(:a :b :c :b :a))

:P07
(defn p07
  "Flatten a nested list structure"
  [coll]
  (filter (complement seq?) (tree-seq seq? seq coll)))
(p07 '(:a (:c :d) :e))


:P08
(defn filter2
  "filter based on the next two elements in a sequence"
  [pred coll]
  (when-let [s (seq coll)]
    (let [f (first s) g (second s) r (next s)]
      (if (pred f g)
	(cons f (filter2 pred r))
	(filter2 pred r)))))
(defn p08
  "Eliminate consecutive duplicates of list elements"
  [coll]
  (filter2 #(not= %1 %2) coll))
(p08 '( :a :a :a :a :b :c :c :a :a :d :e :e :e :e))

(defn p09_orig
  "Pack consecutive duplicates of a list into sublists"
  [coll]
  (when-let [s (seq coll)]
    (let [f (first s)
	  r (p09 ( next s))
	  rf (first r)
	  ]
      (if (seq? r)
	  (if (= f (first rf))
	    (cons (cons f rf) (next r))
	    (cons (list f) r))
	  (list (list f))))))
(defn p09
  "Pack consecutive duplicates of a list into sublists"
  ( [coll]
      (p09 [[(first coll) ]] (next coll) ))
  ( [ans coll]
      (if ( seq coll)
	(if (= (first coll) (peek (peek ans)))
	  (p09 (conj (pop ans) (conj (peek ans) ( first coll))) (next coll))
	  (p09 (conj ans [( first coll)]) (next coll)))
	  
	ans)))
(p09 '[ :a :a :a :a :b :c :c :a :a :d :e :e :e :e])

(defn p10
  "Run-length encoding of a list"
  [coll]
  (map #( vec (list ( count %)(first %) )) (p09 coll)))
(p10 [ :a :a :a :a :b :c :c :a :a :d :e :e :e :e])

(defn p11
  "Modified run-length encoding"
  [coll]
  (map #(if (= 1 (count %))
	  (first %)
	  (vec (list (count %)(first %) )))
       (p09 coll)))
(p11 [ :a :a :a :a :b :c :c :a :a :d :e :e :e :e])

(defn p12
  "Decode run-length encoded list generated from p11"
  ( [coll] (vec (p12 [] coll)))
  ([ans coll]
     (if (seq? coll)
       (let [f (first coll)]
	 (prn (vector? f))
	 (if (vector? f)
	   (p12 (concat ans (repeat (first f) (second f))) (next coll))
	   (p12 (concat ans (list f)) (next coll))))
       ans)))
(p12 (p11 [ :a :a :a :a :b :c :c :a :a :d :e :e :e :e]))

(defn p13
  "Redo p11 without using p09"
  ([coll] ( p13 [] (seq coll)) )
  ([ans coll]
     (if (seq? coll)
       (let [f (first coll)
	     f2 (peek ans)]
	 (if (and (vector? f2) (= f (f2 1)))
	   (p13 (conj (pop ans) (update-in f2 [0] inc)) (next coll))
	   (if (= f f2)
	     (p13 (conj (pop ans) (vector 2 f)) (next coll))
	     (p13 (conj ans f ) (next coll)))))
       ans)))
(p13 [ :a :a :a :a :b :c :c :a :a :d :e :e :e :e])


(defn p14
  "Duplicate the elements of a list"
  ([coll] (p14 [] (seq coll)))
  ([ans coll]
     (if (seq coll)
       (let [f (first coll)]
	 (p14 (conj (conj ans f) f) (rest coll)))
       ans)))
(p14 [:a :b :c :c :d])

(defn p15
  "Replicate the elements of a list a given number of times")


;John Aspden Macro Tutorial
(let [x# (* 5 1)]
  (println '(* 5 1))
  x#)
(defmacro dbg
  [ & rest ]
  `(let [x# ~rest]
     (println '~rest " := " x#)
     x#))
(dbg + (dbg * 5  2) 2)
(dbg print "hi")
(macroexpand '(dbg print "hi"))

(defmacro forloop
  [[i start fin] & body]
  `(loop [~i ~start]
     (when (<= ~i ~fin)
       ~@body
       (recur (inc ~i)))))
(forloop [i 1 10]
	 (print i "^2 = ")
	 (println (* i i)))


;John Aspden Speed tutorial
(defn f [t y] (- t y))
(defn solveit
  [t0 y0 t_step steps]
  (if (> steps  0)
    (recur (+ t0 t_step) (+ y0  (* (f t0 y0) t_step )) t_step (dec steps))
    [t0 y0]))
(solveit 0.0 0.0 0.1 10)
(let [steps '(1 10 100 1000 10000 100000)
      results (map #(second ( solveit 0.0 0.0 (/ 1 %) %)) steps)
      errors (map #(- (Math/exp -1) %) results)]
   (interleave steps results errors)
  )

(defmacro timeit
  [code iters]
  `(let [start# (System/nanoTime)
	 ret# (~@code (/ 1 ~iters) ~iters)
	 fin# (System/nanoTime)]
     (println ret#)
     (int (/ (- fin# start#) ~iters))))

(let [results (take 5 (repeatedly #(timeit (solveit 0.0 0.0 ) 100000 )))]
  (println results)
  (println (int ( / (reduce + results) 5))))
;19358

(defn solveit-2
  [t0 y0 t_step steps]
  (let [zero (int 0)]
    (loop [t0 (double t0)
	   y0 (double y0)
	   t_step (double t_step)
	   steps (int steps)
	   ]
      (if (> steps  zero)
	(recur (+ t0 t_step) (+ y0  (* (- t0 y0) t_step )) t_step (dec steps))
	[t0 y0]))))
(let [results (take 5 (repeatedly #(timeit (solveit-2 0.0 0.0 ) 1000000 )))]
  (println results)
  (println (int ( / (reduce + results) 5))))

(defmacro def-let
  [bindings & rest]
  (let [let-expr (macroexpand `(let ~bindings))
	names-values (partition 2 (second let-expr))
	defs (map #(cons 'def %) names-values)]
    (concat (list 'do) defs rest)))

					;Toy with default bindings
(defo f [a b &optional c & d] (inc b))
(f 1 :b 100)

(defo foo
  "A function with multiple optional parameters"
  [[a1 a2] ([b1 b2] [12 10]) (c 20) & d]
  ( println a1 a2 b1 b2 c))

(defn foo
  [[a1 a2]]
  (println a1 a2))
(foo '(1 2))
(seq? [1 2])

(defn simple
  [a & b]
  @body
  )
(defn fin-simple
  [a & rest]
  (let [[ & b] rest]
    @body
    ))


(extract-default-symbol '(a))
(extract-default-symbol '( a 10))
(extract-default-symbol '[ ( a 10) (b 12)])
(extract-default-symbol '[b00  [b10 (b11 12) (b12 14)] (b2 10)])
       

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
(macroexpand-1 '(defo foo
		 [z (a 1) [b1 (b2 12)]]
		 (println a b2)))
(macroexpand-1 '(defo foo2 [a] (println a)))
(foo2 1)

(from-seq '[ z [a1 a2] [b00 [b10 (b11 11) (b12 12)] (b20 14)] (c 20) d & e] [] [] [] false)
(extract-default '[b00  [b10 (b11 12) (b12 14)] (b2 10)] )
;	b11 (if (nil? b11) 11 b11)
;	b12 (if (nil? b12) 12 b12)
;	b20 (if (nil? b20) 14 b20)

(extract-default '([(b1 12) (b2 10)]))
;      b1 '(if (nil? b1) 12 b1)
;      b2 '(if (nil? b2) 10 b2)]

(extract-default '(c 20))
;  [c '(if (nil? c) 20 c)]
(extract-default 'd)
; []
(from-seq l1 [] [] []  false)
(from-seq '( [a1 a2] [b00 [b10 (b11 11) (b12 12)] (b20 14)] (c 20) d & e) [] [] [] false)
(from-seq l2 [] [] []  false)


(defn args
  [[a1 a2] [b00 [b10 (b11 11) (b12 12)] (b20 14)] (c 20) d & e]
  ~@body)
(defn fin-args
  [[a1 a2] & rest]
  (let [[[b00 [ b10 b11 b12] b20 ] c d & e] rest
	b11 (if (nil? b11) 11 b11)
	b12 (if (nil? b12) 12 b12)
	b20 (if (nil? b20) 14 b20)
	c (if (nil? c) 20 c)]
    ~@body))
(defo foo
    [z (a 1) [b1 (b2 12)]]
    (println a b2))
(foo 0)
(defmacro defo
  [fn-name & fn-tail]
 (println fn-tail)
  (let [[fn-name [args & body]] (name-with-attributes fn-name fn-tail)
	[pos kw-vals] (split-with symbol? args)
	syms (map #(-> % name symbol) (take-nth 2 kw-vals))
	values (take-nth 2 (rest kw-vals))
	sym-vals (apply hash-map (interleave syms values))
	de-map {:keys (vec syms)
		:or sym-vals}]
    `(defn ~fn-name
       [~@pos & options#]
       (let [~de-map (apply hash-map options#)]
	 (println '~pos '~syms)
	 ~@body))))
