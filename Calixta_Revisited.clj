;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.


(defn rand-flip-list-test []
  (repeatedly #(rand-int 2)))

(defn rand-flip-list [n]
   (repeatedly n #(rand-int 2)))

(defn count-run-test [L]
  (partition-by identity L) )

(defn get-top-5-runs [n]
  (take-last 5 (sort (map count (partition-by identity (rand-flip-list n))))))

(defn max-run-generator [n]  ;;gets max run from flip sequence size n
  (take-last 1 (sort (map count (partition-by identity (rand-flip-list n))))))

(defn average [L]
  (/ (reduce + L) (count L)))

(defn top-5-list-creator [n]
  (repeatedly n #(get-top-5-runs 10000)))

(defn max-list-creator [n size]  ;;creates n lists of length 'size'
  (repeatedly n #(max-run-generator size)))

;;(defn maxListMergeHelper [n size] ;;A 'wrapper' function which returns a single list, not a list of lists.
;;  (reduce concat (maxListCreator n size)))

(defn max-average-wrapper[L]    ;;converts list L from a list of lists into a single list of numbers, takes average, then converts to double.
  (double (average(reduce concat L))))

;;(defn max-average-wrapper-filter [attempts n])

(defn map-list-count [n]
  (let [
        lst (->> (rand-flip-list n)
                 (partition-by identity)
                 (map #(count %))
                 (sort)
                 (partition-by identity))
        kwords (->> (map first lst)
                    (map str)
                    (map keyword))
        cts (map count lst)]
    (zipmap kwords cts)))

(defn heads-list-count [n]
   (let [
        lst (->> (rand-flip-list n)
                 (partition-by identity)
                 (filter #(not (zero? (first %))))
                 (map #(count %))
                 (sort))
                 ;(partition-by identity))

        kwords (->> lst
                    (partition-by identity)
                    (map first)
                    (map str)
                    (map keyword))
       sequence-cts (map count (partition-by identity lst))]
     (zipmap kwords sequence-cts)))

(defn heads-run-count [num-of-lists list-size]
  (->> (repeatedly num-of-lists #(heads-list-count list-size))
     (apply merge-with +)
   ))

(heads-run-count 20 20)

(let[x (partition-by identity '(1 0 0 0 1))]
  (filter #(not (zero? (first %))) x)
  )


(defn max-run-heads [map-list])

(defn max-run [map-list]
  (->> (keys map-list)
       (map name)
       (map read-string)
       (apply max)
       ))


(defn max-run-occurrences [map-list]
  (->> (max-run map-list)
       (str)
       (keyword)
       (map-list))
  )

(max-run (map-list-count 10000))
(let [trials 1000]
(double (/ (reduce + (repeatedly trials #(max-run (map-list-count 1000)))) trials))

  )

(->> (rand-flip-list 50)
     (partition-by identity)
     (map count)
     ;(map #(reduce + %))
     (sort)
     (partition-by identity)
     ;(hash-map (map #(keyword %)))
     )


;(def lst (partition-by identity '(0 0 0 1 2 3)))
;(def kwords (map keyword (map str (map first lst))))
;(def cts (map count lst))

(map hash-map kwords cts)


(sort (map #(reduce + %) (partition-by identity(vec (rand-flip-list 50)))))

(->> (vec (rand-flip-list 50))
 (partition-by identity )
 (map #(reduce + %))
 (apply max ))

(defn test-average-wrapper[L running-avg n]
   (if (empty? L) running-avg

   (let [new-avg
   (/ (* (+ running-avg (first L)) n) (+ n 1))]
   (test-average-wrapper (rest L) new-avg (+ n 1)))))

   ;;(let [new-avg running-avg]
   ;;(+ new-avg (first L))
   ;;(* new-avg n)
   ;;(/ running-avg (+ n 1))
   ;;(recur test-average-wrapper (rest L) running-avg (+ n 1))


(test-average-wrapper '(2 3 2 5) 0 0)


(defn countRun [L]
  ((fn recCount [prev run L]
     ;;(nil? L nil)
     (peek L)
     (if (prev == (peek L))
       (recCount (pop L) (inc run) (next L))
       (recCount (pop L) (1) (rest L))))
         nil nil L))


(+ -0.5 (+ (+ (/ 0.5772156649 (Math/log 2))) (/ (Math/log 10) (Math/log 2))))

(repeatedly 10 #(double (average(reduce concat (max-list-creator 1000 10)))))
(max-average-wrapper (max-list-creator 100 20))


;; (average (MaxRunGenerator 100000))
;;(countRun '(1 1 0))
;;(partition-by identity (randFlipList 3))
;;(map count (partition-by identity (randFlipList 10)))
;;(take-last 5 (sort (map count (partition-by identity (randFlipList 100000)))))


;;(cons (length '((0 0) (1) (0) (1 1))))
