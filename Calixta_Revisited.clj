;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(defn randFlipListTest []
  (repeatedly #(rand-int 2)))

(defn randFlipList [n]
  (repeatedly n #(rand-int 2)))

(defn countRunTest [L]
  (partition-by identity L) )

(defn getTop5Runs [n]
  (take-last 5 (sort (map count (partition-by identity (randFlipList n))))))

(defn maxRunGenerator [n]  ;;gets max run from flip sequence size n
  (take-last 1 (sort (map count (partition-by identity (randFlipList n))))))

(defn average [L]
  (/ (reduce + L) (count L)))

(defn top5ListCreator [n]
  (repeatedly n #(getTop5Runs 10000)))

(defn maxListCreator [n size]  ;;creates n lists of length 'size'
  (repeatedly n #(maxRunGenerator size)))

;;(defn maxListMergeHelper [n size] ;;A 'wrapper' function which returns a single list, not a list of lists.
;;  (reduce concat (maxListCreator n size)))

(defn maxAverageWrapper[L]    ;;converts list L from a list of lists into a single list of numbers, takes average, then converts to double.
  (double (average(reduce concat L))))


(defn countRun [L]
  ((fn recCount [prev run L]
     ;;(nil? L nil)
     (peek L)
     (if (prev == (peek L))
       (recCount (pop L) (inc run) (next L))
       (recCount (pop L) (1) (rest L))))
         nil nil L))


(+ -0.5 (+ (+ (/ 0.5772156649 (Math/log 2))) (/ (Math/log 10) (Math/log 2))))

(repeatedly 10 #(double (average(reduce concat (maxListCreator 1000 100)))))
(maxAverageWrapper (maxListCreator 1000 20))


;; (average (MaxRunGenerator 100000))
;;(countRun '(1 1 0))
;;(partition-by identity (randFlipList 3))
;;(map count (partition-by identity (randFlipList 10)))
;;(take-last 5 (sort (map count (partition-by identity (randFlipList 100000)))))


;;(cons (length '((0 0) (1) (0) (1 1))))
