(use
  'clojure.core)

(require
  '[clojure.string :as str])

(defn validator [msg k]
  (if (re-matches #"^[A-Za-z\s_]+$" msg)
    (> k 1)
    false))

(defn replace [msg]
  (str/replace msg #" " "_"))

(defn calculate-idx [w h]
  (def row 0)
  (def direction-down false)
  (vec
   (for [col (range 0 w)]
     (do
       (if (or (= 0 row) (= row (- h 1)))
         (def direction-down (not direction-down)))
       (def matrix-index (+ (* row w) col))
       (def row
         (if (true? direction-down)
           (inc row)
           (dec row)))
       matrix-index))))

(defn second-str-idx [cell k]
  (mod cell k))

(defn encrypt-ch [msg k]
  (zipmap
   (calculate-idx (count msg) k)
   (vec msg)))

(defn decrypt-ch [msg k]
  (zipmap
   (sort (calculate-idx (count msg) k))
   (vec msg)))


(defn encrypt [msg k]
  (str/join
   (for [[pos char]
         (sort-by first (encrypt-ch msg k))]
     char)))
(defn encrypt-msg [msg k]
  (if (validator msg k)
    (encrypt (replace msg) k)
    (str "Invalid [msg] or [k]")))


(defn decrypt [msg k]
  (str/join
   (for [[pos ch]
         (sort-by first
                  (for [[index char] (decrypt-ch msg k)]
                    [(second-str-idx index (count msg)) char]))]
     ch)))
(defn decrypt-msg [msg k]
  (if (validator msg k)
    (decrypt (replace msg) k)
    (str "Invalid [msg] or [k]")))