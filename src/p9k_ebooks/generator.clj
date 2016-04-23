(ns p9k-ebooks.generator
  (:require [clojure.set]
            [clojure.java.io :as io]
            [clojure-csv.core :as csv]))

;; TODO:
;;  * organize file
;;  * maybe replace # with ♯ so as not to polute search
;;  * kill words like "kill"


(def ngram-size 2)

(defn word-transitions [sample]
  "Transform text into trigrams"
  (let [words (clojure.string/split sample #"[\s|\n]")]
    (partition-all (inc ngram-size) 1 words)))

(defn word-chain [partitions]
  (reduce (fn [r t]
            (merge-with clojure.set/union r
                        (if (= (inc ngram-size) (count t))
                          ;; we have a suffix
                          {(vec (take ngram-size t)) #{(last t)}}
                          ;; we have no suffix
                          {(vec (take ngram-size t)) #{}}
                          )))
          {}
          partitions))

(defn vector->word-chain [text-vector]
  (->> text-vector
      (mapcat word-transitions)
      word-chain))

(defn chain->text [[initial & remaining]]
  (apply str (clojure.string/capitalize initial) " " (interpose " " remaining)))

(defn walk-chain [chain result]
  "Build a chain until the text version would hit 140 characters"
  (let [prefix (take-last ngram-size result)
        suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [prefix suffix]
            result-with-spaces (chain->text result)
            result-char-count (count result-with-spaces)
            suffix-char-count (inc (count suffix))
            new-result-char-count (+ result-char-count suffix-char-count)]
        (if (> new-result-char-count 140)
          result
          (recur chain (conj result suffix)))))))


(defn generate-text [prefix chain]
  (let [;prefix (clojure.string/split prefix #" ")
        result-chain (walk-chain chain prefix)
        result-text (chain->text result-chain)]
    result-text))

(def tweets
  (->
   (with-open [tweets-file (io/reader "resources/tweets.csv")]
     (doall (csv/parse-csv tweets-file)))
   rest ;; ignore header
   ))


(defn not-retweet? [tweet-row]
  (= "" (nth tweet-row 6)))

(defn tweet-text [tweet-row]
  (nth tweet-row 5))

(defn redact-mentions [text]
  (clojure.string/replace text #"@\S*" ""))

(defn redact-links [text]
  (clojure.string/replace text #"http\S*" ""))

(def original-tweets-corpus
  (->>
   (filter not-retweet? tweets)
   (map tweet-text)
   (map redact-mentions)
   (map redact-links)))


(def branching-prefixes
  "All potential starting points for the generator"
  (keys (filter (fn [[prefix suffixes]]
                  (and (not (empty? suffixes))
                       (re-find #"^[A-Za-z]+[^\.,!\(\)]$" (first prefix)))
                       ) ; words not ending a sentence
                (vector->word-chain original-tweets-corpus))))


(defn finalize-phrase [phrase]
  (-> phrase
      (clojure.string/replace #"[.,][^\.,]*$" ".")))

(defn valid? [phrase]
  (> (count phrase) 20))

(defn gen-random []
  "Generate a random phrase that looks like @p9k coulda twote it"
  (let [prefix (-> branching-prefixes rand-nth)
        phrase (finalize-phrase (generate-text prefix (vector->word-chain original-tweets-corpus)))]
    (if (valid? phrase)
      phrase
      (recur))
      ))

;; frequency counting of source texts
(defn freq-words [stop-words-count]
  "Returns frequent words that aren't the *most* frequent"
  (let [corpus (flatten original-tweets-corpus)
        corpus-list (flatten (map #(clojure.string/split % #"\s") corpus))
        freqs (frequencies corpus-list)]
    (->> freqs
         (sort-by val)
         reverse
         (take 200)
         (drop stop-words-count)
         println)))

(gen-random)
