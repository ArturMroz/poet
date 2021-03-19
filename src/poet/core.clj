(ns poet.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))


(defn word-chain [word-transitions]
  (reduce
   (fn [acc [a b c]]
     (merge-with set/union
                 acc
                 {[a b] (if c #{c} #{})}))
   {}
   word-transitions))


(defn text->word-chain [text]
  (->> (str/split text #"\s|\n")
       (partition-all 3 1)
       word-chain))


(defn chain->text [chain]
  (str/join " " chain))


(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix           (first (shuffle suffixes))
            new-prefix       [(last prefix) suffix]
            result-count     (count (chain->text result))
            suffix-count     (inc (count suffix))
            new-result-count (+ result-count suffix-count)]
        (if (>= new-result-count 140)
          result
          (recur new-prefix chain (conj result suffix)))))))


(defn generate-text
  [start-phrase word-chain]
  (let [prefix       (str/split start-phrase #" ")
        result-chain (walk-chain prefix word-chain prefix)]
    (chain->text result-chain)))


(defn end-at-last-punctuation [text]
  (let [trimmed-to-last-punct (apply str (re-seq #"[\s\w]+[^.!?,]*[.!?,]" text))
        trimmed-to-last-word  (apply str (re-seq #".*[^a-zA-Z]+" text))
        result-text           (if (empty? trimmed-to-last-punct)
                                trimmed-to-last-word
                                trimmed-to-last-punct)
        cleaned-text          (str/replace result-text #"[,| ]$" ".")]
    (str/replace cleaned-text #"\"" "'")))


(defn process-file [fname]
  (->> (clojure.java.io/resource fname)
       slurp
       text->word-chain))


(def files ["poem1.txt" "poem2.txt" "poem3.txt"])


(def prefix-list
  ["On the" "They went" "And all" "We think" "For every" "No other" "To a" "And
  every" "We, too," "For his" "And the" "But the" "Are the" "The Pobble" "For
  the" "When we" "In the" "Yet we" "With only" "Are the" "Though the"  "And
  when" "We sit" "And this" "No other" "With a" "And at" "What a" "Of the" "O
  please" "So that" "And all" "When they" "But before" "Whoso had" "And nobody"
   "And it's" "For any" "For example," "Also in" "In contrast"])


(def training-set
  (->> (map process-file files)
       (apply merge-with set/union)))


(defn create-poetry []
  (->> training-set
       (generate-text (rand-nth prefix-list))
       end-at-last-punctuation))


(create-poetry)
