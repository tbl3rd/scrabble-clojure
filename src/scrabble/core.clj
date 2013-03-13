;;; lein run -m scrabble.core WORD.LST agency
;;; jar -xvf scrabble-standalone.jar scrabble/core.clj
;;; java -jar scrabble-standalone.jar WORD.LST agency
;;; java -jar scrabble-standalone.jar \
;;;   http://itasoftware.com/careers/work-at-ita/PuzzleFiles/WORD.LST agency

(ns scrabble.core
  (:gen-class))

(defn show-usage
  "Show a usage message for this program."
  [] (doseq
         [line
          ["Find words that can be spelled with some letters"
           "as in the Scrabble board game."
           "Usage: scrabble <words-url> <letters> ..."
           "Where: <words-url> is the URL of a list of words."
           "       <letters> ... are letters available to compose words."
           "Print a list of words that can be spelled, longest first."]]
       (println line))
  (System/exit 1))

(defn read-lines
  "Return a sequence of lines from FILE."
  [file]
  (line-seq (clojure.java.io/reader (clojure.java.io/input-stream file))))

(defn make-matcher
  "Return a semi-predicate on word that returns [(count word) word]
   when it can be constructed from the Scrabble tiles.
   Otherwise the predicate returns nil."
  [tiles]
  (let [rack (frequencies tiles)
        max (count tiles)]
    (fn [word]
      (let [length (count word)]
        (if (>= max length)
          (let [counts (frequencies word)]
            (if (every? #(>= (or (rack %) 0) (counts %)) (keys counts))
                 [length word])))))))

(defn scrabble
  "Return the words read from words-url that can be spelled
   with the letters in the Scrabble tiles.
   The result is a map of word length
   to a vector of matching words of that length."
  [words-url tiles]
  (let [matcher (make-matcher tiles)]
    (reduce
     (fn [result word]
       (let [[length match] (matcher word)
             already (or (result length) [])]
         (conj result (if match [length (conj already match)]))))
     {} (read-lines words-url))))

(defn -main [& args]
  (try
    (let [words-url (first args)
          tiles (apply str (rest args))
          result (scrabble words-url tiles)]
      (if (> (count tiles) 0)
        (doseq
            [[length matches] (into (sorted-map-by >) result)]
          (println length ":" (apply str (interpose ", " matches))))
        (show-usage)))
    (System/exit 0)
    (catch Exception _
      (show-usage))))
