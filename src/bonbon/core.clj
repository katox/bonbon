(ns bonbon.core
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [clojure.set :refer [union intersection difference]]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(def num-pralines (* 5 11))

(def praline-kinds #{:manon :eve :desiree :mystere :lingot})
(def colors #{:yellow :red :green :brown})
(def special-characters #{:c1 :c2 :c3 :c4 :c5 :c6 :c7 :c8})

(s/def ::praline praline-kinds)
(s/def ::color colors)
(s/def ::special special-characters)

(s/def ::single-flavour (s/coll-of ::color :kind set? :count 1))
(s/def ::double-flavour (s/coll-of ::color :kind set? :count 2 :distinct true))
(s/def ::quad-flavour (s/coll-of ::color :kind set? :count 4 :distinct true))

(s/def ::flavour (s/or :single ::single-flavour :double ::double-flavour :quad ::quad-flavour))
(s/def ::bonbon (s/keys :req [::praline ::flavour]))
(s/def ::character (s/keys :req [::special]))

(s/def ::card (s/or :bonbon ::bonbon ::character ::character))
(s/def ::deck (s/coll-of ::card :kind set? :count (+ num-pralines 8) :distinct true
                         :gen #(gen/set (s/gen ::card)    ;use custom generator to generate the complete deck
                                        {:num-elements (+ num-pralines 8) :max-tries 10000})))

(def complete-deck (gen/generate (s/gen ::deck)))
(s/def ::hand (s/coll-of complete-deck :kind set? :count 7))

(defn common-color?
  "Truthy iff there is a common color among all flavour combinations"
  [flavours]
  (not-empty
    (apply intersection flavours)))

(defn no-common-color?
  "Truthy iff there is no common color between any pair of flavours"
  [flavours]
  (every? (fn [[a b]] (empty? (intersection a b))) (combo/combinations flavours 2)))

(defn unique-kinds?
  "Truthy iff every kind is different from each other"
  [kinds]
  (apply = 1 (vals (frequencies kinds))))

(defn extract-flavours
  "Extract the flavour part of bonbon structure"
  [bonbons]
  (map ::flavour bonbons))

(defn extract-kind
  "Extract the praline part of bonbon structure"
  [bonbons]
  (map ::praline bonbons))

(defn filter-frequent-vals
  "Return set of values that occur at-least-n times in val-seq"
  [at-least-n val-seq]
  (->> val-seq
       frequencies
       (filter (fn [[_ occurences]] (>= occurences at-least-n)))
       keys
       set))

(defn same-flavour-bonbonieres
  "Return the maximum number of bonbonieres of the same flavour in a set of bonbons"
  [bonbons]
  (let [frequent-flavours (filter-frequent-vals 3 (mapcat ::flavour bonbons)) ;find flavours with at least 3 occurrences
        candidates (filter #(intersection frequent-flavours (::flavour %)) bonbons) ;keep bonbons with such flavours
        bonbonieres (->> (combo/combinations candidates 3)  ;try all 3-tuples of bonbons
                         (filter (comp common-color? extract-flavours))) ; keep only tuples with common color
        rest-counts (for [bonboniere bonbonieres]           ;for every bonboniere examine rest of bonbons in hand
                      (inc
                        (same-flavour-bonbonieres
                          (difference (set bonbons) (set bonboniere)))))]
    (if (empty? bonbonieres)
      0                                                     ;no other bonboniere found
      (apply max rest-counts))))                            ;pick the combo with max number of bonbonieres

(defn unique-flavour-bonbonieres
  "Return the maximum number of bonbonieres with different flavours in a set of bonbons"
  [bonbons]
  (let [bonbonieres (->> (combo/combinations bonbons 3)     ;try all 3-tuples of bonbons
                         (filter (comp no-common-color? extract-flavours))) ; keep only tuples with no common color
        rest-counts (for [bonboniere bonbonieres]           ;for every bonboniere examine rest of bonbons in hand
                      (inc
                        (unique-flavour-bonbonieres
                          (difference (set bonbons) (set bonboniere)))))]
    (if (empty? bonbonieres)
      0                                                     ;no other bonboniere found
      (apply max rest-counts))))                            ;pick the combo with max number of bonbonieres

(defn arbitrary-flavour-bonbonieres
  "Return the maximum number of bonbonieres with arbitrary flavours in a set of bonbons"
  [bonbons]
  (let [bonbonieres (->> (combo/combinations bonbons 3))    ;try all 3-tuples of bonbons
        rest-counts (for [bonboniere bonbonieres]           ;for every bonboniere examine rest of bonbons in hand
                      (inc
                        (arbitrary-flavour-bonbonieres
                          (difference (set bonbons) (set bonboniere)))))]
    (if (empty? bonbonieres)
      0                                                     ;no other bonboniere found
      (apply max rest-counts))))                            ;pick the combo with max number of bonbonieres

(defn same-kind-bonbonieres
  "Return the number of bonbonieres of the same kind in a set of cards (best of combinations)"
  [flavour-fn cards]
  (let [candidates (->> cards
                        (remove ::special)
                        (sort-by ::praline)
                        (partition-by ::praline)
                        (filter #(>= (count %) 3)))
        bonboniere-count (map flavour-fn candidates)]
    (apply + bonboniere-count)))

(defn same-kind-same-flavour-bonbonieres
  "Return the number of bonbonieres of the same kind and flavour in a set of cards (best of combinations)"
  [cards]
  (same-kind-bonbonieres same-flavour-bonbonieres cards))

(defn same-kind-unique-flavours-bonbonieres
  "Return the number of bonbonieres of the same kind and unique flavours in a set of cards (best of combinations)"
  [cards]
  (same-kind-bonbonieres unique-flavour-bonbonieres cards))

(defn same-kind-arbitrary-flavour-bonbonieres
  "Return the number of bonbonieres of the same kind and arbitrary flavours in a set of cards (best of combinations)"
  [cards]
  (same-kind-bonbonieres arbitrary-flavour-bonbonieres cards))

(defn unique-kind-same-flavour-bonbonieres
  "Return the number of bonbonieres of the unique kinds and the same flavour in a set of cards (best of combinations)"
  [cards]
  (let [bonbons (remove ::special cards)                    ;disregard all special cards
        bonbonieres (->> (combo/combinations bonbons 3)     ;try all 3-tuples of cards
                         (filter (comp unique-kinds? extract-kind)) ;keep those with different praline kinds
                         (filter (comp common-color? extract-flavours))) ;and at least one common color
        rest-counts (for [bonboniere bonbonieres]           ;for every bonboniere examine rest of cards in hand
                      (inc
                        (unique-kind-same-flavour-bonbonieres
                          (difference (set bonbons) (set bonboniere)))))]
    (if (empty? bonbonieres)
      0                                                     ;no other bonboniere found
      (apply max rest-counts))))

(defn unique-kind-unique-flavour-bonbonieres
  "Return the number of bonbonieres of the unique kind and the unique flavour in a set of cards (best of combinations)"
  [cards]
  (let [bonbons (remove ::special cards)                    ;disregard all special cards
        bonbonieres (->> (combo/combinations bonbons 3)     ;try all 3-tuples of cards
                         (filter (comp unique-kinds? extract-kind)) ;keep those with different praline kinds
                         (filter (comp no-common-color? extract-flavours))) ;keep those with one unique color
        ;_ (pprint bonbonieres)
        rest-counts (for [bonboniere bonbonieres]           ;for every bonboniere examine rest of cards in hand
                      (inc
                        (unique-kind-unique-flavour-bonbonieres
                          (difference (set bonbons) (set bonboniere)))))]
    (if (empty? bonbonieres)
      0                                                     ;no other bonboniere found
      (apply max rest-counts))))

(defn unique-kind-arbitrary-flavour-bonbonieres
  "Return the number of bonbonieres of the unique kinds and arbitrary flavours in a set of cards (best of combinations)"
  [cards]
  (let [bonbons (remove ::special cards)                    ;disregard all special cards
        bonbonieres (->> (combo/combinations bonbons 3)     ;try all 3-tuples of cards
                         (filter (comp unique-kinds? extract-kind))) ;keep those with different praline kinds
        rest-counts (for [bonboniere bonbonieres]           ;for every bonboniere examine rest of cards in hand
                      (inc
                        (unique-kind-arbitrary-flavour-bonbonieres
                          (difference (set bonbons) (set bonboniere)))))]
    (if (empty? bonbonieres)
      0                                                     ;no other bonboniere found
      (apply max rest-counts))))

(defn percents
  "Compute percentages from absolute numbers in histograms"
  [kw-prefix total histogram]
  (into (sorted-map)
        (map (fn [[k v]]
               (let [pk (keyword (str kw-prefix k))
                     pv (float (-> v (/ total) (* 100)))]
                 [pk pv]))
             (dissoc histogram 0))))

(defn stats
  "Compute bonboniere stats"
  ([hand-count]
    (stats hand-count false))
  ([hand-count verbose]
   (let [hands (gen/sample (s/gen ::hand) hand-count)
         same-kind-same-flavour-count (->> (map same-kind-same-flavour-bonbonieres hands)
                                           frequencies
                                           (percents "same-kind-same-flavour-" hand-count))
         same-kind-unique-flavours-count (->> (map same-kind-unique-flavours-bonbonieres hands)
                                              frequencies
                                              (percents "same-kind-unique-flavours-" hand-count))
         same-kind-arbitrary-flavour-count (->> (map same-kind-arbitrary-flavour-bonbonieres hands)
                                                 frequencies
                                                 (percents "same-kind-arbitrary-flavour-" hand-count))
         unique-kind-same-flavour-count (->> (map unique-kind-same-flavour-bonbonieres hands)
                                             frequencies
                                             (percents "unique-kind-same-flavour-" hand-count))
         unique-kind-unique-flavour-count (->> (map unique-kind-unique-flavour-bonbonieres hands)
                                               frequencies
                                               (percents "unique-kind-unique-flavour-" hand-count))
         unique-kind-arbitrary-flavour-count (->> (map unique-kind-arbitrary-flavour-bonbonieres hands)
                                                   frequencies
                                                   (percents "unique-kind-arbitrary-flavour-" hand-count))]
     (when verbose
       (doseq [hand hands]
         (println "========================================================")
         (clojure.pprint/print-table [::special ::praline ::flavour] (sort-by ::praline hand))))
     (merge
       same-kind-same-flavour-count
       same-kind-unique-flavours-count
       same-kind-arbitrary-flavour-count
       unique-kind-same-flavour-count
       unique-kind-unique-flavour-count
       unique-kind-arbitrary-flavour-count))))

(defn- parse-hand-count [args]
  (when-let [cnt (first args)]
    (try
      (Integer/parseInt cnt)
      (catch NumberFormatException _))))

(defn -main
  [& args]
  (let [default-hand-count (* 10 1000)
        hand-count (or (parse-hand-count args) default-hand-count)]
    (println "Computing stats for" hand-count "handouts")
    (pprint (stats hand-count))))
