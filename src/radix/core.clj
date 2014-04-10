(ns radix.core
  (:require
   [clojure.string :as str]
   [clojure.pprint :as pp]))

(defrecord Edge [edge target])
(defrecord Node [label leaf edges])

(defn- leaf [str]
  (->Node str true (sorted-map)))

(defn- edge-to-leaf [edge-str s]
  (->Edge edge-str (leaf s)))

;; radix tree (rock, rocket, rested, restaurant, trump, trust)
;; [root]
;;       r
;;           ock --> _
;;               --> et
;;           est --> aurant
;;               --> ed
;;               --> s
;;       t
;;           ru  --> mp
;;               --> st
;;
;; roc --> rock, rocket, rested, restaurant,
;; p   --> nil
;;

(defn- find-first [seq pred]
  (first (filter pred seq)))

(defn find-overlap [seq1 seq2]
  (->> (map vector seq1 seq2)
       (take-while (fn [[c1 c2]] (= c1 c2)))
       (map first)
       (apply str)
       not-empty))

(defn find-outgoing-edge [edges prefix]
  (->> edges
       (filter #(find-overlap (first %) prefix))
       first
       last))

(defn find-node [node prefix]
  (when-let [edge (find-outgoing-edge (:edges node) prefix)]
    (if (= (find-overlap (:edge edge) prefix) prefix) ;; TODO
      (:target edge)
      (find-node (:target edge) (subs prefix (count (:edge edge)))))))

(defn find-all-leafs [node]
  (if (:leaf node)
    (list (:label node))
    (let [nodes (vals (:edges node))]
      (apply concat (map #(find-all-leafs (:target %)) (vals (:edges node)))))))

(defn- remove-last [s n]
  (subs s 0 (- (count s) n)))

(defn- remove-first [s r] (str/replace-first s r ""))

(defn split-edge [edge prefix]
  (let [overlap (find-overlap prefix (:edge edge))
        edge-rem (remove-first (:edge edge) overlap)
        prefix-rem (remove-first prefix overlap)
        node-label (:label (:target edge))
        new-node-label (remove-last node-label (- (count (:edge edge)) (count overlap)))]
    (->Edge overlap
            (->Node new-node-label
                    false
                    {prefix-rem
                     (edge-to-leaf prefix-rem (str new-node-label prefix-rem))

                     edge-rem
                     (->Edge edge-rem (:target edge))}))))

(defn- insert-edge [new-edge edges]
  (assoc edges (:edge new-edge) new-edge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defn find-prefix
  "finds all the entries that start with the given string"
  [node query]
  (->> query
       (find-node node)
       find-all-leafs))

(defn insert [tree prefix]
  (let [edge (find-outgoing-edge (:edges tree) prefix)]
    (if edge
      (let [overlap (find-overlap (:edge edge) prefix)]
        (if (and (= (count overlap) (count ( :edge edge))) (not  (:leaf (:target edge))))
          (let [node (insert (:target edge) (subs prefix (count overlap)))]
            (->Node (:label tree)
                    false
                    (assoc (dissoc (:edges tree) (:edge edge)) (:edge edge) (->Edge overlap node))))
          (let [new-edge (split-edge edge prefix)]
            (->Node (:label tree)
                    false
                    (assoc (dissoc (:edges tree) (:edge edge)) (:edge new-edge) new-edge)))))
      (->Node (if (:label tree)
                (:label tree)
                "")
              false
              (insert-edge (edge-to-leaf prefix (str (:label tree) prefix)) (:edges tree))))))