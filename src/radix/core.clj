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
(defn- starts-with [s subs]
  (= 0 (.indexOf s subs)))

(defn find-overlap [seq1 seq2]
  (->> (map vector seq1 seq2)
       (take-while (fn [[c1 c2]] (= c1 c2)))
       (map first)
       (apply str)))

(defn find-outgoing-edge [edges prefix]
  (last (first (filter #(not (empty? (find-overlap (first %) prefix))) edges))))

(defn find-node [node prefix]
  (if (empty? prefix)
    node
    (if-let [edge (find-outgoing-edge (:edges node) prefix)]
      (if (= (count (find-overlap (:edge edge) prefix)) (count prefix))
        (:target edge)
        (find-node (:target edge) (subs prefix (count (:edge edge)))))
      node)))

(defn find-all-leafs [node]
  (if (:leaf node)
    (list (:label node))
    (let [nodes (vals (:edges node))]
      (apply concat (map #(find-all-leafs (:target %))  (vals (:edges node)))))))

(defn find-prefix [node str]
  (let [node (find-node node str)]
    (if (starts-with (:label node) str)
      (find-all-leafs node)
      '())))

(defn replace-last [s n]
  (subs s 0 (- (count s) n)))

(defn merge-edge [edge prefix]
  (if edge
    (let [overlap (find-overlap prefix (:edge edge))
          edge-rem (str/replace-first (:edge edge) overlap "")
          prefix-rem (str/replace-first prefix overlap "")
          node-label (:label (:target edge))
          truncate-by (max 0  (- (count (:edge edge)) (count overlap)))
          new-node-label (replace-last node-label truncate-by)]
      (do
        (->Edge overlap
                (->Node new-node-label
                        false
                        (sorted-map prefix-rem
                                    (edge-to-leaf prefix-rem (str new-node-label prefix-rem))
                                    edge-rem
                                    (->Edge edge-rem (:target edge))
                                    )))))
    (->Edge prefix (->Node prefix true (sorted-map)))))

(defn- insert-edge [new-edge edges]
  (assoc edges (:edge new-edge) new-edge))

(defn insert [tree prefix]
  (let [edge (find-outgoing-edge (:edges tree) prefix)]
    (if edge
      (let [overlap (find-overlap (:edge edge) prefix)]
        (if (and (= (count overlap) (count ( :edge edge))) (not  (:leaf (:target edge))))
          (let [node (insert (:target edge) (subs prefix (count overlap)))]
            (->Node (:label tree)
                    false
                    (assoc (dissoc (:edges tree) (:edge edge)) (:edge edge) (->Edge overlap node)))
            )
          (let [new-edge (merge-edge edge prefix)]
            (->Node (:label tree)
                    false
                    (assoc (dissoc (:edges tree) (:edge edge)) (:edge new-edge) new-edge)))))
      (->Node (if (:label tree)
                (:label tree)
                "")
              false
              (insert-edge (edge-to-leaf prefix (str (:label tree) prefix)) (:edges tree))))))