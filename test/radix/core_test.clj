(ns radix.core-test
  (:use clojure.test radix.core))

(defn build-tree [& strings]
  (reduce insert nil strings))

(defmacro is-=-by
  [f & seqs]
  `(is (apply = (map ~f [~@seqs]))))

(deftest find-overlap-test
  (is (= (find-overlap "cat" "cats") "cat"))
  (is (nil? (find-overlap "cat" "dog"))))

(deftest find-outgoing-edge-test
  (let [tree (build-tree "slow" "slower")]
    (is (= "slow" (:edge (find-outgoing-edge (:edges tree) "sl"))))
    (is (= "slow" (:edge (find-outgoing-edge (:edges tree) "slow"))))
    (is (= "slow" (:edge (find-outgoing-edge (:edges tree) "sloppy"))))
    (is (= "slow" (:edge (find-outgoing-edge (:edges tree) "slower"))))
    (is (nil? (:edge (find-outgoing-edge (:edges tree) ""))))
    (is (nil? (:edge (find-outgoing-edge (:edges tree) "abc"))))
    ))

(deftest find-node-test
  (let [tree (build-tree "slow" "slower" "slowly" "slowers")]
    (doseq [[label query] [["slow" "s"]
                           ["slow" "slow"]
                           ["slower" "slower"]
                           [nil "abc"]]]
      (is (= label (:label (find-node tree query)))))))

(deftest test-insert
  (let [tree (build-tree "slow" "slower")]
    (is-=-by set ["slow" "slower"]
             (find-prefix tree "sl")
             (find-prefix tree "slow"))
    (is-=-by set ["slower"]
             (find-prefix tree "slower")))

  (is-=-by set ["slow" "slower"] (find-prefix (build-tree "slower" "slow") "sl"))

  (let [tree (build-tree "test" "team")]
    (is-=-by set ["team"] (find-prefix tree "tea"))
    (is-=-by set ["team" "test"] (find-prefix tree "te")))

  (let [tree (build-tree "test" "team" "tag")]
    (is-=-by set ["tag"] (find-prefix tree "ta")))

  (let [tree (build-tree "test" "team" "ten")]
    (is-=-by set [ "team" "ten" "test"] (find-prefix tree "te")))

  (let [tree (build-tree "test" "team" "ten" "tux" "tuxedo")]
    (is (= (nil? (find-prefix tree "teamster"))))
    (is-=-by set ["tux" "tuxedo"] (find-prefix tree "tu"))))
