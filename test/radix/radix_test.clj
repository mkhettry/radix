(ns radix.core-test
  (:use clojure.test radix.core))


(deftest test-insert
  (is-= (list "test")
        (let [tree (insert nil "test")]
          (find-prefix tree "t")))
  (is-= (list "slow" "slower")
        (let [tree (insert (insert nil "slow") "slower")]
          (find-prefix tree "sl")))
  (is-= (list "slow" "slower")
        (let [tree (-> nil (insert "slower") (insert "slow"))]
          (find-prefix tree "sl")))
  (let [tree (-> nil (insert "test") (insert "team"))]
    (is-= (list "team") (find-prefix tree "tea"))
    (is-= (list "team" "test") (find-prefix tree "te")))

  (let [tree (-> nil (insert "test") (insert "team") (insert "tag"))]
    (is-= (list "tag") (find-prefix tree "ta")))

  (is-= (list "abc")
        (let [tree (insert (insert nil "abc") "def")]
          (find-prefix tree "ab"))))
