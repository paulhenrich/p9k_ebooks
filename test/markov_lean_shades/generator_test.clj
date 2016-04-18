(ns markov-lean-shades.generator-test
  (:require [clojure.test :refer :all]
            [markov-lean-shades.generator :refer :all]))

;; most tests are copied directly or nearly so from the HowIStart article.

(deftest test-word-chain
  (testing "it produces a chain of the possible two step transitions"
    (let  [example '(("And" "the" "Golden")
                     ("the" "Golden" "Grouse")
                     ("And" "the" "Pobble")
                     ("the" "Pobble" "who"))]
      (is (= {["the" "Pobble"] #{"who"}
              ["the" "Golden"] #{"Grouse"}
              ["And" "the"] #{"Pobble" "Golden"}}
             (word-chain example))))))

(deftest test-text->word-chain
  (testing "transforms string with spaces and newlines"
    (let [example "And the Golden Grouse\nAnd the Pobble who"]
      (is (= {["who" nil] #{}
              ["Pobble" "who"] #{}
              ["the" "Pobble"] #{"who"}
              ["Grouse" "And"] #{"the"}
              ["Golden" "Grouse"] #{"And"}
              ["the" "Golden"] #{"Grouse"}
              ["And" "the"] #{"Pobble" "Golden"}}
             (text->word-chain example))))))

(deftest test-walk-chain
  (let [chain {["who" nil] #{},
               ["Pobble" "who"] #{},
               ["the" "Pobble"] #{"who"},
               ["Grouse" "And"] #{"the"},
               ["Golden" "Grouse"] #{"And"},
               ["the" "Golden"] #{"Grouse"},
               ["And" "the"] #{"Pobble" "Golden"}}]
    (testing "dead end"
      (let [prefix ["the" "Pobble"]]
        (is (= ["the" "Pobble" "who"]
               (walk-chain chain prefix)))))
    (testing "multiple options"
      (with-redefs [shuffle identity]
        (let [prefix ["And" "the"]]
          (is (= ["And" "the" "Pobble" "who"]
                 (walk-chain chain prefix))))))
    (testing "repeating chains"
      (with-redefs [shuffle reverse]
        (let [prefix ["And" "the"]]
          (is (> 140
                 (count (apply str (walk-chain chain prefix)))))
          (is (= ["And" "the" "Golden" "Grouse" "And" "the" "Golden" "Grouse"]
                 (take 8 (walk-chain chain prefix)))))))))

(deftest test-generate-text
  "it strings together and capitalizes the text"
  (with-redefs [shuffle (fn [c] c)]
    (let [chain {["who" nil] #{}
                 ["Pobble" "who"] #{}
                 ["the" "Pobble"] #{"who"}
                 ["Grouse" "And"] #{"the"}
                 ["Golden" "Grouse"] #{"And"}
                 ["the" "Golden"] #{"Grouse"}
                 ["And" "the"] #{"Pobble" "Golden"}}]
      (is (= "The Pobble who" (generate-text ["the" "Pobble"] chain)))
      (is (= "And the Pobble who" (generate-text ["And" "the"] chain))))))

(deftest test-finalize-phrase
  "Cuts off run-on phrases at the last punctuation, <- for instance right there"
  (is (= "We test. Iteratively." (finalize-phrase "We test. Iteratively. Then")))
  (is (= "We test, iteratively." (finalize-phrase "We test, iteratively, then"))))

(deftest test-score
  "scores the entire phrase for target words"
  (let [lean-words #{"lean" "startup" "disrupt"}]
    (is (= 0 (score "Massage my clavicle, he murmured" lean-words)))
    (is (= 1 (score "Then we disrupt everything!" lean-words)))
    (is (= 2 (score "He murmured, disrupt every startup." lean-words)))))
