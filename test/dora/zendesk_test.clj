(ns dora.zendesk-test
  (:require  [clojure.test :refer :all]
             [dora.p.zendesk :refer :all]))

(deftest zendesk-connect
  (testing "can connect to zendesk"
    (is (map? @(zendesk-request :tickets)))))

(deftest zendesk-get-tickets
  (testing "can get more than 200 tickets"
    (is (> (count (all-tickets)) 200))))
