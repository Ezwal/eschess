(defpackage eschess-project/tests/main
  (:use :cl
        :eschess-project
        :rove))
(in-package :eschess-project/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :eschess-project)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
