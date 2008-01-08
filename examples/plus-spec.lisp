;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.

(define-specification "the plus operator" ()
  ("should add two numbers"
   (=> (+ 1 2) should = 3))
  ("should add three numbers"
   (=> (+ 1 2 3) should = 6))
  ("should be commutative"
   (=> (+ 2 3) should = (+ 3 2)))
  ("should be associative"
   (=> (+ (+ 1 2) 3) should = (+ 1 (+ 2 3))))
  ("should have a left identity"
   (=> (+ 0 1) should = 1)
   (=> (+ 0 10) should = 10))
  ("should have a right identity"
   (=> (+ 1 0) should = 1)
   (=> (+ 10 0) should = 10))
  ("should have inverses"
   (=> (+ 1 -1) should be zero)
   (=> (+ 10 -10) should be zero)))
