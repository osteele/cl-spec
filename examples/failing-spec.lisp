;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.

(in-package #:cl-user)

(cl-spec:specify "a spec with some failing examples" ()
  ("should pass"
   (=> (+ 1 2) should = 3))
  ("should fail"
   (=> (+ 1 2) should = 4))
  ("should also fail"
   (=> (+ 1 2) should = 4)))

(cl-spec:specify "a spec with all passing examples" ()
  ("first example should pass"
   (=> (+ 1 2) should = 3))
  ("second example should also pass"
   (=> (+ 1 2) should = 3)))
