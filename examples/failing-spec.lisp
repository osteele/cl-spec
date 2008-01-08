;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.

(define-specification "a spec with some failing examples" ()
  ("should pass"
   (=> (+ 1 2) should = 3))
  ("should fail"
   (=> (+ 1 2) should = 4))
  ("should also fail"
   (=> (+ 1 2) should = 4)))

(define-specification "a spec with all passing example" ()
  ("first example should pass"
   (=> (+ 1 2) should = 3))
  ("second example should also pass"
   (=> (+ 1 2) should = 3)))
