(define-specification "failure" ()
  ("should pass"
   (=> (+ 1 2) should = 3))
  ("should fail"
   (=> (+ 1 2) should = 4))
  ("should also fail"
   (=> (+ 1 2) should = 4)))

(define-specification "success" ()
  ("first example should pass"
   (=> (+ 1 2) should = 3))
  ("second example should also pass"
   (=> (+ 1 2) should = 3)))
