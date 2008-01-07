(define-specification "failure" ()
  ("should pass"
   (=> (+ 1 2) should = 3))
  ("should fail"
   (=> (+ 1 2) should = 4)))
