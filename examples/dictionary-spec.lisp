;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.

(define-specification "an empty dictionary" ((dict {}))
  ("should have no keys"
   (=> (length (keys dict)) should be zero))
  ("should not have an entry for :key"
   (=> (ref1 dict :key) should eq nil)))

(define-specification "a dictionary with one entry" ((dict {a-key 'a-value}))
  ("should have one key"
   (=> (length (keys dict)) should eq 1))
  ("should have a set of keys that contains its key"
   (format t "keys = ~S" (keys dict))
   (=> (keys dict) should equalp '(a-key)))
  ("should have an entry for its key"
   (=> (ref1 dict 'a-key) should eq 'a-value))
  ("should not have an entry for a different key"
   (=> (ref1 dict :key) should eq nil)))
