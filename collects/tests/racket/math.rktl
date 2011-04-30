(load-relative "loadtest.rktl")
(Section 'math)
(require scheme/math)

(test 0 order-of-magnitude 1)
(test 0 order-of-magnitude 9)
(test 1 order-of-magnitude 10)
(test 1 order-of-magnitude 17)
(test 1 order-of-magnitude 99)
(test 2 order-of-magnitude 100)
(test 2 order-of-magnitude 200)
(test 2 order-of-magnitude 999)
(test 3 order-of-magnitude 1000)
(test 3 order-of-magnitude 5000)
(test 3 order-of-magnitude 9999)
(test 4 order-of-magnitude 10000)
(test -2 order-of-magnitude 1/100)
(test -3 order-of-magnitude 1/101)

(test 25 sqr 5)
(test 25 sqr -5)

(test #t <= (abs (sin pi)) 0.0001)

(test 1 sgn 1)
(test -1 sgn -1)
(test 0 sgn 0)
(test 1 sgn 999)
(test -1 sgn -999)
