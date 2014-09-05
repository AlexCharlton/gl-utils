(use test gl-utils-bytevector gl-utils-srfi-4)

(test #u8(1 2 3 4)
      (bytevector-append #u8(1 2) #u8(3 4)))

(test #u8(1 2 3 4)
      (bytevector-copy #u8(1 2 3 4)))

(test #u8(1 2)
      (bytevector-copy #u8(1 2 3 4) 0 2))

(test #u8(3 4)
      (bytevector-copy #u8(1 2 3 4) 2))

(test #u8(3 4)
      (bytevector-copy #u8(1 2 3 4 5) 2 4))

(let ((a #u8(1 2 3 4 5))
      (b #u8(10 20 30 40 50)))
  (bytevector-copy! b 1 a 0 2)
  (test #u8(10 1 2 40 50) b))

(let ((a #u8(1 2 3 4 5)))
  (bytevector-f32-set! a 1 32.8)
  (test 32.8 (bytevector-f32-ref a 1)))

(test-exit)
