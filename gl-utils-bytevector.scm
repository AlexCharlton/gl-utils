(module gl-utils-bytevector *

(import chicken scheme foreign)
(use gl-utils-srfi-4 srfi-1)

;; Setting
(define bytevector-u8-set!
  (foreign-lambda* void
      ((u8vector bv) (size_t off) (unsigned-byte x))
    "((uint8_t *)bv)[off] = x;"))

(define bytevector-s8-set!
  (foreign-lambda* void
      ((u8vector bv) (size_t off) (byte x))
    "((int8_t *)bv)[off] = x;"))

(define bytevector-u16-set!
  (foreign-lambda* void
      ((u8vector bv) (size_t off) (unsigned-short x))
    "*(uint16_t *)(&((char *)bv)[off]) = x;"))

(define bytevector-s16-set!
  (foreign-lambda* void
      ((u8vector bv) (size_t off) (short x))
    "*(int16_t *)(&((char *)bv)[off]) = x;"))

(define bytevector-u32-set!
  (foreign-lambda* void
      ((u8vector bv) (size_t off) (unsigned-int32 x))
    "*(uint32_t *)(&((char *)bv)[off]) = x;"))

(define bytevector-s32-set!
  (foreign-lambda* void
      ((u8vector bv) (size_t off) (int32 x))
    "*(int32_t *)(&((char *)bv)[off]) = x;"))

(define bytevector-u64-set!
  (foreign-lambda* void
      ((u8vector bv) (size_t off) (unsigned-integer64 x))
    "*(uint64_t *)(&((char *)bv)[off]) = x;"))

(define bytevector-s64-set!
  (foreign-lambda* void
      ((u8vector bv) (size_t off) (integer64 x))
    "*(int64_t *)(&((char *)bv)[off]) = x;"))

(define bytevector-f32-set!
  (foreign-lambda* void
      ((u8vector bv) (size_t off) (float x) )
    "*(float *)(&((char *)bv)[off]) = x;"))

(define bytevector-f64-set!
  (foreign-lambda* void
      ((u8vector bv) (size_t off) (double x) )
    "*(double *)(&((char *)bv)[off]) = x;"))


;; Referencing
(define bytevector-u8-ref
  (foreign-lambda* unsigned-byte
      ((u8vector bv) (size_t off))
    "C_return(((uint8_t *)bv)[off]);"))

(define bytevector-s8-ref
  (foreign-lambda* byte
      ((u8vector bv) (size_t off))
    "C_return(((int8_t *)bv)[off]);"))

(define bytevector-u16-ref
  (foreign-lambda* unsigned-short
      ((u8vector bv) (size_t off))
    "C_return(*(uint16_t *)(&(((char *)bv)[off])));"))

(define bytevector-s16-ref
  (foreign-lambda* short
      ((u8vector bv) (size_t off))
    "C_return(*(int16_t *)(&(((char *)bv)[off])));"))

(define bytevector-u32-ref
  (foreign-lambda* unsigned-int32
      ((u8vector bv) (size_t off))
    "C_return(*(uint32_t *)(&(((char *)bv)[off])));"))

(define bytevector-s32-ref
  (foreign-lambda* int32
      ((u8vector bv) (size_t off))
    "C_return(*(int32_t *)(&(((char *)bv)[off])));"))

(define bytevector-u64-ref
  (foreign-lambda* unsigned-integer64
      ((u8vector bv) (size_t off))
    "C_return(*(uint64_t *)(&(((char *)bv)[off])));"))

(define bytevector-s64-ref
  (foreign-lambda* integer64
      ((u8vector bv) (size_t off))
    "C_return(*(int64_t *)(&(((char *)bv)[off])));"))

(define bytevector-f32-ref
  (foreign-lambda* float
      ((u8vector bv) (size_t off))
    "C_return(*(float *)(&(((char *)bv)[off])));"))

(define bytevector-f64-ref
  (foreign-lambda* double
      ((u8vector bv) (size_t off))
    "C_return(*(double *)(&(((char *)bv)[off])));"))


;; Constructors
(define bytevector u8vector)
(define make-bytevector make-u8vector)

;; Length 
(define bytevector-length u8vector-length)

;; Predicates
(define bytevector? u8vector?)

;; Manipulation
(define (bytevector-append bv . bvs)
  (when (and (null? bvs) (not (list? bv)))
    (error 'bytevector-append "When applied to a single element, a list is expected"
           bv))
  (let* ((bvs (if (null? bvs)
                  bv
                  (cons bv bvs)))
         (length (fold (lambda (bv n)
                         (+ (bytevector-length bv) n))
                       0 bvs))
         (result (make-bytevector length)))
    (let loop ((vectors bvs) (i 0))
      (if (null? vectors)
          result
          (begin
            (let* ((v (car vectors))
                   (len (bytevector-length v)))
              ((foreign-lambda* void ((u8vector to) (u8vector from) (size_t len)
                                      (size_t off))
                 "memcpy(&to[off], from, len);")
               result v len i)
              (loop (cdr vectors) (+ i len))))))))

(define (bytevector-copy bv #!optional (start 0 ) (end (bytevector-length bv)))
  (when (or (< start 0) (>= start end) (> end (bytevector-length bv)))
    (error 'bytevector-copy "Bad range" (list start end)))
  (let* ((len (- end start))
         (result (make-bytevector len)))
    ((foreign-lambda* void ((u8vector to) (u8vector from) (size_t len) (size_t off))
       "memcpy(to, &from[off], len);")
     result bv len start)
    result))

(define (bytevector-copy! to at from
                          #!optional (start 0 ) (end (bytevector-length from)))
  (when (or (< start 0) (>= start end) (> end (bytevector-length from)))
    (error 'bytevector-copy "Bad from range" (list start end)))
  (when (or (< at 0) (> at (bytevector-length to)))
    (error 'bytevector-copy "Bad at" at))
  (when (< (- (bytevector-length to) at)
           (- end start))
    (error 'bytevector-copy
           "Bytevector being copied into is not long enough to hold range" to))
  ((foreign-lambda* void ((u8vector to) (u8vector from) (size_t len)
                          (size_t to_off) (size_t from_off))
     "memcpy(&to[to_off], &from[from_off], len);")
   to from (- end start) at start))

(define bytevector->pointer
  (foreign-lambda* c-pointer ((u8vector v))
    "C_return(v);"))

) ; end gl-utils-bytevector
