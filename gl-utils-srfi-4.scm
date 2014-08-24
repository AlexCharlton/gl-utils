(module gl-utils-srfi-4
  (u8vector
   s8vector
   u16vector
   s16vector
   u32vector
   s32vector
   f32vector
   f64vector
   list->u8vector
   list->s8vector
   list->u16vector
   list->s16vector
   list->u32vector
   list->s32vector
   list->f32vector
   list->f64vector
   make-u8vector
   make-s8vector
   make-u16vector
   make-s16vector
   make-u32vector
   make-s32vector
   make-f32vector
   make-f64vector)

(import chicken scheme)
(use (prefix srfi-4 v:))

(reexport (except srfi-4
                  u8vector
                  s8vector
                  u16vector
                  s16vector
                  u32vector
                  s32vector
                  f32vector
                  f64vector
                  list->u8vector
                  list->s8vector
                  list->u16vector
                  list->s16vector
                  list->u32vector
                  list->s32vector
                  list->f32vector
                  list->f64vector
                  make-u8vector
                  make-s8vector
                  make-u16vector
                  make-s16vector
                  make-u32vector
                  make-s32vector
                  make-f32vector
                  make-f64vector))

;;; Vector initialization
;; Stolen from srfi-4.scm
(define-syntax list->NNNvector 
  (ir-macro-transformer 
   (lambda (x i c)
     (let* ((tag (strip-syntax (cadr x)))
	    (tagstr (symbol->string tag))
	    (name (string->symbol (string-append "list->" tagstr)))
	    (make (string->symbol (string-append "v:make-" tagstr)))
	    (set (string->symbol (string-append "v:" tagstr "-set!"))))
       `(define ,name
          (lambda (lst)
            (##sys#check-list lst ',tag)
            (let* ((n (##core#inline "C_i_length" lst))
                   (v (,make n 0 #t)) )
              (do ((p lst (##core#inline "C_slot" p 1))
                   (i 0 (##core#inline "C_fixnum_plus" i 1)))
                  ((##core#inline "C_eqp" p '()) v)
                (if (and (##core#inline "C_blockp" p) (##core#inline "C_pairp" p))
                    (,set v i (##core#inline "C_slot" p 0))
                    (##sys#error-not-a-proper-list lst))))))))))

(list->NNNvector u8vector)
(list->NNNvector s8vector)
(list->NNNvector u16vector)
(list->NNNvector s16vector)
(list->NNNvector u32vector)
(list->NNNvector s32vector)
(list->NNNvector f32vector)
(list->NNNvector f64vector)

(define u8vector
  (lambda xs (list->u8vector xs)))

(define s8vector
  (lambda xs (list->s8vector xs)))

(define u16vector
  (lambda xs (list->u16vector xs)))

(define s16vector
  (lambda xs (list->s16vector xs)))

(define u32vector
  (lambda xs (list->u32vector xs)))

(define s32vector
  (lambda xs (list->s32vector xs)))

(define f32vector
  (lambda xs (list->f32vector xs)))

(define f64vector
  (lambda xs (list->f64vector xs)))

(define-syntax make-NNNvector 
  (ir-macro-transformer 
   (lambda (x i c)
     (let* ((tag (strip-syntax (cadr x)))
	    (tagstr (symbol->string tag))
	    (name (string->symbol (string-append "make-" tagstr)))
	    (make (string->symbol (string-append "v:make-" tagstr))))
       `(define (,name n #!optional (value 0) (gc #t) (finalize #t))
          (,make n value gc finalize))))))

(make-NNNvector u8vector)
(make-NNNvector s8vector)
(make-NNNvector u16vector)
(make-NNNvector s16vector)
(make-NNNvector u32vector)
(make-NNNvector s32vector)
(make-NNNvector f32vector)
(make-NNNvector f64vector)

)
