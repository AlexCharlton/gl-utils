(module gl-utils-mesh (make-mesh
                       mesh-vertex-attributes
                       mesh-index-type
                       mesh-vertex-data
                       mesh-index-data
                       mesh-n-vertices
                       mesh-n-indices
                       mesh-vertex-buffer
                       mesh-index-buffer
                       mesh-stride
                       mesh-mode
                       mesh-vao
                       mesh-usage
                       mesh-attribute-locations-set!
                       mesh-make-vao
                       usage->gl
                       mode->gl)

(import chicken scheme)
(use (prefix gl-utils-core gl:) (prefix opengl-glew gl:) gl-utils-bytevector
     srfi-1 srfi-99 miscmacros matchable extras lolevel)

;;;; Mesh record
(define-record-type mesh
  (make-mesh%) #t
  (vertex-attributes)
  (index-type)
  (vertex-data)
  (index-data)
  (n-vertices)
  (n-indices)
  (vertex-buffer)
  (index-buffer)
  (vao)
  (stride)
  (mode)
  (usage)
  (dirty))

(define-record-type mesh-vertex-attribute
  #t #t
  name type number normalized (location) (offset))

(define-record-printer (mesh-vertex-attribute s out)
  (fprintf out "#(mesh-vertex-attribute name: ~S type: ~S number: ~S normalized: ~S location: ~S offset: ~S)"
    (mesh-vertex-attribute-name s) (mesh-vertex-attribute-type s)
    (mesh-vertex-attribute-number s) (mesh-vertex-attribute-normalized s)
    (mesh-vertex-attribute-location s) (mesh-vertex-attribute-offset s)))

(define (delete-mesh m)
  (if* (mesh-vertex-buffer m)
       (gl:delete-buffer it))
  (if* (mesh-index-buffer m)
       (gl:delete-buffer it))
  (if* (mesh-vao m)
       (gl:delete-vertex-array it)))


;;;; Mesh initialization
(define (make-mesh #!key vertices indices (mode triangles:))
  (unless vertices
    (error 'make-mesh "Must be called with vertices: keyword"))
  (let ((mesh (make-mesh%)))
    (make-mesh-vertices mesh vertices)
    (if indices
        (make-mesh-indices mesh indices)
        (begin
          (mesh-index-data-set! mesh #f)
          (mesh-index-type-set! mesh #f)
          (mesh-n-indices-set! mesh 0)))
    (mesh-vertex-buffer-set! mesh #f)
    (mesh-index-buffer-set! mesh #f)
    (mesh-mode-set! mesh mode)
    mesh))

;;; Vertex initialization
(define (get-vertex-attribute name attributes)
  (if* (find (lambda (attribute)
               (equal? name (mesh-vertex-attribute-name attribute)))
             attributes)
       it
       (error 'make-mesh "No attribute of this name in mesh's vertex-attributes" name attributes)))

(define (vertex-length attributes init)
  (let* ((lengths (map (lambda (i)
                         (if* (get-vertex-attribute (car i) attributes)
                              (quotient (length (cdr i))
                                        (mesh-vertex-attribute-number it))
                              (error 'make-mesh "No vertex attributes to match initial-element" (car i))))
                       init))
         (length (car lengths)))
    (for-each (lambda (l)
                (unless (= l length)
                  (error 'make-mesh "Vertex initial elements do not all have the same number of vertices")))
              (cdr lengths))
    length))

(define (get-attributes vertices)
  (map (match-lambda
         (((? symbol? name)
           (? (cut member <> valid-type) type)
           (? (lambda (x) (and (integer? x)
                        (< 0 x 5)))
              n)
           . keywords)
          (make-mesh-vertex-attribute
           name type n
           (get-keyword normalized: keywords)
           (get-keyword location: keywords (lambda () -1))
           0))
         (attr (error 'make-mesh "Expected attribute attributes in the form (NAME TYPE NUMBER [NORMALIZED] [LOCATION])" attr)))
       (get-keyword attributes: vertices
                    (lambda ()
                      (error 'make-mesh "vertices: keyword must contain a attributes: keyword" vertices)))))

(define (get-stride attributes)
  (let ((offset 0))
    (fold (lambda (a n)
            (let ((size (* (gl:type->bytes (mesh-vertex-attribute-type a))
                           (mesh-vertex-attribute-number a))))
              (mesh-vertex-attribute-offset-set! a offset)
              (inc! offset size)
              (+ size n)))
          0 attributes)))

(define (set-initial-vertices attributes stride inits vertex-vector)
  (let loop ((inits inits))
    (unless (null? inits)
      (let* ((init (car inits))
             (attribute (get-vertex-attribute (car init) attributes))
             (set (type->setter (mesh-vertex-attribute-type attribute)))
             (offset (mesh-vertex-attribute-offset attribute))
             (size (gl:type->bytes (mesh-vertex-attribute-type attribute)))
             (number (mesh-vertex-attribute-number attribute)))
        (do ((i 0 (add1 i))
             (init (cdr init) (cdr init)))
            ((null? init))
          (set vertex-vector (+ (* (quotient i number) stride)
                                (* (remainder i number) size)
                                offset)
               (car init))))
      (loop (cdr inits)))))

(define (make-mesh-vertices mesh vertices)
  (let* ((vertex-init (get-keyword initial-elements: vertices))
         (attributes (get-attributes vertices))
         (stride (get-stride attributes)))
    (mesh-vertex-attributes-set! mesh attributes)
    (mesh-stride-set! mesh stride)
    (if (bytevector? vertex-init)
        (begin (mesh-vertex-data-set! mesh vertex-init)
               (mesh-n-vertices-set! mesh (quotient (bytevector-length
                                                     (mesh-vertex-data mesh))
                                                    stride)))
        (begin (mesh-n-vertices-set!
                mesh (if vertex-init
                         (vertex-length attributes vertex-init)
                         (get-keyword n-vertices: vertices
                                      (lambda () (error 'make-mesh "n-vertices: keyword required when no intial-elements: is provided" vertices)))))
               (mesh-vertex-data-set!
                mesh (make-bytevector (* (mesh-n-vertices mesh) stride)))
               (when vertex-init
                 (set-initial-vertices attributes stride vertex-init
                                       (mesh-vertex-data mesh)))))))

;;; Index initialization
(define (set-initial-indices index-type stride init index-vector)
  (let ((set (type->setter index-type)))
    (do ((i 0 (add1 i))
         (init init (cdr init)))
        ((null? init))
      (set index-vector (* i stride) (car init)))))

(define (make-mesh-indices mesh indices)
  (let* ((index-init (get-keyword initial-elements: indices))
         (index-type (get-keyword type: indices
                                  (lambda ()
                                    (error 'make-mesh "indices: keyword must contain a type: keyword"
                                           indices))))
         (stride (gl:type->bytes index-type)))
    (unless (member index-type valid-type)
      (error 'make-mesh "Mesh index-type must be a valid type)" index-type))
    (mesh-index-type-set! mesh index-type)
    (if (bytevector? index-init)
        (begin (mesh-index-data-set! mesh index-init)
               (mesh-n-indices-set! mesh (quotient (bytevector-length
                                                    (mesh-index-data mesh))
                                                   stride)))
        (begin (mesh-n-indices-set!
                mesh (if index-init
                         (length index-init)
                         (get-keyword n-indices: indices
                                      (lambda () (error 'make-mesh "n-indices: keyword required when no intial-elements: is provided" indices)))))
               (mesh-index-data-set!
                mesh (make-bytevector (* (mesh-n-indices mesh) stride)))
               (when index-init
                 (set-initial-indices index-type stride index-init
                                      (mesh-index-data mesh)))))))


;;;; Mesh accessors

(define (mesh-attribute-locations-set! mesh locations)
  (let ((attributes (mesh-vertex-attributes mesh)))
    (let loop ((locations locations))
      (unless (null? locations)
        (let ((attribute (get-vertex-attribute (caar locations) attributes)))
          (mesh-vertex-attribute-location-set! attribute (cdar locations)))
        (loop (cdr locations))))))

#;
(define (with-mesh mesh thunk)
  (gl:bind-buffer gl:+array-buffer+ (mesh-vertex-buffer mesh))
  (thunk)
  (when (mesh-dirty? mesh)
    (case (mesh-usage mesh)
      ((dynamic:) (gl:buffer-sub-data ; TODO
                   ))
      ((stream:) (gl:buffer-data   ; TODO
                   ))))
  (gl:bind-buffer gl:+array-buffer+ 0))

#;
(define (mesh-vertex-set! mesh vertex attribute attr-index value)  ;; make value a vector and remove attr-index?
  ;; Set mesh-dirty min max index
  (mesh-dirty-set! mesh #t))

#;
(define (mesh-vertex-ref mesh vertex attribute attr-index)
  )

;;;; Mesh operations
(define (mesh-make-vao mesh #!optional (usage #:static))
  (let* ((vao (gl:gen-vertex-array))
         (stride (mesh-stride mesh))
         (vertex-buffer (gl:gen-buffer))
         (index-data (mesh-index-data mesh))
         (index-buffer (if index-data
                           (gl:gen-buffer)))
         (gl-usage (usage->gl usage)))
    (gl:bind-buffer gl:+array-buffer+ vertex-buffer)
    (gl:buffer-data gl:+array-buffer+ (* stride
                                         (mesh-n-vertices mesh))
                    (bytevector->pointer (mesh-vertex-data mesh))
                    gl-usage)
    (when index-data
      (gl:bind-buffer gl:+element-array-buffer+ index-buffer)
      (gl:buffer-data gl:+element-array-buffer+
                      (* (gl:type->bytes (mesh-index-type mesh))
                         (mesh-n-indices mesh))
                      (bytevector->pointer index-data)
                      gl-usage))
    ;; start vertex-array
    (gl:bind-vertex-array vao)
    (for-each (lambda (attribute)
                (gl:vertex-attrib-pointer
                 (mesh-vertex-attribute-location attribute)
                 (mesh-vertex-attribute-number attribute)
                 (gl:type->gl (mesh-vertex-attribute-type attribute))
                 (mesh-vertex-attribute-normalized attribute)
                 stride
                 (address->pointer (mesh-vertex-attribute-offset attribute)))
                (gl:enable-vertex-attrib-array
                 (mesh-vertex-attribute-location attribute)))
              (mesh-vertex-attributes mesh))
    (when index-data
      (gl:bind-buffer gl:+element-array-buffer+ index-buffer))
    (gl:bind-vertex-array 0)
    ;; end vertex-array
    (mesh-usage-set! mesh usage)
    (mesh-vertex-buffer-set! mesh vertex-buffer)
    (when index-data
      (mesh-index-buffer-set! mesh index-buffer))
    (mesh-vao-set! mesh vao)
    (set-finalizer! mesh delete-mesh)
    (when (member usage '(static: static-read: static-copy:))
      (mesh-index-buffer-set! mesh #f)
      (mesh-vertex-buffer-set! mesh #f)
      (mesh-index-data-set! mesh #f)
      (mesh-vertex-data-set! mesh #f)
      (gl:delete-buffer vertex-buffer)
      (gl:delete-buffer index-buffer))))

#;
(define (copy-mesh! to at from))

  #;
(define (copy-mesh mesh))

#;
(define (mesh-append mesh . meshes))

#;
(define (mesh-transform-append position-name pair . pairs))

#;
(define (mesh-apply-transform! position-name mesh transform))


;;;; Type keywords
(define valid-type '(char: int8: byte: uchar: uint8: unsigned-byte:
                     short: int16: ushort: uint16: unsigned-short:
                     int: int32: uint: uint32: unsigned-int: unsigned-int32:
                     integer: integer32:  unsigned-integer: unsigned-integer32:
                     float: float32: double: float64:))

(define (type->setter type)
  (ecase type
    ((char: int8: byte:) bytevector-s8-set!)
    ((uchar: uint8: unsigned-byte:) bytevector-u8-set!)
    ((short: int16:) bytevector-s16-set!)
    ((ushort: uint16: unsigned-short:) bytevector-u16-set!)
    ((int: int32: integer: integer32:) bytevector-s32-set!)
    ((uint: uint32: unsigned-int: unsigned-int32:
	    unsigned-integer: unsigned-integer32:)
     bytevector-u32-set!)
    ((float: float32:) bytevector-f32-set!)
    ((double: float64:) bytevector-f64-set!)))

(define (usage->gl usage)
  (ecase usage
    ((dynamic:) gl:+dynamic-draw+)
    ((stream:) gl:+stream-draw+)
    ((static:) gl:+static-draw+)
    ((dynamic-read:) gl:+dynamic-read+)
    ((stream-read:) gl:+stream-read+)
    ((static-read:) gl:+static-read+)
    ((dynamic-copy:) gl:+dynamic-copy+)
    ((stream-copy:) gl:+stream-copy+)
    ((static-copy:) gl:+static-copy+)))

(define (mode->gl mode)
  (ecase mode
    ((points:) gl:+points+)
    ((line-strip:) gl:+line-strip+)
    ((line-loop:) gl:+line-loop+)
    ((line-strip-adjacency:) gl:+line-strip-adjacency+)
    ((lines-adjacency:) gl:+lines-adjacency+)
    ((triangle-strip:) gl:+triangle-strip+)
    ((triangle-fan:) gl:+triangle-fan+)
    ((triangles:) gl:+triangles+)
    ((triangle-strip-adjacency:) gl:+triangle-strip-adjacency+)
    ((triangles-adjacency:) gl:+triangles-adjacency+)
    ((patches:) gl:+patches+)))

) ; end gl-utils-mesh
