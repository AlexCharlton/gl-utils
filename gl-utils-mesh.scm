(module gl-utils-mesh (make-mesh
                       mesh?
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
                       mesh-vertex-ref
                       mesh-vertex-set!
                       vertex-attribute-name
                       vertex-attribute-type
                       vertex-attribute-number
                       vertex-attribute-normalized
                       vertex-attribute-offset
                       mesh-make-vao!
                       mesh-update!
                       with-mesh
                       mesh-copy!
                       mesh-copy
                       mesh-append
                       mesh-transform!
                       mesh-transform-append
                       usage->gl
                       mode->gl)

(import chicken scheme foreign)
(use (prefix gl-utils-core gl:) (prefix opengl-glew gl:) gl-utils-bytevector
     srfi-1 srfi-4 srfi-99 miscmacros matchable extras lolevel gl-math
     data-structures)

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

(define-record-type vertex-attribute
  #t #t
  name type number normalized (offset))

(define-record-printer (vertex-attribute s out)
  (fprintf out "#(vertex-attribute name: ~S type: ~S number: ~S normalized: ~S offset: ~S)"
    (vertex-attribute-name s) (vertex-attribute-type s)
    (vertex-attribute-number s) (vertex-attribute-normalized s)
    (vertex-attribute-offset s)))

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
          (mesh-n-indices-set! mesh (mesh-n-vertices mesh))))
    (mesh-vertex-buffer-set! mesh #f)
    (mesh-index-buffer-set! mesh #f)
    (mesh-vao-set! mesh #f)
    (mesh-dirty-set! mesh #f)
    (mesh-usage-set! mesh #f)
    (mesh-mode-set! mesh mode)
    mesh))

;;; Vertex initialization
(define (get-vertex-attribute name attributes)
  (if* (find (lambda (attribute)
               (equal? name (vertex-attribute-name attribute)))
             attributes)
       it
       (error 'make-mesh "No attribute of this name in mesh's vertex-attributes" name attributes)))

(define (vertex-length attributes init)
  (let* ((lengths (map (lambda (i)
                         (quotient (length (cdr i))
                                   (vertex-attribute-number
                                    (get-vertex-attribute (car i) attributes))))
                       init))
         (length (car lengths)))
    (for-each (lambda (l)
                (unless (= l length)
                  (error 'make-mesh "Vertex elements do not all have the same number of vertices")))
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
          (make-vertex-attribute
           name type n
           (get-keyword normalized: keywords)
           0))
         (attr (error 'make-mesh "Expected attribute attributes in the form (NAME TYPE NUMBER [NORMALIZED])" attr)))
       (get-keyword attributes: vertices
                    (lambda ()
                      (error 'make-mesh "vertices: keyword must contain a attributes: keyword" vertices)))))

(define (get-stride attributes)
  (let ((offset 0))
    (fold (lambda (a n)
            (let ((size (* (gl:type->bytes (vertex-attribute-type a))
                           (vertex-attribute-number a))))
              (cond-expand
                (gles (set! size (align-to-word size)))
                (else))
              (vertex-attribute-offset-set! a offset)
              (inc! offset size)
              (+ size n)))
          0 attributes)))

(define (unsigned? type)
  (member type '(uchar: uint8: unsigned-byte:
                 ushort: uint16: unsigned-short:
                 uint: uint32: unsigned-int: unsigned-int32:
                 unsigned-integer: unsigned-integer32:)))

(define (set-vertices mesh vertices)
  (let ((attributes (mesh-vertex-attributes mesh))
        (stride (mesh-stride mesh))
        (vertex-vector (mesh-vertex-data mesh)))
    (let loop ((inits vertices))
      (unless (null? inits)
        (let* ((init (car inits))
               (attribute (get-vertex-attribute (car init) attributes))
               (set (type->setter (vertex-attribute-type attribute)))
               (offset (vertex-attribute-offset attribute))
               (size (gl:type->bytes (vertex-attribute-type attribute)))
               (number (vertex-attribute-number attribute))
               (type (vertex-attribute-type attribute))
               (de-normalize
                (if (and (vertex-attribute-normalized attribute)
                         (not (member type
                                      '(float: float32: double: float64:))))
                    (if (unsigned? type)
                        (lambda (x)
                          (inexact->exact
                           (fpfloor (* (fpmax -1.0 (min 1.0 (exact->inexact x)))
                                       (sub1 (expt 2 (* size 8)))))))
                        (lambda (x)
                          (fx- (inexact->exact
                                (fpfloor (* (fp+ (fp* (fpmax -1.0 (min 1.0 (exact->inexact x)))
                                                      0.5)
                                                 0.5)
                                            (sub1 (expt 2 (* size 8))))))
                               (fxshl 2 (fx- (fx* size 8)
                                             1)))))
                    (lambda (x) x))))
          (do ((i 0 (add1 i))
               (init (cdr init) (cdr init)))
              ((null? init))
            (set vertex-vector (+ (* (quotient i number) stride)
                                  (* (remainder i number) size)
                                  offset)
                 (de-normalize (car init)))))
        (loop (cdr inits))))))

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
                 (set-vertices mesh vertex-init))))))

;;; Index initialization
(define (set-indices mesh indices)
  (let* ((index-type (mesh-index-type mesh))
         (stride (gl:type->bytes index-type))
         (index-vector (mesh-index-data mesh))
         (set (type->setter index-type)))
    (do ((i 0 (add1 i))
         (init indices (cdr init)))
        ((null? init))
      (set index-vector (* i stride) (car init)))))

(define (make-mesh-indices mesh indices)
  (let* ((index-init (get-keyword initial-elements: indices))
         (index-type (get-keyword type: indices
                                  (lambda ()
                                    (error 'make-mesh "indices: keyword must contain a type: keyword"
                                           indices))))
         (stride (gl:type->bytes index-type)))
    (unless (unsigned? index-type)
      (error 'make-mesh "Mesh index-type must be a valid, unsigned type)" index-type))
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
                 (set-indices mesh index-init))))))


;;;; Mesh accessors
(define (with-mesh mesh thunk)
  (gl:bind-buffer gl:+array-buffer+ (mesh-vertex-buffer mesh))
  (thunk)
  (if* (mesh-dirty mesh)
       (let ((usage (mesh-usage mesh)))
         (ecase usage
           ((dynamic: dynamic-draw: dynamic-read: dynamic-copy:)
            (let ((lower (car it))
                  (upper (cdr it)))
              (gl:buffer-sub-data gl:+array-buffer+
                                  (car it)
                                  (- upper lower)
                                  (pointer+ (bytevector->pointer
                                             (mesh-vertex-data mesh))
                                            lower))))
           ((stream: stream-draw: stream-copy: stream-read:)
            (gl:buffer-data gl:+array-buffer+
                            (* (mesh-stride mesh)
                               (mesh-n-vertices mesh))
                            (bytevector->pointer (mesh-vertex-data mesh))
                            (usage->gl usage))))
         (mesh-dirty-set! mesh #f)))
  (gl:bind-buffer gl:+array-buffer+ 0))

;; Note: value is unsafe: entering a vector that is too short will have unspecified, bad consequences.
(define (mesh-vertex-set! mesh attribute vertex value)
  (when (or (negative? vertex) (>= vertex (mesh-n-vertices mesh)))
    (error 'mesh-vertex-ref "Vertex not in range" vertex))
  (let* ((attribute (get-vertex-attribute attribute (mesh-vertex-attributes mesh)))
         (type (vertex-attribute-type attribute))
         (number (vertex-attribute-number attribute))
         (length (* (gl:type->bytes type) number))
         (offset (vertex-attribute-offset attribute))
         (stride (mesh-stride mesh))
         (position (+ offset (* stride vertex))))
    ((foreign-lambda* void ((u8vector to) (c-pointer from) (size_t start)
                            (size_t length))
       "memcpy((&((char *)to)[start]), from, length);")
     (mesh-vertex-data mesh) (gl:->pointer value) position length)
    (when (mesh-vertex-buffer mesh)
      (if (member (mesh-usage mesh) '(stream: stream-read: stream-copy:))
          (mesh-dirty-set! mesh #t)
          (let* ((dirty (mesh-dirty mesh))
                 (lower (if dirty
                            (min (car dirty) position)
                            position))
                 (upper (if dirty
                            (max (cdr dirty) (+ position length))
                            (+ position length))))
            (mesh-dirty-set! mesh (cons lower upper)))))))

(define (mesh-vertex-ref mesh attribute vertex)
  (when (or (negative? vertex) (>= vertex (mesh-n-vertices mesh)))
    (error 'mesh-vertex-ref "Vertex not in range" vertex))
  (let* ((attribute (get-vertex-attribute attribute (mesh-vertex-attributes mesh)))
         (type (vertex-attribute-type attribute))
         (number (vertex-attribute-number attribute))
         (length (* (gl:type->bytes type) number))
         (offset (vertex-attribute-offset attribute))
         (stride (mesh-stride mesh))
         (vec ((type->make-vector type) number)))
    ((foreign-lambda* void ((c-pointer to) (u8vector from) (size_t start)
                            (size_t length))
       "memcpy(to, (&((char *)from)[start]), length);")
     (gl:->pointer vec) (mesh-vertex-data mesh) (+ (* stride vertex) offset) length)
    vec))

(define (mesh-update! mesh vertices #!optional indices)
  (let ((usage (and (mesh-usage mesh) (usage->gl (mesh-usage mesh))))
        (stride (mesh-stride mesh))
        (index-stride (gl:type->bytes (mesh-index-type mesh)))
        (vertex-data (mesh-vertex-data mesh))
        (index-data (mesh-index-data mesh))
        (n-vertices (vertex-length (mesh-vertex-attributes mesh) vertices))
        (n-indices (and indices (length indices)))
        (vertex-buffer (mesh-vertex-buffer mesh))
        (index-buffer (mesh-index-buffer mesh)))
    (unless vertex-data
      (error 'mesh-update! "Trying to update a mesh with no data. Usage should be dynamic or stream"))
    (when (> n-vertices (/ (bytevector-length vertex-data)
                           stride))
      (error 'mesh-update! "Cannot update mesh with more vertices than will fit in its array:" vertices))
    (when (and indices (> n-indices (/ (bytevector-length index-data)
                                       index-stride)))
      (error 'mesh-update! "Cannot update mesh with more indices than will fit in its array:" indices))
    (mesh-n-vertices-set! mesh n-vertices)
    (set-vertices mesh vertices)
    (when indices
      (mesh-n-indices-set! mesh n-indices)
      (set-indices mesh indices))
    (when vertex-buffer
      (gl:bind-buffer gl:+array-buffer+ vertex-buffer)
      (gl:buffer-data gl:+array-buffer+
                      (* stride n-vertices)
                      (bytevector->pointer vertex-data)
                      usage)
      (gl:bind-buffer gl:+array-buffer+ 0))
    (when (and indices index-buffer)
      (gl:bind-buffer gl:+element-array-buffer+ index-buffer)
      (gl:buffer-data gl:+element-array-buffer+
                      (* index-stride n-indices)
                      (bytevector->pointer index-data)
                      usage)
      (gl:bind-buffer gl:+element-array-buffer+ 0)))
  mesh)

;;;; Mesh operations
(define (mesh-make-vao! mesh locations #!optional (usage #:static))
  (when (mesh-vao mesh)
    (error 'mesh-make-vao! "Mesh already has vao" mesh))
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
                (let ((location (alist-ref (vertex-attribute-name attribute)
                                           locations)))
                  (when location
                    (gl:vertex-attrib-pointer
                    location
                    (vertex-attribute-number attribute)
                    (gl:type->gl (vertex-attribute-type attribute))
                    (vertex-attribute-normalized attribute)
                    stride
                    (address->pointer (vertex-attribute-offset attribute)))
                   (gl:enable-vertex-attrib-array
                    location))))
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
    (set-finalizer! mesh delete-mesh)))

(define (mesh-copy! to at from
                    #!optional (start 0) (end (mesh-n-vertices mesh)))
  (let ((stride (mesh-stride from)))
    (bytevector-copy! (mesh-vertex-data to) (* at (mesh-stride to))
                      (mesh-vertex-data from) (* start stride) (* end stride))))

(define (mesh-copy mesh)
  (make-mesh vertices: `(attributes: ,(map (lambda (a)
                                             (list (vertex-attribute-name a)
                                                   (vertex-attribute-type a)
                                                   (vertex-attribute-number a)
                                                   normalized:
                                                   (vertex-attribute-normalized a)))
                                           (mesh-vertex-attributes mesh))
                         initial-elements: ,(bytevector-copy (mesh-vertex-data mesh)))
             indices: `(type: ,(mesh-index-type mesh)
                        initial-elements: ,(bytevector-copy (mesh-index-data mesh)))))

(define (mesh-append meshes)
  (let* ((mesh (car meshes))
         (new (make-mesh vertices:
                         `(attributes: ,(map (lambda (a)
                                               (list (vertex-attribute-name a)
                                                     (vertex-attribute-type a)
                                                     (vertex-attribute-number a)
                                                     normalized:
                                                     (vertex-attribute-normalized a)))
                                             (mesh-vertex-attributes mesh))
                           initial-elements: ,(bytevector-append
                                               (map mesh-vertex-data meshes)))
                         indices:
                         `(type: ,(mesh-index-type mesh)
                           initial-elements: ,(bytevector-append
                                               (map mesh-index-data meshes)))))
         (index-data (mesh-index-data new))
         (index-type (mesh-index-type new))
         (index-size (gl:type->bytes index-type))
         (set (type->setter index-type))
         (get (type->getter index-type)))
    (let loop ((meshes meshes) (index-index 0) (vertex-offset 0))
      (if (null? meshes)
          new
          (let* ((mesh (car meshes))
                 (n-vertices (mesh-n-vertices mesh))
                 (n-indices (mesh-n-indices mesh)))
            (do ((i 0 (add1 i)))
                ((= i n-indices))
              (let ((k (* (+ index-index i) index-size)))
                (set index-data k
                     (+ (get index-data k)
                        vertex-offset))))
            (loop (cdr meshes)
                  (+ index-index n-indices)
                  (+ vertex-offset n-vertices)))))))

(define (mesh-transform! mesh transform
                         #!key  (start 0) (end (mesh-n-vertices mesh))
                         (position-name 'position) (normal-name 'normal)
                         normal-transform)
  (when (or (negative? start) (> end (mesh-n-vertices mesh))
           (<= (- end start) 0))
    (error 'mesh-vertex-ref "Bad vertex range" start end))
  (let* ((stride (mesh-stride mesh))
         (attributes (mesh-vertex-attributes mesh))
         (position-offset (vertex-attribute-offset
                           (get-vertex-attribute position-name attributes)))
         (normal-offset (if* (find (lambda (attribute)
                                     (equal? normal-name
                                             (vertex-attribute-name attribute)))
                                   attributes)
                             (vertex-attribute-offset it)
                             #f))
         (inverse-transpose (when normal-offset
                              (transpose (inverse (or normal-transform transform))))))
    (m*vector-array! transform
                     (pointer+ (bytevector->pointer (mesh-vertex-data mesh))
                               (+ position-offset (* start stride)))
                     stride: stride
                     length: (- end start))
    (when normal-offset
      (m*vector-array! inverse-transpose
                       (pointer+ (bytevector->pointer (mesh-vertex-data mesh))
                                 (+ normal-offset (* start stride)))
                       stride: stride
                       length: (- end start)))))

(define (mesh-transform-append pairs #!key (position-name 'position)
                               (normal-name 'normal))
  (let* ((meshes (map first pairs))
         (transforms (map second pairs))
         (normal-transforms (if (> (length (car pairs)) 2)
                                (map third pairs)
                                #f))
         (mesh (mesh-append meshes))
         (attributes (mesh-vertex-attributes mesh))
         (position-offset (vertex-attribute-offset
                           (get-vertex-attribute position-name attributes)))
         (normal-offset (if* (find (lambda (attribute)
                                     (equal? normal-name
                                             (vertex-attribute-name attribute)))
                                   attributes)
                             (vertex-attribute-offset it)
                             #f))
         (stride (mesh-stride mesh))
         (vertex-data (bytevector->pointer (mesh-vertex-data mesh))))
    (let loop ((meshes meshes) (transforms transforms)
               (normal-transforms normal-transforms) (vertex-offset 0))
      (if (null? meshes)
          mesh
          (let ((n-vertices (mesh-n-vertices (car meshes)))
                (offset (* vertex-offset stride)))
            (m*vector-array! (car transforms)
                             (pointer+ vertex-data
                                       (+ position-offset offset))
                             stride: stride
                             length: n-vertices)
            (when normal-offset
              (m*vector-array! (transpose (inverse (car (or normal-transforms
                                                            transforms))))
                               (pointer+ vertex-data
                                         (+ normal-offset offset))
                               stride: stride
                               length: n-vertices))
            (loop (cdr meshes) (cdr transforms)
                  (and normal-transforms (cdr normal-transforms))
                  (+ vertex-offset n-vertices)))))))


;;;; Type keywords
(define valid-type '(char: int8: byte: uchar: uint8: unsigned-byte:
                     short: int16: ushort: uint16: unsigned-short:
                     int: int32: uint: uint32: unsigned-int: unsigned-int32:
                     integer: integer32:  unsigned-integer: unsigned-integer32:
                     float: float32: double: float64:))

(define (type->make-vector type)
  (ecase type
    ((char: int8: byte:) make-s8vector)
    ((uchar: uint8: unsigned-byte:) make-u8vector)
    ((short: int16:) make-s16vector)
    ((ushort: uint16: unsigned-short:) make-u16vector)
    ((int: int32: integer: integer32:) make-s32vector)
    ((uint: uint32: unsigned-int: unsigned-int32:
	    unsigned-integer: unsigned-integer32:)
     make-u32vector)
    ((float: float32:) make-f32vector)
    ((double: float64:) make-f64vector)))

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

(define (type->getter type)
  (ecase type
    ((char: int8: byte:) bytevector-s8-ref)
    ((uchar: uint8: unsigned-byte:) bytevector-u8-ref)
    ((short: int16:) bytevector-s16-ref)
    ((ushort: uint16: unsigned-short:) bytevector-u16-ref)
    ((int: int32: integer: integer32:) bytevector-s32-ref)
    ((uint: uint32: unsigned-int: unsigned-int32:
	    unsigned-integer: unsigned-integer32:)
     bytevector-u32-ref)
    ((float: float32:) bytevector-f32-ref)
    ((double: float64:) bytevector-f64-ref)))

(define (usage->gl usage)
  (ecase usage
    ((dynamic: dynamic-draw:) gl:+dynamic-draw+)
    ((stream: stream-draw:) gl:+stream-draw+)
    ((static: static-draw:) gl:+static-draw+)
    ((dynamic-read:) gl:+dynamic-read+)
    ((stream-read:) gl:+stream-read+)
    ((static-read:) gl:+static-read+)
    ((dynamic-copy:) gl:+dynamic-copy+)
    ((stream-copy:) gl:+stream-copy+)
    ((static-copy:) gl:+static-copy+)))

(cond-expand
  (gles (define (mode->gl mode)
          (ecase mode
            ((points:) gl:+points+)
            ((lines:) gl:+lines+)
            ((line-strip:) gl:+line-strip+)
            ((line-loop:) gl:+line-loop+)
            ((triangle-strip:) gl:+triangle-strip+)
            ((triangle-fan:) gl:+triangle-fan+)
            ((triangles:) gl:+triangles+))))
  (else (define (mode->gl mode)
          (ecase mode
            ((points:) gl:+points+)
            ((lines:) gl:+lines+)
            ((line-strip:) gl:+line-strip+)
            ((line-loop:) gl:+line-loop+)
            ((line-strip-adjacency:) gl:+line-strip-adjacency+)
            ((lines-adjacency:) gl:+lines-adjacency+)
            ((triangle-strip:) gl:+triangle-strip+)
            ((triangle-fan:) gl:+triangle-fan+)
            ((triangles:) gl:+triangles+)
            ((triangle-strip-adjacency:) gl:+triangle-strip-adjacency+)
            ((triangles-adjacency:) gl:+triangles-adjacency+)
            ((patches:) gl:+patches+)))))

) ; end gl-utils-mesh
