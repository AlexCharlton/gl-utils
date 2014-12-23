;; This example illustrates streaming mesh data using mesh-update!

(import chicken scheme)
(use (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-math gl-utils)

(define *vertex* 
#<<END
#version 330
in vec2 position;
in vec3 color;
out vec3 c;
uniform mat4 MVP;

void main(){
   gl_Position = MVP * vec4(position, 0.0, 1.0);
   c = color;
}
END
)

(define *fragment*
#<<END
#version 330
in vec3 c;
out vec4 fragColor;
void main(){
  fragColor = vec4(c, 1.0);
}
END
)

(define rect (make-mesh
              vertices: '(attributes: ((position #:float 2)
                                       (color #:unsigned-byte 3
                                              normalized: #t))
                          initial-elements: ((position . (-1 -1
                                                           1 -1
                                                           1  1
                                                           -1  1))
                                             (color . (255 0   0
                                                       0   255 0
                                                       0   0   255
                                                       255 0   255))))
              indices: '(type: #:ushort
                         initial-elements: (0 1 2
                                            0 2 3))))

(define program (make-parameter #f))

(define projection-matrix
  (perspective 640 480 0.1 100 70))

(define view-matrix
  (look-at (make-point 1 0 3)
           (make-point 0 0 0)
           (make-point 0 1 0)))

(define model-matrix (mat4-identity))

(define (render)
  (gl:use-program (program))
  (gl:uniform-matrix4fv (gl:get-uniform-location (program) "MVP")
                        1 #f
                        (m* projection-matrix
                            (m* view-matrix model-matrix)))
  (gl:bind-vertex-array (mesh-vao rect))
  (gl:draw-elements-base-vertex (mode->gl (mesh-mode rect))
                                (mesh-n-indices rect)
                                (type->gl (mesh-index-type rect))
                                #f 0)

  (check-error)
  (gl:bind-vertex-array 0))

(define (cycle-colors)
  (let* ((time (glfw:get-time))
         (n (- time (floor time))))
    (mesh-update! rect
                  `((position . (-1 -1
                                  1 -1
                                  1  1
                                 -1  1))
                    (color . (,n  0  0
                               0 ,n  0
                               0  0 ,n
                              ,n  0 ,n)))
                  '(0 1 2
                    0 2 3))))

(glfw:with-window (640 480 "Example" resizable: #f
                       context-version-major: 3
                       context-version-minor: 3)
  (gl:init)
  (set! *vertex* (make-shader gl:+vertex-shader+ *vertex*))
  (set! *fragment* (make-shader gl:+fragment-shader+ *fragment*))
  (program (make-program (list *vertex* *fragment*)))
  (mesh-make-vao! rect
                  `((position . ,(gl:get-attrib-location
                                  (program) "position"))
                    (color . ,(gl:get-attrib-location
                               (program) "color")))
                  #:stream)
  (let loop ()
    (cycle-colors)
    (glfw:swap-buffers (glfw:window))
    (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (render)
    (glfw:poll-events) ; Because of the context version, initializing GLEW results in a harmless invalid enum
    (unless (glfw:window-should-close (glfw:window))
      (loop))))
