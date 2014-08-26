# gl-utils
Provides a higher level interface to OpenGL. The following modules are included:

- opengl-utils: Exports all of the modules in this egg
- opengl-utils-core: Convenience procedures for OpenGL, abstracting over common tasks
- opengl-utils-srfi-4: OpenGL-safe numeric vectors
- opengl-utils-ply: [PLY](http://paulbourke.net/dataformats/ply/) file loading

## Installation
This repository is a [Chicken Scheme](http://call-cc.org/) egg.

It is part of the [Chicken egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html) and can be installed with `chicken-install gl-utils`.

## Requirements
- z3
- matchable
- miscmacros
- srfi-42
- make

## Documentation
### gl-utils-core
    [procedure] (check-error)

Performs `get-error` (`glGetError`) and prints the error type when an error is returned.

    [procedure] (make-shader TYPE SOURCE)

Creates and compiles a shader object given the shader's type (e.g. `+vertex-shader+`, `+geometry-shader+`, `+fragment-shader+`), and a string containing the GLSL source. Returns an integer representing the ID of the shader.

    [procedure] (make-program SHADER-LIST [PROGRAM-ID])

Creates and links a program object, given a list of shader objects (i.e. the integers returned by `make-shader`. Returns an integer representing the ID of the program.

Accepts an optional `PROGRAM-ID` argument. If given, `make-program` will use this ID rather than generating a new one.

    [procedure] (gen-buffer)
    [procedure] (gen-framebuffer)
    [procedure] (gen-program-pipeline)
    [procedure] (gen-query)
    [procedure] (gen-renderbuffer)
    [procedure] (gen-sampler)
    [procedure] (gen-texture)
    [procedure] (gen-transform-feedback)
    [procedure] (gen-vertex-array)

Analogous to their pluralized counterparts, but only generates and returns one (integer) object.

    [procedure] (delete-buffer BUFFER)
    [procedure] (delete-framebuffer FRAMEBUFFER)
    [procedure] (delete-program-pipeline PROGRAM-PIPELINE)
    [procedure] (delete-query QUERY)
    [procedure] (delete-renderbuffer RENDERBUFFER)
    [procedure] (delete-sampler SAMPLER)
    [procedure] (delete-texture TEXTURE)
    [procedure] (delete-transform-feedback TRANSFORM-FEEDBACK)
    [procedure] (delete-vertex-array VERTEX-ARRAY)

Analogous to their pluralized counterparts, but only accepts and deletes one (integer) object.

    [macro] (with-buffer TYPE BUFFER BODY ...)
    [macro] (with-framebuffer FRAMEBUFFER BODY ...)
    [macro] (with-program-pipeline PROGRAM-PIPELINE BODY ...)
    [macro] (with-renderbuffer RENDERBUFFER BODY ...)
    [macro] (with-sampler UNIT SAMPLER BODY ...)
    [macro] (with-texture TYPE TEXTURE BODY ...)
    [macro] (with-transform-feedback TRANSFORM-FEEDBACK BODY ...)
    [macro] (with-vertex-array VERTEX-ARRAY BODY ...)

Equivalent to binding the object, executing the body, then binding 0. E.g. `(bind-texture TYPE TEXTURE) BODY ... (bind-texture TYPE 0)`.

    [procedure] (set-texture-properties ID type: TYPE mag: MAG min: MIN wrap: WRAP wrap-s: WRAP-S wrap-t: WRAP-T wrap-r: WRAP-R)

Conveniently set the most common properties of the texture `ID`. `TYPE` is the texture type, defaulting to `+texture-2d+`. `MAG` and `MIN` are the texture magnify and minifying functions, which default to `+linear+`. `WRAP-S`, `WRAP-T`, and `WRAP-R` set the wrapping parameters which default to `+repeat+`. `WRAP` sets all three wrapping parameters to the same value.

    [procedure] (create-framebuffer WIDTH HEIGHT channels: CHANNELS type: TYPE)

Create a framebuffer with a texture and depth renderbuffer attached. The texture and renderbuffer are given the dimensions `WITH` and `HEIGHT`. `CHANNELS` is the number of channels that the texture has: 1, 2, 3, or 4, corresponding to `+red+`, `+rg+`, `+rgb+, and `+rgba+ respectively, defaulting to 4. `TYPE` is the type of the texture data which defaults to `+unsigned-byte+`. Returns three values: The framebuffer, the texture, and the renderbuffer.

    [procedure] (make-vao VERTEX-DATA INDEX-DATA ATTRIBUTES [USAGE])

`make-vao` generalizes the typically repetitious code used to initialize vertex attribute objects. It deals with the case of having packed vertex data (`VERTEX-DATA`) and a vector of indices (`INDEX-DATA`) for those vertexes. This data may be passed as any sort of (srfi-4) vector or a blob.

`ATTRIBUTES` is the list of data necessary for the vertex attributes, in the form of `((LOCATION TYPE N [normalize?: NORMALIZE?]) ...)`. `LOCATION` is the attribute location which may be given as `#f` if the attribute is not used. `TYPE` is the type of data corresponding to the given attribute, given as a keyword. For possible types, see `type->gl-type`. `N` is the number of elements for the given attribute. The keyword `normalize?:` accepts a boolean argument which instructs OpenGL to normalize the attribute or not. Defaults to `#f`.

The optional `USAGE` must be one of `+stream-data+`, `+stream-read+`, `+stream-copy+`, `+static-data+`, `+static-read+`, `+static-copy+`, `+dynamic-data+`, `+dynamic-read+`, `+dynamic-copy+`. Defaults to `+static-draw+`.

`make-vao` returns the ID of a vertex array object. This object should be deleted with `delete-vertex-array` when no longer used. Intermediate vertex buffers are generated and deleted, thus only the returned vertex array ID needs to be managed.

    [procedure] (->pointer VECTOR)

Returns the pointer to a srfi-4 vector or blob.

    [procedure] (size VECTOR)

Returns the size, in bytes, of a srfi-4 vector or blob.

    [procedure] (type->bytes TYPE)

Returns the size of `TYPE` (as accepted by `type->gl-type`) in number of bytes.

    [procedure] (type->gl-type TYPE)

Converts the keyword `TYPE` into a OpenGL type enum value. Accepted types (grouped by synonyms) are: 

- `char:` `int8:` `byte:`
- `uchar:` `uint8:` `unsigned-byte:`
- `short:` `int16:`
- `ushort:` `uint16:` `unsigned-short:`
- `int:` `int32:` `integer:` `integer32:`
- `uint:` `uint32:` `unsigned-int:` `unsigned-int32:` `unsigned-integer:` `unsigned-integer32:`
- `float:` `float32:`
- `double:` `float64:`

### gl-utils-srfi4
gl-utils-srfi4 reexports a version of [srfi-4](http://api.call-cc.org/doc/srfi-4) that gives preference to vectors being created in non-garbage collected memory. This is useful for use with OpenGL, since it is often desirable to pass vectors to OpenGL that will remain in one place. All srfi-4 functions not mentioned below are reexported without changes.

The `NNNvector` and `list->NNNvector` constructors have been modified so that they return vectors in non-garbage collected memory.

The `make-NNNvector` constructors act as their srfi-4 counterparts, except they return vectors in non-garbage collected memory by default.

### gl-utils-ply
    [procedure] (load-ply FILE BUFFER-SPEC)

Loads a [PLY](http://paulbourke.net/dataformats/ply/) file. `FILE` is a path that may be pointing either to a gziped PLY file or a regular PLY file. `BUFFER-SPEC` is a list in the form `((NAME VARS) ...)` where `NAME` is the name of an element in the PLY file and `VARS` is either a list of property names or, in the case of a property list, a single name. Two values are returned: a list of u8vectors which correspond to the buffers named in `BUFFER-SPEC` and a list of the elements that are in the PLY file in the form of:

    (element-name n-elements (property-name property-type))

Or, when an element is a property list:

    (element-name n-elements (property-name (list: list-length-type element-type)))

The buffers returned are packed with the contents of the properties named in the `BUFFER-SPEC`. Thus, for a PLY file that has element `vertex` with properties `float x`, `float y`, `float z`, `float confidence`, `uchar r`, `uchar g`, and `uchar b`, as well as an element `face` with a property list `uchar ushort vertex_index`, the following `BUFFER-SPEC` could be used:

    (load-ply "example.ply.gz" '((vertex: (x y z r g b)) (face: vertex_index)))

This buffer spec would result in a list of two u8vectors being returned: one with the packed elements corresponding to properties `x`, `y`, `z`, `r`, `g`, and `b` (with the corresponding property types), and the second containing the vertex indices.

    [procedure] (load-ply-vao FILE vertex: VERTEX face: FACE)

Similar to `load-ply`, but returns a number of values:

- A vertex array ID as generated by `make-vao`. 
- A u8vector representing the vertex data of the model
- A u8vector representing the index data of the model
- The number of vertices of the model
- The GL enum value of the type of primitive used for the model (e.g. `+triangles+`)
- The GL enum value of the element data type 

`FILE` is a PLY file (which may be gziped). The PLY file must contain at least the elements `vertex` and `face` (other elements will be ignored). `VERTEX` is a list of `(attribute-location property-name ...)` elements, which specifies how the vertex buffers of the VAO will be arranged. All properties named by each element of `VERTEX` must be of the same type. `FACE` is the name of the face property list.

Again, for a PLY file that has element `vertex` with properties `float x`, `float y`, `float z`, `float confidence`, `uchar r`, `uchar g`, and `uchar b`, as well as a element `face` with a property list `uchar ushort vertex_index`, the following could be used:

    (load-ply-vao "example.ply" vertex: `((,vertex-location x y z) 
                                          (,color-location r g b))
                                face: vertex_index)

## Examples
This example depends on the [opengl-glew](http://wiki.call-cc.org/eggref/4/opengl-glew) egg, the [glfw3](http://wiki.call-cc.org/eggref/4/glfw3) egg for window and context creation, and the [gl-math](http://wiki.call-cc.org/eggref/4/gl-math) egg for matrix math.

```Scheme
(import chicken scheme)
(use (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-math (prefix gl-utils gl:)
     gl-utils-srfi-4)

(define *vertex* 
#<<END
#version 330
in vec2 vertex;
in vec3 color;
out vec3 c;
uniform mat4 MVP;

void main(){
   gl_Position = MVP * vec4(vertex, 0.0, 1.0);
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

(define vertex-data (f32vector -1 -1 1 0 0
                               1 -1 0 1 0
                               1 1 0 0 1
                               -1 1 1 0 1))

(define index-data (u16vector 0 1 2
                              0 2 3))

(define vao (make-parameter #f))

(define program (make-parameter #f))

(define projection-matrix
  (perspective 640 480 0.1 100 70))

(define view-matrix
  (look-at 1 0 3
           0 0 0
           0 1 0))

(define model-matrix (mat4-identity))

(define (render)
  (gl:use-program (program))
  (gl:uniform-matrix4fv (gl:get-uniform-location (program) "MVP")
                        1 #f
                        (m* projection-matrix
                            (m* view-matrix model-matrix)))
  (gl:bind-vertex-array (vao))
  (gl:draw-elements-base-vertex gl:+triangles+ 6 (gl:type->gl-type ushort:) #f 0)

  (gl:check-error)
  (gl:bind-vertex-array 0))

(glfw:with-window (640 480 "Example" resizable: #f
                       context-version-major: 3
                       context-version-minor: 3)
  (gl:init)

  (print (gl:supported? "GL_ARB_framebuffer_object"))

  (set! *vertex* (gl:make-shader gl:+vertex-shader+ *vertex*))
  (set! *fragment* (gl:make-shader gl:+fragment-shader+ *fragment*))

  (program (gl:make-program (list *vertex* *fragment*)))

  (vao (gl:make-vao vertex-data index-data
                    `((,(gl:get-attrib-location (program) "vertex") float: 2)
                      (,(gl:get-attrib-location (program) "color") float: 3))))
  (let loop ()
     (glfw:swap-buffers (glfw:window))
     (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
     (render)
     (glfw:poll-events) ; Because of the context version, initializing GLEW results in a harmless invalid enum
     (unless (glfw:window-should-close (glfw:window))
       (loop))))
```

## Version history
### Version 0.1.1
* Initial release

## Source repository
Source available on [GitHub](https://github.com/AlexCharlton/gl-utils).

Bug reports and patches welcome! Bugs can be reported via GitHub or to alex.n.charlton at gmail.

## Author
Alex Charlton

## License
BSD
