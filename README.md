# gl-utils
Provides a higher level interface to OpenGL. The following modules are included:

- gl-utils: Exports all of the modules in this egg
- gl-utils-core: Convenience procedures for OpenGL, abstracting over common tasks
- gl-utils-bytevector: OpenGL-safe R7RS style bytevectors with extended accessors
- gl-utils-mesh: Convenient vertex array interface for creation, modification, and use
- gl-utils-ply: [PLY](http://paulbourke.net/dataformats/ply/) file loading
- gl-utils-srfi-4: OpenGL-safe numeric vectors

gl-utils works with OpenGL ES. Just define `gles` when compiling. gl-utils will automatically compile with ES support on ARM hardware.

## Installation
This repository is a [Chicken Scheme](http://call-cc.org/) egg.

It is part of the [Chicken egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html) and can be installed with `chicken-install gl-utils`.

## Requirements
- z3
- matchable
- miscmacros
- srfi-42
- opengl-glew
- gl-math
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

If `ID` is `#f`, no texture will be bound, and therefore properties of whatever texture is currently bound will be set.

    [procedure] (create-framebuffer WIDTH HEIGHT channels: CHANNELS type: TYPE)

Create a framebuffer with a texture and depth renderbuffer attached. The texture and renderbuffer are given the dimensions `WITH` and `HEIGHT`. `CHANNELS` is the number of channels that the texture has: 1, 2, 3, or 4, corresponding to `+red+`, `+rg+`, `+rgb+, and `+rgba+ respectively, defaulting to 4. `TYPE` is the type of the texture data which defaults to `+unsigned-byte+`. Returns three values: The framebuffer, the texture, and the renderbuffer.

    [procedure] (->pointer VECTOR)

Returns the pointer to a srfi-4 vector or blob.

    [procedure] (size VECTOR)

Returns the size, in bytes, of a srfi-4 vector or blob.

    [procedure] (type->bytes TYPE)

Returns the size of `TYPE` (as accepted by `type->gl`) in number of bytes.

    [procedure] (type->gl TYPE)

Converts the keyword `TYPE` into a OpenGL type enum value. Accepted types (grouped by synonyms) are: 

- `char:` `int8:` `byte:`
- `uchar:` `uint8:` `unsigned-byte:`
- `short:` `int16:`
- `ushort:` `uint16:` `unsigned-short:`
- `int:` `int32:` `integer:` `integer32:`
- `uint:` `uint32:` `unsigned-int:` `unsigned-int32:` `unsigned-integer:` `unsigned-integer32:`
- `float:` `float32:`
- `double:` `float64:`

Double is not, however, a valid type when using GL ES.

### gl-utils-bytevector
r7rs style bytevectors with unsafe accessors. As in gl-utils-srfi4, bytevectors are created in non-garbage-collected memory. They will still be freed when no longer used.

    [procedure] (bytevector BYTE ...)

Returns a newly allocated bytevector containing its arguments.

    [procedure] (make-bytevector K [BYTE] [NONGC] [FINALIZE])

Return a newly-allocated bytevector of length `K`. If the optional fill `BYTE` is specified, it specifies the initial value for each slot in the bytevector.

The optional arguments `NONGC` and `FINALIZE` define whether the vector should be allocated in a memory area not subject to garbage collection and whether the associated storage should be automatically freed (using finalization) when there are no references from Scheme variables and data. `NONGC` defaults to #t (the vector will be located in non-garbage-collected memory) and `FINALIZE` defaults to #t. Note that the `FINALIZE` argument is only used when `NONGC` is true.

    [procedure] (bytevector-length BYTEVECTOR)

Return the length in bytes of `BYTEVECTOR`.

    [procedure] (bytevector? BYTEVECTOR)

Returns true if `BYTEVECTOR` is a bytevector, false otherwise.

    [procedure] (bytevector-u8-set! BYTEVECTOR K UNSIGNED-BYTE)
    [procedure] (bytevector-s8-set! BYTEVECTOR K BYTE)
    [procedure] (bytevector-u16-set! BYTEVECTOR K UNSIGNED-SHORT)
    [procedure] (bytevector-s16-set! BYTEVECTOR K SHORT)
    [procedure] (bytevector-u32-set! BYTEVECTOR K UNSIGNED-INT)
    [procedure] (bytevector-s32-set! BYTEVECTOR K INT)
    [procedure] (bytevector-u64-set! BYTEVECTOR K UNSIGNED-LONG)
    [procedure] (bytevector-s64-set! BYTEVECTOR K LONG)
    [procedure] (bytevector-f32-set! BYTEVECTOR K FLOAT)
    [procedure] (bytevector-f64-set! BYTEVECTOR K DOUBLE)

Sets the byte `K` of the given bytevector to be the value of the given fixnum or flonum. These functions are unsafe, so be sure `K` is a valid location in the bytevector.

    [procedure] (bytevector-u8-ref BYTEVECTOR K)
    [procedure] (bytevector-s8-ref BYTEVECTOR K)
    [procedure] (bytevector-u16-ref BYTEVECTOR K)
    [procedure] (bytevector-s16-ref BYTEVECTOR K)
    [procedure] (bytevector-u32-ref BYTEVECTOR K)
    [procedure] (bytevector-s32-ref BYTEVECTOR K)
    [procedure] (bytevector-u64-ref BYTEVECTOR K)
    [procedure] (bytevector-s64-ref BYTEVECTOR K)
    [procedure] (bytevector-f32-ref BYTEVECTOR K)
    [procedure] (bytevector-f64-ref BYTEVECTOR K)

Returns the fixnum or flonum of the given size located at byte `K` of the given bytevector. These functions are unsafe, so be sure `K` is a valid location in the bytevector.

    [procedure] (bytevector–>pointer BYTEVECTOR)

Returns the pointer to `BYTEVECTOR`’s data.usage 

    [procedure] (bytevector-append BYTEVECTOR . BYTEVECTORSS)

Returns a newly allocated bytevector whose elements are the concatenation of the elements in the given bytevectors. If only one argument is passed to `bytevector-append`, it is assumed that it is a list of bytevectors.

    [procedure] (bytevector-copy BYTEVECTOR [START] [END])

Returns a newly allocated copy of the elements of the given bytevector between `START` and `END`.

    [procedure] (bytevector-copy! TO AT FROM [START] [END])

Copies the elements of bytevector `FROM` between `START` and `END` to bytevector `TO`, starting at `AT`. It is an error if `AT` is less than zero or greater than the length of `TO`. It is also an error if `(- (bytevector-length TO) AT)` is less than `(- END START)`.


### gl-utils-mesh
    [procedure] (make-mesh vertices: VERTICES [indices: INDICES] [mode: MODE])

Create a new mesh. `VERTICES` is a list of key value pairs that specifies the mesh’s vertex data. It should be in the form:

    (attributes: ATTRIBUTES [initial-elements: INITIAL-ELEMENTS] [n-vertices: N-VERTICES])

`ATTRIBUTES` is a list in the form:

    (NAME TYPE N [normalized: NORMALIZED])

where `NAME` is the attribute name (as a symbol), `TYPE` is the type of the attribute as accepted by `type->gl`, `N` is the number of elements in the attribute, `NORMALIZED` is a boolean value indicating whether the attribute’s values should be normalized (defaulting to `#f`).

`INITIAL-ELEMENTS` is either a bytevector or a list of `(NAME . VALUE)` pairs where name is the name of the attribute to set (as per the name given in `ATTRIBUTES`) and `VALUE` is the initial contents of that attribute. When a list is given and more than one attribute is given initial-elements, the `VALUE`s should represent the same number of vertices. Values associated with attributes that are `NORMALIZED` should be provided as float between `0.0` and `1.0` (for unsigned types) or `-1.0` and `1.0`, which are then normalized. If `INITIAL-ELEMENTS` is given as a bytevector, that bytevector is used as the entire mesh’s vertex data and `N-VERTICES` – the number of vertices – must be provided.

`INDICES` is an optional list of key value pairs that specifies the mesh’s index data, if it exists. It should be in the form:

    (type: TYPE [initial-elements: INITIAL-ELEMENTS] [n-indices: N-INDICES])

`TYPE` is a type keyword (as accepted by `type->gl`) that must be a valid type for an element array buffer (i.e. an unsigned fixnum). `INITIAL-ELEMENTS` is either a bytevector or a list of values. If `INITIAL-ELEMENTS` is given as a bytevector,  `N-INDICES` – the number of indices – must be provided.

`MODE` is the keyword (as accepted by `mode->gl`) that defines what the mesh is supposed to represent. Defaults to `#:triangles`.

    [record] (mesh VERTEX-ATTRIBUTES INDEX-TYPE VERTEX-DATA INDEX-DATA N-VERTICES N-INDICES VERTEX-BUFFER INDEX-BUFFER VAO STRIDE MODE USAGE)

The type of record returned by `make-mesh`. `VERTEX-ATTRIBUTES` is a list of `vertex-attribute` records. `INDEX-TYPE` is the type given to the `indices:` argument of `make-mesh`. `VERTEX-DATA` and `INDEX-DATA` are the bytevectors representing the vertex and index data. `N-VERTICES` and `N-INDICES` are the number of vertices and indices present in the data. `VERTEX-BUFFER`, `INDEX-BUFFER` and `VAO` are the vertex buffers and VAO created by `mesh-make-vao!`. `STRIDE` is the number of bytes between the start of consecutive vertices. `MODE` is the value of the `mode:` argument provided to `make-mesh`. `USAGE` is the buffer usage that is set with `make-mesh-vao!`.

    [record] (vertex-attribute name type number normalized)

The type of record returned by `mesh-vertex-attributes`. Getters for all of the fields are provided.

    [procedure] (mesh-make-vao! MESH LOCATIONS [USAGE])

Create a vertex attribute object (VAO) for `MESH`. `LOCATIONS` is a list of `(ATTRIBUTE-NAME . LOCATION)` pairs. `USAGE` is the buffer usage hint keyword as accepted by `usage->gl`, defaulting to `#:static`. Vertex buffer objects (VBOs) are created for the vertex and index data. The VAO binds these buffers, and sets the vertex attribute pointers of attributes for which locations have been given. If the usage is one of the static types, the vertex and index data of the mesh are deleted, as are the vertex and index buffers. The VBOs and VAO created by `make-mesh-vao!` are managed and should not be deleted.

    [procedure] (mesh-vertex-ref MESH ATTRIBUTE VERTEX)

Return a vector containing the values of the attribute named `ATTRIBUTE` corresponding to the `VERTEX`th vertex of the `MESH`. The result will be a srfi-4 numeric vector, corresponding to the type of the given attribute.

    [procedure] (mesh-vertex-set! MESH ATTRIBUTE VERTEX VALUE)

Set the `VERTEX`th vertex of the `MESH`’s attribute named `ATTRIBUTE` to the values provided by the srfi-4 numeric vector `VALUE`. `VALUE` must correspond to the type of the attribute, and should be the same length as the attribute. Using a `VALUE` that is too short is unsafe.

If `mesh-make-vao!` has been called already, `mesh-vertex-set!` should be called inside a `with-mesh`. If `mesh-make-vao!` was called with a static usage, `mesh-vertex-set!` will result in an error, since there is no longer any vertex data to set.

    [procedure] (with-mesh MESH THUNK)

Calls `THUNK` with the VBO of the `MESH`’s vertex buffer bound. If the mesh has been modified by `mesh-vertex-set!`, the vertex buffer’s data will be updated before the VBO is unbound. When the usage hint given to `mesh-make-vao!` is dynamic, `buffer-sub-data` will be used to update the buffer data. When the usage hint is stream, `buffer-data` will be used.

    [procedure] (mesh-copy! TO AT FROM [START] [END])

Similar to `bytevector-copy!`, copies the vertices of mesh `FROM` between vertices `START` and `END` to mesh `TO`, starting at vertex `AT`.

    [procedure] (mesh-copy MESH)

Creates a fresh copy of `MESH`.

    [procedure] (mesh-append MESH . MESHES)

Creates a new mesh resulting from appending the vertices of the given meshes together. The indices are also appended and modified so they point to the same vertices. The attributes of all the meshes are assumed to be the same, otherwise bad things will probably happen. If only one argument is passed to `mesh-append` it is assumed that it is a list of meshes.

    [procedure] (mesh-transform! POSITION-NAME MESH TRANSFORM)

Destructively modifies the `POSITION-NAME` attribute of `MESH` by the [gl-math](http://wiki.call-cc.org/eggref/4/gl-math) matrix `TRANSFORM`. `POSITION-NAME` must be the name of a three element float attribute of `MESH`.

    [procedure] (mesh-transform-append POSITION-NAME MESH-TRANSFORM-PAIR . MESH-TRANSFORM-PAIRS)

Creates a new mesh resulting by appending all the given meshes together, then transforming the attribute named by `POSITION-NAME` by the given [gl-math](http://wiki.call-cc.org/eggref/4/gl-math) transform matrices. `MESH-TRANSFORM-PAIR` and `MESH-TRANSFORM-PAIRS` are `(MESH . TRANFORM)` pairs. `POSITION-NAME` must be the name of a three element float attribute of `MESH`. The attributes of all the meshes are assumed to be the same, otherwise bad things will probably happen. If only two arguments are passed to `mesh-transform-append` it is assumed that the second is a list of mesh/transform pairs.

    [procedure] (usage->gl USAGE)

Converts the keyword `USAGE` into a OpenGL usage enum value. Accepted usages (grouped by synonyms) are: 

- `dynamic:` `dynamic-draw:`
- `stream:` `stream-draw:`
- `static:` `static-draw:`
- `dynamic-read:`
- `stream-read:`
- `static-read:`
- `dynamic-copy:`
- `stream-copy:`
- `static-copy:`

This line is here to prevent a Markdown parsing error :|


    [procedure] (mode->gl MODE)

Converts the keyword `MODE` into a OpenGL mode enum value. Accepted modes are: 

- `points:`
- `line-strip:`
- `line-loop:`
- `line-strip-adjacency:`**
- `lines-adjacency:`**
- `triangle-strip:`
- `triangle-fan:`
- `triangles:`
- `triangle-strip-adjacency:`**
- `triangles-adjacency:`**
- `patches:`**

** Not available in GL ES

### gl-utils-ply
    [procedure] (load-ply FILE BUFFER-SPEC)

Loads a [PLY](http://paulbourke.net/dataformats/ply/) file. `FILE` is a path that may be pointing either to a gziped PLY file or a regular PLY file. `BUFFER-SPEC` is a list in the form `((NAME VARS) ...)` where `NAME` is the name of an element in the PLY file and `VARS` is either a list of property names or, in the case of a property list, a single name. Two values are returned: a list of bytevectors which correspond to the buffers named in `BUFFER-SPEC` and a list of the elements that are in the PLY file in the form of:

    (element-name n-elements (property-name property-type))

Or, when an element is a property list:

    (element-name n-elements (property-name (list: list-length-type element-type)))

The buffers returned are packed with the contents of the properties named in the `BUFFER-SPEC`. Thus, for a PLY file that has element `vertex` with properties `float x`, `float y`, `float z`, `float confidence`, `uchar r`, `uchar g`, and `uchar b`, as well as an element `face` with a property list `uchar ushort vertex_index`, the following `BUFFER-SPEC` could be used:

    (load-ply "example.ply.gz" '((vertex: (x y z r g b)) (face: vertex_index)))

This buffer spec would result in a list of two u8vectors being returned: one with the packed elements corresponding to properties `x`, `y`, `z`, `r`, `g`, and `b` (with the corresponding property types), and the second containing the vertex indices.

    [procedure] (load-ply-mesh FILE vertex: VERTEX face: FACE)

Similar to load-ply, but returns a mesh. `FILE` is a PLY file (which may be gziped). The PLY file must contain at least the elements `vertex` and `face` (other elements will be ignored). `VERTEX` is a list of `(ATTRIBUTE-NAME PROPERTY-NAME ...)` elements, which specifies which PLY properties are associate with which attribute, in which order. Attributes are all normalized. All properties named by each element of `VERTEX` must be of the same type. `FACE` is the name of the face property list.

Again, for a PLY file that has element `vertex` with properties `float x`, `float y`, `float z`, `float confidence`, `uchar r`, `uchar g`, and `uchar b`, as well as a element `face` with a property list `uchar ushort vertex_index`, the following could be used:

    (load-ply-mesh "example.ply" vertex: `((position x y z) 
                                          (color r g b))
                                face: vertex_index)

This would create a mesh with vertex attributes `(position #:float 3)` and `(color #:unsigned-byte 3 normalized: #t)` and the `#:unsigned-short` indices given by `vertex_index`.


### gl-utils-srfi4
gl-utils-srfi4 reexports a version of [srfi-4](http://api.call-cc.org/doc/srfi-4) that gives preference to vectors being created in non-garbage collected memory. This is useful for use with OpenGL, since it is often desirable to pass vectors to OpenGL that will remain in one place. All srfi-4 functions not mentioned below are reexported without changes.

The `NNNvector` and `list->NNNvector` constructors have been modified so that they return vectors in non-garbage collected memory. They will still be freed when no longer used.

The `make-NNNvector` constructors act as their srfi-4 counterparts, except they return vectors in non-garbage collected memory by default. They will still be freed when non longer used.


## Examples
This example depends on the [opengl-glew](http://wiki.call-cc.org/eggref/4/opengl-glew) egg, the [glfw3](http://wiki.call-cc.org/eggref/4/glfw3) egg for window and context creation, and the [gl-math](http://wiki.call-cc.org/eggref/4/gl-math) egg for matrix math.

For more examples, check out the [examples directory](https://github.com/AlexCharlton/gl-utils/tree/master/examples).

```Scheme
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

(glfw:with-window (640 480 "Example" resizable: #f
                       context-version-major: 3
                       context-version-minor: 3)
  (gl:init)

  (print (gl:supported? "GL_ARB_framebuffer_object"))

  (set! *vertex* (make-shader gl:+vertex-shader+ *vertex*))
  (set! *fragment* (make-shader gl:+fragment-shader+ *fragment*))
  (program (make-program (list *vertex* *fragment*)))

  (mesh-make-vao! rect `((position . ,(gl:get-attrib-location
                                       (program) "position"))
                         (color . ,(gl:get-attrib-location
                                    (program) "color"))))
  (let loop ()
    (glfw:swap-buffers (glfw:window))
    (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (render)
    (glfw:poll-events) ; Because of the context version, initializing GLEW results in a harmless invalid enum
    (unless (glfw:window-should-close (glfw:window))
      (loop))))
```

## Version history
### Version 0.3.0
11 September 2014

* Merge `mesh-attribute-locations-set!` with `mesh-make-vao!`

### Version 0.2.0
10 September 2014

* Add gl-utils-bytevector module
* Add gl-utils-mesh module
* Remove now-unneeded (and broken) `make-vao`, `make-ply-vao`

### Version 0.1.2

2 September 2014

* Update example for gl-math 0.5

**Version 0.1.1**
* Initial release

## Source repository
Source available on [GitHub](https://github.com/AlexCharlton/gl-utils).

Bug reports and patches welcome! Bugs can be reported via GitHub or to alex.n.charlton at gmail.

## Author
Alex Charlton

## License
BSD
