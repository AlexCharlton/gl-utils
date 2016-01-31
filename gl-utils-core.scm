(module gl-utils-core (type->bytes
                       type->gl
                       make-shader
                       make-program
                       check-gl
                       check-error
                       gen-buffer
                       delete-buffer
                       gen-framebuffer
                       delete-framebuffer
                       gen-program-pipeline
                       delete-program-pipeline
                       gen-query
                       delete-query
                       gen-renderbuffer
                       delete-renderbuffer
                       gen-sampler
                       delete-sampler
                       gen-texture
                       delete-texture
                       gen-transform-feedback
                       delete-transform-feedback
                       gen-vertex-array
                       delete-vertex-array
                       set-texture-properties
                       with-buffer
                       with-framebuffer
                       with-program-pipeline
                       with-renderbuffer
                       with-sampler
                       with-texture
                       with-transform-feedback
                       with-vertex-array
                       create-framebuffer
                       ->pointer
                       size)

(import chicken scheme foreign)
(use srfi-4 matchable (prefix opengl-glew gl:) miscmacros srfi-1 lolevel)

#>
#include <stdlib.h>
#include <stdio.h>
#if defined (__APPLE__)
#include <OpenGL/gl.h>
#elif defined (_WIN32)
#include <GL/glew.h>
#include <GL/glext.h>
#include <GL/gl.h>
#elif defined (GLES)
#include <GLES3/gl3.h>
#else
#include <GL/gl.h>
#endif
static void showInfoLog(GLuint object){
    GLint logLength;
    char *log;
    glGetShaderiv(object, GL_INFO_LOG_LENGTH, &logLength);
    log = malloc(logLength);
    glGetShaderInfoLog(object, logLength, NULL, log);
    fprintf(stderr, "%s\n", log);
    free(log);
}
<#

(define (type->bytes type)
  (ecase type
    ((char: int8: byte: uchar: uint8: unsigned-byte:) 1)
    ((short: int16: ushort: uint16: unsigned-short:) 2)
    ((int: int32: integer: integer32: uint: uint32: unsigned-int: unsigned-int32:
	   unsigned-integer: unsigned-integer32: float: float32:)
     4)
    ((double: float64:) (cond-expand
                          ((not gles) 8)
                          (else (error 'type->bytes "Doubles not a valid ES type"))))))

(define (type->gl type)
  (ecase type
    ((char: int8: byte:) gl:+byte+)
    ((uchar: uint8: unsigned-byte:) gl:+unsigned-byte+)
    ((short: int16:) gl:+short+)
    ((ushort: uint16: unsigned-short:) gl:+unsigned-short+)
    ((int: int32: integer: integer32:) gl:+int+)
    ((uint: uint32: unsigned-int: unsigned-int32:
	    unsigned-integer: unsigned-integer32:)
     gl:+unsigned-int+)
    ((float: float32:) gl:+float+)
    ((double: float64:) (cond-expand
                          ((not gles) gl:+double+)
                          (else (error 'type->bytes "Doubles not a valid ES type"))))))

(define make-shader
  (foreign-lambda* unsigned-int ((unsigned-int type) (c-string source))
#<<END
    GLuint shader;
    GLint shaderOk;
    shader = glCreateShader(type);
    glShaderSource(shader, 1, (const GLchar**)&source, NULL);
    glCompileShader(shader);
    glGetShaderiv(shader, GL_COMPILE_STATUS, &shaderOk);
    if (!shaderOk) {
       fprintf(stderr, "Failed to compile shader:\n\n");
       int i = 0, line = 2;
       fprintf(stderr, "   1|  ");
       while (source[i]){
         fputc(source[i], stderr);
         if(source[i] == '\n'){
           fprintf(stderr, "%4d|  ", line);
           line++;
         }
         i++;
       }
       fprintf(stderr, "\n\n");
       showInfoLog(shader);
       glDeleteShader(shader);
       C_return(0);
    }
    C_return(shader);
END
))

(define (make-program shaders #!optional (program (gl:create-program)))
  (let loop ((shaders shaders))
    (if (not (null? shaders))
        (begin (gl:attach-shader program (car shaders))
               (loop (cdr shaders)))))
  (gl:link-program program)
  ((foreign-lambda* unsigned-integer ((unsigned-integer program))
     "GLint programOk;
      glGetProgramiv(program, GL_LINK_STATUS, &programOk);
      if (!programOk) {
          fprintf(stderr, \"Failed to link shader program:\\n\\n\");
          showInfoLog(program);
          glDeleteProgram(program);
          C_return(0);
      }
      C_return(program);")
   program))

; Usage:
; (check-gl
;   (gl:do-that ...)
;   (gl:do-this ...))
(define-syntax check-gl
    (syntax-rules ()
        ((_ body ... last)
            (begin
                body ...
                (let ((ret last)) (check-error) ret)))))

(define (check-error)
    (let ((e (gl:get-error)))
        (when (not (= e gl:+no-error+))
            (display (gl-error->string e) (current-error-port)))))

(define (gl-error->string e)
    (cond
        ((= e gl:+no-error+) "No GL error\n")
        ((= e gl:+invalid-enum+) "GL error: invalid enum\n")
        ((= e gl:+invalid-value+) "GL error: invalid value\n")
        ((= e gl:+invalid-operation+) "GL error: invalid operation\n")
        ((= e gl:+invalid-framebuffer-operation+) "GL error: invalid framebuffer operation\n")
        ((= e gl:+out-of-memory+) "GL error: out of memory\n")
        ;((= e gl:+context-lost+) "GL error: context lost\n") ; GL 4.5 or GLES 3.2
        (else
            (cond-expand
                ((not gles)
                    (cond
                        ((= e gl:+stack-overflow+) "GL error: stack overflow\n")
                        ((= e gl:+stack-underflow+) "GL error: stack underflow\n")
                        (else (string-append "Unknown GL error: " (number->string e) "\n"))))
                (else
                    (string-append "Unknown GL error: " (number->string e) "\n"))))))

(define (gen-buffer)
  (let ((vec (make-u32vector 1)))
    (gl:gen-buffers 1 vec)
    (u32vector-ref vec 0)))

(define (delete-buffer x)
  (let ((vec (u32vector x)))
    (gl:delete-buffers 1 vec)))

(define (gen-framebuffer)
  (let ((vec (make-u32vector 1)))
    (gl:gen-framebuffers 1 vec)
    (u32vector-ref vec 0)))

(define (delete-framebuffer x)
  (let ((vec (u32vector x)))
    (gl:delete-framebuffers 1 vec)))

(define (gen-program-pipeline)
  (let ((vec (make-u32vector 1)))
    (gl:gen-program-pipelines 1 vec)
    (u32vector-ref vec 0)))

(define (delete-program-pipeline x)
  (let ((vec (u32vector x)))
    (gl:delete-program-pipelines 1 vec)))

(define (gen-query)
  (let ((vec (make-u32vector 1)))
    (gl:gen-queries 1 vec)
    (u32vector-ref vec 0)))

(define (delete-query x)
  (let ((vec (u32vector x)))
    (gl:delete-queries 1 vec)))

(define (gen-renderbuffer)
  (let ((vec (make-u32vector 1)))
    (gl:gen-renderbuffers 1 vec)
    (u32vector-ref vec 0)))

(define (delete-renderbuffer x)
  (let ((vec (u32vector x)))
    (gl:delete-renderbuffers 1 vec)))

(define (gen-sampler)
  (let ((vec (make-u32vector 1)))
    (gl:gen-samplers 1 vec)
    (u32vector-ref vec 0)))

(define (delete-sampler x)
  (let ((vec (u32vector x)))
    (gl:delete-samplers 1 vec)))

(define (gen-texture)
  (let ((vec (make-u32vector 1)))
    (gl:gen-textures 1 vec)
    (u32vector-ref vec 0)))

(define (delete-texture x)
  (let ((vec (u32vector x)))
    (gl:delete-textures 1 vec)))

(define (gen-transform-feedback)
  (let ((vec (make-u32vector 1)))
    (gl:gen-transform-feedbacks 1 vec)
    (u32vector-ref vec 0)))

(define (delete-transform-feedback x)
  (let ((vec (u32vector x)))
    (gl:delete-transform-feedbacks 1 vec)))

(define (gen-vertex-array)
  (let ((vec (make-u32vector 1)))
    (gl:gen-vertex-arrays 1 vec)
    (u32vector-ref vec 0)))

(define (delete-vertex-array x)
  (let ((vec (u32vector x)))
    (gl:delete-vertex-arrays 1 vec)))

(define-syntax with-buffer
  (syntax-rules ()
    ((with-buffer type buffer body body-rest ...)
     (begin (gl:bind-buffer type buffer)
            body body-rest ...
            (gl:bind-buffer type 0)))))

(define-syntax with-framebuffer
  (syntax-rules ()
    ((with-framebuffer fbo body body-rest ...)
     (begin (gl:bind-framebuffer gl:+framebuffer+ fbo)
            body body-rest ...
            (gl:bind-framebuffer gl:+framebuffer+ 0)))))

(define-syntax with-program-pipeline
  (syntax-rules ()
    ((with-program-pipeline pipeline body body-rest ...)
     (begin (gl:bind-program-pipeline pipeline)
            body body-rest ...
            (gl:bind-program-pipeline 0)))))

(define-syntax with-renderbuffer
  (syntax-rules ()
    ((with-renderbuffer rb body body-rest ...)
     (begin (gl:bind-renderbuffer gl:+renderbuffer+ rb)
            body body-rest ...
            (gl:bind-renderbuffer gl:+renderbuffer+ 0)))))

(define-syntax with-sampler
  (syntax-rules ()
    ((with-sampler unit sampler body body-rest ...)
     (begin (gl:bind-sampler unit sampler)
            body body-rest ...
            (gl:bind-sampler unit 0)))))

(define-syntax with-texture
  (syntax-rules ()
    ((with-texture type texture body body-rest ...)
     (begin (gl:bind-texture type texture)
            body body-rest ...
            (gl:bind-texture type 0)))))

(define-syntax with-transform-feedback
  (syntax-rules ()
    ((with-transform-feedback id body body-rest ...)
     (begin (gl:bind-transform-feedback gl:+transform-feedback+ id)
            body body-rest ...
            (gl:bind-transform-feedback gl:+transform-feedback+ 0)))))

(define-syntax with-vertex-array
  (syntax-rules ()
    ((with-vertex-array array body body-rest ...)
     (begin (gl:bind-vertex-array array)
            body body-rest ...
            (gl:bind-vertex-array 0)))))

(define (set-texture-properties id #!key (type gl:+texture-2d+)
                                (mag gl:+linear+) (min gl:+linear+)
                                wrap
                                (wrap-s gl:+repeat+) (wrap-t gl:+repeat+)
                                (wrap-r gl:+repeat+))
  (when id
    (gl:bind-texture type id))
  (gl:tex-parameteri type gl:+texture-mag-filter+ mag)
  (gl:tex-parameteri type gl:+texture-min-filter+ min)
  (if wrap
      (begin (gl:tex-parameteri type gl:+texture-wrap-s+ wrap)
             (gl:tex-parameteri type gl:+texture-wrap-t+ wrap)
             (gl:tex-parameteri type gl:+texture-wrap-r+ wrap))
      (begin (gl:tex-parameteri type gl:+texture-wrap-s+ wrap-s)
             (gl:tex-parameteri type gl:+texture-wrap-t+ wrap-t)
             (gl:tex-parameteri type gl:+texture-wrap-r+ wrap-r)))
  (when id
    (gl:bind-texture type 0)))

(define (create-framebuffer width height #!key (channels 4) (type gl:+unsigned-byte+))
  (let ((tex (gen-texture))
        (rend (gen-renderbuffer))
        (fbo (gen-framebuffer))
        (format (case channels
                  ((1) gl:+red+)
                  ((2) gl:+rg+)
                  ((3) gl:+rgb+)
                  ((4) gl:+rgba+))))
    (set-texture-properties tex)
    (with-texture gl:+texture-2d+ tex
      (gl:tex-image-2d gl:+texture-2d+ 0 format width height 0 format type #f))
    (gl:bind-renderbuffer gl:+renderbuffer+ rend)
    (gl:renderbuffer-storage gl:+renderbuffer+ gl:+depth-component+ width height)
    (gl:bind-renderbuffer gl:+renderbuffer+ 0)
    (with-framebuffer fbo
      (gl:framebuffer-texture-2d gl:+framebuffer+ gl:+color-attachment0+
                                 gl:+texture-2d+ tex 0)
      (gl:framebuffer-renderbuffer gl:+framebuffer+ gl:+depth-attachment+
                                   gl:+renderbuffer+ rend))

    (values fbo tex rend)))

(define (->pointer v)
  (make-locative v))

(define (size v)
  (cond
   ((blob? v) (blob-size v))
   ((u8vector? v) (u8vector-length v))
   ((s8vector? v) (s8vector-length v))
   ((u16vector? v) (* (u16vector-length v) 2))
   ((s16vector? v) (* (s16vector-length v) 2))
   ((u32vector? v) (* (u32vector-length v) 4))
   ((s32vector? v) (* (s32vector-length v) 4))
   ((f32vector? v) (* (f32vector-length v) 4))
   ((f64vector? v) (* (f64vector-length v) 8))
   (else (error 'size "Not a blob or vector" v))))

) ; end gl-utils-core
