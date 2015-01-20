;;;; simple.scm
;;;;
;;;; A simple hyperscene example
;;;;
;;;; Compile with csc -lGL simple.scm

(module simple-scene ()

(import chicken scheme)
(use hyperscene glls-render (prefix glfw3 glfw:) (prefix opengl-glew gl:)
     gl-math gl-utils miscmacros lolevel extras srfi-1)

;; Mesh
(define cube (make-mesh vertices: '(attributes: ((position #:float 3))
                                    initial-elements: ((position . (0 0 0
                                                                    1 0 0
                                                                    1 1 0
                                                                    0 1 0
                                                                    0 0 1
                                                                    1 0 1
                                                                    1 1 1
                                                                    0 1 1))))
                        indices: '(type: #:ushort
                                   initial-elements: (0 1 2
                                                      2 3 0
                                                      7 6 5
                                                      5 4 7
                                                      0 4 5
                                                      5 1 0
                                                      1 5 6
                                                      6 2 1
                                                      2 6 7
                                                      7 3 2
                                                      3 7 4
                                                      3 4 0))))

;; glls pipeline
(define-pipeline simple-shader
  ((#:vertex input: ((position #:vec3))
             uniform: ((mvp #:mat4)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))))
  ((#:fragment output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (vec4 1)))))

;; hyperscene pipeline
(define simple-pipeline
  (let-values (((_ __ ___ ____ begin render end _____)
                (simple-shader-fast-render-functions)))
    (add-pipeline begin render end #f)))

;; Scene and camera parameters
(define scene (make-parameter #f))
(define camera (make-parameter #f))

;; Add a node to the scene
(define (add-cube)
  (let ((data (allocate (renderable-size simple-shader))))
    (make-simple-shader-renderable data: data
           mesh: cube
           mvp: (current-camera-model-view-projection))
    (add-node (scene)
              data 
              simple-pipeline
              (foreign-value "&free" c-pointer))))

;;; Initialization and main loop
(init)

(glfw:with-window (480 480 "Example" resizable: #f)
  (gl:init)
  (gl:enable gl:+depth-test+)
  (gl:depth-func gl:+less+)
  (compile-pipelines)
  (mesh-make-vao! cube (pipeline-mesh-attributes simple-shader))
  (scene (make-scene))
  (camera (make-camera #:perspective #:look-at (scene)
                       width: 480 height: 480))
  (set-camera-position! (camera) (make-point 2 2 6))
  (add-cube)
  (let loop ()
    (glfw:swap-buffers (glfw:window))
    (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (update-cameras)
    (render-cameras)
    (check-error)
    (glfw:poll-events)
    (unless (glfw:window-should-close (glfw:window))
      (loop))))

) ;end module
