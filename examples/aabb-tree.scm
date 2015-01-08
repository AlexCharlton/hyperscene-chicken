;;;; aabb-tree.scm
;;;;
;;;; A program made to test out the partition interface. Try compiling hyperscene with -D debug to see the aabb-tree debugging information.
;;;;
;;;; Compile with csc -lGL aabb-tree.scm
;;;;
;;;; Add nodes with `a`, delete them with `d`.
;;;; Toggle the number of nodes that are being added and deleted, between 1 and 10,  with `n`.
;;;; Toggle the axes that the nodes are being added to with `x`, `y`, and `z` (x axis on when the program starts)
;;;; Move the camera back and forward, left and right with the arrows
;;;; Move the camera up and down with shift/up and down arrows
;;;; Rotate the camera left and right with shift/left and right arrows

(module aabb-tree-test ()

(import chicken scheme)
(use glls-render (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-math gl-utils
     hyperscene srfi-1 extras miscmacros lolevel)

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

(define-pipeline simple-shader
  ((#:vertex input: ((position #:vec3))
             uniform: ((mvp #:mat4)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))))
  ((#:fragment output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (vec4 1)))))

(define simple-pipeline
  (let-values (((_ __ ___ ____ begin render end _____)
                (simple-shader-fast-render-functions)))
    (add-pipeline begin render end #f)))

(define (add-node* parent . args)
  (let ((data (allocate (renderable-size simple-shader))))
    (if* (get-keyword mesh: args)
         (unless (mesh-vao it)
           (mesh-make-vao! it (pipeline-mesh-attributes simple-shader))))
    (apply make-simple-shader-renderable data: data
           (append args (list mvp: (current-camera-model-view-projection))))
    (add-node parent
              data 
              simple-pipeline
              (foreign-value "&free" c-pointer))))

(define scene (make-parameter #f))
(define camera (make-parameter #f))

(define forward (make-parameter 0))
(define up (make-parameter 0))
(define right (make-parameter 0))
(define rotate (make-parameter 0))

(glfw:key-callback
 (lambda (window key scancode action mods)
   (cond
    ((and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
     (glfw:set-window-should-close window 1))
    ((and (eq? key glfw:+key-left+) (eq? action glfw:+press+)
          (eq? mods glfw:+mod-shift+))
     (rotate (sub1 (rotate))))
    ((and (eq? key glfw:+key-right+) (eq? action glfw:+press+)
          (eq? mods glfw:+mod-shift+))
     (rotate (add1 (rotate))))
    ((and (eq? key glfw:+key-left+) (eq? action glfw:+release+)
          (eq? mods glfw:+mod-shift+))
     (rotate (add1 (rotate))))
    ((and (eq? key glfw:+key-right+) (eq? action glfw:+release+)
          (eq? mods glfw:+mod-shift+))
     (rotate (sub1 (rotate))))
    ((and (eq? key glfw:+key-up+) (eq? action glfw:+press+)
          (eq? mods glfw:+mod-shift+))
     (up (add1 (up))))
    ((and (eq? key glfw:+key-down+) (eq? action glfw:+press+)
          (eq? mods glfw:+mod-shift+))
     (up (sub1 (up))))
    ((and (eq? key glfw:+key-up+) (eq? action glfw:+release+)
          (eq? mods glfw:+mod-shift+))
     (up (sub1 (up))))
    ((and (eq? key glfw:+key-down+) (eq? action glfw:+release+)
          (eq? mods glfw:+mod-shift+))
     (up (add1 (up))))
    ((and (eq? key glfw:+key-left+) (eq? action glfw:+press+))
     (right (sub1 (right))))
    ((and (eq? key glfw:+key-right+) (eq? action glfw:+press+))
     (right (add1 (right))))
    ((and (eq? key glfw:+key-left+) (eq? action glfw:+release+))
     (right (add1 (right))))
    ((and (eq? key glfw:+key-right+) (eq? action glfw:+release+))
     (right (sub1 (right))))
    ((and (eq? key glfw:+key-up+) (eq? action glfw:+press+))
     (forward (add1 (forward))))
    ((and (eq? key glfw:+key-down+) (eq? action glfw:+press+))
     (forward (sub1 (forward))))
    ((and (eq? key glfw:+key-up+) (eq? action glfw:+release+))
     (forward (sub1 (forward))))
    ((and (eq? key glfw:+key-down+) (eq? action glfw:+release+))
     (forward (add1 (forward))))
    ((and (eq? key glfw:+key-a+) (eq? action glfw:+press+))
     (add-cubes))
    ((and (eq? key glfw:+key-n+) (eq? action glfw:+press+))
     (toggle-n))
    ((and (eq? key glfw:+key-x+) (eq? action glfw:+press+))
     (toggle-axis 'x))
    ((and (eq? key glfw:+key-y+) (eq? action glfw:+press+))
     (toggle-axis 'y))
    ((and (eq? key glfw:+key-z+) (eq? action glfw:+press+))
     (toggle-axis 'z))
    ((and (eq? key glfw:+key-d+) (eq? action glfw:+press+))
     (delete-cubes)))))

(define (update)
  (define speed 0.5)
  (move-camera-forward! (camera) (* speed (forward)))
  (strafe-camera! (camera) (* speed (right)))
  (move-camera-up! (camera) (* speed (up)))
  (yaw-camera! (camera) (* 0.05 (rotate)))
  (update-scenes))

(define axes (make-parameter '(x)))
(define nodes (make-parameter '()))
(define n (make-parameter 1))

(define (toggle-n)
  (if (= (n) 1)
      (n 10)
      (n 1)))

(define (toggle-axis axis)
  (if (member axis (axes))
      (axes (delete axis (axes)))
      (axes (cons axis (axes)))))

(define (add-cube)
  (define (random*)
    (- (random 100) 50))
  (unless (null? (axes))
    (let ((x (if (member 'x (axes)) (random*) 0))
          (y (if (member 'y (axes)) (random*) 0))
          (z (if (member 'z (axes)) (random*) 0))
          (node (add-node* (scene) mesh: cube)))
      (set-node-position! node (make-point x y z))
      (nodes (cons node (nodes))))))

(define (add-cubes)
  (dotimes (i (n))
    (add-cube)))

(define (delete-cube)
  (unless (null? (nodes))
    (let ((node (list-ref (nodes) (random (length (nodes))))))
      (delete-node node)
      (nodes (delete node (nodes))))))

(define (delete-cubes)
  (dotimes (i (n))
    (delete-cube)))

;;; Initialization and main loop
(init (lambda () (glfw:get-window-size (glfw:window))))

(glfw:with-window (480 480 "Example" resizable: #f)
  (gl:init)
  (gl:enable gl:+depth-test+)
  (gl:depth-func gl:+less+)
  (compile-pipelines)
  (scene (make-scene))
  (camera (make-camera #:perspective #:first-person (scene)))
  (set-camera-position! (camera) (make-point 0 0 60))
  (let loop ()
    (glfw:swap-buffers (glfw:window))
    (update)
    (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (update-cameras)
    (render-cameras)
    (check-error)
    (glfw:poll-events)
    (unless (glfw:window-should-close (glfw:window))
      (loop))))

) ;end module
