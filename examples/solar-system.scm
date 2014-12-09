;;;; solar-system.scm
;;;;
;;;; Create bad looking, but dynamically lit "solar systems"
;;;;
;;;; Compile with csc -lGL solar-system.scm
;;;;
;;;; Add a new system with `a`, delete them with `d`.
;;;; Move the camera back and forward, left and right with the arrows
;;;; Move the camera up and down with shift/up and down arrows
;;;; Rotate the camera left and right with shift/left and right arrows

(module solar-system ()

(import chicken scheme)
(use glls-render (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-math gl-utils
     hyperscene miscmacros lolevel extras srfi-1)

(define cube (make-mesh vertices: '(attributes: ((position #:float 3)
                                                 (normal #:float 3))
                                    initial-elements: ((position . (-1 -1 -1
                                                                    1 -1 -1
                                                                    1 1 -1
                                                                    -1 1 -1
                                                                    -1 -1 1
                                                                    1 -1 1
                                                                    1 1 1
                                                                    -1 1 1))
                                                       (normal . (-1 -1 -1
                                                                  1 -1 -1
                                                                  1 1 -1
                                                                  -1 1 -1
                                                                  -1 -1 1
                                                                  1 -1 1
                                                                  1 1 1
                                                                  -1 1 1))))
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

(define planet-material (make-material 0.8 0.8 1 100))

(define sun-colors (list (make-point 1 1 1 #t)
                         (make-point 1 1 0.5 #t)
                         (make-point 0.5 0.5 1 #t)
                         (make-point 1 0.1 0 #t)))

(define-shader phong-lighting
    (#:fragment uniform: ((camera-position #:vec3)
                          (ambient #:vec3)
                          (n-lights #:int)
                          (light-positions (#:array #:vec3 8))
                          (light-colors (#:array #:vec3 8))
                          (light-intensities (#:array #:float 8))
                          (material #:vec4))
                export: (light))
  (define gamma #:vec3 (vec3 (/ 1 2.2)))
  (define (light (surface-color #:vec4) (position #:vec3) (normal #:vec3)) #:vec4
    (let ((linear-color #:vec3 (* ambient surface-color.rgb)))
      (do-times (i n-lights)
        (let* ((light-position #:vec3 (array-ref light-positions i))
               (light-color #:vec3 (array-ref light-colors i))
               (light-intensity #:float (array-ref light-intensities i))
               (surface-specular #:vec3 (vec3 material))
               (specular-exponent #:float material.a)
               (distance #:vec3 (- light-position position))
               (intensity #:float (clamp (/ light-intensity
                                            (+ 0.001
                                               (* distance.x distance.x)
                                               (* distance.y distance.y)
                                               (* distance.z distance.z)))
                                         0 1))
               (to-light #:vec3 (normalize distance))
               (to-camera #:vec3 (normalize (- camera-position position)))
               (diffuse-intensity #:float (max (dot normal to-light) 0))
               (diffuse #:vec3 (* surface-color.rgb light-color diffuse-intensity))
               (specular-intensity
                #:float (if (> diffuse-intensity 0)
                            (max (dot to-camera
                                      (reflect (- to-light) normal))
                                 0)
                            0))
               (specular #:vec3 (* light-color surface-specular
                                   (expt specular-intensity specular-exponent))))
          (+= linear-color (* intensity (+ diffuse specular)))))
      (vec4 (pow linear-color gamma) surface-color.a))))

(define-pipeline phong-shader 
  ((#:vertex input: ((position #:vec3) (normal #:vec3))
             uniform: ((mvp #:mat4) (model #:mat4) (inverse-transpose-model #:mat4))
             output: ((p #:vec3) (n #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! p (vec3 (* model (vec4 position 1))))
     (set! n (normalize (* (mat3 inverse-transpose-model) normal)))))
  ((#:fragment input: ((n #:vec3) (p #:vec3))
               use: (phong-lighting)
               uniform: ((camera-position #:vec3)
                         (ambient #:vec3)
                         (n-lights #:int)
                         (light-positions (#:array #:vec3 8))
                         (light-colors (#:array #:vec3 8))
                         (light-intensities (#:array #:float 8))
                         (material #:vec4))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (light (vec4 1) p n)))))

(define phong-pipeline
  (let-values (((_ __ ___ begin render end) (phong-shader-fast-render-functions)))
    (add-pipeline begin render end #f)))

(define (add-planet-node parent . args)
  (let* ((data (allocate (renderable-size phong-shader)))
         (node (add-node parent
                         data 
                         phong-pipeline
                         (foreign-value "&free" c-pointer))))
    (apply make-phong-shader-renderable data: data
           (append args (list mvp: (current-camera-model-view-projection)
                              material: planet-material
                              camera-position: (current-camera-position)
                              ambient: (current-ambient-light)
                              model: (node-transform node)
                              light-positions: (current-light-positions)
                              light-colors: (current-light-colors)
                              light-intensities: (current-light-intensities)
                              light-directions: (current-light-directions)
                              n-lights: (n-current-lights)
                              inverse-transpose-model:
                              (current-inverse-transpose-model))))
    node))

(define-pipeline sun-shader
  ((#:vertex input: ((position #:vec3))
             uniform: ((mvp #:mat4)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))))
  ((#:fragment uniform: ((color #:vec3))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (vec4 color 1)))))

(define sun-pipeline
  (let-values (((_ __ ___ begin render end) (sun-shader-fast-render-functions)))
    (add-pipeline begin render end #f)))

(define (add-sun-node parent . args)
  (let* ((data (allocate (renderable-size sun-shader)))
         (node (add-node parent
                         data 
                         sun-pipeline
                         (foreign-value "&free" c-pointer))))
    (apply make-sun-shader-renderable data: data
           (append args (list mvp: (current-camera-model-view-projection))))
    node))

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
     (add-system))
    ((and (eq? key glfw:+key-d+) (eq? action glfw:+press+))
     (delete-system)))))

(define systems (make-parameter '()))

(define-record body
  node
  distance
  velocity
  satelites
  angle
  light)

(define (delete-body body)
  (for-each delete-body (body-satelites body))
  (delete-node (body-node body))
  (if* (body-light body)
      (delete-node it)))

(define (add-system)
  (define (random*)
    (- (random 100) 50))
  (define (satelites parent order)
    (if (positive? order)
        (map (lambda (i)
               (let ((distance (* (add1 i) (expt 4 order)))
                     (velocity (/ (- (random 100) 50) 1000))
                     (node (add-planet-node parent mesh: cube)))
                 (set-node-position! node (make-point distance 0 0))
                 (make-body node distance velocity (satelites node (sub1 order)) 0 #f)))
                (iota (+ 2 (random 4))))
        '()))
  (let* ((x (random*))
         (z (random*))
         (color (list-ref sun-colors (random (length sun-colors))))
         (node (add-sun-node (scene) mesh: cube color: color))
         (light (add-light (scene) color 1000 (make-point 0 0 0) 0)))
    (set-node-position! node (make-point x 0 z))
    (set-node-position! light (make-point x 0 z))
    (set-node-bounding-sphere! light 20)
    (systems (cons (make-body node #f #f (satelites node 2)
                              0 light)
                   (systems)))))

(define (delete-system)
  (unless (null? (systems))
    (let ((body (list-ref (systems) (random (length (systems))))))
      (delete-body body)
      (systems (delete body (systems))))))

(define (update-satelites sats)
  (for-each (lambda (body)
              (let* ((a (+ (body-angle body) (body-velocity body)))
                     (x (* (body-distance body) (cos a)))
                     (z (* (body-distance body) (sin a))))
                (body-angle-set! body a)
                (set-node-position! (body-node body) (make-point x 0 z))
                (update-satelites (body-satelites body))))
            sats))

(define (update-systems)
  (for-each (lambda (s)
              (update-satelites (body-satelites s)))
            (systems)))

(define (update)
  (define camera-speed 0.5)
  (move-camera-forward! (camera) (* camera-speed (forward)))
  (strafe-camera! (camera) (* camera-speed (right)))
  (move-camera-up! (camera) (* camera-speed (up)))
  (yaw-camera! (camera) (* 0.05 (rotate)))
  (update-systems)
  (update-scenes))

;;; Initialization and main loop
(init (lambda () (glfw:get-window-size (glfw:window))))

(glfw:with-window (480 480 "Example" resizable: #f)
  (gl:init)
  (gl:enable gl:+depth-test+)
  (gl:depth-func gl:+less+)
  (compile-pipelines)
  (mesh-make-vao! cube (pipeline-mesh-attributes phong-shader))
  (scene (make-scene))
  (activate-extension (scene) (lighting))
  (set-ambient-light! (scene) (make-point 0.001 0.001 0.001))
  (camera (make-camera #:perspective #:first-person (scene)))
  (set-camera-position! (camera) (make-point 0 0 130))
  (let loop ()
    (glfw:swap-buffers (glfw:window))
    (update)
    (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (render-cameras)
    (check-error)
    (glfw:poll-events)
    (unless (glfw:window-should-close (glfw:window))
      (loop))))

) ;end module
