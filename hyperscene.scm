(module hyperscene
  (init
   make-scene
   activate-scene
   deactivate-scene
   update-scenes
   add-pipeline
   delete-pipeline
   activate-extension
   set-node-pool-size!
   set-aabb-tree-pool-size!

   add-node
   delete-node
   unsafe-delete-node
   node-scene
   set-node-bounding-sphere!
   node-bounding-sphere
   move-node!
   set-node-position!
   node-position
   node-needs-update!
   node-rotation
   node-transform
   node-data

   make-camera
   render-cameras
   render-camera
   activate-camera
   deactivate-camera
   resize-cameras
   set-camera-clip-planes!
   set-camera-view-angle!
   move-camera!
   set-camera-position!
   camera-position
   camera-rotation
   set-camera-up!
   camera-look-at!
   yaw-camera!
   set-camera-yaw!
   pitch-camera!
   set-camera-pitch!
   zoom-camera!
   set-camera-zoom!
   roll-camera!
   set-camera-roll!
   move-camera-forward!
   move-camera-up!
   strafe-camera!
   current-camera-position
   current-camera-view
   current-camera-projection
   current-camera-view-projection
   current-camera-model-view-projection
   current-inverse-transpose-model

   lighting
   max-lights
   set-max-lights!
   set-light-pool-size!
   n-current-lights
   current-light-positions
   current-light-colors
   current-light-intensities
   current-light-directions
   current-ambient-light
   add-light
   light-color
   set-light-color!
   light-intensity
   set-light-intensity!
   light-direction
   set-light-direction!
   light-spot-angle
   set-light-spot-angle!
   ambient-light
   set-ambient-light!
   make-material
   set-material-shininess!
   set-material-specular-color!)

(import chicken scheme foreign)
(use srfi-4 miscmacros lolevel)

(foreign-declare "#include <hyperscene.h>")
(foreign-declare "#include <hypersceneLighting.h>")

(define *window-size-fun* (lambda () (values 0 0)))

(define-external (windowSizeFun ((c-pointer int) *x) ((c-pointer int) *y)) void
  (let-values (((x y) (*window-size-fun*)))
    (pointer-s32-set! *x x)
    (pointer-s32-set! *y y)))

(define (init fun)
  (set! *window-size-fun* fun)
  ((foreign-lambda void "hpsInit" c-pointer) #$windowSizeFun))

(define (add-pipeline pre-render render post-render #!optional (alpha? #f))
  ((foreign-lambda c-pointer "hpsAddPipeline"
     c-pointer c-pointer c-pointer bool)
   pre-render render post-render alpha?))

(define delete-pipeline
  (foreign-lambda void "hpsDeletePipeline" c-pointer))


;;; Scenes
(define (make-scene)
  (set-finalizer! ((foreign-lambda c-pointer "hpsMakeScene"))
                  delete-scene))

(define delete-scene
  (foreign-lambda void "hpsDeleteScene" c-pointer))

(define partition-interface
  (foreign-value "hpsPartitionInterface" c-pointer))

(define set-partition-interface!
  (foreign-lambda* void ((c-pointer i))
    "hpsPartitionInterface = i;"))

(define (aabb-tree-interface)
  (foreign-value "hpsAABBpartitionInterface" c-pointer))

(define deactivate-scene
  (foreign-lambda void "hpsDeactivateScene" c-pointer))

(define activate-scene
  (foreign-lambda void "hpsActivateScene" c-pointer))

(define update-scenes
  (foreign-lambda void "hpsUpdateScenes"))

(define activate-extension
  (foreign-lambda void "hpsActivateExtension" c-pointer c-pointer))

(define (set-node-pool-size! n)
  ((foreign-lambda* void ((unsigned-int n))
     "hpsNodePoolSize = n;")
   n))

(define (set-aabb-tree-pool-size! n)
  ((foreign-lambda* void ((unsigned-int n))
     "hpsAABBpartitionPoolSize = n;")
   n))

;;; Nodes
(define add-node
  (foreign-lambda c-pointer "hpsAddNode" c-pointer c-pointer c-pointer c-pointer))

(define unsafe-delete-node
  (foreign-lambda void "hpsDeleteNode" c-pointer))

(define delete-node
  (foreign-safe-lambda void "hpsDeleteNode" c-pointer))

(define node-scene
  (foreign-lambda void "hpsGetScene" c-pointer))

(define set-node-bounding-sphere!
  (foreign-lambda void "hpsSetNodeBoundingSphere" c-pointer float))

(define (node-bounding-sphere node)
  (let ((bs (make-f32vector 4))
        (bs* ((foreign-lambda c-pointer "hpsNodeBoundingSphere" c-pointer) node)))
    (f32vector-set! bs 0 (pointer-f32-ref bs*))
    (f32vector-set! bs 1 (pointer-f32-ref (pointer+ bs* 4)))
    (f32vector-set! bs 2 (pointer-f32-ref (pointer+ bs* 8)))
    (f32vector-set! bs 3 (pointer-f32-ref (pointer+ bs* 12)))
    bs))

(define move-node!
  (foreign-lambda void "hpsMoveNode" c-pointer f32vector))

(define set-node-position!
  (foreign-lambda void "hpsSetNodePosition" c-pointer f32vector))

(define node-needs-update!
  (foreign-lambda void "hpsNodeRotation" c-pointer))

(define node-rotation
  (foreign-lambda c-pointer "hpsNodeRotation" c-pointer))

(define (node-position node)
  (let ((pos (make-f32vector 3))
        (pos* ((foreign-lambda c-pointer "hpsNodePosition" c-pointer) node)))
    (f32vector-set! pos 0 (pointer-f32-ref pos*))
    (f32vector-set! pos 1 (pointer-f32-ref (pointer+ pos* 4)))
    (f32vector-set! pos 2 (pointer-f32-ref (pointer+ pos* 8)))
    pos))

(define node-transform
  (foreign-lambda c-pointer "hpsNodeTransform" c-pointer))

(define node-data
  (foreign-lambda c-pointer "hpsNodeData" c-pointer))


;;; Cameras
(define +ortho+ 0)
(define +perspective+ 1)

(define +position+ 0)
(define +look-at+ 1)
(define +orbit+ 2)
(define +first-person+ 3)

(define (make-camera type style scene #!key (near 1) (far 10000) (angle 70))
  (let ((camera (set-finalizer!
                 ((foreign-safe-lambda c-pointer "hpsMakeCamera"
                    unsigned-int unsigned-int c-pointer)
                  (ecase type
                    ((ortho:) +ortho+)
                    ((perspective:) +perspective+))
                  (ecase style
                    ((position:) +position+)
                    ((look-at:) +look-at+)
                    ((orbit:) +orbit+)
                    ((first-person:) +first-person+))
                  scene)
                 delete-camera)))
    (set-camera-clip-planes! camera near far)
    (set-camera-view-angle! camera angle)
    camera))

(define delete-camera
  (foreign-lambda void "hpsDeleteCamera" c-pointer))

(define deactivate-camera
  (foreign-lambda void "hpsDeactivateCamera" c-pointer))

(define activate-camera
  (foreign-lambda void "hpsActivateCamera" c-pointer))

(define set-camera-clip-planes!
  (foreign-safe-lambda void "hpsSetCameraClipPlanes" c-pointer float float))

(define set-camera-view-angle!
  (foreign-safe-lambda void "hpsSetCameraViewAngle" c-pointer float))

(define render-cameras
  (foreign-safe-lambda void "hpsRenderCameras"))

(define render-camera
  (foreign-safe-lambda void "hpsRenderCamera" c-pointer))

(define resize-cameras
  (foreign-safe-lambda void "hpsResizeCameras"))

(define move-camera!
  (foreign-lambda void "hpsMoveCamera" c-pointer f32vector))

(define set-camera-position!
  (foreign-lambda void "hpsSetCameraPosition" c-pointer f32vector))

(define (camera-position camera)
  (let ((pos (make-f32vector 3))
        (pos* ((foreign-lambda c-pointer "hpsCameraPosition" c-pointer) camera)))
    (f32vector-set! pos 0 (pointer-f32-ref pos*))
    (f32vector-set! pos 1 (pointer-f32-ref (pointer+ pos* 4)))
    (f32vector-set! pos 2 (pointer-f32-ref (pointer+ pos* 8)))
    pos))

(define camera-rotation
  (foreign-lambda c-pointer "hpsCameraRotation" c-pointer))

(define set-camera-up!
  (foreign-lambda void "hpsSetCameraUp" c-pointer f32vector))

(define camera-look-at!
  (foreign-lambda void "hpsCameraLookAt" c-pointer f32vector))

(define yaw-camera!
  (foreign-lambda void "hpsYawCamera" c-pointer float))

(define set-camera-yaw!
  (foreign-lambda void "hpsSetCameraYaw" c-pointer float))

(define pitch-camera!
  (foreign-lambda void "hpsPitchCamera" c-pointer float))

(define set-camera-pitch!
  (foreign-lambda void "hpsSetCameraPitch" c-pointer float))

(define zoom-camera!
  (foreign-lambda void "hpsZoomCamera" c-pointer float))

(define set-camera-zoom!
  (foreign-lambda void "hpsSetCameraZoom" c-pointer float))

(define roll-camera!
  (foreign-lambda void "hpsRollCamera" c-pointer float))

(define set-camera-roll!
  (foreign-lambda void "hpsSetCameraRoll" c-pointer float))

(define move-camera-forward!
  (foreign-lambda void "hpsMoveCameraForward" c-pointer float))

(define move-camera-up!
  (foreign-lambda void "hpsMoveCameraUp" c-pointer float))

(define strafe-camera!
  (foreign-lambda void "hpsStrafeCamera" c-pointer float))

(define (current-camera-position)
  (foreign-value "hpsCurrentCameraPosition" c-pointer))

(define (current-camera-view)
  (foreign-value "hpsCurrentCameraView" c-pointer))

(define (current-camera-projection)
  (foreign-value "hpsCurrentCameraProjection" c-pointer))

(define (current-camera-view-projection)
  (foreign-value "hpsCurrentCameraViewProjection" c-pointer))

(define (current-camera-model-view-projection)
  (foreign-value "hpsCurrentCameraModelViewProjection" c-pointer))

(define (current-inverse-transpose-model)
  (foreign-value "hpsCurrentInverseTransposeModel" c-pointer))

;;; Lighting
(define (lighting)
  (foreign-value "hpsLighting" c-pointer))

(define (max-lights)
  (foreign-value "hpsMaxLights" unsigned-int))

(define set-light-pool-size!
  (foreign-lambda* void ((unsigned-int n))
    "hpsLightPoolSize = n;"))

(define set-max-lights!
  (foreign-lambda* void ((unsigned-int max))
    "hpsMaxLights = max;"))

(define (n-current-lights)
  (foreign-value "hpsNCurrentLights" c-pointer))

(define (current-light-positions)
  (foreign-value "hpsCurrentLightPositions" c-pointer))

(define (current-light-colors)
  (foreign-value "hpsCurrentLightColors" c-pointer))

(define (current-light-intensities)
  (foreign-value "hpsCurrentLightIntensities" c-pointer))

(define (current-light-directions)
  (foreign-value "hpsCurrentLightDirections" c-pointer))

(define (current-ambient-light)
  (foreign-value "hpsCurrentAmbientLight" c-pointer))

(define origin (f32vector 0 0 0))

(define (add-light node color intensity #!optional (direction origin) (spot-angle 0))
  ((foreign-lambda c-pointer "hpsAddLight" c-pointer f32vector float f32vector float)
   node color intensity direction spot-angle))

(define (light-color node)
  (let ((color (make-f32vector 3))
        (color* ((foreign-lambda c-pointer "hpsLightColor" c-pointer) node)))
    (f32vector-set! color 0 (pointer-f32-ref color*))
    (f32vector-set! color 1 (pointer-f32-ref (pointer+ color* 4)))
    (f32vector-set! color 2 (pointer-f32-ref (pointer+ color* 8)))
    color))

(define set-light-color!
  (foreign-lambda void "hpsSetLightColor" c-pointer f32vector))

(define light-intensity
  (foreign-lambda float "hpsLightIntensity" c-pointer))

(define set-light-intensity!
  (foreign-lambda void "hpsSetLightIntensity" c-pointer float))

(define (light-direction node)
  (let ((dir (make-f32vector 3))
        (dir* ((foreign-lambda c-pointer "hpsLightDirection" c-pointer) node)))
    (f32vector-set! dir 0 (pointer-f32-ref dir*))
    (f32vector-set! dir 1 (pointer-f32-ref (pointer+ dir* 4)))
    (f32vector-set! dir 2 (pointer-f32-ref (pointer+ dir* 8)))
    dir))

(define set-light-direction!
  (foreign-lambda void "hpsSetLightDirection" c-pointer f32vector))

(define light-spot-angle
  (foreign-lambda float "hpsSpotAngle" c-pointer))

(define set-light-spot-angle!
  (foreign-lambda void "hpsSetLightSpotAngle" c-pointer float))

(define (ambient-light scene)
(let ((color (make-f32vector 3))
      (color* ((foreign-lambda c-pointer "hpsAmbientLight" c-pointer) scene)))
    (f32vector-set! color 0 (pointer-f32-ref color*))
    (f32vector-set! color 1 (pointer-f32-ref (pointer+ color* 4)))
    (f32vector-set! color 2 (pointer-f32-ref (pointer+ color* 8)))
    color))

(define set-ambient-light!
  (foreign-lambda void "hpsSetAmbientLight" c-pointer f32vector))

(define (make-material r g b shininess)
  (let ((material (make-f32vector 4 0 #t)))
    (f32vector-set! material 0 r)
    (f32vector-set! material 1 g)
    (f32vector-set! material 2 b)
    (f32vector-set! material 3 shininess)
    material))

(define (set-material-specular-color! material r g b)
  (f32vector-set! material 0 r)
  (f32vector-set! material 1 g)
  (f32vector-set! material 2 b))

(define (set-material-shininess! material shininess)
  (f32vector-set! material 3 shininess))

) ; end hyperscene
