(module hyperscene
  (init
   make-scene
   activate-scene
   deactivate-scene
   update-scene
   update-scenes
   add-pipeline
   delete-pipeline
   set-pipeline-alpha

   add-node
   delete-node
   set-bounding-sphere!
   move-node!
   set-node-position!
   node-rotation
   node-position
   node-data

   make-camera
   render-cameras
   render-camera
   safe-render-cameras
   safe-render-camera
   resize-cameras
   set-camera-clip-planes!
   set-camera-view-angle!
   move-camera!
   set-camera-position!
   camera-rotation
   set-camera-up!
   camera-look-at!
   pan-camera!
   set-camera-pan!
   tilt-camera!
   set-camera-tilt!
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
   current-camera-model-view-projection)

(import chicken scheme foreign)
(use srfi-4 miscmacros lolevel)

(foreign-declare "#include <hyperscene.h>")

(define *window-size-fun* (lambda () (values 0 0)))

(define-external (windowSizeFun ((c-pointer int) *x) ((c-pointer int) *y)) void
  (let-values (((x y) (*window-size-fun*)))
    (pointer-s32-set! *x x)
    (pointer-s32-set! *y y)))

(define (init fun)
  (set! *window-size-fun* fun)
  ((foreign-lambda void "hpgInitScenes" c-pointer) #$windowSizeFun))

(define add-pipeline
  (foreign-lambda c-pointer "hpgAddPipeline"
    c-pointer c-pointer c-pointer bool))

(define delete-pipeline
  (foreign-lambda void "hpgDeletePipeline" c-pointer))

(define set-pipeline-alpha
  (foreign-lambda void "hpgPipelineAlpha" c-pointer bool))


;;; Scenes
(define (make-scene)
  (set-finalizer! ((foreign-lambda c-pointer "hpgMakeScene" c-pointer)
                   (aabb-tree-interface))
                  delete-scene))

(define delete-scene
  (foreign-lambda void "hpgDeleteScene" c-pointer))

(define aabb-tree-interface
  (foreign-lambda c-pointer "hpgAABBpartitionInterface"))

(define deactivate-scene
  (foreign-lambda void "hpgDeactiveateScene" c-pointer))

(define activate-scene
  (foreign-lambda void "hpgActiveateScene" c-pointer))

(define update-scene
  (foreign-lambda void "hpgUpdateScene" c-pointer))

(define update-scenes
  (foreign-lambda void "hpgUpdateScenes"))

;;; Nodes
(define add-node
  (foreign-lambda c-pointer "hpgAddNode" c-pointer c-pointer c-pointer c-pointer))

(define delete-node
  (foreign-safe-lambda void "hpgDeleteNode" c-pointer))

(define set-bounding-sphere!
  (foreign-lambda void "hpgSetBoundingSphere" c-pointer float))

(define move-node!
  (foreign-lambda void "hpgMoveNode" c-pointer f32vector))

(define set-node-position!
  (foreign-lambda void "hpgSetNodePosition" c-pointer f32vector))

(define node-rotation
  (foreign-lambda c-pointer "hpgNodeRotation" c-pointer))

(define node-position*
  (foreign-lambda c-pointer "hpgNodePosition" c-pointer))

(define (node-position node)
  (let ((pos (make-f32vector 3))
        (pos* (node-position* node)))
    (print pos*)
    (f32vector-set! pos 0 (pointer-f32-ref pos*))
    (f32vector-set! pos 1 (pointer-f32-ref (pointer+ pos* 4)))
    (f32vector-set! pos 2 (pointer-f32-ref (pointer+ pos* 8)))
    pos))

(define node-data
  (foreign-lambda c-pointer "hpgNodeData" c-pointer))


;;; Cameras
(define +ortho+ 0)
(define +perspective+ 1)

(define +position+ 0)
(define +look-at+ 1)
(define +orbit+ 2)
(define +first-person+ 3)

(define (make-camera type style scene #!key (near 0.1) (far 100) (angle 70))
  (let ((camera (set-finalizer!
                 ((foreign-safe-lambda c-pointer "hpgMakeCamera"
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
  (foreign-lambda void "hpgDeleteCamera" c-pointer))

(define set-camera-clip-planes!
  (foreign-safe-lambda void "hpgSetCameraClipPlanes" c-pointer float float))

(define set-camera-view-angle!
  (foreign-safe-lambda void "hpgSetCameraViewAngle" c-pointer float))

(define render-cameras
  (foreign-lambda void "hpgRenderCameras"))

(define safe-render-cameras
  (foreign-safe-lambda void "hpgRenderCameras"))

(define render-camera
  (foreign-lambda void "hpgRenderCamera" c-pointer))

(define safe-render-camera
  (foreign-safe-lambda void "hpgRenderCamera" c-pointer))

(define resize-cameras
  (foreign-lambda void "hpgResizeCameras" int int))

(define move-camera!
  (foreign-lambda void "hpgMoveCamera" c-pointer f32vector))

(define set-camera-position!
  (foreign-lambda void "hpgSetCameraPosition" c-pointer f32vector))

(define camera-rotation
  (foreign-lambda c-pointer "hpgCameraRotation" c-pointer))

(define set-camera-up!
  (foreign-lambda void "hpgSetCameraUp" c-pointer f32vector))

(define camera-look-at!
  (foreign-lambda void "hpgCameraLookAt" c-pointer f32vector))

(define pan-camera!
  (foreign-lambda void "hpgPanCamera" c-pointer float))

(define set-camera-pan!
  (foreign-lambda void "hpgSetCameraPan" c-pointer float))

(define tilt-camera!
  (foreign-lambda void "hpgTiltCamera" c-pointer float))

(define set-camera-tilt!
  (foreign-lambda void "hpgSetCameraTilt" c-pointer float))

(define zoom-camera!
  (foreign-lambda void "hpgZoomCamera" c-pointer float))

(define set-camera-zoom!
  (foreign-lambda void "hpgSetCameraZoom" c-pointer float))

(define roll-camera!
  (foreign-lambda void "hpgRollCamera" c-pointer float))

(define set-camera-roll!
  (foreign-lambda void "hpgSetCameraRoll" c-pointer float))

(define move-camera-forward!
  (foreign-lambda void "hpgMoveCameraForward" c-pointer float))

(define move-camera-up!
  (foreign-lambda void "hpgMoveCameraUp" c-pointer float))

(define strafe-camera!
  (foreign-lambda void "hpgStrafeCamera" c-pointer float))

(define current-camera-position
  (foreign-lambda c-pointer "hpgCurrentCameraPosition"))

(define current-camera-view
  (foreign-lambda c-pointer "hpgCurrentCameraView"))

(define current-camera-projection
  (foreign-lambda c-pointer "hpgCurrentCameraProjection"))

(define current-camera-view-projection
  (foreign-lambda c-pointer "hpgCurrentCameraViewProjection"))

(define current-camera-model-view-projection
  (foreign-lambda c-pointer "hpgCurrentCameraModelViewProjection"))

) ; end hyperscene
