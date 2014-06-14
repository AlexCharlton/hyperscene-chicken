(module hyperscene
  (init
   make-scene
   activate-scene
   deactivate-scene
   add-pipeline
   delete-pipeline
   set-pipeline-alpha

   add-node
   delete-node
   set-bounding-sphere!
   move-node!
   set-node-position!
   rotate-node!
   set-node-rotation!
   node-model-matrix
   node-data

   make-camera
   render-cameras
   render-camera
   resize-cameras
   set-camera-clip-planes!
   set-camera-view-angle!
   move-camera!
   set-camera-position!
   rotate-camera!
   set-camera-rotation!
   camera-look-at!
   current-camera-position
   current-camera-projection
   current-camera-model-view
   current-camera-model-view-projection)

(import chicken scheme foreign lolevel)
(use miscmacros)

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
  (set-finalizer! ((foreign-lambda c-pointer "hpgDeactiveateScene" c-pointer)
                   (aabb-tree-interface))
                  delete-scene))

(define aabb-tree-interface
  (foreign-lambda c-pointer "hpgAABBpartitionInterface"))

(define deactivate-scene
  (foreign-lambda void "hpgDeactiveateScene" c-pointer))

(define activate-scene
  (foreign-lambda void "hpgActiveateScene" c-pointer))

(define deactivate-scene
  (foreign-lambda void "hpgDeactiveateScene" c-pointer))

(define delete-scene
  (foreign-lambda void "hpgDeleteScene" c-pointer))


;;; Nodes
(define add-node
  (foreign-lambda c-pointer "hpgAddNode" c-pointer c-pointer c-pointer c-pointer))

(define delete-node
  (foreign-lambda void "hpgDeleteNode" c-pointer))

(define set-bounding-sphere!
  (foreign-lambda void "hpgSetBoundingSphere" c-pointer float))

(define move-node!
  (foreign-lambda void "hpgMoveNode" c-pointer float float float))

(define set-node-position!
  (foreign-lambda void "hpgSetNodePosition" c-pointer float float float))

(define rotate-node!
  (foreign-lambda void "hpgRotateNode" c-pointer float))

(define set-node-rotation!
  (foreign-lambda void "hpgSetNodeRotation" c-pointer float float float float))

(define node-model-matrix
  (foreign-lambda c-pointer "hpgNodeTransform" c-pointer))

(define node-data
  (foreign-lambda c-pointer "hpgNodeData" c-pointer))


;;; Cameras
(define +ortho+ 0)
(define +perspective+ 1)

(define (make-camera type scene #!key (near 1) (far 100) (angle 70))
  (let ((camera (set-finalizer!
                 ((foreign-lambda c-pointer "hpgMakeCamera" unsigned-int c-pointer)
                  (ecase type
                    ((ortho:) +ortho+)
                    ((perspective:) +perspective+))
                  scene)
                 delete-camera)))
    (set-camera-clip-planes! camera near far)
    (set-camera-view-angle! camera angle)
    camera))

(define delete-camera
  (foreign-lambda void "hpgDeleteCamera" c-pointer))

(define set-camera-clip-planes!
  (foreign-lambda void "hpgSetCameraClipPlanes" c-pointer float float))

(define set-camera-view-angle!
  (foreign-lambda void "hpgSetCameraViewAngle" c-pointer float))

(define render-cameras
  (foreign-lambda void "hpgRenderCameras"))

(define render-camera
  (foreign-lambda void "hpgRenderCamera" c-pointer))

(define resize-cameras
  (foreign-lambda void "hpgResizeCameras"))

(define move-camera!
  (foreign-lambda void "hpgMoveCamera" c-pointer float float float))

(define set-camera-position!
  (foreign-lambda void "hpgSetCameraPosition" c-pointer float float float))

(define rotate-camera!
  (foreign-lambda void "hpgRotateCamera" c-pointer float ))

(define set-camera-rotation!
  (foreign-lambda void "hpgSetCameraRotation" c-pointer float float float float))

(define camera-look-at!
  (foreign-lambda void "hpgCameraLookAt" c-pointer float float float))

(define current-camera-position
  (foreign-lambda c-pointer "hpgCurrentCameraPosition"))

(define current-camera-projection
  (foreign-lambda c-pointer "hpgCurrentCameraProjection"))

(define current-camera-model-view
  (foreign-lambda c-pointer "hpgCurrentCameraModelView"))

(define current-camera-model-view-projection
  (foreign-lambda c-pointer "hpgCurrentCameraModelViewProjection"))

) ; end hyperscene
