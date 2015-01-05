# Hyperscene
Hyperscene is a scene library – made for placing objects in a shared world for the purpose of rendering – for CHICKEN Scheme. Hyperscene features a scene graph, cameras with a variety of movement types, frustum culling based on an configurable spatial partitioning system and a lighting extension. Hyperscene is target agnostic: it is not bound to any particular rendering target and should work equally well for curses, OpenGL, and anything in between.

Hyperscene is a set of bindings to the [Hyperscene C library](https://github.com/AlexCharlton/Hyperscene). It’s fairly rough around the edges for general use in Scheme. It should generally be wrapped in order to make it more palatable. [Hypergiant](http://wiki.call-cc.org/eggref/4/hypergiant) is one such library that wraps Hyperscene for use with OpenGL.

## Installation
This repository is a [Chicken Scheme](http://call-cc.org/) egg.

It is part of the [Chicken egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html) and can be installed with `chicken-install hyperscene`.

Various debugging statements are printed (in case you’re wondering how many things are being drawn, or what the partitioning system is doing) when the feature `#:debug` is defined at compile-time.

## Requirements
- miscmacros

## Documentation
Hyperscene’s scenes rely on a number of elements to be in place before a scene can be rendered. First is the scene itself. A **scene** could be thought of as the world or coordinate system that serves as the base for all the rendering operations. Second is a node. A **node** is the “physical” thing that can being rendered. Nodes can be added to scenes, or can be added to each other if you want a node to be defined in terms of its relation to another (hence the scene “graph”). Third is a camera. **Cameras** have a position and orientation in a scene, as well as a projection. Cameras can be rendered, which renders the part of the scene that they are pointing at. The fourth element that must be present is a pipeline. **Pipelines** are the collection of functions that explain how to render a node. If a node is to be rendered, it must have a pipeline associated with it.

These four elements are all represented as c-pointers. Passing the wrong pointer to the wrong function will result in bad things happening. Sorry.

The basic use of Hyperscene is as follows: First you create pipelines and a scene. Then you add nodes to that scene (or to nodes that are already in the scene). These nodes are assigned a pipeline that knows how it can draw them. Then you create a camera associated with that scene, that has a particular position, orientation, and projection. Cameras are then called upon to render the scene.

    [procedure] (init WINDOW-SIZE-FUN)

Before any other functions can be used (except for pipeline creation), Hyperscene must be initialized.

`WINDOW-SIZE-FUN` is a function that returns two values: the width and height of the current window.

### Scenes
Scenes can be either active or inactive. The only difference is that active scenes are updated with a call to `update-scenes`.

    [procedure] (make-scene)

Create a new scene. The scene’s space is partitioned with the [partition interface](#spatial-partitioning) given by `partition-interface`. New scenes are automatically activated.

    [procedure] (delete-scene SCENE)

Delete the given scene. 

    [procedure] (activate-scene SCENE)

Activate the given scene. If the scene is already active, this has no effect.

    [procedure] (deactivate-scene SCENE)

Deactivate the given scene.

    [procedure] (update-scenes)

Update all active scenes. This must be called every frame in order to make sure all nodes are positioned correctly.

### Nodes
Nodes are the elements that are rendered in Hyperscene. They have five primary properties:

- Their parent: which can either be a scene, or another node
- Their position and orientation: how they are positioned relative to their parent and the world
- Their data: user-specified data
- Their pipeline: the set of functions that accepts the data, and renders the node
- Their bounding sphere radius: the radius that defines a sphere that encloses the entire visible space of the node

The following functions are used create, delete, and work with nodes:

    [procedure] (add-node PARENT DATA PIPELINE DELETE)

Create a new node with the given parent. `PARENT` can either be a scene or another node. `DATA` is a user supplied pointer to some data which is then passed to the `PIPELINE` functions as well as to `DELETE` when the node is deleted.

    [procedure] (delete-node NODE)
    [procedure] (unsafe-delete-node NODE)

Delete the given node, removing it from the scene and calling `DELETE` on its `DATA`. `unsafe-delete-node` should only be called if the `DELETE` function is a pure C function that not call back into Scheme.

    [procedure] (node-scene NODE)

Return the scene that the node belongs to.

    [procedure] (set-node-bounding-sphere! NODE RADIUS)

Set the radius of the node’s bounding sphere. This is important to set so that Hyperscene knows when the node is inside a camera’s bounding volume or not. When a node is created, the bounding sphere radius is initially set to `1`.

    [procedure] (node-bounding-sphere NODE)

Return the `#f32(x y z radius)` bounding sphere of the node. The bounding sphere is positioned in world coordinates. Modifying the returned f32vector will have no effect.

    [procedure] (set-node-position! NODE POINT)

Set the position of the node, relative to its parent, to be the `#f32(x y z)` `POINT`.

    [procedure] (move-node NODE VECTOR)

Move the node by the `#f32(x y z)` `VECTOR`.

    [procedure] (node-position NODE)

Return the `#f32(x y z)` position of the node relative to its parent. Modifying this value will not change the node’s position.

    [procedure] (node-rotation NODE)

Return a pointer to the node’s quaternion `(x y z w)` that describes the rotation of the node relative to its parent. Modifying this quaternion (e.g. with gl-math’s imperative [quaternion functions](http://wiki.call-cc.org/eggref/4/gl-math#quaternion-operations)) will rotate the node. Make sure to call `node-needs-update!` after modifying the returned quaternion.

    [procedure] (node-needs-update! NODE)

Nodes need to be informed when they have been modified in such a way that they need to be updated. Most node modification functions (`set-node-position!`, `move-node!`, `set-node-bounding-sphere!`) call this automatically, but Hyperscene cannot tell when a node’s rotation quaternion has been modified. Make sure to call `node-needs-update!` after modifying `node-rotation`’s return value.

    [procedure] (node-transform NODE)

Return a pointer to the 4x4 transform matrix that describes the position and orientation of the node in world space. Consecutive elements of the matrix represent columns. Any modifications to the transform matrix will be lost when the scene is updated.

    [procedure] (node-data NODE)

Return a pointer to the node’s user supplied data.

#### Memory management
    [procedure] (set-node-pool-size! SIZE)

Hyperscene uses memory pools to store its data relating to nodes, which makes creation and deletion of nodes and scenes quick. For best performance, set the node pool size to be as large as the greatest number of nodes that will be needed for a scene. When a scene is created with `make-scene` its node pool is set to this size. Defaults to `4096`.


### Pipelines
Pipelines are structures consisting of three functions: a pre-render function, a render function, and a post-render function. When a scene (camera) is rendered, the visible nodes are sorted by their pipelines before they are drawn. Then, for every group of pipelines, the pre-render function is called with the first node as an argument. Every node is then passed to the render function. Finally, the post-render function is called to clean up. The sorting is done – and the pre/post-render functions are only called once – in order to minimize the amount of state changes that need to occur during rendering.

The exception to this is when a pipeline represents an element that could be partially transparent. “Alpha” pipelines get drawn after all the other ones, and the nodes that are associate with alpha pipelines are always drawn in order of decreasing distance from the camera. This ensures that the transparent parts can be rendered correctly.

When targeting OpenGL, one pipeline per shader program is generally desirable. The pre-render function should therefore call `gl:use-program`, while the render function should not. The post-render function can reset any state (e.g. `(gl:use-program 0)`, etc).

    [procedure] (add-pipeline PRE-RENDER RENDER POST-RENDER [ALPHA])

Create a new pipeline with the given [callbacks](http://wiki.call-cc.org/man/4/Callbacks) (or pointers to pure C functions). `ALPHA?` indicates whether or not the pipeline can render any transparent elements (defaults to `#f`).

    [procedure] (delete-pipeline PIPELINE)

Delete the given pipeline.


### Cameras
Cameras, aside from having an orientation and position within a given scene, have two main properties. Their *type* is the sort of [projection](http://en.wikipedia.org/wiki/Graphical_projection) that the camera uses: either orthographic or perspective. The *style* of the camera indicates the way in which the camera can be moved (see [Movement and rotation](#movement-and-rotation) for details of each function):

- A *position* camera is one where the position and rotation of the camera is explicitly set. Movement functions: `move-camera!`, `set-camera-position!`, `camera-rotation`.
- A *look-at* camera is given a position, an up-vector, and a point that it is looking at. The rotation of the camera is determined from these vectors. Movement functions: `move-camera!`, `set-camera-position!`, `set-camera-up!`, `camera-look-at!`.
- An *orbit* camera is given a point to look at, a distance from that object, and a yaw (rotation around the vertical axis), pitch (rotation around the side-to-side axis), and roll (rotation around the front-to-back axis). Movement functions: `camera-look-at!`, `yaw-camera!`, `set-camera-yaw!`, `pitch-camera!`, `set-camera-pitch!`, `roll-camera!`, `set-camera-roll!`, `zoom-camera!`, `set-camera-zoom!`.
- A *first-person* camera has a position, yaw, pitch, and roll, and has special functions to move it forward and back, left and right, and up and down. Movement functions: `move-camera!`, `move-camera-forward!`, `set-camera-position!`, `yaw-camera!`, `set-camera-yaw!`, `pitch-camera!`, `set-camera-pitch!`, `roll-camera!`, `set-camera-roll!`, `move-camera-forward!`, `move-camera-up!`, `strafe-camera!`.

The following functions are used create, delete, and work with cameras:

    [procedure] (make-camera TYPE STYLE SCENE [near: NEAR] [far: FAR] [angle: ANGLE])

Create a new camera associated with the given scene. `TYPE` must be one of `#:ortho` or `#:perspective` for an orthographic or a perspective camera, respectively. `STYLE` must be one of `#:position`, `#:look-at`, `#:orbit`, or `#:first-person`. New cameras are automatically activated. `NEAR` is the near plane of the camera, defaulting to `1`. `FAR` is the far plane of the camera, defaulting to `10000`. `ANGLE` is the view-angle, in degrees, for perspective cameras, defaulting to `70`.

    [procedure] (delete-camera CAMERA)

Delete the given camera.

    [procedure] (render-camera CAMERA)

Render the given camera. When cameras are rendered, all of the visible nodes are sorted: first into groups of nodes that have an alpha pipline or that don’t. Alpha nodes are sorted by decreasing distance from the camera and rendered last. Non-alpha nodes are sorted by pipeline. Each pipeline is then sorted again by increasing distance from the camera before they are rendered. Drawing the things that are closest to the camera first (“reverse painter” sorting) can help graphics hardware determine when later bits of the scene are hidden, thus saving some rendering time. Not all applications will benefit from this extra step, though, and it can be disabled by defining the feature `#:no-reverse-painter` at compilation time.

    [procedure] (update-camera CAMERA)

Update the given camera. This updates the view matrix of the camera to reflect any changes that may have occurred. This should always be done before rendering.


    [procedure] (activate-camera CAMERA)

Add the camera to the list of active cameras (or push it to the back of the list, thus setting it to be rendered last). New cameras are automatically activated.

    [procedure] (deactivate-camera CAMERA)

Remove the camera from the list of active cameras.

    [procedure] (render-cameras)

Render all the active cameras.

    [procedure] (update-cameras)

Update all the active cameras.

    [procedure] (resize-cameras)

Modify the projection matrix of all cameras, based on the window dimensions supplied by the `WINDOW-SIZE-CALLBACK` that was passed to `init`. Should be called whenever the window is resized.

    [procedure] (set-camera-clip-planes! CAMERA NEAR FAR)

Set the near and far clip planes of the camera. Nodes closer to or further away from these plans will not be visible.

    [procedure] (set-camera-view-angle! CAMERA ANGLE)

Set the viewing angle of the perspective camera to `angle` degrees. This doesn’t have any effect on orthographic cameras.

#### Movement and rotation
    [procedure] (move-camera! CAMERA VECTOR)

Move the position of the camera by the vector `#f32(x y z)`. Cannot be called with an *orbit* camera.

    [procedure] (set-camera-position! CAMERA POINT)

Set the position of the camera to the `#f32(x y z)` point . Cannot be called with an *orbit* camera.

    [procedure] (camera-position CAMERA)

Return the `#f32(x y z)` position of the camera. Modifying this vector will not affect the camera.

    [procedure] (camera-rotation CAMERA)

Return a pointer to the node’s quaternion `(x y z w)` that describes the rotation of the camera. Modifying this quaternion (e.g. with gl-math’s imperative [quaternion functions](http://wiki.call-cc.org/eggref/4/gl-math#quaternion-operations)) will rotate *position* cameras. The returned quaternion must not be modified for any other camera styles.

    [procedure] (camera-look-at! CAMERA POINT)

Set the `#f32(x y z)` point the *look-at* or *orbit* cameras are looking at.

    [procedure] (set-camera-up! CAMERA UP)

Set the camera’s `#f32(x y z)` up-vector. Cannot be called with a non-*look-at* camera.

    [procedure] (yaw-camera! CAMERA ANGLE)

Add `ANGLE` radians to the *orbit* or *first-person* camera’s yaw.

    [procedure] (set-camera-yaw! CAMERA ANGLE)

Set the yaw of the *orbit* or *first-person* camera to `ANGLE` radians.

    [procedure] (pitch-camera! CAMERA ANGLE)

Add `ANGLE` radians to the *orbit* or *first-person* camera’s pitch.

    [procedure] (set-camera-pitch! CAMERA ANGLE)

Set the pitch of the *orbit* or *first-person* camera to `ANGLE` radians.

    [procedure] (roll-camera! CAMERA ANGLE)

Add `ANGLE` radians to the *orbit* or *first-person* camera’s roll.

    [procedure] (set-camera-roll! CAMERA ANGLE)

Set the roll of the *orbit* or *first-person* camera to `ANGLE` radians.

    [procedure] (zoom-camera CAMERA DISTANCE)

Add `DISTANCE` to the *orbit* camera’s zoom.

    [procedure] (set-camera-zoom CAMERA DISTANCE)

Set the zoom of the *orbit* camera to `DISTANCE`.

    [procedure] (move-camera-forward! CAMERA DISTANCE)

Move the *first-person* camera forward by `DISTANCE`. Only the camera’s yaw is taken into account for the movement.

    [procedure] (move-camera-up! CAMERA DISTANCE)

Move the *first-person* camera up by `DISTANCE`.

    [procedure] (strafe-camera! CAMERA DISTANCE)

Move the *first-person* camera to the right by `DISTANCE`. Only the camera’s yaw is taken into account for the movement.

#### Camera matrix and position access
    [procedure] (camera-projection CAMERA)

Returns a pointer to the projection matrix of the camera.

    [procedure] (camera-view CAMERA)

Returns a pointer to the view matrix of the camera.

    [procedure] (camera-view-projection CAMERA)

Returns a pointer to the `projection * view` matrix of the camera.


##### Currently rendering camera
While rendering, it can be desirable to have pointers to various matrices relating to the camera and node being rendered (e.g. to be used as uniform values). These pointers always point to the relevant value of the camera currently being rendered.

    [procedure] (current-camera-position)

Returns a pointer to the `(x y z)` position of the camera currently being rendered.

    [procedure] (current-camera-view)

Returns a pointer to the view matrix of the camera currently being rendered.

    [procedure] (current-camera-projection)

Returns a pointer to the projection matrix of the camera currently being rendered.

    [procedure] (current-camera-view-projection)

Returns a pointer to the `projection * view` matrix of the camera currently being rendered.

    [procedure] (current-camera-model-view-projection)

Returns a pointer to the `projection * view * model` matrix of the node currently being rendered.

    [procedure] (current-inverse-transpose-model)

Returns a pointer to the inverse transpose model matrix of the node currently being rendered. This matrix is useful for lighting. If it is not wanted, the calculation of this value can be omitted by defining the feature `#:no-inverse-transpose` at compile time.


### Spatial Partitioning
Hyperscene only renders nodes that are within the bounds of a camera (i.e. it performs view frustum culling). In order for it to efficiently sort through the nodes, a [spatial partitioning](http://en.wikipedia.org/wiki/Space_partitioning) system is used. Different spatial partitioning systems can be used on a per-scene basis. 

If you wish to write a new partition interface, see the [Hyperscene](https://github.com/AlexCharlton/Hyperscene) documentation.

    [procedure] (partition-interface)
    [procedure] (set-partition-interface! INTERFACE)

Returns/sets the partition interface that scenes are initialized with. Defaults to `aabb-tree-interface`.

    [procedure] (aabb-tree-interface)

`aabb-tree-interface` is a hybrid AABB ternary tree/nonatree/isoceptree inspired heavily by [Dynamic Spatial Partitioning for Real-Time Visibility Determination](http://www.cs.nmsu.edu/~joshagam/Solace/papers/master-writeup-print.pdf). 
When trees split, they try to split only along those axis where the nodes are most well dispersed. For example, if you have a 2D game, chances are nodes will be arranged along the `X` and `Y` axes, with little separation on the `Z` axis. In this situation, when enough nodes are added to a given AABB tree, it will only split along those two axes. In doing so, it avoids extraneous tree creation. This can be taken advantage of most in 2D situations by not using too much (`Z`) distance between layers.

    [procedure] (set-aabb-tree-pool-size! SIZE)

Set the memory pool size of the `aabb-tree-interface`. This pool sets the number of trees in the partition interface’s pool, which is initialized for each scene when `make-scene` is called. Defaults to `4096`.


### Extensions
Hyperscene features an extension system, so that the rendering of a scene can be augmented in new and exciting ways.

Extensions can add special nodes to scenes. If a node is created that is given a pointer to an extension in place of a pipeline, that node will not be rendered but will instead be handled by its extension during rendering and updating.

    [procedure] (activate-extension SCENE EXTENSION)

Before an extension can be used in a given scene, it must be activated.

#### Lights
Hyperscene supplies an extension that provides a generic lighting system:

    [procedure] (lighting)

Before lights can be used in a scene, `lighting` must be activated with `activate-extension`.

    [procedure] (max-lights)

Return the maximum number of lights that may be visible at once. Defaults to `8`.

    [procedure] (set-max-lights! N)

Set the maximum lights that may be visible at once. Must not be called after the first scene with lighting is initialized.

    [procedure] (set-ambient-light! SCENE COLOR)

Scenes that use the lighting extension have an `#f32(r g b)` ambient light associated with them, set by this function.

    [procedure] (add-light PARENT COLOR INTENSITY [direction: direction] [spot-angle: SPOT-ANGLE])

Adds a new light to the given `PARENT` node (or scene) with `#f32(r g b)` `COLOR`. `INTENSITY` is the floating point value associated with the brightness of the light. `DIRECTION` is an `#f32(x y z)` vector that indicates the direction that the light is pointing, defaulting to `#f32(0 0 0)`. `SPOT-ANGLE` indicates the angle in radians that the light is spread over (defaulting to `0`, representing a non-spotlight source). A node is returned that can be moved, rotated, and sized like any other node.

    [procedure] (set-light-color! LIGHT COLOR)

Sets the `#f32(r g b)` color of the light.

    [procedure] (light-color LIGHT)

Returns the `#f32(r g b)` color of the light.

    [procedure] (set-light-intensity! LIGHT INTENSITY)

Sets the intensity of the light.

    [procedure] (light-intensity LIGHT)

Returns the intensity of the light.

    [procedure] (set-light-direction! LIGHT DIRECTION)

Sets the `#f32(x y z)` direction of the light.

    [procedure] (light-direction LIGHT)

Returns the `#f32(x y z)` direction of the light.

    [procedure] (set-light-spot-angle! LIGHT ANGLE)

Sets the angle, in radians, over which the light is spread.

    [procedure] (light-spot-angle LIGHT)

Returns the angle over which the light is spread.

    [procedure] (n-current-lights)

Returns a pointer to the number of visible lights in the scene currently being rendered.

    [procedure] (current-ambient-light)

Returns a pointer to the `(r g b)` color of ambient light in the scene currently being rendered.

    [procedure] (current-light-positions)

Returns a pointer to the array of packed `(x y z)` positions of the visible lights in the scene currently being rendered.

    [procedure] (current-light-colors)

Returns a pointer to the array of packed `(r g b)` colors of the visible lights in the scene currently being rendered.

    [procedure] (current-light-intensities)

Returns a pointer to the array of intensities of the visible lights in the scene currently being rendered.

    [procedure] (current-light-directions)

Returns a pointer to the array of packed `(x y z spotAngle)` directions and angles of the visible lights in the scene currently being rendered.

    [procedure] (make-material R G B SHININESS)

Make a simple material definition consisting of an RGB color (specular color, perhaps) and shininess value. Returns a four element f32vector, located in non-garbage-collected memory. Intended for use as a uniform value.

    [procedure] (set-material-specular-color! MATERIAL R G B)

Set the RGB values of the given material.

    [procedure] (set-material-shininess! MATERIAL SHININESS)

Set the shininess value of the given material.

    [procedure] (set-light-pool-size! SIZE)

Every scene is given a pool from which to allocate lights, the size of which can be modified by calling this function before initialization (defaults to `1024`).


#### Writing your own extensions
The [Hyperscene C library](https://github.com/AlexCharlton/Hyperscene) is extensible in C. See [its documentation](https://github.com/AlexCharlton/Hyperscene#writing-your-own-extensions) for details.

## Examples
See the [examples directory](https://github.com/AlexCharlton/hyperscene-chicken/tree/master/examples) to see Hyperscene in action.

## Version history
### Version 0.2.0
20 December 2014

* Separate camera updating from rendering
* Remove `unsafe-render-camera*` functions
* `current-ambient-light` now takes no argument
* `add-light` takes keyword args, not optional

### Version 0.1.0
* Initial release

## Source repository
Source available on [GitHub](https://github.com/AlexCharlton/hyperscene-chicken).

Bug reports and patches welcome! Bugs can be reported via GitHub or to alex.n.charlton at gmail.

## Author
Alex Charlton

## License
BSD
