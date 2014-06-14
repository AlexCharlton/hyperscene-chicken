#include <stdbool.h>
#include "memory.h"
#include "hypermath.h"
#include "partition.h"

#define HPG_DEFAULT_NEAR_PLANE 1.0
#define HPG_DEFAULT_FAR_PLANE 10000.0
#define HPG_DEFAULT_VIEW_ANGLE 70.0

typedef struct node HPGnode;
typedef struct scene HPGscene;
typedef struct camera HPGcamera;
typedef struct pipeline HPGpipeline;
typedef void (*HPGwindowSizeFun)(int *, int *);
typedef void (*HPGcameraUpdateFun)(int, int, HPGcamera*);

typedef enum {
    HPG_ORTHO, HPG_PERSPECTIVE
} HPGcameraType;

struct pipeline {
    bool hasAlpha;
    void (*preRender)(void *);
    void (*render)(void *);
    void (*postRender)();
};

struct node {
    struct node *parent;
    Node partitionData;
    HPGscene *scene;
    HPGvector children;
    float x, y, z, rx, ry, rz, angle;
    float *transform;
    struct pipeline *pipeline;
    void (*delete)(void *); //(data)
    void *data;
    bool needsUpdate;
};

struct scene {
    void *null; // used to distinguish top-level nodes;
    HPGvector topLevelNodes;
    PartitionInterface *partitionInterface;
    void *partitionStruct;
    HPGpool nodePool, boundingSpherePool, transformPool, partitionPool;
};

typedef struct {
    float x, y, z;
} Point;

struct camera {
    HPGscene *scene;
    Point position, up, object;
    bool isLookAt;
    float angle;
    float n, f, viewAngle;
    float projection[16];
    float viewProjection[16];
    float modelViewProjection[16];
    Plane planes[6];
    HPGcameraUpdateFun update;
};

void hpgInitCameras();
void hpgSetWindowSizeFun(HPGwindowSizeFun fun);
