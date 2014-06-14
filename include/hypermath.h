#ifndef HPG_MATH
#ifndef CHICKEN
#define HPG_MATH 1


float hpgDegreesToRadians(float deg);

float hpgRadiansToDegrees(float rad);

void hpgMultMat4(const float *matA, const float *matB, float *result);

void hpgPrintMat4(const float *m);

void hpgIdentityMat4(float *m);

void hpgTranslate(float x, float y, float z, float *mat);

void hpgRotateX(float rotation, float *mat);

void hpgRotateY(float rotation, float *mat);

void hpgRotateZ(float rotation, float *mat);

void hpgRotate(float x, float y, float z, float w, float *mat);

void hpgScale2D(float scaleX, float scaleY, float *mat);

void hpgScale3D(float scaleX, float scaleY, float scaleZ, float *mat);

void hpgScale(float factor, float *mat);

void hpgFlipX(float *mat);

void hpgFlipY(float *mat);

void hpgFlipZ(float *mat);

void hpgTranslateScale(float x, float y, float z, float scale, float *mat);

void hpgTranslateRotateScale2D(float x, float y, float z, float angle, float scale,
                               float *mat);

void hpgTranslateRotateScale(float x, float y, float z, 
			     float qx, float qy, float qz, float qw,
                             float scale, float *mat);

void hpgTranspose(const float *mat, float *result);

void hpgInverse(const float *mat, float *result);

// Vector operations
void hpgCross(float ax, float ay, float az, float bx, float by, float bz, float *rx, float *ry, float *rz);

void hpgNormalize(float x, float y, float z, float *rx, float *ry, float *rz);

float hpgDot(float ax, float ay, float az, float bx, float by, float bz);

// Projection
void hpgOrtho(int width, int height, float near, float far, float *mat);

void hpgFrustum(float left, float right, float bottom, float top,
		float near, float far, float *mat);

void hpgPerspective(int width, int height, float near, float far, float angle,
		    float *mat);

// Camera
void hpgLookAt(float eyeX, float eyeY, float eyeZ, float x, float y, float z, float upX, float upY, float upZ, float *mat);

void hpgCameraInverse(const float *camera, float *inverse);

#endif
#endif
