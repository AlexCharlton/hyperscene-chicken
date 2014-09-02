#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "memory.h"

HPGpool hpgMakePool(size_t blockSize, size_t nBlocks, char name[32]){
    int i;
    size_t size = (blockSize < sizeof(void *)) ? sizeof(void *) : blockSize;
    char *pool = (char *) malloc(size * nBlocks + sizeof(struct pool));
    struct pool *data = (struct pool*) pool;
    char *poolStart = &pool[sizeof(struct pool)];
    data->blockSize = size;
    data->nBlocks = nBlocks;
    data->nextPool = NULL;
    data->freeBlock = (void**) poolStart;
    strcpy(data->name, name);
    for(i = 0; i < nBlocks - 1; i++){
	void **next = (void **) &poolStart[i * size];
	*next = &poolStart[(i+1) * size];
    }
    void **last = (void **) &poolStart[(nBlocks - 1) * size];
    *last = NULL;
    return (void *) pool;
}

void hpgDeletePool(HPGpool pool){
    struct pool *data = (struct pool*) pool;
    if (data->nextPool)
	hpgDeletePool(data->nextPool);
    free(pool);
}

void hpgClearPool(HPGpool pool){
    int i;
    struct pool *data = (struct pool*) pool;
    if (data->nextPool)
	hpgClearPool(data->nextPool);
    char *poolStart = &((char *) pool)[sizeof(struct pool)];
    const unsigned int size = data->blockSize;
    const unsigned int nBlocks = data->nBlocks;
    data->freeBlock = (void**) poolStart;
    for(i = 0; i < nBlocks - 1; i++){
	void **next = (void **) &poolStart[i * size];
	*next = &poolStart[(i+1) * size];
    }
    void **last = (void **) &poolStart[(nBlocks - 1) * size];
    *last = NULL;
}

static HPGpool newestPool(HPGpool pool){
    struct pool *data = (struct pool*) pool;
    if (data->nextPool)
	return newestPool(data->nextPool);
    else
	return pool;
}

static void growPool(HPGpool pool){
    struct pool *data = (struct pool*) pool;
    fprintf(stderr, "Warning: had to grow pool: %s\n", data->name);
    HPGpool newest = newestPool(pool);
    struct pool *newestData = (struct pool*) newest;
    newestData->nextPool = hpgMakePool(data->blockSize, data->nBlocks, "");
    struct pool *newData = (struct pool*) newestData->nextPool;
    data->freeBlock = newData->freeBlock;
}

void *hpgAllocateFrom(HPGpool pool){
    #ifdef DEBUG
      if (!pool){
      fprintf(stderr, "Fatal: trying to allocate to a pool that doesn't exist!\n");
      exit(EXIT_FAILURE);
      }
    #endif
    struct pool *data = (struct pool*) pool;
    void **block = data->freeBlock;
    if (!block){
	growPool(pool);
	block = data->freeBlock;
    }
    data->freeBlock = *block;
    return block;
}

void hpgDeleteFrom(void *block, HPGpool pool){
    struct pool *data = (struct pool*) pool;
    void **b = (void **)block;
    *b = data->freeBlock;
    data->freeBlock = b;
}