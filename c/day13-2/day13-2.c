#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

typedef enum {
    UP,
    DOWN
} Direction;

typedef struct {
    int depth;
    int range;
    int scannerRange;
    Direction dir;
} Layer;

#define NUM_LAYERS (sizeof savedLayers / sizeof *savedLayers)

static bool caught(void);
static void advanceScanners(Layer *layersToModify);

static Layer *layers;

static Layer savedLayers[] = {
/*
{0, 3, 0, DOWN},
{1, 2, 0, DOWN},
{4, 4, 0, DOWN},
{6, 4, 0, DOWN}
*/
    { 0, 5, 0, DOWN },
    { 1, 2, 0, DOWN },
    { 2, 3, 0, DOWN },
    { 4, 4, 0, DOWN },
    { 6, 6, 0, DOWN },
    { 8, 4, 0, DOWN },
    { 10, 6, 0, DOWN },
    { 12, 10, 0, DOWN },
    { 14, 6, 0, DOWN },
    { 16, 8, 0, DOWN },
    { 18, 6, 0, DOWN },
    { 20, 9, 0, DOWN },
    { 22, 8, 0, DOWN },
    { 24, 8, 0, DOWN },
    { 26, 8, 0, DOWN },
    { 28, 12, 0, DOWN },
    { 30, 12, 0, DOWN },
    { 32, 8, 0, DOWN },
    { 34, 8, 0, DOWN },
    { 36, 12, 0, DOWN },
    { 38, 14, 0, DOWN },
    { 40, 12, 0, DOWN },
    { 42, 10, 0, DOWN },
    { 44, 14, 0, DOWN },
    { 46, 12, 0, DOWN },
    { 48, 12, 0, DOWN },
    { 50, 24, 0, DOWN },
    { 52, 14, 0, DOWN },
    { 54, 12, 0, DOWN },
    { 56, 12, 0, DOWN },
    { 58, 14, 0, DOWN },
    { 60, 12, 0, DOWN },
    { 62, 14, 0, DOWN },
    { 64, 12, 0, DOWN },
    { 66, 14, 0, DOWN },
    { 68, 14, 0, DOWN },
    { 72, 14, 0, DOWN },
    { 74, 14, 0, DOWN },
    { 80, 14, 0, DOWN },
    { 82, 14, 0, DOWN },
    { 86, 14, 0, DOWN },
    { 90, 18, 0, DOWN },
    { 92, 17, 0, DOWN }
};

int main(void) {
    layers = calloc(NUM_LAYERS, sizeof *layers);

    if (!layers) {
        perror("calloc");
        return EXIT_FAILURE;
    }

    for (unsigned long delay = 0;; delay++) {
        if (delay % 1000 == 0) {
            printf("trying %lu\n", delay);
        }

        if (!caught()) {
            printf("%lu\n", delay);
            return 0;
        }

        advanceScanners(savedLayers);
    }

    return 0;
}

static bool caught() {
    int playerDepth = -1;
    memcpy(layers, savedLayers, sizeof savedLayers);

    while (playerDepth <= layers[NUM_LAYERS-1].depth) {
        playerDepth++;

        for (size_t i = 0; i < NUM_LAYERS; i++) {
            if (layers[i].depth == playerDepth && layers[i].scannerRange == 0) {
                return true;
            }
        }

        advanceScanners(layers);
    }

    return false;
}


static void advanceScanners(Layer *layersToModify) {
    for (size_t i = 0; i < NUM_LAYERS; i++) {
        if (layersToModify[i].range == 1) {
            continue;
        }

        if (layersToModify[i].dir == DOWN) {
            if (layersToModify[i].scannerRange + 1 < layersToModify[i].range) {
                layersToModify[i].scannerRange++;
            } else {
                layersToModify[i].scannerRange--;
                layersToModify[i].dir = UP;
            }
        } else {
            if (layersToModify[i].scannerRange > 0) {
                layersToModify[i].scannerRange--;
            } else {
                layersToModify[i].scannerRange++;
                layersToModify[i].dir = DOWN;
            }
        }
    }
}
