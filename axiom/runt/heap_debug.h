#ifndef axiom_runt_heap_debug_H_
#define axiom_runt_heap_debug_H_

#include "heap.h"
#include <stdio.h>

void showRoots(FILE*, const RootStack*);
void showBlockNodes(FILE*, const BlockHeader*);
void showBlocksNodes(FILE*, const BlockHeader*);
void showBlockSpace(FILE*, const BlockSpace*);
void showHeap(FILE*, const Heap*);

#endif
