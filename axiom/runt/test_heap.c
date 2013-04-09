#include "heap_debug.h"
#include "node_debug.h"

#define TEST_TAG TAG_USERDEFINED+1
#define TEST_NODE_SIZE 4

Word* makeNode(Heap* h, Word* other, Word val) {
    Word* node = allocWords(h, TEST_NODE_SIZE);
    node[0] = TEST_TAG;
    node[1] = (Word)other;
    node[2] = (Word)other;
    node[3] = val;
    return node;
}

int main() {
    #ifdef NDEBUG
    printf("NDEBUG\n");
    #endif
    Layouts_init();
    Layouts_setLargestTag(TEST_TAG);
    Layout* layout = initLayout(TEST_TAG);
    layout->type = LAYOUT_SMALL_PTRSFIRST;
    layout->x.ptrsFirst.nWords = TEST_NODE_SIZE;
    layout->x.ptrsFirst.nPtrs = 1;

    Allocator a;
    Allocator_init(&a);
    RootStack rs;
    RootStack_init(&rs, &a);
    Heap h;
    Heap_init(&h, &rs, &a);

    Word* node = allocWords(&h, TEST_NODE_SIZE);
    node[0] = TEST_TAG;
    node[1] = (Word)node;
    node[2] = (Word)node;
    node[3] = 5;
    Word* n2 = makeNode(&h, node, 6);
    Word* n3 = makeNode(&h, n2, 7);

    Word** roots = RootStack_push(&rs, 2);
    roots[0] = n2;
    roots[1] = node;

    //    showAddrNode(stdout, roots[0]);
    showHeap(stdout, &h);
    gcHeap(&h, 0);
    showHeap(stdout, &h);
    //    showAddrNode(stdout, roots[0]);
    //    showAddrNode(stderr, node); // should be deallocated, so could segfault

    Heap_destroy(&h);
    RootStack_destroy(&rs);
    Allocator_destroy(&a);
    return 0;
}
