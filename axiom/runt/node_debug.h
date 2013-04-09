#ifndef axiom_runt_node_debug_H_
#define axiom_runt_node_debug_H_

#include "node.h"
#include <stdio.h>

void showLayout(FILE*, const Layout*);
void showNode(FILE*, const Word*);
void showAddrNode(FILE*, const Word*);

#endif
