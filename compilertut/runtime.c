#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
/* define all scheme constants */
#define bool_f                0x2F
#define bool_t                0x6F
#define rep_nil               0x3F
#define fx_mask               0x03
#define fx_tag                0x00
#define fx_shift                 2
#define char_mask             0xFF
#define char_tag              0x0F
#define char_shift               8

#define tag_mask              0x07
#define pair_tag              0x01
#define vector_tag            0x05
#define string_tag            0x06
/* all scheme values are of type ptrs */
typedef long unsigned int ptr;
ptr* pair_addr(ptr p) { return (ptr*)(p-pair_tag); }
ptr pair_car(ptr p) { return pair_addr(p)[0]; }
ptr pair_cdr(ptr p) { return pair_addr(p)[1]; }
static void print_ptr(ptr x) {
    if((x & fx_mask) == fx_tag){
        printf("%ld", ((long int)x) >> fx_shift);
    } else if(x == bool_f){
        printf("#f");
    } else if(x == bool_t){
        printf("#t");
    } else if (x == rep_nil) {
        printf("()");
    } else if ((x & char_mask) == char_tag) {
        char c = (char)(((long int)x) >> char_shift);
        switch (c) {
            case '\t': printf("#\\tab"); break;
            case '\n': printf("#\\newline"); break;
            case '\r': printf("#\\return"); break;
            case ' ': printf("#\\space"); break;
            default: printf("#\\%c", c);
        }
    } else {
        ptr tag = x & tag_mask;
        switch (tag) {
            case pair_tag:
                printf("(");
                print_ptr(pair_car(x));
                x = pair_cdr(x);
                tag = x & tag_mask;
                while (tag == pair_tag) {
                    printf(" ");
                    print_ptr(pair_car(x));
                    x = pair_cdr(x);
                    tag = x & tag_mask;
                }
                if (x != rep_nil) {
                    printf(" . ");
                    print_ptr(x);
                }
                printf(")");
                break;
/*             case vector_tag: break; */
/*             case string_tag: break; */
            default: printf("#<unknown 0x%16lx>", x);
        }
    }
}

void alloc_err() {
    fprintf(stderr, "allocation failed\n");
    fflush(stderr);
    exit(1);
}
static char* allocate_protected_space(int size){
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  //  printf("%d %d\n", size, aligned_size);
  char* p = mmap(0, aligned_size+ 2 * page,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 -1, 0);
  if (p == MAP_FAILED){ alloc_err(); }
  status = mprotect(p, page, PROT_NONE);
  if(status != 0){ alloc_err(); }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if(status != 0){ alloc_err(); }
  return (p + page);
}
static void deallocate_protected_space(char* p, int size){
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  //  printf("%d %d\n", size, aligned_size);
  status = munmap(p - page, aligned_size + 2 * page);
  if(status != 0){
      fprintf(stderr, "deallocation failed\n");
      fflush(stderr);
      exit(1);
  }
}

ptr scheme_entry(char*);
int main(int argc, char** argv){
/*     printf("%ld\n", sizeof(ptr)); */
/*     printf("%ld\n", sizeof(void*)); */
    int heap_size = 32*4096;
    char* heap_top = allocate_protected_space(heap_size);
    char* heap_base = heap_top;// + heap_size;
    print_ptr(scheme_entry(heap_base));
    printf("\n");
/*     printf("top: %ld\n", (long int)heap_top); */
/*     printf("base: %ld\n", (long int)heap_base); */
/*     printf("result: %ld\n", scheme_entry(heap_base)); */
    //    scheme_entry();
    deallocate_protected_space(heap_top, heap_size);
    return 0;
}
