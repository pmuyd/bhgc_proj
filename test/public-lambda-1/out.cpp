#include "../../prelude.h"


void pr2892(any,any,any);

void pr2892(any _en2893, any _x44134414, any _k6348)
{
  any _t6329 = prim_cdr(_x44134414);
  any _t6349 = prim_car(_t6329);
 ((proc)((any*)(UNTAG(_k6348)))[0])(_k6348, _t6349, _t6349);
}


int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any* cl2894 = (any*)GC_MALLOC(1*sizeof(any));
  cl2894[0] = (any)(&pr2892);
  any _t6330 = TAG((any)(cl2894), CLO_TAG);
  any _t6331 = INT(8);
  any _t6333 = INT(7);
  any _t6335 = INT(6);
  any _t6337 = INT(5);
  any _t6339 = INT(4);
  any _t6341 = EMPTY;
  any _t6343 = prim_cons(_t6339, _t6341);
  any _t6344 = prim_cons(_t6337, _t6343);
  any _t6345 = prim_cons(_t6335, _t6344);
  any _t6346 = prim_cons(_t6333, _t6345);
  any _t6347 = prim_cons(_t6331, _t6346);
 ((proc)((any*)(UNTAG(_t6330)))[0])(_t6330, _t6347, _halt);
  force_gc();
return 0;
}


