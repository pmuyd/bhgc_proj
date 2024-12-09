#include "../../prelude.h"


void pr2894(any,any,any);
void pr2892(any,any,any);

void pr2894(any _en2895, any _a43434345, any _k6147)
{
  any _t6128 = prim_car(_a43434345);
  any _t6129 = prim_cdr(_a43434345);
 ((proc)((any*)(UNTAG(_t6128)))[0])(_t6128, _t6129, _k6147);
}


void pr2892(any _en2893, any _args43444346, any _k6148)
{
  any _t6149 = applyprim__42(_args43444346);
 ((proc)((any*)(UNTAG(_k6148)))[0])(_k6148, _t6149, _t6149);
}


int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any* cl2896 = (any*)GC_MALLOC(1*sizeof(any));
  cl2896[0] = (any)(&pr2894);
  any _t6130 = TAG((any)(cl2896), CLO_TAG);
  any* cl2897 = (any*)GC_MALLOC(1*sizeof(any));
  cl2897[0] = (any)(&pr2892);
  any _t6131 = TAG((any)(cl2897), CLO_TAG);
  any _t6132 = INT(3);
  any _t6134 = INT(4);
  any _t6136 = INT(5);
  any _t6138 = INT(6);
  any _t6140 = EMPTY;
  any _t6142 = prim_cons(_t6138, _t6140);
  any _t6143 = prim_cons(_t6136, _t6142);
  any _t6144 = prim_cons(_t6134, _t6143);
  any _t6145 = prim_cons(_t6132, _t6144);
  any _t6146 = prim_cons(_t6131, _t6145);
 ((proc)((any*)(UNTAG(_t6130)))[0])(_t6130, _t6146, _halt);
  force_gc();
return 0;
}


