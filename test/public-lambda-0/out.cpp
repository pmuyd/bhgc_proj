#include "../../prelude.h"


void pr2892(any,any,any);

void pr2892(any _en2893, any _p369644074410, any _k6327)
{
  any _x44084411 = prim_car(_p369644074410);
  any _p369644094412 = prim_cdr(_p369644074410);
  any _t6320 = prim_null_63(_p369644094412);
 if (GETBOOL(_t6320))
 {
 ((proc)((any*)(UNTAG(_k6327)))[0])(_k6327, _x44084411, _x44084411);
 }
 else
 {
  any _t6328 = prim_error();
 ((proc)((any*)(UNTAG(_k6327)))[0])(_k6327, _t6328, _t6328);
 }
}


int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any* cl2894 = (any*)GC_MALLOC(1*sizeof(any));
  cl2894[0] = (any)(&pr2892);
  any _t6321 = TAG((any)(cl2894), CLO_TAG);
  any _t6322 = INT(9);
  any _t6324 = EMPTY;
  any _t6326 = prim_cons(_t6322, _t6324);
 ((proc)((any*)(UNTAG(_t6321)))[0])(_t6321, _t6326, _halt);
  force_gc();
return 0;
}


