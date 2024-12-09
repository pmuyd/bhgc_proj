#include "../../prelude.h"


void pr2894(any,any,any);
void pr2892(any,any,any);

void pr2894(any _en2895, any _p362943284338, any _k6124)
{
  any _x43294339 = prim_car(_p362943284338);
  any _p362943304340 = prim_cdr(_p362943284338);
  any _y43314341 = prim_car(_p362943304340);
  any _p362943324342 = prim_cdr(_p362943304340);
  any _t6125 = prim_cons(_x43294339, _p362943324342);
 ((proc)((any*)(UNTAG(_k6124)))[0])(_k6124, _t6125, _t6125);
}


void pr2892(any _en2893, any _t6123, any __956127)
{
 any _halt = ((any*)UNTAG(_en2893))[1]; any _t6105 = ((any*)UNTAG(_en2893))[2]; ((proc)((any*)(UNTAG(_t6105)))[0])(_t6105, _t6123, _halt);
}


int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any _t6094 = INT(1);
  any _foo43264335 = prim_make_45vector(_t6094);
  any _t6096 = INT(0);
  any _t6098 = EMPTY;
  any __9543344336 = prim_vector_45set_33(_foo43264335, _t6096, _t6098);
  any _t6100 = INT(0);
  any* cl2896 = (any*)GC_MALLOC(1*sizeof(any));
  cl2896[0] = (any)(&pr2894);
  any _t6102 = TAG((any)(cl2896), CLO_TAG);
  any __95362843274337 = prim_vector_45set_33(_foo43264335, _t6100, _t6102);
  any _t6103 = INT(0);
  any _t6105 = prim_vector_45ref(_foo43264335, _t6103);
  any _t6106 = INT(0);
  any _t6108 = prim_vector_45ref(_foo43264335, _t6106);
  any _t6109 = INT(1);
  any _t6111 = INT(2);
  any _t6113 = INT(3);
  any _t6115 = INT(4);
  any _t6117 = EMPTY;
  any _t6119 = prim_cons(_t6115, _t6117);
  any _t6120 = prim_cons(_t6113, _t6119);
  any _t6121 = prim_cons(_t6111, _t6120);
  any _t6122 = prim_cons(_t6109, _t6121);
  any* cl2897 = (any*)GC_MALLOC(3*sizeof(any));
  cl2897[0] = (any)(&pr2892);
  cl2897[1] = _halt;
  cl2897[2] = _t6105;
  any _c6126 = TAG((any)(cl2897), CLO_TAG);
 ((proc)((any*)(UNTAG(_t6108)))[0])(_t6108, _t6122, _c6126);
  force_gc();
return 0;
}


