#include "../../prelude.h"


void pr2892(any,any,any);

void pr2892(any _en2893, any _p366743794386, any _k6283)
{
  any _x43804387 = prim_car(_p366743794386);
  any _p366743814388 = prim_cdr(_p366743794386);
  any _t6268 = prim_null_63(_p366743814388);
 if (GETBOOL(_t6268))
 {
  any _t6269 = INT(1);
  any _t6271 = INT(2);
  any _t6273 = prim__42(_t6271, _x43804387);
  any _t6284 = prim__43(_t6269, _t6273);
 ((proc)((any*)(UNTAG(_k6283)))[0])(_k6283, _t6284, _t6284);
 }
 else
 {
  any _t6285 = prim_error();
 ((proc)((any*)(UNTAG(_k6283)))[0])(_k6283, _t6285, _t6285);
 }
}


int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any _t6260 = INT(1);
  any _foo43774383 = prim_make_45vector(_t6260);
  any _t6262 = INT(0);
  any _t6264 = EMPTY;
  any __9543824384 = prim_vector_45set_33(_foo43774383, _t6262, _t6264);
  any _t6266 = INT(0);
  any* cl2894 = (any*)GC_MALLOC(1*sizeof(any));
  cl2894[0] = (any)(&pr2892);
  any _t6274 = TAG((any)(cl2894), CLO_TAG);
  any __95366643784385 = prim_vector_45set_33(_foo43774383, _t6266, _t6274);
  any _t6275 = INT(0);
  any _t6277 = prim_vector_45ref(_foo43774383, _t6275);
  any _t6278 = INT(5);
  any _t6280 = EMPTY;
  any _t6282 = prim_cons(_t6278, _t6280);
 ((proc)((any*)(UNTAG(_t6277)))[0])(_t6277, _t6282, _halt);
  force_gc();
return 0;
}


