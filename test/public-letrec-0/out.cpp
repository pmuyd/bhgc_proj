#include "../../prelude.h"


void pr2894(any,any,any);
void pr2892(any,any,any);

void pr2894(any _en2895, any _p370344204436, any _k6402)
{
 any _odd_6344184433 = ((any*)UNTAG(_en2895))[1];  any _n44214437 = prim_car(_p370344204436);
  any _p370344224438 = prim_cdr(_p370344204436);
  any _t6362 = prim_null_63(_p370344224438);
 if (GETBOOL(_t6362))
 {
  any _t6363 = INT(0);
  any _t6365 = prim__61(_t6363, _n44214437);
 if (GETBOOL(_t6365))
 {
  any _t6366 = TRUE;
 ((proc)((any*)(UNTAG(_k6402)))[0])(_k6402, _t6366, _t6366);
 }
 else
 {
  any _t6367 = INT(0);
  any _t6369 = prim_vector_45ref(_odd_6344184433, _t6367);
  any _t6370 = INT(1);
  any _t6372 = prim__45(_n44214437, _t6370);
  any _t6373 = EMPTY;
  any _t6375 = prim_cons(_t6372, _t6373);
 ((proc)((any*)(UNTAG(_t6369)))[0])(_t6369, _t6375, _k6402);
 }
 }
 else
 {
  any _t6403 = prim_error();
 ((proc)((any*)(UNTAG(_k6402)))[0])(_k6402, _t6403, _t6403);
 }
}


void pr2892(any _en2893, any _p370544244440, any _k6404)
{
 any _even_6344174431 = ((any*)UNTAG(_en2893))[1];  any _n44254441 = prim_car(_p370544244440);
  any _p370544264442 = prim_cdr(_p370544244440);
  any _t6379 = prim_null_63(_p370544264442);
 if (GETBOOL(_t6379))
 {
  any _t6380 = INT(0);
  any _t6382 = prim__61(_t6380, _n44254441);
 if (GETBOOL(_t6382))
 {
  any _t6383 = FALSE;
 ((proc)((any*)(UNTAG(_k6404)))[0])(_k6404, _t6383, _t6383);
 }
 else
 {
  any _t6384 = INT(0);
  any _t6386 = prim_vector_45ref(_even_6344174431, _t6384);
  any _t6387 = INT(1);
  any _t6389 = prim__45(_n44254441, _t6387);
  any _t6390 = EMPTY;
  any _t6392 = prim_cons(_t6389, _t6390);
 ((proc)((any*)(UNTAG(_t6386)))[0])(_t6386, _t6392, _k6404);
 }
 }
 else
 {
  any _t6405 = prim_error();
 ((proc)((any*)(UNTAG(_k6404)))[0])(_k6404, _t6405, _t6405);
 }
}


int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any _t6350 = EMPTY;
  any _t6351 = EMPTY;
  any _t6352 = INT(1);
  any _even_6344174431 = prim_make_45vector(_t6352);
  any _t6354 = INT(0);
  any __9544274432 = prim_vector_45set_33(_even_6344174431, _t6354, _t6350);
  any _t6356 = INT(1);
  any _odd_6344184433 = prim_make_45vector(_t6356);
  any _t6358 = INT(0);
  any __9544284434 = prim_vector_45set_33(_odd_6344184433, _t6358, _t6351);
  any _t6360 = INT(0);
  any* cl2896 = (any*)GC_MALLOC(2*sizeof(any));
  cl2896[0] = (any)(&pr2894);
  cl2896[1] = _odd_6344184433;
  any _t6376 = TAG((any)(cl2896), CLO_TAG);
  any __95370244194435 = prim_vector_45set_33(_even_6344174431, _t6360, _t6376);
  any _t6377 = INT(0);
  any* cl2897 = (any*)GC_MALLOC(2*sizeof(any));
  cl2897[0] = (any)(&pr2892);
  cl2897[1] = _even_6344174431;
  any _t6393 = TAG((any)(cl2897), CLO_TAG);
  any __95370444234439 = prim_vector_45set_33(_odd_6344184433, _t6377, _t6393);
  any _t6394 = INT(0);
  any _t6396 = prim_vector_45ref(_even_6344174431, _t6394);
  any _t6397 = INT(88);
  any _t6399 = EMPTY;
  any _t6401 = prim_cons(_t6397, _t6399);
 ((proc)((any*)(UNTAG(_t6396)))[0])(_t6396, _t6401, _halt);
  force_gc();
return 0;
}


