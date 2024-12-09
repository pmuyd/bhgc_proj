#include "../../prelude.h"


void pr2892(any,any,any);
void pr2894(any,any,any);

void pr2892(any _en2893, any _p373844774490, any _k6563)
{
 any _loop44754487 = ((any*)UNTAG(_en2893))[1];  any _lst44784491 = prim_car(_p373844774490);
  any _p373844794492 = prim_cdr(_p373844774490);
  any _r44804493 = prim_car(_p373844794492);
  any _p373844814494 = prim_cdr(_p373844794492);
  any _t6520 = prim_null_63(_p373844814494);
 if (GETBOOL(_t6520))
 {
  any _t6521 = prim_null_63(_lst44784491);
 if (GETBOOL(_t6521))
 {
 ((proc)((any*)(UNTAG(_k6563)))[0])(_k6563, _r44804493, _r44804493);
 }
 else
 {
  any _t6522 = INT(0);
  any _t6524 = prim_vector_45ref(_loop44754487, _t6522);
  any _t6525 = prim_cdr(_lst44784491);
  any _t6526 = prim_car(_lst44784491);
  any _t6527 = prim_cons(_t6526, _r44804493);
  any _t6528 = EMPTY;
  any _t6530 = prim_cons(_t6527, _t6528);
  any _t6531 = prim_cons(_t6525, _t6530);
 ((proc)((any*)(UNTAG(_t6524)))[0])(_t6524, _t6531, _k6563);
 }
 }
 else
 {
  any _t6564 = prim_error();
 ((proc)((any*)(UNTAG(_k6563)))[0])(_k6563, _t6564, _t6564);
 }
}


void pr2894(any _en2895, any _p373644724484, any _k6562)
{
  any _lst44734485 = prim_car(_p373644724484);
  any _p373644744486 = prim_cdr(_p373644724484);
  any _t6511 = prim_null_63(_p373644744486);
 if (GETBOOL(_t6511))
 {
  any _t6512 = INT(1);
  any _loop44754487 = prim_make_45vector(_t6512);
  any _t6514 = INT(0);
  any _t6516 = EMPTY;
  any __9544824488 = prim_vector_45set_33(_loop44754487, _t6514, _t6516);
  any _t6518 = INT(0);
  any* cl2896 = (any*)GC_MALLOC(2*sizeof(any));
  cl2896[0] = (any)(&pr2892);
  cl2896[1] = _loop44754487;
  any _t6532 = TAG((any)(cl2896), CLO_TAG);
  any __95373744764489 = prim_vector_45set_33(_loop44754487, _t6518, _t6532);
  any _t6533 = INT(0);
  any _t6535 = prim_vector_45ref(_loop44754487, _t6533);
  any _t6536 = EMPTY;
  any _t6538 = EMPTY;
  any _t6540 = prim_cons(_t6536, _t6538);
  any _t6541 = prim_cons(_lst44734485, _t6540);
 ((proc)((any*)(UNTAG(_t6535)))[0])(_t6535, _t6541, _k6562);
 }
 else
 {
  any _t6565 = prim_error();
 ((proc)((any*)(UNTAG(_k6562)))[0])(_k6562, _t6565, _t6565);
 }
}


int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any* cl2897 = (any*)GC_MALLOC(1*sizeof(any));
  cl2897[0] = (any)(&pr2894);
  any _reverse44714483 = TAG((any)(cl2897), CLO_TAG);
  any _t6542 = INT(1);
  any _t6544 = INT(2);
  any _t6546 = INT(3);
  any _t6548 = INT(4);
  any _t6550 = INT(5);
  any _t6552 = EMPTY;
  any _t6554 = prim_cons(_t6550, _t6552);
  any _t6555 = prim_cons(_t6548, _t6554);
  any _t6556 = prim_cons(_t6546, _t6555);
  any _t6557 = prim_cons(_t6544, _t6556);
  any _t6558 = prim_cons(_t6542, _t6557);
  any _t6559 = EMPTY;
  any _t6561 = prim_cons(_t6558, _t6559);
 ((proc)((any*)(UNTAG(_reverse44714483)))[0])(_reverse44714483, _t6561, _halt);
  force_gc();
return 0;
}


