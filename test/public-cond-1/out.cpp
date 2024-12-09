#include "../../prelude.h"


void pr2892(any,any,any);

void pr2892(any _en2893, any __95363143484352, any __956193)
{
 any _x43474350 = ((any*)UNTAG(_en2893))[1];  any _t6183 = INT(0);
  any _t6185 = prim_vector_45ref(_x43474350, _t6183);
  any _t6186 = INT(5);
  any _t6188 = prim__60(_t6185, _t6186);
 if (GETBOOL(_t6188))
 {
  any _t6189 = INT(1);
  halt(_t6189);
 }
 else
 {
  any _t6190 = INT(0);
  any _t6194 = prim_vector_45ref(_x43474350, _t6190);
  halt(_t6194);
 }
}


int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any _t6163 = INT(1);
  any _x43474350 = prim_make_45vector(_t6163);
  any _t6165 = INT(0);
  any _t6167 = INT(2);
  any __9543494351 = prim_vector_45set_33(_x43474350, _t6165, _t6167);
  any _t6169 = INT(0);
  any _t6171 = prim_vector_45ref(_x43474350, _t6169);
  any _t6172 = INT(2);
  any _t6174 = prim__61(_t6171, _t6172);
  any* cl2894 = (any*)GC_MALLOC(2*sizeof(any));
  cl2894[0] = (any)(&pr2892);
  cl2894[1] = _x43474350;
  any _c6192 = TAG((any)(cl2894), CLO_TAG);
 if (GETBOOL(_t6174))
 {
  any _t6175 = INT(0);
  any _t6177 = INT(0);
  any _t6179 = prim_vector_45ref(_x43474350, _t6177);
  any _t6180 = INT(3);
  any _t6182 = prim__42(_t6179, _t6180);
  any _t6195 = prim_vector_45set_33(_x43474350, _t6175, _t6182);
 ((proc)((any*)(UNTAG(_c6192)))[0])(_c6192, _t6195, _t6195);
 }
 else
 {
  any _t6196 = prim_void();
 ((proc)((any*)(UNTAG(_c6192)))[0])(_c6192, _t6196, _t6196);
 }
  force_gc();
return 0;
}


