#include "../../prelude.h"



int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any _t6197 = prim_void();
  any _t6198 = prim_void_63(_t6197);
 if (GETBOOL(_t6198))
 {
  any _t6199 = INT(1);
  any _y43534365 = prim_make_45vector(_t6199);
  any _t6201 = INT(0);
  any _t6203 = EMPTY;
  any __9543614366 = prim_vector_45set_33(_y43534365, _t6201, _t6203);
  any _t6205 = INT(1);
  any _x43544367 = prim_make_45vector(_t6205);
  any _t6207 = INT(0);
  any _t6209 = EMPTY;
  any __9543624368 = prim_vector_45set_33(_x43544367, _t6207, _t6209);
  any _t6211 = INT(0);
  any _t6213 = INT(7);
  any __95363243554369 = prim_vector_45set_33(_x43544367, _t6211, _t6213);
  any _t6215 = INT(0);
  any _t6217 = INT(0);
  any _t6219 = prim_vector_45ref(_x43544367, _t6217);
  any __95363343564370 = prim_vector_45set_33(_y43534365, _t6215, _t6219);
  any _t6220 = INT(0);
  any _t6245 = prim_vector_45ref(_y43534365, _t6220);
  halt(_t6245);
 }
 else
 {
  any _t6222 = INT(1);
  any _y43574371 = prim_make_45vector(_t6222);
  any _t6224 = INT(0);
  any _t6226 = EMPTY;
  any __9543634372 = prim_vector_45set_33(_y43574371, _t6224, _t6226);
  any _t6228 = INT(1);
  any _x43584373 = prim_make_45vector(_t6228);
  any _t6230 = INT(0);
  any _t6232 = EMPTY;
  any __9543644374 = prim_vector_45set_33(_x43584373, _t6230, _t6232);
  any _t6234 = INT(0);
  any _t6236 = INT(9);
  any __95363443594375 = prim_vector_45set_33(_x43584373, _t6234, _t6236);
  any _t6238 = INT(0);
  any _t6240 = INT(0);
  any _t6242 = prim_vector_45ref(_x43584373, _t6240);
  any __95363543604376 = prim_vector_45set_33(_y43574371, _t6238, _t6242);
  any _t6243 = INT(0);
  any _t6246 = prim_vector_45ref(_y43574371, _t6243);
  halt(_t6246);
 }
  force_gc();
return 0;
}


