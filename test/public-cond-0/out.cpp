#include "../../prelude.h"



int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any _t6150 = INT(1);
  any _t6152 = INT(0);
  any _t6154 = prim__60(_t6150, _t6152);
 if (GETBOOL(_t6154))
 {
  any _t6155 = INT(5);
  halt(_t6155);
 }
 else
 {
  any _t6156 = INT(1);
  any _t6158 = INT(0);
  any _t6160 = prim__62(_t6156, _t6158);
 if (GETBOOL(_t6160))
 {
  any _t6161 = INT(6);
  halt(_t6161);
 }
 else
 {
  any _t6162 = prim_void();
  halt(_t6162);
 }
 }
  force_gc();
return 0;
}


