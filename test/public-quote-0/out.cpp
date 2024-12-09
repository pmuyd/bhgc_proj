#include "../../prelude.h"



int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any _t6497 = INT(1);
  any _t6499 = INT(2);
  any _t6501 = INT(3);
  any _t6503 = INT(4);
  any _t6505 = EMPTY;
  any _t6507 = prim_cons(_t6503, _t6505);
  any _t6508 = prim_cons(_t6501, _t6507);
  any _t6509 = prim_cons(_t6499, _t6508);
  any _t6510 = prim_cons(_t6497, _t6509);
  halt(_t6510);
  force_gc();
return 0;
}


