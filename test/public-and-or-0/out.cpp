#include "../../prelude.h"


void pr2892(any,any,any);
void pr2894(any,any,any);

void pr2892(any _en2893, any _t5945, any __955955)
{
 if (GETBOOL(_t5945))
 {
  any _t5946 = TRUE;
 if (GETBOOL(_t5946))
 {
  any _t5948 = INT(87);
  halt(_t5948);
 }
 else
 {
  any _t5949 = FALSE;
  halt(_t5949);
 }
 }
 else
 {
  any _t5950 = FALSE;
  halt(_t5950);
 }
}


void pr2894(any _en2895, any _t5941, any __955953)
{
 if (GETBOOL(_t5941))
 {
  any _t5942 = FALSE;
  any* cl2896 = (any*)GC_MALLOC(1*sizeof(any));
  cl2896[0] = (any)(&pr2892);
  any _c5954 = TAG((any)(cl2896), CLO_TAG);
 if (GETBOOL(_t5942))
 {
 ((proc)((any*)(UNTAG(_c5954)))[0])(_c5954, _t5942, _t5942);
 }
 else
 {
  any _t5943 = FALSE;
 if (GETBOOL(_t5943))
 {
 ((proc)((any*)(UNTAG(_c5954)))[0])(_c5954, _t5943, _t5943);
 }
 else
 {
  any _t5944 = TRUE;
 ((proc)((any*)(UNTAG(_c5954)))[0])(_c5954, _t5944, _t5944);
 }
 }
 }
 else
 {
  any _t5951 = FALSE;
  halt(_t5951);
 }
}


int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any _t5938 = TRUE;
  any* cl2897 = (any*)GC_MALLOC(1*sizeof(any));
  cl2897[0] = (any)(&pr2894);
  any _c5952 = TAG((any)(cl2897), CLO_TAG);
 if (GETBOOL(_t5938))
 {
 ((proc)((any*)(UNTAG(_c5952)))[0])(_c5952, _t5938, _t5938);
 }
 else
 {
  any _t5939 = FALSE;
 if (GETBOOL(_t5939))
 {
 ((proc)((any*)(UNTAG(_c5952)))[0])(_c5952, _t5939, _t5939);
 }
 else
 {
  any _t5940 = TRUE;
 ((proc)((any*)(UNTAG(_c5952)))[0])(_c5952, _t5940, _t5940);
 }
 }
  force_gc();
return 0;
}


