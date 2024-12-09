#include "../../prelude.h"


void pr2896(any,any,any);
void pr2892(any,any,any);
void pr2894(any,any,any);

void pr2896(any _en2897, any _t6005, any __956041)
{
 any _x43144320 = ((any*)UNTAG(_en2897))[1]; if (GETBOOL(_t6005))
 {
  any _t6006 = FALSE;
  any* cl2898 = (any*)GC_MALLOC(2*sizeof(any));
  cl2898[0] = (any)(&pr2894);
  cl2898[1] = _x43144320;
  any _c6042 = TAG((any)(cl2898), CLO_TAG);
 if (GETBOOL(_t6006))
 {
  any _t6008 = INT(0);
  any _t6010 = INT(3);
  any _t6012 = INT(0);
  any _t6014 = prim_vector_45ref(_x43144320, _t6012);
  any _t6015 = prim__42(_t6010, _t6014);
  any _t6048 = prim_vector_45set_33(_x43144320, _t6008, _t6015);
 ((proc)((any*)(UNTAG(_c6042)))[0])(_c6042, _t6048, _t6048);
 }
 else
 {
  any _t6016 = FALSE;
 ((proc)((any*)(UNTAG(_c6042)))[0])(_c6042, _t6016, _t6016);
 }
 }
 else
 {
  any _t6038 = FALSE;
  halt(_t6038);
 }
}


void pr2892(any _en2893, any _t6034, any __956045)
{
 any _x43144320 = ((any*)UNTAG(_en2893))[1]; if (GETBOOL(_t6034))
 {
  any _t6035 = INT(0);
  any _t6046 = prim_vector_45ref(_x43144320, _t6035);
  halt(_t6046);
 }
 else
 {
  any _t6037 = FALSE;
  halt(_t6037);
 }
}


void pr2894(any _en2895, any _or362543164323, any __956043)
{
 any _x43144320 = ((any*)UNTAG(_en2895))[1];  any* cl2899 = (any*)GC_MALLOC(2*sizeof(any));
  cl2899[0] = (any)(&pr2892);
  cl2899[1] = _x43144320;
  any _c6044 = TAG((any)(cl2899), CLO_TAG);
 if (GETBOOL(_or362543164323))
 {
 ((proc)((any*)(UNTAG(_c6044)))[0])(_c6044, _or362543164323, _or362543164323);
 }
 else
 {
  any _t6017 = INT(0);
  any _t6019 = INT(0);
  any _t6021 = prim_vector_45ref(_x43144320, _t6019);
  any _t6022 = INT(5);
  any _t6024 = prim__42(_t6021, _t6022);
  any __95362743184325 = prim_vector_45set_33(_x43144320, _t6017, _t6024);
  any _t6025 = TRUE;
 if (GETBOOL(_t6025))
 {
 ((proc)((any*)(UNTAG(_c6044)))[0])(_c6044, _t6025, _t6025);
 }
 else
 {
  any _t6026 = INT(0);
  any _t6028 = INT(0);
  any _t6030 = prim_vector_45ref(_x43144320, _t6028);
  any _t6031 = INT(7);
  any _t6033 = prim__43(_t6030, _t6031);
  any _t6047 = prim_vector_45set_33(_x43144320, _t6026, _t6033);
 ((proc)((any*)(UNTAG(_c6044)))[0])(_c6044, _t6047, _t6047);
 }
 }
}


int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any _t5974 = INT(1);
  any _x43144320 = prim_make_45vector(_t5974);
  any _t5976 = INT(0);
  any _t5978 = INT(42);
  any __9543194321 = prim_vector_45set_33(_x43144320, _t5976, _t5978);
  any _t5980 = INT(0);
  any _t5982 = INT(0);
  any _t5984 = prim_vector_45ref(_x43144320, _t5982);
  any _t5985 = INT(1);
  any _t5987 = prim__43(_t5984, _t5985);
  any _t5988 = prim_vector_45set_33(_x43144320, _t5980, _t5987);
 if (GETBOOL(_t5988))
 {
  any _t5989 = INT(0);
  any _t5991 = INT(0);
  any _t5993 = prim_vector_45ref(_x43144320, _t5991);
  any _t5994 = INT(3);
  any _t5996 = prim__43(_t5993, _t5994);
  any _or362443154322 = prim_vector_45set_33(_x43144320, _t5989, _t5996);
  any* cl2900 = (any*)GC_MALLOC(2*sizeof(any));
  cl2900[0] = (any)(&pr2896);
  cl2900[1] = _x43144320;
  any _c6040 = TAG((any)(cl2900), CLO_TAG);
 if (GETBOOL(_or362443154322))
 {
 ((proc)((any*)(UNTAG(_c6040)))[0])(_c6040, _or362443154322, _or362443154322);
 }
 else
 {
  any _t5997 = INT(0);
  any _t5999 = INT(0);
  any _t6001 = prim_vector_45ref(_x43144320, _t5999);
  any _t6002 = INT(17);
  any _t6004 = prim__43(_t6001, _t6002);
  any _t6049 = prim_vector_45set_33(_x43144320, _t5997, _t6004);
 ((proc)((any*)(UNTAG(_c6040)))[0])(_c6040, _t6049, _t6049);
 }
 }
 else
 {
  any _t6039 = FALSE;
  halt(_t6039);
 }
  force_gc();
return 0;
}


