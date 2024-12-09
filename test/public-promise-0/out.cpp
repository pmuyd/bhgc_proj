#include "../../prelude.h"


void pr2896(any,any,any);
void pr2902(any,any,any);
void pr2892(any,any,any);
void pr2900(any,any,any);
void pr2898(any,any,any);
void pr2894(any,any,any);

void pr2896(any _en2897, any _t6477, any __956495)
{
 any _t6474 = ((any*)UNTAG(_en2897))[1];  any _t6496 = prim__43(_t6474, _t6477);
  halt(_t6496);
}


void pr2902(any _en2903, any _p370744464462, any _k6478)
{
 any _cv370644454460 = ((any*)UNTAG(_en2903))[1]; any _x44434457 = ((any*)UNTAG(_en2903))[2];  any _t6418 = prim_null_63(_p370744464462);
 if (GETBOOL(_t6418))
 {
  any _t6419 = INT(0);
  any _t6421 = prim_vector_45ref(_cv370644454460, _t6419);
  any _t6422 = prim_not(_t6421);
  any* cl2904 = (any*)GC_MALLOC(3*sizeof(any));
  cl2904[0] = (any)(&pr2892);
  cl2904[1] = _cv370644454460;
  cl2904[2] = _k6478;
  any _c6479 = TAG((any)(cl2904), CLO_TAG);
 if (GETBOOL(_t6422))
 {
  any _t6423 = INT(0);
  any _t6425 = INT(0);
  any _t6427 = INT(0);
  any _t6429 = prim_vector_45ref(_x44434457, _t6427);
  any _t6430 = INT(1);
  any _t6432 = prim__43(_t6429, _t6430);
  any __95370944484464 = prim_vector_45set_33(_x44434457, _t6425, _t6432);
  any _t6433 = INT(0);
  any _t6435 = prim_vector_45ref(_x44434457, _t6433);
  any _t6436 = EMPTY;
  any _t6438 = prim_cons(_t6435, _t6436);
  any _t6482 = prim_vector_45set_33(_cv370644454460, _t6423, _t6438);
 ((proc)((any*)(UNTAG(_c6479)))[0])(_c6479, _t6482, _t6482);
 }
 else
 {
  any _t6483 = prim_void();
 ((proc)((any*)(UNTAG(_c6479)))[0])(_c6479, _t6483, _t6483);
 }
 }
 else
 {
  any _t6484 = prim_error();
 ((proc)((any*)(UNTAG(_k6478)))[0])(_k6478, _t6484, _t6484);
 }
}


void pr2892(any _en2893, any __95370844474463, any __956480)
{
 any _cv370644454460 = ((any*)UNTAG(_en2893))[1]; any _k6478 = ((any*)UNTAG(_en2893))[2];  any _t6439 = INT(0);
  any _t6441 = prim_vector_45ref(_cv370644454460, _t6439);
  any _t6481 = prim_car(_t6441);
 ((proc)((any*)(UNTAG(_k6478)))[0])(_k6478, _t6481, _t6481);
}


void pr2900(any _en2901, any _p371144514468, any _k6485)
{
 any _cv371044504466 = ((any*)UNTAG(_en2901))[1]; any _x44434457 = ((any*)UNTAG(_en2901))[2];  any _t6448 = prim_null_63(_p371144514468);
 if (GETBOOL(_t6448))
 {
  any _t6449 = INT(0);
  any _t6451 = prim_vector_45ref(_cv371044504466, _t6449);
  any _t6452 = prim_not(_t6451);
  any* cl2905 = (any*)GC_MALLOC(3*sizeof(any));
  cl2905[0] = (any)(&pr2894);
  cl2905[1] = _cv371044504466;
  cl2905[2] = _k6485;
  any _c6486 = TAG((any)(cl2905), CLO_TAG);
 if (GETBOOL(_t6452))
 {
  any _t6453 = INT(0);
  any _t6455 = INT(0);
  any _t6457 = INT(0);
  any _t6459 = prim_vector_45ref(_x44434457, _t6457);
  any _t6460 = INT(2);
  any _t6462 = prim__42(_t6459, _t6460);
  any __95371344534470 = prim_vector_45set_33(_x44434457, _t6455, _t6462);
  any _t6463 = INT(0);
  any _t6465 = prim_vector_45ref(_x44434457, _t6463);
  any _t6466 = EMPTY;
  any _t6468 = prim_cons(_t6465, _t6466);
  any _t6489 = prim_vector_45set_33(_cv371044504466, _t6453, _t6468);
 ((proc)((any*)(UNTAG(_c6486)))[0])(_c6486, _t6489, _t6489);
 }
 else
 {
  any _t6490 = prim_void();
 ((proc)((any*)(UNTAG(_c6486)))[0])(_c6486, _t6490, _t6490);
 }
 }
 else
 {
  any _t6491 = prim_error();
 ((proc)((any*)(UNTAG(_k6485)))[0])(_k6485, _t6491, _t6491);
 }
}


void pr2898(any _en2899, any _t6474, any __956493)
{
 any _y44444459 = ((any*)UNTAG(_en2899))[1];  any _t6475 = EMPTY;
  any* cl2906 = (any*)GC_MALLOC(2*sizeof(any));
  cl2906[0] = (any)(&pr2896);
  cl2906[1] = _t6474;
  any _c6494 = TAG((any)(cl2906), CLO_TAG);
 ((proc)((any*)(UNTAG(_y44444459)))[0])(_y44444459, _t6475, _c6494);
}


void pr2894(any _en2895, any __95371244524469, any __956487)
{
 any _cv371044504466 = ((any*)UNTAG(_en2895))[1]; any _k6485 = ((any*)UNTAG(_en2895))[2];  any _t6469 = INT(0);
  any _t6471 = prim_vector_45ref(_cv371044504466, _t6469);
  any _t6488 = prim_car(_t6471);
 ((proc)((any*)(UNTAG(_k6485)))[0])(_k6485, _t6488, _t6488);
}


int main()
{
  GC_INIT();
  GC_find_leak = 1;
  any _t6406 = INT(1);
  any _x44434457 = prim_make_45vector(_t6406);
  any _t6408 = INT(0);
  any _t6410 = INT(3);
  any __9544544458 = prim_vector_45set_33(_x44434457, _t6408, _t6410);
  any _t6412 = INT(1);
  any _cv370644454460 = prim_make_45vector(_t6412);
  any _t6414 = INT(0);
  any _t6416 = FALSE;
  any __9544554461 = prim_vector_45set_33(_cv370644454460, _t6414, _t6416);
  any* cl2907 = (any*)GC_MALLOC(3*sizeof(any));
  cl2907[0] = (any)(&pr2902);
  cl2907[1] = _cv370644454460;
  cl2907[2] = _x44434457;
  any _y44444459 = TAG((any)(cl2907), CLO_TAG);
  any _t6442 = INT(1);
  any _cv371044504466 = prim_make_45vector(_t6442);
  any _t6444 = INT(0);
  any _t6446 = FALSE;
  any __9544564467 = prim_vector_45set_33(_cv371044504466, _t6444, _t6446);
  any* cl2908 = (any*)GC_MALLOC(3*sizeof(any));
  cl2908[0] = (any)(&pr2900);
  cl2908[1] = _cv371044504466;
  cl2908[2] = _x44434457;
  any _z44494465 = TAG((any)(cl2908), CLO_TAG);
  any _t6472 = EMPTY;
  any* cl2909 = (any*)GC_MALLOC(2*sizeof(any));
  cl2909[0] = (any)(&pr2898);
  cl2909[1] = _y44444459;
  any _c6492 = TAG((any)(cl2909), CLO_TAG);
 ((proc)((any*)(UNTAG(_z44494465)))[0])(_z44494465, _t6472, _c6492);
  force_gc();
return 0;
}


