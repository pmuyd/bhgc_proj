
#include <iostream>

typedef void* any;
typedef void (*proc)(any, any, any);
typedef unsigned long long u64;
typedef signed s32;
typedef unsigned u32;

#define INT_TAG 1
#define ENUM_TAG 2
#define VEC_TAG 5
#define CONS_TAG 6
#define CLO_TAG  7

#define ENUM(n) (TAG((u64)(n) << 3,ENUM_TAG))

#define TRUE ENUM(1)
#define FALSE ENUM(2)
#define EMPTY ENUM(3) 
#define VOID ENUM(4) 

#define INT(n) ((any)TAG(((u64)(u32)(s32)(n) << 32), INT_TAG))
#define GETBOOL(n) (((u64)(n)==(u64)FALSE)?false:true)
#define GETINT(n) ((s32)((u32)((u64)(n)>>32)))
#define TAG(a, t) ((any)(((u64)(a)) | (t)))
#define GETTAG(a) (((u64)a) & 7)
#define UNTAG(a) ((any)(((u64)(a)&0xfffffffffffffff8)))

any prim_car(any);
any prim_cdr(any);
void write(any x)
{
  switch (GETTAG(x))
  {
  case INT_TAG: // integers
    std::cout << GETINT(x);
    return;
  case ENUM_TAG:
    if ((u64)x == (u64)TRUE) // true
      std::cout << "#t";
    else if ((u64)x == (u64)FALSE) // false
      std::cout << "#f";
    else if ((u64)x == (u64)EMPTY) // '()
      std::cout << "()";
    else if ((u64)x == (u64)VOID) // #<void>
      std::cout << "#<void>";
      // unfortunately, racket does not appear to (read) this
    return;
  case CONS_TAG: // cons pair
    // basic and nonstandard writing but works with (read)
    std::cout << "(";
    write(prim_car(x));
    std::cout << " . ";
    write(prim_cdr(x));
    std::cout << ")";
    return;
  case CLO_TAG: // closure
    std::cout << "#<procedure>";
    return;
  }
}

void halt(any x)
{
  write(x);
  std::cout << std::endl;
  // simply makes no tail call, so finally returns and exits
}

void __halt(any en, any v, any _)
{
  halt(v);
}
any _haltclo[1] = {(any)&__halt};
any _halt = TAG((any)&_haltclo,CLO_TAG);

any prim_void()
{
  return VOID;
}

any prim_not(any x)
{
  return (u64)x==(u64)FALSE ? TRUE : FALSE; 
}

any prim_void_63(any x)
{
  return ((u64)x==(u64)VOID) ? TRUE : FALSE;
}

any prim_error()
{
  std::cout << "error" << std::endl;
  exit(1);
  return 0;
}

any prim_cons(any x, any y)
{
  any* pa = (any*)malloc(2*sizeof(any));
  pa[0] = x;
  pa[1] = y;
  return TAG((any)pa, 6);
}

any prim_car(any x)
{
  if (GETTAG(x) != CONS_TAG) prim_error();
  return ((any*)UNTAG(x))[0];
}

any prim_cdr(any x)
{
  if (GETTAG(x) != CONS_TAG) prim_error();
  return ((any*)UNTAG(x))[1];
}

any prim_null_63(any x)
{
  return ((u64)x==(u64)EMPTY) ? TRUE : FALSE;
}

any prim_make_45vector(any i, any v)
{
  int ii = GETINT(i);
  any* b = (any*)malloc((1+ii)*sizeof(any));
  for (int n = 1; n <= ii; ++n)
    b[n] = v;
  b[0] = TAG(ii, INT_TAG);
  return TAG((any)b, VEC_TAG); 
}

any prim_make_45vector(any i)
{
  return prim_make_45vector(i,INT(0)); 
}

any prim_vector_45ref(any x, any i)
{
  if (GETTAG(x) != VEC_TAG) prim_error();
  return ((any*)UNTAG(x))[GETINT(i)+1]; 
}

any prim_vector_45set_33(any x, any i, any v)
{
  if (GETTAG(x) != VEC_TAG) prim_error();
  int ii = GETINT(i);
  any* b = (any*)UNTAG(x);
  if ((u64)b[0] < ii || ii < 0) prim_error();
  b[ii+1] = v;
  return VOID;
}

any prim__42(any x, any y)
{
  return INT(GETINT(x) * GETINT(y));
}

any prim__42(any x, any y, any z)
{
  return INT(GETINT(x) * GETINT(y) * GETINT(z));
}

any applyprim__42(any lst)
{
  any product = INT(1);
  while (!GETBOOL(prim_null_63(lst)))
  {
    if (GETTAG(lst) != CONS_TAG) prim_error();
    product = prim__42(product, prim_car(lst));
    lst = prim_cdr(lst);
  }
  return product;
}

any prim__43(any x, any y)
{
  // specialized for two arguments
  return INT(GETINT(x) + GETINT(y));
}

any prim__43(any x, any y, any z)
{
  // specialized for three arguments
  return INT(GETINT(x) + GETINT(y) + GETINT(z));
}

any applyprim__43(any lst)
{
  // handles variadic calls: (apply + lst)
  any sum = INT(0);
  while (!GETBOOL(prim_null_63(lst)))
  {
    if (GETTAG(lst) != CONS_TAG) prim_error();
    sum = prim__43(sum, prim_car(lst));
    lst = prim_cdr(lst);
  }
  return sum;
}

any prim__45(any x, any y)
{
  return INT(GETINT(x) - GETINT(y));
}

any prim__47(any x, any y)
{
  return INT(GETINT(x) / GETINT(y));
}

any prim_modulo(any x, any y)
{
  return INT(GETINT(x) % GETINT(y));
}

any prim__61(any x, any y)
{
  if (GETINT(x) == GETINT(y))
    return TRUE;
  else return FALSE;
}

any prim__60(any x, any y)
{
  if (GETINT(x) < GETINT(y))
    return TRUE;
  else return FALSE;
}

any prim__60_61(any x, any y)
{
  if (GETINT(x) <= GETINT(y))
    return TRUE;
  else return FALSE;
}

any prim__62(any x, any y)
{
  if (GETINT(x) > GETINT(y))
    return TRUE;
  else return FALSE;
}


