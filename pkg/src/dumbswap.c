/* not clever, but comprehensible, I let the compiler do the optimization */

double dumb_dswap(double x,int swap_code){
  if(!swap_code) return x;
  union {
    double d;
    char bytes[8];
  } from;
  union {
    double d;
    char bytes[8];
  } to;
  from.d = x;
  to.bytes[0] = from.bytes[7];
  to.bytes[1] = from.bytes[6];
  to.bytes[2] = from.bytes[5];
  to.bytes[3] = from.bytes[4];
  to.bytes[4] = from.bytes[3];
  to.bytes[5] = from.bytes[2];
  to.bytes[6] = from.bytes[1];
  to.bytes[7] = from.bytes[0];
  return to.d;
}

int dumb_iswap(int x,int swap_code){
  if(!swap_code) return x;
  union {
    int i;
    char bytes[4];
  } from;
  union {
    int i;
    char bytes[4];
  } to;
  from.i = x;
  to.bytes[0] = from.bytes[3];
  to.bytes[1] = from.bytes[2];
  to.bytes[2] = from.bytes[1];
  to.bytes[3] = from.bytes[0];
  return to.i;
}

short dumb_sswap(short x, int swap_code){
  if(!swap_code) return x;
  union {
    short s;
    char bytes[2];
  } from;
  union {
    short s;
    char bytes[2];
  } to;
  from.s = x;
  to.bytes[0] = from.bytes[1];
  to.bytes[1] = from.bytes[0];
  return to.s;
}

float dumb_fswap(float x, int swap_code){
  if(!swap_code) return x;
  union {
    float f;
    char bytes[2];
  } from;
  union {
    float f;
    char bytes[2];
  } to;
  from.f = x;
  to.bytes[0] = from.bytes[3];
  to.bytes[1] = from.bytes[2];
  to.bytes[2] = from.bytes[1];
  to.bytes[3] = from.bytes[0];
  return to.f;
}
