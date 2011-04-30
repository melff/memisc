double dumb_dswap(double,int);
int dumb_iswap(int,int);



short dumb_sswap(short,int);
float dumb_fswap(float,int);

#define dswap_if(x,sw) x = dumb_dswap(x,sw)
#define iswap_if(x,sw) x = dumb_iswap(x,sw)
#define sswap_if(x,sw) x = dumb_sswap(x,sw)
#define fswap_if(x,sw) x = dumb_fswap(x,sw)

#define dswap(x,sw) dumb_dswap(x,sw)
#define iswap(x,sw) dumb_iswap(x,sw)
#define sswap(x,sw) dumb_sswap(x,sw)
#define fswap(x,sw) dumb_fswap(x,sw)
