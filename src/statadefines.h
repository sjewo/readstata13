#ifndef STATADEFINES
#define STATADEFINES

/*Define missings*/
#define STATA_BYTE_NA_MIN -127
#define STATA_BYTE_NA_MAX +100
#define STATA_BYTE_NA +101
#define STATA_SHORTINT_NA_MIN -32767
#define STATA_SHORTINT_NA_MAX +32740
#define STATA_SHORTINT_NA +32741
#define STATA_INT_NA_MIN -2147483647
#define STATA_INT_NA_MAX +2147483620
#define STATA_INT_NA +2147483621
#define STATA_FLOAT_NA_MAX (1+15/pow(16,1)+15/pow(16,2)+15/pow(16,3)+15/pow(16,4)+15/pow(16,5)+14/pow(16,6))*pow(2,126)
#define STATA_FLOAT_NA_MIN -STATA_FLOAT_NA_MAX
#define STATA_FLOAT_NA pow(2,126)
#define STATA_DOUBLE_NA_MAX (1+15/pow(16,1)+15/pow(16,2)+15/pow(16,3)+15/pow(16,4)+15/pow(16,5)+15/pow(16,6)+15/pow(16,7)+15/pow(16,8)+15/pow(16,9)+15/pow(16,10)+15/pow(16,11)+15/pow(16,12)+15/pow(16,13))*pow(2,1022)
#define STATA_DOUBLE_NA_MIN -1*(1+15/pow(16,1)+15/pow(16,2)+15/pow(16,3)+15/pow(16,4)+15/pow(16,5)+15/pow(16,6)+15/pow(16,7)+15/pow(16,8)+15/pow(16,9)+15/pow(16,10)+15/pow(16,11)+15/pow(16,12)+15/pow(16,13))*pow(2,1023)
#define STATA_DOUBLE_NA pow(2,1023)

#endif
