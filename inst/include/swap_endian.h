#ifndef SWAP_ENDIAN
#define SWAP_ENDIAN

/*#include <stdint.h>*/
#include <typeinfo>

#define GCC_VERSION (__GNUC__ * 10000 \
+ __GNUC_MINOR__ * 100                \
+ __GNUC_PATCHLEVEL__)

/* Test for GCC < 4.8.0 */
#if GCC_VERSION < 40800 & !__clang__
static inline unsigned short __builtin_bswap16(unsigned short a)
{
  return (a<<8)|(a>>8);
}
#endif

template <typename T>
T swap_endian(T t) {
  if (typeid(T) == typeid(int16_t))
    return __builtin_bswap16(t);
  if (typeid(T) == typeid(uint16_t))
    return __builtin_bswap16(t);

  if (typeid(T)  == typeid(int32_t))
    return __builtin_bswap32(t);
  if (typeid(T)  == typeid(uint32_t))
    return __builtin_bswap32(t);

  if (typeid(T)  == typeid(int64_t))
    return __builtin_bswap64(t);
  if (typeid(T)  == typeid(uint64_t))
    return __builtin_bswap64(t);

  union v {
    double      d;
    float       f;
    uint32_t    i32;
    uint64_t    i64;
  } val;

  if (typeid(T) == typeid(float)){
    val.f = t;
    val.i32 = __builtin_bswap32(val.i32);
    return val.f;
  }

  if (typeid(T) == typeid(double)){
    val.d = t;
    val.i64 = __builtin_bswap64(val.i64);
    return val.d;
  }

  else
    return t;
}

#endif
