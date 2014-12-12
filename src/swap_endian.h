#ifndef SWAP_ENDIAN
#define SWAP_ENDIAN

#include <stdint.h>

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

  if (typeid(T) == typeid(float)){
    union v {
      float       f;
      uint32_t    i;
    } val;

    val.f = t;
    val.i = __builtin_bswap32(val.i);
    return val.f;
  }

  if (typeid(T) == typeid(double)){
    union v {
      double       d;
      uint64_t    i;
    } val;

    val.d = t;
    val.i = __builtin_bswap64(val.i);
    return val.d;
  }
  else
    return t;
};

#endif
