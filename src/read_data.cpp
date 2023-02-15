/*
 * Copyright (C) 2014-2018 Jan Marvin Garbuszus and Sebastian Jeworutzki
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include "readstata.h"

using namespace Rcpp;
using namespace std;

List read_data(FILE * file,
               const IntegerVector vartype_kk,
               const bool missing, const int8_t release,
               const uint64_t nn, uint32_t kk,
               const IntegerVector vartype_sj,
               const std::string byteorder, const bool swapit) {

  // 1. create the list
  List df(kk);
  for (uint32_t i=0; i<kk; ++i)
  {
    int const type = vartype_kk[i];

    switch(type)
    {
    case STATA_DOUBLE:
    case STATA_FLOAT:
      SET_VECTOR_ELT(df, i, NumericVector(no_init(nn)));
      break;

    case STATA_INT:
    case STATA_SHORTINT:
    case STATA_BYTE:
      SET_VECTOR_ELT(df, i, IntegerVector(no_init(nn)));
      break;

    default:
      SET_VECTOR_ELT(df, i, CharacterVector(no_init(nn)));
    break;
    }
  }

  // updated kk to reflect the jump size
  kk = vartype_sj.size();

  uint32_t ii = 0;
  for (uint64_t j=0; j<nn; ++j)
  {
    // reset partial index
    ii = 0;
    for (uint32_t i=0; i<kk; ++i)
    {
      int const type = vartype_sj[i];

      switch(((type >0) & (type < 2046)) ? STATA_STR : type)
      {
        // double
      case STATA_DOUBLE:
      {
        double val_d = 0;
        val_d = readbin(val_d, file, swapit);

        if ((missing == 0) && !(val_d == R_NegInf) && ((val_d<STATA_DOUBLE_NA_MIN) || (val_d>STATA_DOUBLE_NA_MAX)) )
          REAL(VECTOR_ELT(df,ii))[j] = NA_REAL;
        else
          REAL(VECTOR_ELT(df,ii))[j] = val_d;

        break;
      }
        // float
      case STATA_FLOAT:
      {
        float val_f = 0;
        val_f = readbin(val_f, file, swapit);

        if ((missing == 0) && ((val_f<STATA_FLOAT_NA_MIN) || (val_f>STATA_FLOAT_NA_MAX)) )
          REAL(VECTOR_ELT(df,ii))[j] = NA_REAL;
        else
          REAL(VECTOR_ELT(df,ii))[j] = val_f;

        break;
      }
        // long
      case STATA_INT:
      {
        int32_t val_l = 0;
        val_l = readbin(val_l, file, swapit);

        if ((missing == 0) && ((val_l<STATA_INT_NA_MIN) || (val_l>STATA_INT_NA_MAX)) )
          INTEGER(VECTOR_ELT(df,ii))[j] = NA_INTEGER;
        else
          INTEGER(VECTOR_ELT(df,ii))[j] = val_l;

        break;
      }
        // int
      case STATA_SHORTINT:
      {
        int16_t val_i = 0;
        val_i = readbin(val_i, file, swapit);

        if ((missing == 0) && ((val_i<STATA_SHORTINT_NA_MIN) || (val_i>STATA_SHORTINT_NA_MAX)) )
          INTEGER(VECTOR_ELT(df,ii))[j] = NA_INTEGER;
        else
          INTEGER(VECTOR_ELT(df,ii))[j] = val_i;

        break;
      }
        // byte
      case STATA_BYTE:
      {
        int8_t val_b = 0;
        val_b = readbin(val_b, file, swapit);

        if (missing == 0 && ( (val_b<STATA_BYTE_NA_MIN) || (val_b>STATA_BYTE_NA_MAX)) )
          INTEGER(VECTOR_ELT(df,ii))[j] = NA_INTEGER;
        else
          INTEGER(VECTOR_ELT(df,ii))[j] = val_b;

        break;
      }
        // strings with 2045 or fewer characters
      case STATA_STR:
      {
        int32_t len = 0;
        len = vartype_sj[i];
        std::string val_s (len, '\0');

        readstring(val_s, file, val_s.size());
        as<CharacterVector>(df[ii])[j] = val_s;
        break;
      }
        // string of any length
      case STATA_STRL:
      {// strL 2*4bit or 2 + 6 bit

        // FixMe: Strl in 118
        switch (release)
      {

      case 117:
      {
        uint32_t v = 0, o = 0;

        v = readbin(v, file, swapit);
        o = readbin(o, file, swapit);

        stringstream val_stream;
        val_stream << v << '_' << o;
        string val_strl = val_stream.str();

        as<CharacterVector>(df[ii])[j] = val_strl;

        break;
      }
      case 118:
      {
        int16_t v = 0;
        int64_t o = 0, z = 0;

        z = readbin(z, file, swapit);

        // works for LSF on little- and big-endian
        if (byteorder.compare("LSF")==0) {
          v = (int16_t)z;
          o = (z >> 16);
        }

        // works if we read a big-endian file on little-endian
        if (byteorder.compare("MSF")==0) {
          v = (z >> 48) & ((1 << 16) - 1);
          o = z & ((1 << 16) - 1);
        }

        stringstream val_stream;
        val_stream << v << '_' << o;
        string val_strl = val_stream.str();

        as<CharacterVector>(df[ii])[j] = val_strl;

        break;
      }
      case 119:
      {
        int32_t v = 0;
        int64_t o = 0, z = 0;

        z = readbin(z, file, swapit);

        // works for LSF on little- and big-endian
        if (byteorder.compare("LSF")==0) {
          v = (int32_t)z & ((1 << 24) - 1);
          o = (z >> 24);
        }

        // FixMe: works if we read a big-endian file on little-endian
        if (byteorder.compare("MSF")==0) {
          v = (z >> 40) & ((1 << 24) - 1);
          o = z & ((1 << 24) - 1);
        }

        stringstream val_stream;
        val_stream << v << '_' << o;
        string val_strl = val_stream.str();

        as<CharacterVector>(df[ii])[j] = val_strl;

        break;
      }
      }
        break;
      }
        // case < 0:
      default:
      {
        // skip to the next valid case
        fseeko64(file, abs(type), SEEK_CUR);
        break;
      }
      }

      if (type >= 0) ii += 1;

      checkUserInterrupt();
    }
  }

  return(df);
}
