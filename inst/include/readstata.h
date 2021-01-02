/*
 * Copyright (C) 2015-2017 Jan Marvin Garbuszus and Sebastian Jeworutzki
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

#ifndef READSTATA_H
#define READSTATA_H

#include <Rcpp.h>
#include <fstream>
#include <string>
#include <iostream>
#include <sstream>

#define GCC_VERSION (__GNUC__ * 10000 \
+ __GNUC_MINOR__ * 100                \
+ __GNUC_PATCHLEVEL__)

/* Test for GCC < 4.9.0 */
#if GCC_VERSION < 40900 & !__clang__
typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef signed short int16_t;
typedef unsigned short uint16_t;
typedef signed int int32_t;
typedef unsigned int uint32_t;
#else
#include <stdint.h>
#endif

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__DragonFly__) || defined(__OpenBSD__) || defined(__APPLE__) || defined(__ANDROID__)
#  define fseeko64 fseeko
#endif

#include "read_dta.h"
#include "read_pre13_dta.h"

#include "statadefines.h"
#include "swap_endian.h"

template <typename T>
T readbin( T t , FILE * file, bool swapit)
{
  if (fread(&t, sizeof(t), 1, file) != 1) {
    if (feof(file))
      return 0; // this is expected after reading the labeltable
  } else if (ferror(file)){
    Rcpp::warning("num: a binary read error occurred.");
  }
  if (swapit==0)
    return(t);
  else
    return(swap_endian(t));
}

template <typename T>
T readuint48( T t , FILE * file, bool swapit)
{
  char uint48[6];
  if (fread(uint48, sizeof(uint48), 1, file) != 1) {
    if (feof(file))
      return 0; // this is expected after reading the labeltable
  } else if (ferror(file)){
    Rcpp::warning("num: a binary read error occurred.");
  }

  t = *(uint64_t *)&uint48;

  if (swapit==0)
    return(t);
  else
    return(swap_endian(t));
}

static void readstring(std::string &mystring, FILE * fp, int nchar)
{
  if (!fread(&mystring[0], nchar, 1, fp))
    Rcpp::warning("char: a binary read error occurred");
}

inline void test(std::string testme, FILE * file)
{
  std::string test(testme.size(), '\0');

  readstring(test,file, test.size());
  if (testme.compare(test)!=0)
  {
    fclose(file);
    Rcpp::warning("\n testme:%s \n test: %s\n", testme.c_str(), test.c_str());
    Rcpp::stop("When attempting to read %s: Something went wrong!", testme.c_str());
  }
}

template <typename T>
static void writebin(T t, std::fstream& dta, bool swapit)
{
  if (swapit==1){
    T t_s = swap_endian(t);
    dta.write((char*)&t_s, sizeof(t_s));
  } else {
    dta.write((char*)&t, sizeof(t));
  }
}



template <typename T>
static void writestr(std::string val_s, T len, std::fstream& dta)
{

  std::stringstream val_stream;
  val_stream << std::left << std::setw(len) << std::setfill('\0') << val_s;
  std::string val_strl = val_stream.str();

  dta.write(val_strl.c_str(),val_strl.length());

}

inline Rcpp::IntegerVector calc_rowlength(Rcpp::IntegerVector vartype) {

  uint32_t k = vartype.size();

  Rcpp::IntegerVector rlen(k);
  // calculate row length in byte
  for (uint32_t i=0; i<k; ++i)
  {
    int const type = vartype[i];
    switch(type)
    {
    case STATA_DOUBLE:
      rlen(i) = 8;
      break;
    case STATA_FLOAT:
    case STATA_INT:
      rlen(i) = 4;
      break;
    case STATA_SHORTINT:
      rlen(i) = 2;
      break;
    case STATA_BYTE:
      rlen(i) = 1;
      break;
    case STATA_STRL:
      rlen(i) = 8;
      break;
    default:
      rlen(i) = type;
    break;
    }
  }

  return(rlen);
}

// return only the matched positions. Either Rcpps in() can't handle Character-
// Vectors or I could not make it work. Wanted to select the selected varname
// position from the varnames vector.
inline Rcpp::IntegerVector choose(Rcpp::CharacterVector x,
                                  Rcpp::CharacterVector y)
{
  // ToDo: Maybe we can skip the select and nselect in read_dta.cpp if we match
  // the other way around and use Rcpp::is_na on the result which then could be
  // used as an additional index
  Rcpp::IntegerVector mm = Rcpp::match(x, y);

  if (Rcpp::any(Rcpp::is_na(mm))) {
    Rcpp::LogicalVector ll = !Rcpp::is_na(mm);

    Rcpp::CharacterVector ms = x[ll==0];

    // does not work if ms contains multiple names: Rcpp::as<std::string>(ms)
    Rcpp::Rcout << "Variable " << ms <<
      " was not found in dta-file." << std::endl;
  }

  // report position for found cases
  mm = Rcpp::match(y, x);

  return(mm);
}

// calculate the maximum jump. This calculates the maximum space we can skip if
// reading only a single variable. Before we skipped over each variable. Now we
// skip over them combined. Therefore if a value in x is positive push it
// into a new vector. If negative, sum the length up.
inline Rcpp::IntegerVector calc_jump(Rcpp::IntegerVector x) {

  Rcpp::IntegerVector y;
  int64_t val = 0;
  bool last = 0;

  uint32_t k = x.size();

  for (uint32_t i=0; i<k; ++i)
  {

    int32_t value = x(i);

    if (value < 0) {

      // after start or if last was pos fill to val
      if ( (i == 0) || (last == 1)) {
        val = value;
      } else {
        val += value;
      }
      last = 0;

    } else {

      // push back if last was neg
      if ((i > 0) & (last == 0))
        y.push_back(val);

      val = value;
      y.push_back(val);

      last = 1;
    }

    if ((i+1 == k) & (last == 0)) {
      y.push_back(val);
    }

  }

  return(y);
}

#endif
