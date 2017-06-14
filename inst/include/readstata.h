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
  Rcpp::IntegerVector mm = Rcpp::match(x, y);

  if (Rcpp::any(Rcpp::is_na(mm))) {
    Rcpp::LogicalVector ll = !Rcpp::is_na(mm);

    Rcpp::CharacterVector ms = x[ll==0];

    Rcpp::Rcout << "Variable " <<  ms <<
      " was not found in dta-file." << std::endl;

    mm = mm[ll==1];
  }

  return(mm);
}

// return only the positions of variables, we have selected.
inline Rcpp::IntegerVector which_pos(Rcpp::IntegerVector cvec,
                                     Rcpp::IntegerVector select)
{
  // integer position of not selected variables
  // This drops all the positions we do not need. Initially I wanted something
  // like cvec[select], but that somehow did not work, possibly this could be
  // improved.
  std::vector<int> vec = Rcpp::as< std::vector<int> >(cvec);
  for (uint32_t i=0; i<select.size(); ++i) {
    vec.erase(std::remove(vec.begin(), vec.end(), select(i)), vec.end());
  }
  Rcpp::IntegerVector nselect = Rcpp::wrap(vec);

  // return to C-index
  nselect = nselect -1;

  return(nselect);
}

// calculate the maximum jump. This calculates the maximum space we can skip if
// reading only a single variable. Before we skipped over each variable. Now we
// skip over them combined. Therefore if a value in vartype3 is positive push it
// into a new vector. If negative, sum the length up.
inline Rcpp::IntegerVector calc_jump(Rcpp::IntegerVector vartype3) {

  Rcpp::IntegerVector vartype4;
  int64_t val = 0;
  bool last = 0;

  uint32_t k = vartype3.size();

  for (uint32_t i=0; i<k; ++i)
  {

    int32_t value = vartype3(i);

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
        vartype4.push_back(val);

      val = value;
      vartype4.push_back(val);

      last = 1;
    }

    if ((i+1 == k) & (last == 0)) {
      vartype4.push_back(val);
    }

  }

  return(vartype4);
}

#endif
