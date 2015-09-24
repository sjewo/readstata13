/*
 * Copyright (C) 2015 Jan Marvin Garbuszus and Sebastian Jeworutzki
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
#include "read_dta.h"
#include "read_pre13_dta.h"

#include "string"
#include <stdint.h>
#include <fstream>
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

#endif
