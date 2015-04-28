/*
 * Copyright (C) 2014-2015 Jan Marvin Garbuszus and Sebastian Jeworutzki
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

#include <Rcpp.h>
#include "string"
#include <stdint.h>
#include "statadefines.h"
#include "swap_endian.h"

using namespace Rcpp;
using namespace std;

/* Test for a little-endian machine */
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define lsf "LSF"
#else
#define lsf "MSF"
#endif

template <typename T>
T readbin( T t , FILE * file, bool swapit)
{
  if (fread(&t, sizeof(t), 1, file) != 1)
    Rcpp::warning("num: a binary read error occurred");
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

void test(std::string testme, FILE * file)
{
  std::string test(testme.size(), '\0');

  readstring(test,file, test.size());
  if (testme.compare(test)!=0)
  {
    Rcpp::stop("When attempting to read %s: Something went wrong!", testme.c_str());
  }
}

// Reads the binary Stata file
//
// @param filePath The full systempath to the dta file you want to import.
// @param missing logical if missings should be converted outside of Rcpp.
// @import Rcpp
// @export
// [[Rcpp::export]]
List stata(const char * filePath, const bool missing)
{
  FILE *file = NULL;    // File pointer

  /*
  * Open the file in binary mode using the "rb" format string
  * This also checks if the file exists and/or can be opened for reading correctly
  */

  if ((file = fopen(filePath, "rb")) == NULL)
    throw std::range_error("Could not open specified file.");

  /*
  * check the first byte. continue if "<"
  */

  std::string fbit(1, '\0');
  readstring(fbit, file, fbit.size());

  std::string expfbit = "<";

  if (fbit.compare(expfbit)!=0)
    Rcpp::stop("First byte: Not a version 13/14 dta-file.");

  fseek(file, 18, SEEK_CUR);// stata_dta><header>
  test("<release>", file);

  /*
  * version is a 4 byte character e.g. "117"
  */

  int8_t fversion = 117L; //f = first
  int8_t lversion = 118L; //l = last

  std::string version(3, '\0');
  readstring(version, file, version.size());

  int8_t const release = atoi(version.c_str());

  IntegerVector versionIV(1);
  versionIV(0) = release;

  // check the release version.
  if (release<fversion || release>lversion)
  {
    Rcpp::stop("File version is %d.\nVersion: Not a version 13/14 dta-file", release);
  }

  uint8_t nvarnameslen = 0;
  int8_t nformatslen = 0;
  uint8_t nvalLabelslen = 0;
  uint16_t nvarLabelslen = 0;
  int32_t chlen = 0;
  uint8_t lbllen = 0;

  switch(release)
  {
  case 117:
    nvarnameslen = 33;
    nformatslen = 49;
    nvalLabelslen = 33;
    nvarLabelslen = 81;
    chlen = 33;
    lbllen = 33;
    break;
  case 118:
    nvarnameslen = 129;
    nformatslen = 57;
    nvalLabelslen = 129;
    nvarLabelslen = 321;
    chlen = 129;
    lbllen = 129;
    break;
  }

  fseek(file, 10, SEEK_CUR); // </release>
  test("<byteorder>", file);

  /*
  * byteorder is a 4 byte character e.g. "LSF". MSF referes to big-memory data.
  */

  std::string byteorder(3, '\0');
  readstring(byteorder,file, byteorder.size());

  fseek(file, 12, SEEK_CUR); // </byteorder>
  test("<K>", file);

  bool swapit = 0;
  swapit = strcmp(byteorder.c_str(), lsf);

  /*
  * Number of Variables
  */

  uint16_t k = 0;
  k = readbin(k, file, swapit);

  fseek(file, 4, SEEK_CUR); //</K>
  test("<N>", file);

  /*
  * Number of Observations
  */

  int64_t n = 0;

  if(release==117) {
    n = readbin((int32_t)n, file, swapit);
  }
  if (release ==118) {
    n = readbin(n, file, swapit);
  }

  fseek(file, 4, SEEK_CUR); //</N>
  test("<label>", file);

  /*
  * A dataset may have a label e.g. "Written by R".
  * First we read its length (ndlabel), later the actual label (datalabel).
  * ndlabel:   length of datalabel (excl. binary 0)
  * datalabel: string max length 80
  */

  uint16_t ndlabel = 0;

  if (release==118)
    ndlabel = readbin(ndlabel, file, swapit);
  if (release==117)
    ndlabel = readbin((int8_t)ndlabel, file, swapit);

  std::string datalabel(ndlabel, '\0');

  if (ndlabel>0)
  {
    readstring(datalabel, file, datalabel.size());
  } else {
    datalabel = "";
  }

  CharacterVector datalabelCV(1);
  datalabelCV(0) = datalabel;

  fseek(file, 8, SEEK_CUR); //</label>
  test("<timestamp>", file);

  /*
  * A dataset may have a timestamp. If it has a timestamp the length of the
  * timestamp (ntimestamp) is 17. Else it is zero.
  * ntimestamp: 0 or 17
  * timestamp: empty or 17 byte string
  */

  uint8_t ntimestamp = 0;
  ntimestamp = readbin(ntimestamp, file, swapit);

  std::string timestamp(17, '\0');

  if (ntimestamp == 17) // ntimestap is 0 or 17
  {
    readstring(timestamp, file, timestamp.size());
  } else {
    timestamp = "";
  }

  CharacterVector timestampCV = timestamp;

  fseek(file, 21, SEEK_CUR); //</timestamp></header>
  test("<map>", file);

  /*
  * Stata stores the byteposition of certain areas of the file here. Currently
  * this is of no use to us.
  * 1.  <stata_data>
  * 2.  <map>
  * 3.  <variable_types>
  * 4.  <varnames>
  * 5.  <sortlist>
  * 6.  <formats>
  * 7.  <value_label_names>
  * 8.  <variable_labels>
  * 9.  <characteristics>
  * 10. <data>
  * 11. <strls>
  * 12. <value_labels>
  * 13. </stata_data>
  * 14. end-of-file
  */

  IntegerVector map(14);
  for (int i=0; i <14; ++i)
  {
    uint64_t nmap = 0;
    nmap = readbin(nmap, file, swapit);
    map[i] = nmap;
  }

  fseek(file, 6, SEEK_CUR); //</map>
  test("<variable_types>", file);

  /*
  * vartypes.
  * 0-2045: strf (String: Max length 2045)
  * 32768:  strL (long String: Max length 2 billion)
  * 65526:  double
  * 65527:  float
  * 65528:  long
  * 65529:  int
  * 65530:  byte
  */

  IntegerVector vartype(k);
  for (uint16_t i=0; i<k; ++i)
  {
    uint16_t nvartype = 0;
    nvartype = readbin(nvartype, file, swapit);
    vartype[i] = nvartype;
  }

  fseek(file, 17, SEEK_CUR); //</variable_types>
  test("<varnames>", file);

  /*
  * varnames.
  */

  std::string nvarnames(nvarnameslen, '\0');

  CharacterVector varnames(k);
  for (uint16_t i=0; i<k; ++i)
  {
    readstring(nvarnames, file, nvarnames.size());
    varnames[i] = nvarnames;
  }

  fseek(file, 11, SEEK_CUR); //</varnames>
  test("<sortlist>", file);

  /*
  * sortlist. Stata stores the information which variable of a dataset was
  * sorted. Depending on byteorder sortlist is written different. Currently we
  * do not use this information.
  * Vector size is k+1.
  */

  uint32_t big_k = k+1;

  IntegerVector sortlist(big_k);
  for (uint32_t i=0; i<big_k; ++i)
  {
    uint16_t nsortlist = 0;
    nsortlist = readbin(nsortlist, file, swapit);
    sortlist[i] = nsortlist;
  }

  fseek(file, 11, SEEK_CUR); //</sortlist>
  test("<formats>", file);

  /*
  * formats handle how Stata prints a variable. Currently we do not use this
  * information.
  */

  std::string nformats(nformatslen, '\0');

  CharacterVector formats(k);
  for (uint16_t i=0; i<k; ++i)
  {
    readstring(nformats, file, nformats.size());
    formats[i] = nformats;
  }

  fseek(file, 10, SEEK_CUR); //</formats>
  test("<value_label_names>",file);

  /*
  * value_label_names. Stata stores variable labels by names.
  * nvalLabels: length of the value_label_name
  * valLabels:
  */

  std::string nvalLabels(nvalLabelslen, '\0');

  CharacterVector valLabels(k);
  for (uint16_t i=0; i<k; ++i)
  {
    readstring(nvalLabels, file, nvalLabels.size());
    valLabels[i] = nvalLabels;
  }

  fseek(file, 20, SEEK_CUR); //</value_label_names>
  test("<variable_labels>", file);

  /*
  * variabel_labels
  */

  std::string nvarLabels (nvarLabelslen, '\0');

  CharacterVector varLabels(k);
  for (uint16_t i=0; i<k; ++i)
  {
    readstring(nvarLabels, file, nvarLabels.size());
    varLabels[i] = nvarLabels;
  }

  fseek(file, 18, SEEK_CUR); //</variable_labels>
  test("<characteristics>", file);

  /*
  * characteristics. Stata can store additional information this way. It may
  * contain notes (for the dataset or a variable) or about label language sets.
  * Characteristics are not documented. We export them as attribute:
  * expansion.fields. Characteristics are seperated by <ch> tags. Each <ch> has:
  * nocharacter:  length of the characteristics
  * chvarname:    varname (binary 0 terminated)
  * chcharact:    characteristicsname (binary 0 terminated)
  * nnocharacter: contes (binary 0 terminated)
  */

  std::string chtag = "<ch>";

  std::string tago(4, '\0');
  readstring(tago, file, tago.size());

  List ch = List();
  CharacterVector chs(3);

  while (chtag.compare(tago)==0)
  {
    uint32_t nocharacter = 0;
    nocharacter = readbin(nocharacter, file, swapit);

    std::string chvarname(chlen, '\0');
    std::string chcharact(chlen, '\0');
    std::string nnocharacter(nocharacter-chlen*2, '\0');

    readstring(chvarname, file, chvarname.size());
    readstring(chcharact, file, chcharact.size());
    readstring(nnocharacter, file, nnocharacter.size());

    // chs vector
    CharacterVector chs(3);
    chs[0] = chvarname;
    chs[1] = chcharact;
    chs[2] = nnocharacter;

    // add characteristics to the list
    ch.push_front( chs );

    //fseek(file, 5, SEEK_CUR); // </ch>
    test("</ch>", file);

    // read next tag
    readstring(tago, file, tago.size());
  }

  fseek(file, 14, SEEK_CUR); //[</ch]aracteristics>
  test("<data>", file);

  /*
  * data. First a list is created with vectors. The vector type is defined by
  * vartype. Stata stores data columnwise so we loop over it and store the
  * data in the list of the first step. Third variable- and row-names are
  * attatched and the list type is changed to data.frame.
  */

  // 1. create the list
  List df(k);
  for (uint16_t i=0; i<k; ++i)
  {
    int const type = vartype[i];
    switch(type)
    {
    case 65526:
    case 65527:
      SET_VECTOR_ELT(df, i, NumericVector(no_init(n)));
      break;

    case 65528:
    case 65529:
    case 65530:
      SET_VECTOR_ELT(df, i, IntegerVector(no_init(n)));
      break;

    default:
      SET_VECTOR_ELT(df, i, CharacterVector(no_init(n)));
    break;
    }
  }

  // 2. fill it with data
  for(uint32_t j=0; j<n; ++j)
  {
    for (uint16_t i=0; i<k; ++i)
    {
      int const type = vartype[i];
      switch(type < 2046 ? 2045 : type)
      {
        // double
      case 65526:
      {
        double val_d = 0;
        val_d = readbin(val_d, file, swapit);

        if ((missing == FALSE) & !(val_d == R_NegInf) & ((val_d<STATA_DOUBLE_NA_MIN) | (val_d>STATA_DOUBLE_NA_MAX)) )
          REAL(VECTOR_ELT(df,i))[j] = NA_REAL;
        else
          REAL(VECTOR_ELT(df,i))[j] = val_d;
        break;
      }
        // float
      case 65527:
      {
        float val_f = 0;
        val_f = readbin(val_f, file, swapit);

        if ((missing == FALSE) & ((val_f<STATA_FLOAT_NA_MIN) | (val_f>STATA_FLOAT_NA_MAX)) )
          REAL(VECTOR_ELT(df,i))[j] = NA_REAL;
        else
          REAL(VECTOR_ELT(df,i))[j] = val_f;
        break;
      }
        //long
      case 65528:
      {
        int32_t val_l = 0;
        val_l = readbin(val_l, file, swapit);

        if ((missing == FALSE) & ((val_l<STATA_INT_NA_MIN) | (val_l>STATA_INT_NA_MAX)) )
          INTEGER(VECTOR_ELT(df,i))[j]  = NA_INTEGER;
        else
          INTEGER(VECTOR_ELT(df,i))[j] = val_l;
        break;
      }
        // int
      case 65529:
      {
        int16_t val_i = 0;
        val_i = readbin(val_i, file, swapit);

        if ((missing == FALSE) & ((val_i<STATA_SHORTINT_NA_MIN) | (val_i>STATA_SHORTINT_NA_MAX)) )
          INTEGER(VECTOR_ELT(df,i))[j] = NA_INTEGER;
        else
          INTEGER(VECTOR_ELT(df,i))[j] = val_i;
        break;
      }
        // byte
      case 65530:
      {
        int8_t val_b = 0;
        val_b = readbin(val_b, file, swapit);

        if ((missing == FALSE) & ( (val_b<STATA_BYTE_NA_MIN) | (val_b>STATA_BYTE_NA_MAX)) )
          INTEGER(VECTOR_ELT(df,i))[j] = NA_INTEGER;
        else
          INTEGER(VECTOR_ELT(df,i))[j] = val_b;
        break;
      }
        // strings with 2045 or fewer characters
      case 2045:
      {
        int32_t len = 0;
        len = vartype[i];
        std::string val_s (len, '\0');

        readstring(val_s, file, val_s.size());
        as<CharacterVector>(df[i])[j] = val_s;
        break;
      }
        // string of any length
      case 32768:
      {// strL 2 4bit

        // FixMe: Strl in 118
        // if (release==117)
        int32_t v = 0, o = 0;
        //         if (release==118) {
        //           int32_t v=0;
        //           int64_t o=0;
        //         }
        v = readbin(v, file, swapit);
        o = readbin(o, file, swapit);

        // if(release==117)
        char val_strl[22];
        //         if(release==118)
        //           char val_strl[22];
        sprintf(val_strl, "%010d%010d", v, o);
        as<CharacterVector>(df[i])[j] = val_strl;
        break;
      }
      }
    }
  }

  // 3. Create a data.frame
  R_xlen_t nrows = Rf_length(df[0]);
  df.attr("row.names") = IntegerVector::create(NA_INTEGER, nrows);
  df.attr("names") = varnames;
  df.attr("class") = "data.frame";

  fseek(file, 7, SEEK_CUR); //</data>
  test("<strls>", file);

  /*
  * strL. Stata 13 introduced long strings up to 2 billon characters. strLs are
  * sperated by "GSO".
  * (v,o): Position in the data.frame.
  * t:     129/130 defines whether or not the strL is stored with a binary 0.
  * len:   length of the strL.
  * strl:  long string.
  */

  std::string gso = "GSO";

  std::string tags(3, '\0');
  readstring(tags, file, tags.size());

  List strlstable = List(); //put strLs into this list

  while(gso.compare(tags)==0)
  {
    CharacterVector strls(2);

    // 2x4 bit (strl[vo1,vo2])
    int32_t v = 0, o = 0;
    v = readbin(v, file, swapit);
    o = readbin(o, file, swapit);
    char erg[22];
    sprintf(erg, "%010d%010d", v, o);

    strls(0) = erg;

    // (129 = binary) | (130 = ascii)
    uint8_t t = 0;
    t = readbin(t, file, swapit);

    uint32_t len = 0;
    len = readbin(len, file, swapit);

    // 129 len = len; 130 len = len +'\0';

    std::string strl(len, '\0');
    readstring(strl, file, strl.size());

    strls(1) = strl;

    strlstable.push_back( strls );

    readstring(tags, file, tags.size());
  }

  // after strls
  fseek(file, 5, SEEK_CUR); //[</s]trls>
  test("<value_labels>", file);

  /*
  * labels are seperated by <lbl>-tags. Labels may appear in any order e.g.
  * 2 "female" 1 "male 9 "missing". They are stored as tables.
  * nlen:     length of label.
  * nlabname: label name.
  * labn:     number of labels in this set (e.g. "male" "female" = 2)
  * txtlen:   length of the label text.
  * off:      offset defines where to read a new label in txtlen.
  */

  std::string lbltag = "<lbl>";

  std::string tag(5, '\0');
  readstring(tag, file, tag.size());

  List labelList = List(); //put labels into this list

  while(lbltag.compare(tag)==0)
  {
    int32_t nlen = 0, labn = 0, txtlen = 0, noff = 0, val = 0;

    // length of value_label_table
    nlen = readbin(nlen, file, swapit);

    // name of this label set

    std::string nlabname(lbllen, '\0');

    readstring(nlabname, file, nlabname.size());

    //padding
    fseek(file, 3, SEEK_CUR);

    // value_label_table for actual label set
    labn = readbin(labn, file, swapit);
    txtlen = readbin(txtlen, file, swapit);

    // offset for each label
    // off0 : label 0 starts at off0
    // off1 : label 1 starts at off1 ...
    IntegerVector off(labn);
    for (int i=0; i < labn; ++i) {
      noff = readbin(noff, file, swapit);
      off[i] = noff;
    }

    // needed for match
    IntegerVector laborder = clone(off);
    //laborder.erase(labn+1);
    IntegerVector labordersort = clone(off);
    //labordersort.erase(labn+1);
    std::sort(labordersort.begin(), labordersort.end());

    // needs txtlen for loop
    off.push_back(txtlen);

    // sort offsets so we can read labels sequentially
    std::sort(off.begin(), off.end());

    // create an index to sort lables along the code values
    // this is done while factor creation
    IntegerVector indx(labn);
    indx = match(laborder,labordersort);

    // code for each label
    IntegerVector code(labn);
    for (int i=0; i < labn; ++i) {
      val = readbin(val, file, swapit);
      code[i] = val;
    }

    // label text
    CharacterVector label(labn);
    for (int i=0; i < labn; ++i) {
      int lablen = off[i+1]-off[i];

      std::string lab (lablen, '\0');

      readstring(lab, file, lablen);
      label[i] = lab;
    }

    // sort labels according to indx
    CharacterVector labelo(labn);
    for (int i=0; i < labn; ++i) {
      labelo[i] = label[indx[i]-1];
    }
    // create table for actual label set
    string const labset = nlabname;
    code.attr("names") = labelo;

    // add this set to output list
    labelList.push_front( code, labset);

    fseek(file, 6, SEEK_CUR); //</lbl>

    readstring(tag, file, tag.size());
  }

  /*
   * Final test if we reached the end of the file
   * close the file
   */

  fseek(file, 10, SEEK_CUR); // [</val]ue_labels>
  test("</stata_dta>", file);

  fclose(file);

  /*
   * assign attributes to the resulting data.frame
   */

  df.attr("datalabel") = datalabelCV;
  df.attr("time.stamp") = timestampCV;
  df.attr("formats") = formats;
  df.attr("types") = vartype;
  df.attr("val.labels") = valLabels;
  df.attr("var.labels") = varLabels;
  df.attr("version") = versionIV;
  df.attr("label.table") = labelList;
  df.attr("expansion.fields") = ch;
  df.attr("strl") = strlstable;
  df.attr("byteorder") = wrap(byteorder);

  return df;
}
