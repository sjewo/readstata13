/*
 * Copyright (C) 2014 Jan Marvin Garbuszus and Sebastian Jeworutzki
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
    perror("a binary read error occurred");
  if (swapit==0)
    return(t);
  else
    return(swap_endian(t));
}

static void readstr(char *var, FILE * fp, int nchar)
{
  nchar = nchar-1;
  if (!fread(var, nchar, 1, fp))
    perror("a binary read error occurred");
  var[nchar] = '\0';
}

void test(std::string testme, FILE * file)
{
  const char *testMe = testme.c_str();
  char test[1+testme.size()];
  readstr(test,file, sizeof(test));
  if (strcmp(testMe,test)!=0)
  {
    printf("When attempting to read %s:", testme.c_str());

    throw std::range_error("Something went wrong!");
  }
}

//' Reads the binary Stata file
//'
//' @param filePath The full systempath to the dta file you want to import.
//' @param missing logical if missings should be converted outside of Rcpp.
//' @import Rcpp
//' @export
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

  char one[2];
  readstr(one, file, sizeof(one));

  char two[2] = "<";
  two[1] = '\0';

  if (strcmp(one,two)!=0)
    throw std::range_error("First byte: Not a version 13 dta-file.");

  fseek(file, 18, SEEK_CUR);// stata_dta><header>
  test("<release>", file);

  /*
  * release is a 4 byte character e.g. "117"
  */

  char gversion[4] = "117";
  gversion[3] = '\0';

  char release [4];
  readstr(release, file, sizeof(release));

  string const relver(release);

  // check the release version. continue if "117"
  if (strcmp(release, gversion)!=0)
    throw std::range_error("Version: Not a version 13 dta-file.");

  fseek(file, 10, SEEK_CUR); // </release>
  test("<byteorder>", file);

  /*
  * byteorder is a 4 byte character e.g. "LSF". MSF referes to big-memory data.
  */

  char byteorder [4];
  readstr(byteorder,file, sizeof(byteorder));

  fseek(file, 12, SEEK_CUR); // </byteorder>
  test("<K>", file);

  bool swapit = 1;
//  swapit = strcmp(byteorder, lsf);

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

  uint32_t n = 0;
  n = readbin(n, file, swapit);

  fseek(file, 4, SEEK_CUR); //</N>
  test("<label>", file);

  /*
  * A dataset may have a label e.g. "Written by R".
  * First we read its length (ndlabel), later the actual label (datalabel).
  * ndlabel:   length of datalabel (excl. binary 0)
  * datalabel: string max length 80
  */

  uint8_t ndlabel = 0;
  ndlabel = readbin(ndlabel, file, swapit);

  char datalabel [ndlabel];
  if (ndlabel>0)
  {
    readstr(datalabel, file, ndlabel+1);
  } else {
    datalabel[0] = '\0';
  };

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

  char timestamp [ntimestamp];
  if (ntimestamp == 17) // ntimestap is 0 or 17
  {
    readstr(timestamp, file, ntimestamp+1);
  } else {
    timestamp[0] = '\0';
  };

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
  for (unsigned int i=0; i<k; ++i)
  {
    uint16_t nvartype = 0;
    nvartype = readbin(nvartype, file, swapit);
    vartype[i] = nvartype;
  }

  fseek(file, 17, SEEK_CUR); //</variable_types>
  test("<varnames>", file);

  /*
  * varnames. Max length 33.
  */

  CharacterVector varnames(k);
  for (unsigned int i=0; i<k; ++i)
  {
    char nvarnames [33];
    readstr(nvarnames, file, sizeof(nvarnames)+1);
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

  IntegerVector sortlist(k+1);
  for (uint16_t i=0; i<k+1; ++i)
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

  CharacterVector formats(k);
  for (unsigned int i=0; i<k; ++i)
  {
    char nformats[49];
    readstr(nformats, file, sizeof(nformats)+1);
    formats[i] = nformats;
  }

  fseek(file, 10, SEEK_CUR); //</formats>
  test("<value_label_names>",file);

  /*
  * value_label_names. Stata stores variable labels by names.
  * nvalLabels: length of the value_label_name
  * valLabels:  Char of max length 33
  */

  CharacterVector valLabels(k);
  for (unsigned int i=0; i<k; ++i)
  {
    char nvalLabels[33];
    readstr(nvalLabels, file, sizeof(nvalLabels)+1);
    valLabels[i] = nvalLabels;
  }

  fseek(file, 20, SEEK_CUR); //</value_label_names>
  test("<variable_labels>", file);

  /*
  * variabel_labels
  */

  CharacterVector varLabels(k);
  for (unsigned int i=0; i<k; ++i)
  {
    char nvarLabels[81];
    readstr(nvarLabels, file, sizeof(nvarLabels)+1);
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

  char chtag[5] = "<ch>";
  chtag[4] = '\0';

  List ch = List();
  CharacterVector chs(3);

  char tago[5];
  readstr(tago, file, sizeof(tago));

  while (strcmp(tago,chtag)==0)
  {
    uint32_t nocharacter = 0;
    nocharacter = readbin(nocharacter, file, swapit);

    char chvarname[33];
    char chcharact[33];
    char nnocharacter[nocharacter-66];

    readstr(chvarname, file, sizeof(chvarname)+1);
    readstr(chcharact, file, sizeof(chcharact)+1);
    readstr(nnocharacter, file, sizeof(nnocharacter)+1);

    // chs vector
    CharacterVector chs(3);
    chs[0] = chvarname;
    chs[1] = chcharact;
    chs[2] = nnocharacter;

    // add characteristics to the list
    ch.push_front( chs );

    fseek(file, 5, SEEK_CUR); // </ch>

    // read next tag
    readstr(tago, file, sizeof(tago));
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
  for (unsigned int i=0; i<k; ++i)
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
        double erg = 0;
        erg = readbin(erg, file, swapit);
        double const dmin = -0x1.fffffffffffffp1023;
        double const dmax = 0x1.fffffffffffffp1022;

        if ((missing == FALSE) & ((erg<dmin) | (erg>dmax)) )
          REAL(VECTOR_ELT(df,i))[j] = NA_REAL;
        else
          REAL(VECTOR_ELT(df,i))[j] = erg;
        break;
      }
        // float
      case 65527:
      {
        float erg = 0;
        erg = readbin(erg, file, swapit);
        float const minmax = 0x1.fffffp126;

        if ((missing == FALSE) & ((erg<(-minmax)) | (erg>minmax)) )
          REAL(VECTOR_ELT(df,i))[j] = NA_REAL;
        else
          REAL(VECTOR_ELT(df,i))[j] = erg;
        break;
      }
        //long
      case 65528:
      {
        int32_t erg = 0;
        erg = readbin(erg, file, swapit);

        if ((missing == FALSE) & ((erg<(-2147483647)) | (erg>2147483620)) )
          INTEGER(VECTOR_ELT(df,i))[j]  = NA_INTEGER;
        else
          INTEGER(VECTOR_ELT(df,i))[j] = erg;
        break;
      }
        // int
      case 65529:
      {
        int16_t erg = 0;
        erg = readbin(erg, file, swapit);

        if ((missing == FALSE) & ((erg<(-32767)) | (erg>32740)) )
          INTEGER(VECTOR_ELT(df,i))[j] = NA_INTEGER;
        else
          INTEGER(VECTOR_ELT(df,i))[j] = erg;
        break;
      }
        // byte
      case 65530:
      {
        int8_t erg = 0;
        erg = readbin(erg, file, swapit);

        if ((missing == FALSE) & ( (erg<(-127)) | (erg>100)) )
          INTEGER(VECTOR_ELT(df,i))[j] = NA_INTEGER;
        else
          INTEGER(VECTOR_ELT(df,i))[j] = erg;
        break;
      }
        // strings with 2045 or fewer characters
      case 2045:
      {
        int32_t gre = 0;
        gre = vartype[i];

        char erg[gre];
        readstr(erg, file, sizeof(erg)+1);
        as<CharacterVector>(df[i])[j] = erg;
        break;
      }
        // string of any length
      case 32768:
      {// strL 2 4bit
        int32_t v = 0, o = 0;
        v = readbin(v, file, swapit);
        o = readbin(o, file, swapit);

        char erg[22];
        sprintf(erg, "%010d%010d", v, o);
        as<CharacterVector>(df[i])[j] = erg;
        break;
      }
      }
    }
  }

  // 3. Create a data.frame
  IntegerVector row_names = no_init(n);
  for (int32_t i = 0; i < row_names.length(); ++i) {
    row_names[i] = i+1;
  }
  df.attr("row.names") = row_names;
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

  List strlstable = List(); //put strLs into this list

  char tags[4];
  readstr(tags, file, sizeof(tags));

  char gso[4] = "GSO";
  gso[3] = '\0';

  while(strcmp(gso,tags)==0)
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

    if (t==129)
    {
      char strl [len];
      readstr(strl, file, sizeof(strl));
      strls(1) = strl;
    } else
    {
      if (t==130)
      {
        char strl [len+1];
        readstr(strl, file, sizeof(strl));
        strls(1) = strl;
      }
    }

    strlstable.push_back( strls );

    readstr(tags, file, sizeof(tags));
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

  List labelList = List(); //put labels into this list
  char tag[6];
  readstr(tag, file, sizeof(tag));

  char lbltag[6] = "<lbl>";
  lbltag[5] = '\0';

  while(strcmp(lbltag,tag)==0)
  {
    int32_t nlen = 0, labn = 0, txtlen = 0, noff = 0, val = 0;

    // length of value_label_table
    nlen = readbin(nlen, file, swapit);

    // name of this label set
    char nlabname[33];
    readstr(nlabname, file, sizeof(nlabname)+1);

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
      int const lablen = off[i+1]-off[i];
      char lab[lablen];
      readstr(lab, file, sizeof(lab)+1);
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

    readstr(tag, file, sizeof(tag));
  }

  /*
  * Final test if we reached the end of the file
  * close the file
  */

  fseek(file, 10, SEEK_CUR); // [</val]ue_labels>
  test("</stata_dta>", file);

  fclose(file);

  /*
   * define R character vectors for meta data
   */

  CharacterVector datalabelCV(1);
  datalabelCV[0] = datalabel;

  CharacterVector timestampCV(1);
  timestampCV[0] = timestamp;

  CharacterVector version(1);
  version[0] = relver;

  /*
   * assign attributes to the resulting data.frame
   */

  df.attr("datalabel") = datalabelCV;
  df.attr("time.stamp") = timestampCV;
  df.attr("formats") = formats;
  df.attr("types") = vartype;
  df.attr("val.labels") = valLabels;
  df.attr("var.labels") = varLabels;
  df.attr("version") = version;
  df.attr("label.table") = labelList;
  df.attr("expansion.fields") = ch;
  df.attr("strl") = strlstable;
  df.attr("byteorder") = byteorder;

  return df;
}
