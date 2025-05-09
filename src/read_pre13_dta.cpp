/*
 * Copyright (C) 2014-2025 Jan Marvin Garbuszus and Sebastian Jeworutzki
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
#include "read_data.h"

using namespace Rcpp;
using namespace std;

List read_pre13_dta(FILE * file,
                    const bool missing,
                    const IntegerVector selectrows,
                    const CharacterVector selectcols_chr,
                    const IntegerVector selectcols_int)
{
  int8_t release = 0;

  rewind(file);
  release = readbin(release, file, 0);

  if (release<102 || release == 109 || release>115)
    stop("First byte: Not a dta-file we can read.");

  IntegerVector versionIV(1);
  versionIV(0) = release;

  /*
  * byteorder is a 4 byte character e.g. "LSF". MSF referes to big-endian.
  */

  uint16_t ndlabel = 81;
  uint8_t nvarnameslen = 33;
  int8_t nformatslen = 49;
  uint8_t nvalLabelslen = 33;
  uint16_t nvarLabelslen = 81;
  int32_t chlen = 33;
  uint8_t lbllen = 33;

  switch(release)
  {
  case 102:
    ndlabel = 30;
    nvarnameslen = 9;
    nformatslen = 7;
    nvalLabelslen = 9;
    nvarLabelslen = 32;
    break;
  case 103:
  case 104:
    ndlabel = 32;
    nvarnameslen = 9;
    nformatslen = 7;
    nvalLabelslen = 9;
    nvarLabelslen = 32;
    break;
  case 105:
  case 106:
    chlen = 9;
    ndlabel = 32;
    nvarnameslen = 9;
    nformatslen = 12;
    nvalLabelslen = 9;
    nvarLabelslen = 32;
    lbllen = 9;
    break;
  case 107:
  case 108:
    chlen = 9;
    nvarnameslen = 9;
    nformatslen = 12;
    nvalLabelslen = 9;
    lbllen = 9;
    break;
  case 110:
  case 111:
  case 112:
  case 113:
    nformatslen = 12;
    break;
  }

  CharacterVector byteorderC(1);
  IntegerVector byteorderI(1);
  bool swapit = 0;

  int8_t byteorder_i = 0;
  byteorder_i = readbin(byteorder_i, file, 0);
  // 1 = MSF 2 = LSF
  swapit = std::abs(SBYTEORDER-byteorder_i);
  byteorderI(0) = byteorder_i;

  std::string byteorder(3, '\0');

  if (byteorder_i == 1)
    byteorder = "MSF";
  else
    byteorder = "LSF";

  // filetype: unknown?
  int8_t ft = 0;
  ft = readbin(ft, file, swapit);

  int8_t unused = 0;
  unused = readbin(unused, file, swapit);


  /*
  * Number of Variables
  */

  uint16_t k = 0;
  k = readbin(k, file, swapit);


  /*
  * Number of Observations
  */

  uint32_t n = 0;
  n = readbin(n, file, swapit);

  // dim to return original dim for partial read files
  IntegerVector dim(2);
  dim(0) = n;
  dim(1) = k;

  /*
  * A dataset may have a label e.g. "Written by R".
  * First we read its length (ndlabel), later the actual label (datalabel).
  * ndlabel:   length of datalabel (excl. binary 0)
  * datalabel: string max length 80
  */


  CharacterVector datalabelCV(1);

  std::string datalabel(ndlabel, '\0');

  if (ndlabel > 0)
    readstring(datalabel, file, datalabel.size());
  else
    datalabel = "";

  datalabelCV(0) = datalabel;

  CharacterVector timestampCV(1);
  std::string timestamp(18, '\0');

  switch (release)
  {

  case 102:
  case 103:
  case 104:
  {
    timestamp = "";
    break;
  }

  default:
  {
    readstring(timestamp, file, timestamp.size());
    break;
  }
  }

  timestampCV(0) = timestamp;

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

  switch (release)
  {

  case 102:
  case 103:
  case 104:
  case 105:
  case 106:
  case 107:
  case 108:
  case 110:
  case 112:
  {
    uint8_t nvartypec = 0;

    for (uint16_t i=0; i<k; ++i)
    {
      nvartypec = readbin(nvartypec, file, swapit);

      if(nvartypec== 98) // b
        vartype[i] = 251;
      if(nvartypec==105) // i
        vartype[i] = 252;
      if(nvartypec==108) // l
        vartype[i] = 253;
      if(nvartypec==102) // f
        vartype[i] = 254;
      if(nvartypec==100) // d
        vartype[i] = 255;
      if(nvartypec>127)
        vartype[i] = nvartypec - 127;
    }
    break;
  }

  case 111:
  case 113:
  case 114:
  case 115:
  {
    uint8_t nvartype = 0;

    for (uint16_t i=0; i<k; ++i)
    {
      nvartype = readbin(nvartype, file, swapit);
      vartype[i] = nvartype;
    }
    break;
  }

  }

  // FixMe: Needs clone otherwise missing.type would not work
  IntegerVector types = clone(vartype);

  /*
  * varnames. Max length 33.
  */

  std::string nvarnames(nvarnameslen, '\0');

  CharacterVector varnames(k);
  for (uint16_t i=0; i<k; ++i)
  {
    readstring(nvarnames, file, nvarnames.size());
    varnames[i] = nvarnames;
  }

  /*
  * sortlist. Stata stores the information which variable of a dataset was
  * sorted. Depending on byteorder sortlist is written differently. Currently we
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

  /*
  * formats handle how Stata prints a variable. Currently we do not use this
  * information.
  */

  CharacterVector formats(k);
  std::string nformats(nformatslen, '\0');

  for (uint16_t i=0; i<k; ++i)
  {
    readstring(nformats, file, nformats.size());
    formats[i] = nformats;
  }

  /*
  * value_label_names. Stata stores variable labels by names.
  * nvalLabels: length of the value_label_name
  * valLabels:  Char of max length 33
  */

  CharacterVector valLabels(k);
  std::string nvalLabels(nvalLabelslen, '\0');

  for (uint16_t i=0; i<k; ++i)
  {
    readstring(nvalLabels, file, nvalLabels.size());
    valLabels[i] = nvalLabels;
  }

  /*
  * variabel_labels
  */

  CharacterVector varLabels(k);
  std::string nvarLabels (nvarLabelslen, '\0');

  for (uint16_t i=0; i<k; ++i)
  {
    readstring(nvarLabels, file, nvarLabels.size());
    varLabels[i] = nvarLabels;
  }

  /* <characteristics> ... </characteristics> */

  List ch = List();
  if (release > 104)
  {
    int8_t datatype = 0;
    uint32_t len = 0;

    datatype = readbin(datatype, file, swapit);
    if (release <= 108)
      len = readbin((uint16_t)len, file, swapit);
    else
      len = readbin(len, file, swapit);


    while (!(datatype==0) && !(len==0))
    {
      std::string chvarname(chlen, '\0');
      std::string chcharact(chlen, '\0');
      std::string nnocharacter(len-chlen*2, '\0');

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

      datatype = readbin(datatype, file, swapit);

      if (release <= 108)
        len = readbin((uint16_t)len, file, swapit);
      else
        len = readbin(len, file, swapit);
    }
  }


  /*
  * data. First a list is created with vectors. The vector type is defined by
  * vartype. Stata stores data columnwise so we loop over it and store the
  * data in the list of the first step. Third variable- and row-names are
  * attached and the list type is changed to data.frame.
  */

  /* replace vartypes of Stata 8 - 12 with Stata 13 values. */
  // 117 contains new variable types (longer strings and strL)
  std::replace (vartype.begin(), vartype.end(), 251, STATA_BYTE);
  std::replace (vartype.begin(), vartype.end(), 252, STATA_SHORTINT);
  std::replace (vartype.begin(), vartype.end(), 253, STATA_INT);
  std::replace (vartype.begin(), vartype.end(), 254, STATA_FLOAT);
  std::replace (vartype.begin(), vartype.end(), 255, STATA_DOUBLE);


  uint64_t nmin = selectrows(0), nmax = selectrows(1);
  uint64_t nn   = 0;

  // if  selectrows is c(0,0) use full data
  if ((nmin == 0) && (nmax == 0)){
    nmin = 1;
    nmax = n;
  }

  // make sure that n is not greater than nmax or nmin
  if (n < nmax)
    nmax = n;
  if (n < nmin)
    nmin = n;

  // sequences of column and row
  IntegerVector cvec = seq(0, (k-1));
  IntegerVector rvec = seq(nmin, nmax);
  nn = rvec.size();

  // use c indexing starting at 0
  nmin = nmin -1;
  nmax = nmax -1;

  // calculate length of each variable stored in file. Calculate row length
  IntegerVector rlen = calc_rowlength(vartype);
  uint64_t rlength = sum(rlen);

  // check if vars are selected
  IntegerVector select = cvec, nselect;

  // select vars: either select every var or only matched cases. This will
  // return index positions of the selected variables. If non are selected the
  // index position is cvec
  //
  // name selection was passed to selectcols
  bool all_na_chr = all(is_na(selectcols_chr));
  if (!all_na_chr) {
    select = choose(selectcols_chr, varnames);
  }

  // numeric selection was passed to selectcols
  bool all_na_int = all(is_na(selectcols_int));
  if (!all_na_int) {
    IntegerVector seq_varnames = seq_along(varnames);
    select = choose(selectcols_int, seq_varnames);
  }

  // separate the selected from the not selected cases
  LogicalVector ll = is_na(select);
  nselect = cvec[ll == 1];
  select = cvec[ll == 0];

  uint32_t kk = select.size();

  // shrink variables to selected size
  CharacterVector varnames_kk = varnames[select];
  IntegerVector vartype_kk = vartype[select];
  IntegerVector vartype_s = vartype;
  IntegerVector types_kk = types[select];

  // replace not selected cases with their negative size values
  IntegerVector rlen2 = rlen[nselect];
  rlen2 = -rlen2;
  vartype_s[nselect] = rlen2;


  // Use vartype_s to calulate jump
  IntegerVector vartype_sj = calc_jump(vartype_s);

  // 2. fill it with data

  // skip into the data part
  fseeko64(file, rlength * nmin, SEEK_CUR);

  List df = read_data(file, vartype_kk, missing, release, nn, kk,
                      vartype_sj, byteorder, swapit);

  // skip to end of data part
  fseeko64(file, rlength * (n - nmax -1), SEEK_CUR);

  // 3. Create a data.frame
  df.attr("row.names") = rvec;
  df.attr("names") = varnames_kk;
  df.attr("class") = "data.frame";

  /*
  * labels are separated by <lbl>-tags. Labels may appear in any order e.g.
  * 2 "female" 1 "male 9 "missing". They are stored as tables.
  * nlen:     length of label.
  * nlabname: label name.
  * labn:     number of labels in this set (e.g. "male" "female" = 2)
  * txtlen:   length of the label text.
  * off:      offset defines where to read a new label in txtlen.
  */

  List labelList = List(); //put labels into this list

  if (release>105) {
    // FixMe: the while statement differs and the final check


    int32_t nlen = 0, labn = 0, txtlen = 0, noff = 0, val = 0;
    std::string tag(5, '\0');

    bool haslabel = false;

    // length of value_label_table
    nlen = readbin(nlen, file, swapit);

    if (!(feof(file) || ferror(file)))
      haslabel = true;

    while(haslabel)
    {

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

      // create an index to sort labels along the code values
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

      // length of value_label_table
      nlen = readbin(nlen, file, swapit);

      if (feof(file) || ferror(file))
        break;
    }
  }

  /*
   * assign attributes to the resulting data.frame
   */

  formats = formats[select];
  valLabels = valLabels[select];
  varLabels = varLabels[select];

  df.attr("datalabel") = datalabelCV;
  df.attr("time.stamp") = timestampCV;
  df.attr("formats") = formats;
  df.attr("types") = types_kk;
  df.attr("val.labels") = valLabels;
  df.attr("var.labels") = varLabels;
  df.attr("version") = versionIV;
  df.attr("label.table") = labelList;
  df.attr("expansion.fields") = ch;
  df.attr("byteorder") = byteorderI;
  df.attr("orig.dim") = dim;

  return df;
}
