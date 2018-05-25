/*
 * Copyright (C) 2014-2017 Jan Marvin Garbuszus and Sebastian Jeworutzki
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

List read_dta(FILE * file, const bool missing, const IntegerVector selectrows,
              const CharacterVector selectcols,
              const bool strlexport, const CharacterVector strlpath)
{
  // stata_dta><header>
  test("stata_dta><header>", file);
  test("<release>", file);

  /*
  * version is a 4 byte character e.g. "117"
  */

  int8_t fversion = 117L; //f = first
  int8_t lversion = 119L; //l = last

  std::string version(3, '\0');
  readstring(version, file, version.size());

  int8_t const release = atoi(version.c_str());

  IntegerVector versionIV(1);
  versionIV(0) = release;

  // check the release version.
  if (release<fversion || release>lversion)
  {
    warning("File version is %d.\nVersion: Not a version 13/14 dta-file", release);
    return -1;
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
  case 119:
    nvarnameslen = 129;
    nformatslen = 57;
    nvalLabelslen = 129;
    nvarLabelslen = 321;
    chlen = 129;
    lbllen = 129;
    break;
  }

  // </release>
  test("</release>", file);
  test("<byteorder>", file);

  /*
  * byteorder is a 4 byte character e.g. "LSF". MSF refers to big-endian.
  */

  std::string byteorder(3, '\0');
  readstring(byteorder,file, byteorder.size());

  // </byteorder>
  test("</byteorder>", file);
  test("<K>", file);

  bool swapit = 0;
  swapit = strcmp(byteorder.c_str(), sbyteorder);

  /*
  * Number of Variables
  */

  uint32_t k = 0;
  if (release < 119)
    k = readbin((uint16_t)k, file, swapit);
  if (release == 119)
    k = readbin(k, file, swapit);

  //</K>
  test("</K>", file);
  test("<N>", file);

  /*
  * Number of Observations
  */

  uint64_t n = 0;

  if (release == 117)
    n = readbin((uint32_t)n, file, swapit);
  if ((release == 118) | (release == 119))
    n = readbin(n, file, swapit);

  //</N>
  test("</N>", file);
  test("<label>", file);

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

  uint16_t ndlabel = 0;

  if (release == 117)
    ndlabel = readbin((int8_t)ndlabel, file, swapit);
  if ((release == 118) | (release == 119))
    ndlabel = readbin(ndlabel, file, swapit);

  std::string datalabel(ndlabel, '\0');

  if (ndlabel>0)
  {
    readstring(datalabel, file, datalabel.size());
  } else {
    datalabel = "";
  }

  CharacterVector datalabelCV(1);
  datalabelCV(0) = datalabel;

  //</label>
  test("</label>", file);
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
  //</timestamp></header>
  test("</timestamp></header>", file);
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

  NumericVector map(14);
  for (int i=0; i <14; ++i)
  {
    uint64_t nmap = 0;
    nmap = readbin(nmap, file, swapit);
    map[i] = nmap;
  }

  //</map>
  test("</map>", file);
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
  for (uint32_t i=0; i<k; ++i)
  {
    uint16_t nvartype = 0;
    nvartype = readbin(nvartype, file, swapit);
    vartype[i] = nvartype;
  }

  //</variable_types>
  test("</variable_types>", file);
  test("<varnames>", file);

  /*
  * varnames.
  */

  std::string nvarnames(nvarnameslen, '\0');

  CharacterVector varnames(k);
  for (uint32_t i=0; i<k; ++i)
  {
    readstring(nvarnames, file, nvarnames.size());
    varnames[i] = nvarnames;
  }

  //</varnames>
  test("</varnames>", file);
  test("<sortlist>", file);

  /*
  * sortlist. Stata stores the information which variable of a dataset was
  * sorted. Depending on byteorder sortlist is written differently. Currently we
  * do not use this information.
  * Vector size is k+1.
  */

  uint64_t big_k = k+1;

  IntegerVector sortlist(big_k);
  for (uint64_t i=0; i<big_k; ++i)
  {
    uint16_t nsortlist = 0;
    nsortlist = readbin(nsortlist, file, swapit);
    sortlist[i] = nsortlist;
  }

  //</sortlist>
  test("</sortlist>", file);
  test("<formats>", file);

  /*
  * formats handle how Stata prints a variable. Currently we do not use this
  * information.
  */

  std::string nformats(nformatslen, '\0');

  CharacterVector formats(k);
  for (uint32_t i=0; i<k; ++i)
  {
    readstring(nformats, file, nformats.size());
    formats[i] = nformats;
  }

  //</formats>
  test("</formats>", file);
  test("<value_label_names>",file);

  /*
  * value_label_names. Stata stores variable labels by names.
  * nvalLabels: length of the value_label_name
  * valLabels:
  */

  std::string nvalLabels(nvalLabelslen, '\0');

  CharacterVector valLabels(k);
  for (uint32_t i=0; i<k; ++i)
  {
    readstring(nvalLabels, file, nvalLabels.size());
    valLabels[i] = nvalLabels;
  }

  //</value_label_names>
  test("</value_label_names>", file);
  test("<variable_labels>", file);

  /*
  * variabel_labels
  */

  std::string nvarLabels (nvarLabelslen, '\0');

  CharacterVector varLabels(k);
  for (uint32_t i=0; i<k; ++i)
  {
    readstring(nvarLabels, file, nvarLabels.size());
    varLabels[i] = nvarLabels;
  }

  //</variable_labels>
  test("</variable_labels>", file);
  test("<characteristics>", file);

  /*
  * characteristics. Stata can store additional information this way. It may
  * contain notes (for the dataset or a variable) or about label language sets.
  * Characteristics are not documented. We export them as attribute:
  * expansion.fields. Characteristics are separated by <ch> tags. Each <ch> has:
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

    // </ch>
    test("</ch>", file);

    // read next tag
    readstring(tago, file, tago.size());
  }

  //[</ch]aracteristics>
  test("aracteristics>", file);
  test("<data>", file);

  /*
  * data. First a list is created with vectors. The vector type is defined by
  * vartype. Stata stores data columnwise so we loop over it and store the
  * data in the list of the first step. Third variable- and row-names are
  * attached and the list type is changed to data.frame.
  */

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
  std::string selcols = as<std::string>(selectcols(0));
  bool selectvars = selcols != "";

  // select vars: either select every var or only matched cases. This will
  // return index positions of the selected variables. If non are selected the
  // index position is cvec
  IntegerVector select = cvec, nselect;
  if (selectvars)
    select = choose(selectcols, varnames);

  // separate the selected from the not selected cases
  LogicalVector ll = is_na(select);
  nselect = cvec[ll == 1];
  select = cvec[ll == 0];

  uint32_t kk = select.size();

  // shrink variables to selected size
  CharacterVector varnames_kk = varnames[select];
  IntegerVector vartype_kk = vartype[select];
  IntegerVector vartype_s = vartype;

  // replace not selected cases with their negative size values
  IntegerVector rlen2 = rlen[nselect];
  rlen2 = -rlen2;
  vartype_s[nselect] = rlen2;



  // Use vartype_s to calculate jump
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

  //</data>
  test("</data>", file);
  test("<strls>", file);

  /*
  * strL. Stata 13 introduced long strings up to 2 billion characters. strLs are
  * separated by "GSO".
  * (v,o): Position in the data.frame.
  * t:     129/130 defines whether or not the strL is stored with a binary 0.
  * len:   length of the strL.
  * strl:  long string.
  */

  std::string gso = "GSO";

  std::string tags(3, '\0');
  readstring(tags, file, tags.size());

  //put strLs into a named vector
  CharacterVector strlvalues(0);
  CharacterVector strlnames(0);

  while (gso.compare(tags)==0)
  {
    CharacterVector strls(2);
    string ref;

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
      ref.assign(val_stream.str());

      break;
    }
    case 118:
    case 119:
    {
      uint32_t v = 0;
      uint64_t o = 0;

      v = readbin(v, file, swapit);
      o = readbin(o, file, swapit);

      stringstream val_stream;
      val_stream << v << '_' << o;
      ref.assign(val_stream.str());

      break;
    }
    }

    // (129 = binary) | (130 = ascii) Note:
    // if 130 full len contains the string. if 130 len includes trailing \0.
    // that does not affect us. we read the full len, and if \0 occurs R
    // will print only the string up to that position. we write 129
    uint8_t t = 0;
    t = readbin(t, file, swapit);

    uint32_t len = 0;
    len = readbin(len, file, swapit);

    std::string strl(len, '\0');

    readstring(strl, file, strl.size());

    // write strl to file. Stata allows binary files in strls
    if (strlexport) {

      std::string path = Rcpp::as<std::string>(strlpath);
      std::string outputpath = path + "/" + ref;

      ofstream file1(outputpath.c_str(), ios::out | ios::binary);
      if (file1.good()) {
        file1.write(strl.c_str(), strl.size());
        file1.close();
      } else {
        Rcpp::Rcout << "strl export failed" << std::endl;
      }

    }

    strlvalues.push_back( strl );
    strlnames.push_back( ref );

    readstring(tags, file, tags.size());
  }

  // set identifier as name
  strlvalues.attr("names") = strlnames;

  // after strls
  //[</s]trls>
  test("trls>", file);
  test("<value_labels>", file);

  /*
  * labels are separated by <lbl>-tags. Labels may appear in any order e.g.
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

  while (lbltag.compare(tag)==0)
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

  // [</val]ue_labels>
  test("ue_labels>", file);
  test("</stata_dta>", file);

  /*
   * assign attributes to the resulting data.frame
   */

  formats = formats[select];
  valLabels = valLabels[select];
  varLabels = varLabels[select];

  df.attr("datalabel") = datalabelCV;
  df.attr("time.stamp") = timestampCV;
  df.attr("formats") = formats;
  df.attr("types") = vartype_kk;
  df.attr("val.labels") = valLabels;
  df.attr("var.labels") = varLabels;
  df.attr("version") = versionIV;
  df.attr("label.table") = labelList;
  df.attr("expansion.fields") = ch;
  df.attr("strl") = strlvalues;
  df.attr("byteorder") = wrap(byteorder);
  df.attr("orig.dim") = dim;

  return df;
}
