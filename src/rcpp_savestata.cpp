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
#include <string>
#include <fstream>
#include <stdint.h>
#include "statadefines.h"
#include "swap_endian.h"
// #include <cstdint> //C++11

using namespace Rcpp;
using namespace std;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define lsf "LSF"
#define byteorder "LSF"
#else
#define lsf "MSF"
#define byteorder "MSF"
#endif

bool swapit = strcmp(byteorder, lsf);

template <typename T>
static void writebin(T t, fstream& dta, bool swapit)
{
  if (swapit==1){
    T t_s = swap_endian(t);
    dta.write((char*)&t_s, sizeof(t_s));
  } else {
    dta.write((char*)&t, sizeof(t));
  }
}

// Writes the binary Stata file
//
// @param filePath The full systempath to the dta file you want to export.
// @param dat an R-Object of class data.frame.
// @export
// [[Rcpp::export]]
int stataWrite(const char * filePath, Rcpp::DataFrame dat)
{
  uint16_t k = dat.size();
  uint64_t n = dat.nrows();

  const string timestamp = dat.attr("timestamp");
  string datalabel = dat.attr("datalabel");
  datalabel[datalabel.size()] = '\0';

  CharacterVector valLabels = dat.attr("vallabels");
  CharacterVector nvarnames = dat.attr("names");

  List chs = dat.attr("expansion.fields");
  List formats = dat.attr("formats");
  List labeltable = dat.attr("label.table");
  List varLabels = dat.attr("var.labels");
  List vartypes = dat.attr("types");

  const string version = dat.attr("version");

  uint8_t const release = atoi(version.c_str());

  uint8_t nvarnameslen = 0, nformatslen = 0, nvalLabelslen = 0, lbllen = 0, ntimestamp = 0;
  uint16_t nvarLabelslen = 0, ndlabel = 0;
  int32_t chlen = 0;

  switch (release)
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

  const string head = "<stata_dta><header><release>";
  const string byteord = "</release><byteorder>";
  const string K = "</byteorder><K>";
  const string num = "</K><N>";
  const string lab = "</N><label>";
  const string timest = "</label><timestamp>";
  const string endheader = "</timestamp></header>";

  const string startmap = "<map>";
  const string endmap = "</map>";

  const string startvart = "<variable_types>";
  const string endvart = "</variable_types>";

  const string startvarn = "<varnames>";
  const string endvarn = "</varnames>";

  const string startsor = "<sortlist>";
  const string endsor = "</sortlist>";

  const string startform = "<formats>";
  const string endform = "</formats>";

  const string startvalLabel = "<value_label_names>";
  const string endvalLabel = "</value_label_names>";

  const string startvarlabel= "<variable_labels>";
  const string endvarlabel= "</variable_labels>";

  const string startcharacteristics = "<characteristics>";
  const string endcharacteristics = "</characteristics>";

  const string startch = "<ch>";
  const string endch = "</ch>";

  const string startdata = "<data>";
  const string enddata = "</data>";

  const string startstrl = "<strls>";
  const string endstrl = "</strls>";

  const string startvall = "<value_labels>";
  const string endvall = "</value_labels>";

  const string startlbl = "<lbl>";
  const string endlbl = "</lbl>";

  string end = "</stata_dta>";
  end[end.size()] = '\0';

  fstream dta (filePath, ios::out | ios::binary);
  if (dta.is_open())
  {
    /* Stata 13 uses <map> to store 14 byte positions in a dta-file. This
    * vector is now created and filled with the correct map positions. At
    * the end of the creation process, all 14 values are known and map will
    * be filled with the correct values.
    */
    IntegerVector map(14);
    map(0) = dta.tellg();

    dta.write(head.c_str(),head.size());
    dta.write(version.c_str(),3); // 117|118 (e.g. Stata 13|14)
    dta.write(byteord.c_str(),byteord.size());
    dta.write(byteorder,3); // LSF
    dta.write(K.c_str(),K.size());
    writebin(k, dta, swapit);
    dta.write(num.c_str(),num.size());
    if (release==117)
      writebin((int32_t)n, dta, swapit);
    if (release==118)
      writebin(n, dta, swapit);
    dta.write(lab.c_str(),lab.size());


    /* write a datalabel */
    if(!datalabel.empty())
    {
      ndlabel = datalabel.size();
      if (release==117)
        writebin((uint8_t)ndlabel, dta, swapit);
      if (release==118)
        writebin(ndlabel, dta, swapit);
      dta.write(datalabel.c_str(),datalabel.size());
    } else {
      dta.write((char*)&ndlabel,sizeof(ndlabel));
    }


    /* timestamp size is 0 (= no timestamp) or 17 */
    dta.write(timest.c_str(),timest.size());
    if (!timestamp.empty()) {
      ntimestamp = 17;
      writebin(ntimestamp, dta, swapit);
      dta.write(timestamp.c_str(),timestamp.size());
    }else{
      writebin(ntimestamp, dta, swapit);
    }
    dta.write(endheader.c_str(),endheader.size());

    /* <map> ... </map> */
    map(1) = dta.tellg();
    dta.write(startmap.c_str(),startmap.size());
    for (int32_t i = 0; i <14; ++i)
    {
      uint64_t nmap = 0;
      writebin(nmap, dta, swapit);
    }
    dta.write(endmap.c_str(),endmap.size());

    /* <variable_types> ... </variable_types> */
    map(2) = dta.tellg();
    dta.write(startvart.c_str(),startvart.size());
    uint16_t nvartype;
    for (uint16_t i = 0; i < k; ++i)
    {
      nvartype = as<uint16_t>(vartypes[i]);

      writebin(nvartype, dta, swapit);
    }
    dta.write(endvart.c_str(),endvart.size());


    /* <varnames> ... </varnames> */
    map(3) = dta.tellg();
    dta.write(startvarn.c_str(), startvarn.size());
    for (uint16_t i = 0; i < k; ++i )
    {
      const string nvarname = as<string>(nvarnames[i]);
      dta.write(nvarname.c_str(),nvarnameslen);
    }
    dta.write(endvarn.c_str(), endvarn.size());


    /* <sortlist> ... </sortlist> */
    map(4) = dta.tellg();
    dta.write(startsor.c_str(),startsor.size());

    uint32_t big_k = k+1;

    for (uint32_t i = 0; i < big_k; ++i)
    {
      uint16_t nsortlist = 0;
      writebin(nsortlist, dta, swapit);
    }
    dta.write(endsor.c_str(),endsor.size());


    /* <formats> ... </formats> */
    map(5) = dta.tellg();
    dta.write(startform.c_str(),startform.size());
    for (uint16_t i = 0; i < k; ++i )
    {
      const string nformats = as<string>(formats[i]);
      dta.write(nformats.c_str(),nformatslen);
    }
    dta.write(endform.c_str(),endform.size());


    /* <value_label_names> ... </value_label_names> */
    map(6) = dta.tellg();
    dta.write(startvalLabel.c_str(),startvalLabel.size());
    for (uint16_t i = 0; i < k; ++i )
    {
      const string nvalLabels = as<string>(valLabels[i]);
      dta.write(nvalLabels.c_str(), nvalLabelslen);
    }
    dta.write(endvalLabel.c_str(),endvalLabel.size());


    /* <variable_labels> ... </variable_labels> */
    map(7) = dta.tellg();
    dta.write(startvarlabel.c_str(),startvarlabel.size());
    for (uint16_t i = 0; i < k; ++i)
    {
      if (!Rf_isNull(varLabels) && Rf_length(varLabels) > 1) {
        const string nvarLabels = as<std::string>(varLabels[i]);
        dta.write(nvarLabels.c_str(),nvarLabelslen);
      } else {
        const string nvarLabels = "";
        dta.write(nvarLabels.c_str(),nvarLabelslen);
      }
    }
    dta.write(endvarlabel.c_str(),endvarlabel.size());


    /* <characteristics> ... </characteristics> */
    map(8) = dta.tellg();
    dta.write(startcharacteristics.c_str(),startcharacteristics.size());
    /* <ch> ... </ch> */

    if (chs.size()>0){
      for (int32_t i = 0; i<chs.size(); ++i){

        dta.write(startch.c_str(),startch.size());

        CharacterVector ch = as<CharacterVector>(chs[i]);

        string ch1 = as<string>(ch[0]);
        ch1[ch1.size()] = '\0';
        string ch2 = as<string>(ch[1]);
        ch2[ch2.size()] = '\0';
        string ch3 = as<string>(ch[2]);
        ch3[ch3.size()] = '\0';

        uint32_t nnocharacter = chlen*2 + ch3.size() +1;
        writebin(nnocharacter, dta, swapit);

        dta.write(ch1.c_str(),chlen);
        dta.write(ch2.c_str(),chlen);
        dta.write(ch3.c_str(),ch3.size()+1);

        dta.write(endch.c_str(),endch.size());
      }
    }

    dta.write(endcharacteristics.c_str(),endcharacteristics.size());


    /* <data> ... </data> */
    map(9) = dta.tellg();
    dta.write(startdata.c_str(),startdata.size());

    IntegerVector V, O;
    CharacterVector STRL;

    for(uint32_t j = 0; j < n; ++j)
    {
      for (uint16_t i = 0; i < k; ++i)
      {
        int const type = vartypes[i];
        switch(type < 2046 ? 2045 : type)
        {
          // store numeric as Stata double (double)
        case 65526:
        {
          double val_d = as<NumericVector>(dat[i])[j];

          if ( (val_d == NA_REAL) | R_IsNA(val_d) )
            val_d = STATA_DOUBLE_NA;

          writebin(val_d, dta, swapit);

          break;
        }
          // float
        case 65527:
        {
          double val_d = as<NumericVector>(dat[i])[j];
          float val_f = (double)(val_d);

          if ( (val_d == NA_REAL) | (R_IsNA(val_d)) )
            val_f = STATA_FLOAT_NA;

          writebin(val_f, dta, swapit);

          break;
        }
          // store integer as Stata long (int32_t)
        case 65528:
        {
          int32_t val_l = as<IntegerVector>(dat[i])[j];

          if ( (val_l == NA_INTEGER) | (R_IsNA(val_l)) )
            val_l = STATA_INT_NA;

          writebin(val_l, dta, swapit);

          break;
        }
          // int
        case 65529:
        {
          union v {
            int32_t   l;
            int16_t   i;
          } val;

          val.l = as<IntegerVector>(dat[i])[j];

          int16_t val_i = val.i;

          if (val.l == NA_INTEGER)
            val_i = STATA_SHORTINT_NA;

          writebin(val_i, dta, swapit);

          break;
        }
          // byte
        case 65530:
        {
          union v {
            int32_t   l;
            int8_t    b;
          } val;

          val.l = as<IntegerVector>(dat[i])[j];

          int8_t val_b = val.b;

          if (val.l == NA_INTEGER)
            val_b = STATA_BYTE_NA;

          writebin(val_b, dta, swapit);

          break;
        }
        case 2045:
        {
          int32_t const len = vartypes[i];
          /* FixMe: Storing the vector in b for each string. */
          CharacterVector b = as<CharacterVector>(dat[i]);
          string val_s = as<string>(b[j]);
          dta.write(val_s.c_str(),len);
          break;
        }
        case 32768:
        {
          /* Stata uses +1 */
          int32_t v = i+1, o = j+1;
          int64_t z = 0;

          CharacterVector b = as<CharacterVector>(dat[i]);
          const string val_strl = as<string>(b[j]);
          if (!val_strl.empty())
          {
            writebin(v, dta, swapit);
            writebin(o, dta, swapit);
            // push back every v, o and val_strl
            V.push_back(v);
            O.push_back(o);
            STRL.push_back(val_strl);
          } else {
            dta.write((char*)&z,sizeof(z));
          }
          break;
        }
        }
      }
    }
    dta.write(enddata.c_str(),enddata.size());


    /* <strls> ... </strls> */
    map(10) = dta.tellg();
    dta.write(startstrl.c_str(),startstrl.size());

    int32_t strlsize = STRL.length();
    for(int i =0; i < strlsize; ++i )
    {
      const string gso = "GSO";
      int32_t v = V[i], o = O[i];
      uint8_t t = 129; //Stata binary type, no trailing zero.
      const string strL = as<string>(STRL[i]);
      uint32_t len = strL.size();

      dta.write(gso.c_str(),gso.size());
      writebin(v, dta, swapit);
      writebin(o, dta, swapit);
      writebin(t, dta, swapit);
      writebin(len, dta, swapit);
      dta.write(strL.c_str(),strL.size());
    }

    dta.write(endstrl.c_str(),endstrl.size());


    /* <value_labels> ... </value_labels> */
    map(11) = dta.tellg();
    dta.write(startvall.c_str(),startvall.size());
    if (labeltable.size()>0)
    {

      CharacterVector labnames = labeltable.attr("names");
      int8_t padding = 0;

      for (int32_t i=0; i < labnames.size(); ++i)
      {
        int32_t txtlen = 0;

        const string labname = as<string>(labnames[i]);
        IntegerVector labvalue = labeltable[labname];
        int32_t N = labvalue.size();
        CharacterVector labelText = labvalue.attr("names");
        IntegerVector off;

        /*
        * Fill off with offset position and create txtlen
        */

        for (int32_t i = 0; i < labelText.size(); ++i)
        {
          string label = as<string>(labelText[i]);
          int32_t labellen = label.size()+1;
          txtlen += labellen;
          off.push_back ( txtlen-labellen );
        }

        int32_t offI, labvalueI;

        int32_t nlen = sizeof(N) + sizeof(txtlen) + sizeof(offI)*N + sizeof(labvalueI)*N + txtlen;

        dta.write(startlbl.c_str(),startlbl.size());
        writebin(nlen, dta, swapit);
        dta.write(labname.c_str(),lbllen);
        dta.write((char*)&padding,3);
        writebin(N, dta, swapit);
        writebin(txtlen, dta, swapit);

        for (int32_t i = 0; i < N; ++i)
        {
          offI = off[i];
          writebin(offI, dta, swapit);
        }

        for (int32_t i = 0; i < N; ++i)
        {
          labvalueI = labvalue[i];
          writebin(labvalueI, dta, swapit);
        }

        for (int32_t i = 0; i < N; ++i)
        {
          string labtext = as<string>(labelText[i]);
          labtext[labtext.size()] = '\0';
          dta.write(labtext.c_str(),labtext.size()+1);
        }
        dta.write(endlbl.c_str(),endlbl.size());
      }

    }
    dta.write(endvall.c_str(),endvall.size());


    /* </stata_data> */
    map(12) = dta.tellg();
    dta.write(end.c_str(),end.size());


    /* end-of-file */
    map(13) = dta.tellg();


    /* seek up to <map> to rewrite it*/
    /* <map> ... </map> */
    dta.seekg(map[1]);
    dta.write(startmap.c_str(),startmap.size());
    for (int i=0; i <14; ++i)
    {
      uint64_t nmap = map[i];
      writebin(nmap, dta, swapit);
    }
    dta.write(endmap.c_str(),endmap.size());

    dta.close();
    return 0;
  }
  else {
    throw std::range_error("Unable to open file.");
    return -1;
  }
}
