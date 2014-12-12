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
#include <string>
#include <fstream>
#include <stdint.h>
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

//' Writes the binary Stata file
//'
//' @param filePath The full systempath to the dta file you want to export.
//' @param dat an R-Object of class data.frame.
//' @export
// [[Rcpp::export]]
int stataWrite(const char * filePath, Rcpp::DataFrame dat)
{
  uint16_t const k = dat.size();
  uint32_t const n = dat.nrows();

  CharacterVector VarNames = dat.attr("names");

  char version[4] = "117";
  uint8_t ntimestamp = 0;

  string datalabel = dat.attr("datalabel");
  datalabel[datalabel.size()] = '\0';
  uint8_t ndlabel = 0;

  const string head = "<stata_dta><header><release>";
  const string byteord = "</release><byteorder>";
  const string K = "</byteorder><K>";
  const string num = "</K><N>";
  const string lab = "</N><label>";
  const string timest = "</label><timestamp>";

  const string timestamp = dat.attr("timestamp");

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

  //     ofstream dta (filePath);
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
    dta.write(version,3); // for now 117 (e.g. Stata 13)
    dta.write(byteord.c_str(),byteord.size());
    dta.write(byteorder,3); // LSF
    dta.write(K.c_str(),K.size());
    writebin(k, dta, swapit);
    dta.write(num.c_str(),num.size());
    writebin(n, dta, swapit);
    dta.write(lab.c_str(),lab.size());

    /* write a datalabel */
    if(!datalabel.empty())
    {
      ndlabel = datalabel.size();
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
    List vartypes = dat.attr("types");
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
      const string VARnames = as<string>(VarNames[i]);
      dta.write(VARnames.c_str(),33);
    }
    dta.write(endvarn.c_str(), endvarn.size());


    /* <sortlist> ... </sortlist> */
    map(4) = dta.tellg();
    dta.write(startsor.c_str(),startsor.size());

    for (uint16_t i = 0; i < k+1; ++i)
    {
      uint16_t nsortlist = 0;
      writebin(nsortlist, dta, swapit);
    }
    dta.write(endsor.c_str(),endsor.size());


    /* <formats> ... </formats> */
    map(5) = dta.tellg();
    dta.write(startform.c_str(),startform.size());
    List formats = dat.attr("formats");
    for (uint16_t i = 0; i < k; ++i )
    {
      const string Fmats = as<string>(formats[i]);
      dta.write(Fmats.c_str(),49);
    }
    dta.write(endform.c_str(),endform.size());


    /* <value_label_names> ... </value_label_names> */
    map(6) = dta.tellg();
    dta.write(startvalLabel.c_str(),startvalLabel.size());
    CharacterVector valLabels = dat.attr("vallabels");
    for (uint16_t i = 0; i < k; ++i )
    {
      const string nvalLabels = as<string>(valLabels[i]);
      dta.write(nvalLabels.c_str(),33);
    }
    dta.write(endvalLabel.c_str(),endvalLabel.size());


    /* <variable_labels> ... </variable_labels> */
    map(7) = dta.tellg();
    List varLabels = dat.attr("var.labels");
    dta.write(startvarlabel.c_str(),startvarlabel.size());
    for (uint16_t i = 0; i < k; ++i)
    {
      if (!Rf_isNull(varLabels) && Rf_length(varLabels) > 1) {
        const string nvarLabels = as<std::string>(varLabels[i]);
        dta.write(nvarLabels.c_str(),81);
      } else {
        const string nvarLabels = "";
        dta.write(nvarLabels.c_str(),81);
      }
    }
    dta.write(endvarlabel.c_str(),endvarlabel.size());


    /* <characteristics> ... </characteristics> */
    map(8) = dta.tellg();
    dta.write(startcharacteristics.c_str(),startcharacteristics.size());
    /* <ch> ... </ch> */

    List expansiontable = dat.attr("expansion.fields");

    if (expansiontable.size()>0){
      for (int32_t i = 0; i<expansiontable.size(); ++i){

        dta.write(startch.c_str(),startch.size());

        CharacterVector chs = as<CharacterVector>(expansiontable[i]);

        string chs1 = as<string>(chs[0]);
        chs1[chs1.size()] = '\0';
        string chs2 = as<string>(chs[1]);
        chs2[chs2.size()] = '\0';
        string chs3 = as<string>(chs[2]);
        chs3[chs3.size()] = '\0';

        uint32_t nocharacter = 33 + 33 + chs3.size() +1;
        writebin(nocharacter, dta, swapit);

        dta.write(chs1.c_str(),33);
        dta.write(chs2.c_str(),33);
        dta.write(chs3.c_str(),chs3.size()+1);

        dta.write(endch.c_str(),endch.size());
      }
    }

    dta.write(endcharacteristics.c_str(),endcharacteristics.size());


    /* <data> ... </data> */
    map(9) = dta.tellg();
    dta.write(startdata.c_str(),startdata.size());

    IntegerVector V;
    IntegerVector O;
    CharacterVector strl;

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
          double const na = (0x1.0000000000000p1023);
          double val_d = as<NumericVector>(dat[i])[j];

          if ( (val_d == NA_REAL) | R_IsNA(val_d) )
            val_d = na;

          writebin(val_d, dta, swapit);

          break;
        }
          // float
        case 65527:
        {
          float const na = (0x1.000000p127);
          float val_f = as<NumericVector>(dat[i])[j];

          if ( (val_f == NA_REAL) | R_IsNA(val_f) )
            val_f = na;

          writebin(val_f, dta, swapit);

          break;
        }
          // store integer as Stata long (int32_t)
        case 65528:
        {
          int32_t const na = (0x7fffffe5);
          int32_t val_i = as<IntegerVector>(dat[i])[j];

          // cout << val_i << endl;

          if (val_i == NA_INTEGER | R_IsNA(val_i) )
            val_i = na;

          writebin(val_i, dta, swapit);

          break;
        }
          // int
        case 65529:
        {
          int16_t const na = (32741);
          union v {
            int32_t   f;
            int16_t    i;
          } val;

          val.f = as<IntegerVector>(dat[i])[j];

          int16_t val_i = val.i;

          if (val.f == NA_INTEGER)
            val_i = na;

          writebin(val_i, dta, swapit);

          break;
        }
          // byte
        case 65530:
        {
          int8_t const na = (101);
          union v {
            int32_t   f;
            int8_t    i;
          } val;

          val.f = as<IntegerVector>(dat[i])[j];

          int8_t val_i = val.i;

          if (val.f == NA_INTEGER)
            val_i = na;

          writebin(val_i, dta, swapit);

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
          int32_t v = i+1;
          int32_t o = j+1;
          int64_t z = 0;

          CharacterVector b = as<CharacterVector>(dat[i]);
          const string val_s = as<string>(b[j]);
          if (!val_s.empty())
          {
            writebin(v, dta, swapit);
            writebin(o, dta, swapit);
            // push back every v, o and val_s
            V.push_back(v);
            O.push_back(o);
            strl.push_back(val_s);
          } else {
            dta.write((char*)&z,sizeof(z));
          }
        }
        }
      }
    }
    dta.write(enddata.c_str(),enddata.size());


    /* <strls> ... </strls> */
    map(10) = dta.tellg();
    dta.write(startstrl.c_str(),startstrl.size());

    int32_t strlsize = strl.length();
    for(int i =0; i < strlsize; ++i )
    {
      const string GSO = "GSO";
      int32_t v = V[i];
      int32_t o = O[i];
      uint8_t t = 129; //Stata binary type, no trailing zero.
      const string strL = as<string>(strl[i]);
      uint32_t len = strL.size();

      dta.write(GSO.c_str(),GSO.size());
      writebin(v, dta, swapit);
      writebin(o, dta, swapit);
      writebin(t, dta, swapit);
      writebin(len, dta, swapit);
      dta.write(strL.c_str(),strL.size());
    }

    dta.write(endstrl.c_str(),endstrl.size());


    /* <value_labels> ... </value_labels> */
    map(11) = dta.tellg();
    List labeltable = dat.attr("label.table");
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

        int32_t len = sizeof(N) + sizeof(txtlen) + sizeof(offI)*N + sizeof(labvalueI)*N + txtlen;


        dta.write(startlbl.c_str(),startlbl.size());
        writebin(len, dta, swapit);
        dta.write(labname.c_str(),33);
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
