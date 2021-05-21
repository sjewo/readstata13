/*
 * Copyright (C) 2014-2019 Jan Marvin Garbuszus and Sebastian Jeworutzki
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

#include <readstata.h>

using namespace Rcpp;
using namespace std;

// Writes the binary Stata file
//
// @param filePath The full systempath to the dta file you want to export.
// @param dat an R-Object of class data.frame.
// @export
// [[Rcpp::export]]
int stata_save(const char * filePath, Rcpp::DataFrame dat)
{
  uint32_t k = dat.size();
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

  uint8_t nformatslen = 0, ntimestamp = 0;
  uint16_t nvarnameslen = 0, nvarLabelslen = 0, nvalLabelslen = 0, ndlabel = 0,
    lbllen = 0;
  uint32_t chlen = 0, maxdatalabelsize = 0, maxlabelsize = 32000;

  switch (release)
  {
  case 117:
    nvarnameslen = 33;
    nformatslen = 49;
    nvalLabelslen = 33;
    nvarLabelslen = 81;
    maxdatalabelsize = 80;
    chlen = 33;
    lbllen = 33;
    break;
  case 118:
  case 119:
    nvarnameslen = 129;
    nformatslen = 57;
    nvalLabelslen = 129;
    nvarLabelslen = 321;
    maxdatalabelsize = 320; // in utf8 4 * 80 byte
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
    NumericVector map(14);
    map(0) = dta.tellg();

    writestr(head, head.size(), dta);
    writestr(version, 3, dta); // 117|118 (e.g. Stata 13|14)
    writestr(byteord, byteord.size(), dta);
    writestr(sbyteorder, 3, dta); // LSF
    writestr(K, K.size(), dta);
    if (release < 119)
      writebin((int16_t)k, dta, swapit);
    if (release == 119)
      writebin(k, dta, swapit);
    writestr(num, num.size(), dta);
    if (release == 117)
      writebin((int32_t)n, dta, swapit);
    if ((release == 118) | (release == 119))
      writebin(n, dta, swapit);
    writestr(lab, lab.size(), dta);


    /* write a datalabel */
    if (!datalabel.empty())
    {
      if (datalabel.size() > maxdatalabelsize)
      {
        Rcpp::warning("Datalabel to long. Resizing. Max size is %d.",
                      maxdatalabelsize);
        datalabel.resize(maxdatalabelsize);
        datalabel[datalabel.size()] = '\0';
      }
      ndlabel = datalabel.size();

      if (release == 117)
        writebin((uint8_t)ndlabel, dta, swapit);
      if ((release == 118) | (release == 119))
        writebin(ndlabel, dta, swapit);

      writestr(datalabel,datalabel.size(), dta);
    } else {
      // empty data label defined by byte(s) of zero
      uint8_t zero = 0;

      if (release == 117) {
        writebin(zero, dta, swapit);
      }
      if ((release == 118) | (release == 119)) {
        writebin(zero, dta, swapit);
        writebin(zero, dta, swapit);
      }
    }


    /* timestamp size is 0 (= no timestamp) or 17 */
    writestr(timest, timest.size(), dta);
    if (!timestamp.empty()) {
      ntimestamp = 17;
      writebin(ntimestamp, dta, swapit);
      writestr(timestamp, timestamp.size(), dta);
    }else{
      writebin(ntimestamp, dta, swapit);
    }
    writestr(endheader, endheader.size(), dta);

    /* <map> ... </map> */
    map(1) = dta.tellg();
    writestr(startmap, startmap.size(), dta);
    for (int32_t i = 0; i <14; ++i)
    {
      uint64_t nmap = 0;
      writebin(nmap, dta, swapit);
    }
    writestr(endmap, endmap.size(), dta);

    /* <variable_types> ... </variable_types> */
    map(2) = dta.tellg();
    writestr(startvart, startvart.size(), dta);
    uint16_t nvartype;
    for (uint32_t i = 0; i < k; ++i)
    {
      nvartype = as<uint16_t>(vartypes[i]);

      writebin(nvartype, dta, swapit);
    }
    writestr(endvart, endvart.size(), dta);


    /* <varnames> ... </varnames> */
    map(3) = dta.tellg();
    writestr(startvarn, startvarn.size(), dta);
    for (uint32_t i = 0; i < k; ++i )
    {
      string nvarname = as<string>(nvarnames[i]);
      nvarname[nvarname.size()] = '\0';

      if (nvarname.size() > nvarnameslen)
        Rcpp::warning("Varname to long. Resizing. Max size is %d",
                      nvarnameslen - 1);

      writestr(nvarname, nvarnameslen, dta);
    }
    writestr(endvarn, endvarn.size(), dta);


    /* <sortlist> ... </sortlist> */
    map(4) = dta.tellg();
    writestr(startsor, startsor.size(), dta);

    uint64_t big_k = k+1;

    for (uint64_t i = 0; i < big_k; ++i)
    {
      uint32_t nsortlist = 0;
      
      if ((release == 117) | (release == 118)) {
        writebin((uint16_t)nsortlist, dta, swapit);
      }
      if (release == 119) {
        writebin(nsortlist, dta, swapit);
      }
    }
    writestr(endsor, endsor.size(), dta);


    /* <formats> ... </formats> */
    map(5) = dta.tellg();
    writestr(startform, startform.size(), dta);
    for (uint32_t i = 0; i < k; ++i )
    {
      string nformats = as<string>(formats[i]);

      if (nformats.size() >= nformatslen)
        Rcpp::warning("Formats to long. Resizing. Max size is %d",
                      nformatslen);

      writestr(nformats, nformatslen, dta);
    }
    writestr(endform, endform.size(), dta);


    /* <value_label_names> ... </value_label_names> */
    map(6) = dta.tellg();
    writestr(startvalLabel, startvalLabel.size(), dta);
    for (uint32_t i = 0; i < k; ++i)
    {
      string nvalLabels = as<string>(valLabels[i]);
      nvalLabels[nvalLabels.size()] = '\0';

      if (nvalLabels.size() > nvalLabelslen)
        Rcpp::warning("Vallabel to long. Resizing. Max size is %d",
                      nvalLabelslen - 1);

      writestr(nvalLabels, nvalLabelslen, dta);
    }
    writestr(endvalLabel, endvalLabel.size(), dta);


    /* <variable_labels> ... </variable_labels> */
    map(7) = dta.tellg();
    writestr(startvarlabel, startvarlabel.size(), dta);
    for (uint32_t i = 0; i < k; ++i)
    {
      if (!Rf_isNull(varLabels) && Rf_length(varLabels) > 1) {
        string nvarLabels = as<string>(varLabels[i]);

        if (nvarLabels.size() > nvarLabelslen)
          Rcpp::warning("Varlabel to long. Resizing. Max size is %d",
                        nvarLabelslen - 1);

        nvarLabels[nvarLabels.size()] = '\0';
        writestr(nvarLabels, nvarLabelslen, dta);
      } else {
        string nvarLabels = "";
        nvarLabels[nvarLabels.size()] = '\0';
        writestr(nvarLabels, nvarLabelslen, dta);
      }
    }
    writestr(endvarlabel, endvarlabel.size(), dta);


    /* <characteristics> ... </characteristics> */
    map(8) = dta.tellg();
    writestr(startcharacteristics, startcharacteristics.size(), dta);
    /* <ch> ... </ch> */

    if (chs.size()>0){
      for (int32_t i = 0; i<chs.size(); ++i){

        writestr(startch, startch.size(), dta);

        CharacterVector ch = as<CharacterVector>(chs[i]);

        string ch1 = as<string>(ch[0]);
        ch1[ch1.size()] = '\0';
        string ch2 = as<string>(ch[1]);
        ch2[ch2.size()] = '\0';
        string ch3 = as<string>(ch[2]);
        ch3[ch3.size()] = '\0';

        uint32_t nnocharacter = chlen*2 + ch3.size() +1;
        writebin(nnocharacter, dta, swapit);

        writestr(ch1, chlen, dta);
        writestr(ch2, chlen, dta);
        writestr(ch3,ch3.size()+1, dta);

        writestr(endch, endch.size(), dta);
      }
    }

    writestr(endcharacteristics, endcharacteristics.size(), dta);


    /* <data> ... </data> */
    map(9) = dta.tellg();
    writestr(startdata, startdata.size(), dta);

    IntegerVector V, O;
    CharacterVector STRL;

    for(uint64_t j = 0; j < n; ++j)
    {
      for (uint32_t i = 0; i < k; ++i)
      {
        int const type = vartypes[i];
        switch(type < 2046 ? 2045 : type)
        {
          // store numeric as Stata double (double)
        case 65526:
        {
          double val_d = 0;

          val_d = as<NumericVector>(dat[i])[j];

          if ( (val_d == NA_REAL) | R_IsNA(val_d) | R_IsNaN(val_d) | std::isinf(val_d) )
            val_d = STATA_DOUBLE_NA;

          writebin(val_d, dta, swapit);

          break;
        }
          // float
        case 65527:
        {
          double val_d = 0;
          float  val_f = 0;

          val_d = as<NumericVector>(dat[i])[j];

          if ( (val_d == NA_REAL) | (R_IsNA(val_d)) | R_IsNaN(val_d) | std::isinf(val_d) )
            val_f = STATA_FLOAT_NA;
          else
            val_f = (double)(val_d);

          writebin(val_f, dta, swapit);

          break;
        }
          // store integer as Stata long (int32_t)
        case 65528:
        {
          int32_t val_l = 0;

          val_l = as<IntegerVector>(dat[i])[j];

          if ( (val_l == NA_INTEGER) | (R_IsNA(val_l)) | R_IsNaN(val_l) | std::isinf(val_l) )
            val_l = STATA_INT_NA;

          writebin(val_l, dta, swapit);

          break;
        }
          // int
        case 65529:
        {
          int16_t val_i = 0;
          int32_t val_l = 0;

          val_l = as<IntegerVector>(dat[i])[j];

          if (val_l == NA_INTEGER)
            val_i = STATA_SHORTINT_NA;
          else
            val_i = val_l;

          writebin(val_i, dta, swapit);

          break;
        }
          // byte
        case 65530:
        {
          int8_t  val_b = 0;
          int32_t val_l = 0;

          val_l = as<IntegerVector>(dat[i])[j];

          if (val_l == NA_INTEGER)
            val_b = STATA_BYTE_NA;
          else
            val_b = val_l;

          writebin(val_b, dta, swapit);

          break;
        }
        // str
        case 2045:
        {
          int32_t const len = vartypes[i];

          string val_s = as<string>(as<CharacterVector>(dat[i])[j]);

          if (val_s == "NA")
            val_s.clear();

          writestr(val_s, len, dta);
          break;
        }
        // strL
        case 32768:
        {
          /* Stata uses +1 */
          int64_t z = 0;

          CharacterVector b = as<CharacterVector>(dat[i]);
          const string val_strl = as<string>(b[j]);
          if (!val_strl.empty())
          {
            switch (release)
            {
            case 117:
          {
            uint32_t v = i+1, o = j+1;

            writebin(v, dta, swapit);
            writebin(o, dta, swapit);

            // push back every v, o and val_strl
            V.push_back(v);
            O.push_back(o);
            break;
          }
            case 118:
          {
            int16_t v = i+1;
            int64_t o = j+1;
            char    z[8];

            // push back every v, o and val_strl
            V.push_back(v);
            O.push_back(o);

            // z is 'vv-- ----'
            memcpy(&z[0], &v, sizeof(v));
            if (SBYTEORDER == 1) {
              o <<= 16;
            }
            memcpy(&z[2], &o, 6);
            // z is 'vvoo oooo'

            dta.write((char*)&z, sizeof(z));
            // writestr((char*)&z, sizeof(z), dta);

            break;
          }
            case 119:
          {
            int32_t v = i+1;
            int64_t o = j+1;
            char    z[8];

            // push back every v, o and val_strl
            V.push_back(v);
            O.push_back(o);

            // z is 'vv-- ----'
            memcpy(&z[0], &v, sizeof(v));
            if (SBYTEORDER == 1) {
              o <<= 24;
            }
            memcpy(&z[3], &o, 5);
            // z is 'vvvo oooo'

            dta.write((char*)&z, sizeof(z));
            // writestr((char*)&z, sizeof(z), dta);

            break;
          }
            }
            STRL.push_back(val_strl);
          } else {
            writestr((char*)&z, sizeof(z), dta);
          }
          break;
        }
        }
      }
    }
    writestr(enddata, enddata.size(), dta);


    /* <strls> ... </strls> */
    map(10) = dta.tellg();
    writestr(startstrl, startstrl.size(), dta);

    int32_t strlsize = STRL.length();
    for(int i =0; i < strlsize; ++i )
    {
      const string gso = "GSO";
      int32_t v = V[i];
      int64_t o = O[i];
      uint8_t t = 129; //Stata binary type, no trailing zero.
      const string strL = as<string>(STRL[i]);
      uint32_t len = strL.size();

      writestr(gso, gso.size(), dta);
      writebin(v, dta, swapit);
      if (release == 117)
        writebin((uint32_t)o, dta, swapit);
      if ((release == 118) | (release == 119))
        writebin(o, dta, swapit);
      writebin(t, dta, swapit);
      writebin(len, dta, swapit);
      writestr(strL, strL.size(), dta);
    }

    writestr(endstrl, endstrl.size(), dta);


    /* <value_labels> ... </value_labels> */
    map(11) = dta.tellg();
    writestr(startvall, startvall.size(), dta);
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
          uint32_t labellen = label.size()+1;
          if (labellen > maxlabelsize+1)
            labellen = maxlabelsize+1;

          txtlen += labellen;
          off.push_back ( txtlen-labellen );
        }

        int32_t offI, labvalueI;

        int32_t nlen = sizeof(N) + sizeof(txtlen) + sizeof(offI)*N +
          sizeof(labvalueI)*N + txtlen;

        writestr(startlbl, startlbl.size(), dta);
        writebin(nlen, dta, swapit);
        writestr(labname, lbllen, dta);
        writestr((char*)&padding, 3, dta);
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

          if (labtext.size() > maxlabelsize)
          {
            Rcpp::warning("Label to long. Resizing. Max size is %d",
                          maxlabelsize);
            labtext.resize(maxlabelsize);
            // labtext[labtext.size()] = '\0';
          }

          writestr(labtext, labtext.size()+1, dta);
        }
        writestr(endlbl, endlbl.size(), dta);
      }

    }
    writestr(endvall, endvall.size(), dta);


    /* </stata_data> */
    map(12) = dta.tellg();
    writestr(end, end.size(), dta);


    /* end-of-file */
    map(13) = dta.tellg();


    /* seek up to <map> to rewrite it*/
    /* <map> ... </map> */
    dta.seekg(map(1));
    writestr(startmap, startmap.size(), dta);
    for (int i=0; i <14; ++i)
    {
      uint64_t nmap = 0;
      uint32_t hi = 0, lo = 0;

      nmap = map(i);
      hi = (nmap >> 32);
      lo = nmap;

      if (SBYTEORDER == 2) { // LSF
        writebin(lo, dta, swapit);
        writebin(hi, dta, swapit);
      } else {               // MSF
        writebin(hi, dta, swapit);
        writebin(lo, dta, swapit);
      }
    }
    writestr(endmap, endmap.size(), dta);

    dta.close();
    return 0;
  }
  else {
    throw std::range_error("Unable to open file.");
    return -1;
  }
}
