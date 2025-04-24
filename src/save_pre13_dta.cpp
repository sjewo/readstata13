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

#include <readstata.h>

using namespace Rcpp;
using namespace std;

// Writes the binary Stata file
//
// @param filePath The full systempath to the dta file you want to export.
// @param dat an R-Object of class data.frame.
// @export
// [[Rcpp::export]]
int stata_pre13_save(const char * filePath, Rcpp::DataFrame dat)
{

  uint16_t k = dat.size();
  uint32_t n = dat.nrows();
  int8_t byteorder = SBYTEORDER;

  string timestamp = dat.attr("timestamp");
  timestamp.resize(18);
  string datalabel = dat.attr("datalabel");
  datalabel[datalabel.size()] = '\0';

  CharacterVector valLabels = dat.attr("vallabels");
  CharacterVector nvarnames = dat.attr("names");

  List chs = dat.attr("expansion.fields");
  List formats = dat.attr("formats");
  List labeltable = dat.attr("label.table");
  List varLabels = dat.attr("var.labels");
  List vartypes = dat.attr("types");

  int8_t version = as<int>(dat.attr("version"));


  fstream dta (filePath, ios::out | ios::binary);
  if (dta.is_open())
  {

    uint32_t ndlabel = 81;
    uint32_t nformatslen = 49;
    uint32_t nvarnameslen = 33;
    uint32_t nvalLabelslen = 33;
    uint32_t nvarLabelslen = 81;
    uint32_t chlen = 33;
    uint32_t maxlabelsize = 32000;
    uint32_t maxstrsize = 244;
    if (version<111 || version==112)
      maxstrsize = 80;

    switch(version)
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
    case 106:// unknown version (SE?)
      chlen = 9;
      ndlabel = 32;
      nvarnameslen = 9;
      nformatslen = 12;
      nvalLabelslen = 9;
      nvarLabelslen = 32;
      break;
    case 107: // unknown version (SE?)
    case 108:
      chlen = 9;
      nvarnameslen = 9;
      nformatslen = 12;
      nvalLabelslen = 9;
    case 110:
    case 111:
    case 112:
    case 113:
      nformatslen = 12;
      break;
    }

    writebin(version, dta, swapit);   // format
    writebin(byteorder, dta, swapit); // LSF
    int8_t ft = 1;                    // filetype
    writebin(ft, dta, swapit);
    int8_t unused = 0;                // unused
    writebin(unused, dta, swapit);
    writebin(k, dta, swapit);         // nvars
    writebin(n, dta, swapit);         // nobs

    /* write a datalabel */
    if (datalabel.size() > ndlabel)
      Rcpp::warning("Datalabel too long. Resizing. Max size is %d.",
                    ndlabel - 1);

    writestr(datalabel, ndlabel, dta);

    /* timestamp size is 17 */
    if (version > 104)
    {
      if (timestamp.size() > 18)
      {
        Rcpp::warning("Timestamp too long. Dropping.");
        timestamp = "";
      }
      writestr(timestamp, timestamp.size(), dta);
    }

    /* <variable_types> ... </variable_types> */
    uint8_t  nvartype;
    for (uint16_t i = 0; i < k; ++i)
    {
      nvartype = as<uint8_t>(vartypes[i]);
      if(version<111 || version==112)
      {
        char c[2];

        switch(nvartype)
        {
        case 255:
          strcpy(c, "d");
          c[1] = '\0';
          dta.write(c, 1);
          break;
        case 254:
          strcpy(c, "f");
          c[1] = '\0';
          dta.write(c, 1);
          break;
        case 253:
          strcpy(c, "l");
          c[1] = '\0';
          dta.write(c, 1);
          break;
        case 252:
          strcpy(c, "i");
          c[1] = '\0';
          dta.write(c, 1);
          break;
        case 251:
          strcpy(c,"b");
          c[1] = '\0';
          dta.write(c, 1);
          break;
        default:
          char d = char(nvartype+127);
        dta.write(&d, 1);
        break;
        }
      }
      else
        writebin(nvartype, dta, swapit);
    }

    /* <varnames> ... </varnames> */
    for (uint16_t i = 0; i < k; ++i )
    {
      string nvarname = as<string>(nvarnames[i]);

      if (nvarname.size() > nvarnameslen)
        Rcpp::warning("Varname too long. Resizing. Max size is %d",
                      nvarnameslen - 1);

      writestr(nvarname, nvarnameslen, dta);
    }

    /* <sortlist> ... </sortlist> */
    uint32_t big_k = k+1;

    for (uint32_t i = 0; i < big_k; ++i)
    {
      uint16_t nsortlist = 0;
      writebin(nsortlist, dta, swapit);
    }

    /* <formats> ... </formats> */
    for (uint16_t i = 0; i < k; ++i )
    {
      string nformats = as<string>(formats[i]);

      if (nformats.size() > nformatslen)
        Rcpp::warning("Formats too long. Resizing. Max size is %d",
                      nformatslen - 1);

      writestr(nformats, nformatslen, dta);
    }

    /* <value_label_names> ... </value_label_names> */
    for (uint16_t i = 0; i < k; ++i )
    {
      string nvalLabels = as<string>(valLabels[i]);

      if (nvalLabels.size() > nvalLabelslen)
        Rcpp::warning("Vallabel too long. Resizing. Max size is %d",
                      nvalLabelslen - 1);

      writestr(nvalLabels, nvalLabelslen, dta);
    }

    /* <variable_labels> ... </variable_labels> */
    for (uint16_t i = 0; i < k; ++i)
    {
      string nvarLabels = "";
      if (!Rf_isNull(varLabels) && Rf_length(varLabels) > 1)
      {
        nvarLabels = as<std::string>(varLabels[i]);

        if (nvarLabels.size() > nvarLabelslen)
          Rcpp::warning("Varlabel too long. Resizing. Max size is %d",
                        nvarLabelslen - 1);
      }
      writestr(nvarLabels, nvarLabelslen, dta);
    }


    /* <characteristics> ... </characteristics> */

    if (version > 104)
    {
      int8_t datatype = 0;
      uint32_t len = 0;

      if (chs.size()>0) {
        for (int32_t i = 0; i<chs.size(); ++i) {

          CharacterVector ch = as<CharacterVector>(chs[i]);

          string ch1 = as<string>(ch[0]);
          ch1[ch1.size()] = '\0';
          string ch2 = as<string>(ch[1]);
          ch2[ch2.size()] = '\0';
          string ch3 = as<string>(ch[2]);
          ch3[ch3.size()] = '\0';

          len = chlen + chlen + ch3.size()+1;
          datatype = 1;

          writebin(datatype, dta, swapit);
          if(version<=108)
            writebin((int16_t)len, dta, swapit);
          else
            writebin(len, dta, swapit);

          writestr(ch1, chlen, dta);
          writestr(ch2, chlen, dta);
          writestr(ch3, ch3.size()+1, dta);

        }
      }

      // five bytes of zero end characteristics
      datatype = 0;
      len = 0;
      writebin(datatype, dta, swapit);
      if (version<=108)
        writebin((int16_t)len, dta, swapit);
      else
        writebin(len, dta, swapit);
    }

    /* <data> ... </data> */

    for(uint32_t j = 0; j < n; ++j)
    {
      for (uint16_t i = 0; i < k; ++i)
      {
        int const type = vartypes[i];
        switch(type)
        {
          // store numeric as Stata double (double)
        case 255:
        {
          double val_d = 0;

          val_d = as<NumericVector>(dat[i])[j];

          if ( (val_d == NA_REAL) | R_IsNA(val_d) )
            val_d = STATA_DOUBLE_NA;

          writebin(val_d, dta, swapit);

          break;
        }
          // float
        case 254:
        {
          double val_d = 0;
          float  val_f = 0;

          val_d = as<NumericVector>(dat[i])[j];

          if ((val_d == NA_REAL) | (R_IsNA(val_d)) )
            val_f = STATA_FLOAT_NA;
          else
            val_f = (float)(val_d);

          writebin(val_f, dta, swapit);

          break;
        }
          // store integer as Stata long (int32_t)
        case 253:
        {
          int32_t val_l = 0;

          val_l = as<IntegerVector>(dat[i])[j];

          if ( (val_l == NA_INTEGER) | (R_IsNA(val_l)) )
          {
            if(version>111)
              val_l = STATA_INT_NA;
            else
              val_l = STATA_INT_NA_108;
          }

          writebin(val_l, dta, swapit);

          break;
        }
          // int
        case 252:
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
        case 251:
        {
          int8_t  val_b = 0;
          int32_t val_l = 0;

          val_l = as<IntegerVector>(dat[i])[j];

          if (val_l == NA_INTEGER) {
            if (version>104)
              val_b = STATA_BYTE_NA;
            else
              val_b = STATA_BYTE_NA_104;
          } else {
            val_b = val_l;
          }

          writebin(val_b, dta, swapit);

          break;
        }
        default:
        {
          int32_t len = vartypes[i];

          CharacterVector cv_s = NA_STRING;
          cv_s = as<CharacterVector>(dat[i])[j];

          std::string val_s = "";

          if (cv_s[0] != NA_STRING)
            val_s = as<std::string>(cv_s);

          // Stata 6-12 can only store 244 byte strings
          if(val_s.size()>maxstrsize)
          {
            Rcpp::warning("Character value too long. Resizing. Max size is %d.",
                          maxstrsize);
          }

          writestr(val_s, len, dta);
          break;
        }

        }
      }
    }


    /* <value_labels> ... </value_labels> */
    if ((labeltable.size()>0) & (version>105))
    {

      CharacterVector labnames = labeltable.attr("names");
      int8_t padding = 0;

      for (int32_t i=0; i < labnames.size(); ++i)
      {
        int32_t txtlen = 0;

        string labname = as<string>(labnames[i]);
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

        writebin(nlen, dta, swapit);

        writestr(labname, nvarnameslen, dta);
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
            Rcpp::warning("Label too long. Resizing. Max size is %d",
                          maxlabelsize);
            labtext.resize(maxlabelsize);
            // labtext[labtext.size()] = '\0';
          }

          writestr(labtext, labtext.size()+1, dta);
        }
      }

    }

    dta.close();
    return 0;
  }
  else {
    Rcpp::stop("Unable to open file.");
    return -1;
  }
}
