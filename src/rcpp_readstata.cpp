#include <Rcpp.h>
#include "string"
#include <stdint.h>
// #include <cstdint> //C++11

using namespace Rcpp;
using namespace std;

//' Reads the binary Stata file
//'
//' @param filePath The full systempath to the dta file you want to import.
//' @param missing logical if missings should be converted outside of Rcpp.
//' @import Rcpp
//' @export
// [[Rcpp::export]]
List stata(const char * filePath, bool missing)
{
  FILE *file = NULL;    // File pointer

  // Open the file in binary mode using the "rb" format string
  // This also checks if the file exists and/or can be opened for reading correctly
  if ((file = fopen(filePath, "rb")) == NULL)
    throw std::range_error("Could not open specified file.");

  // check the first byte. continue if "<"
  char one[2];
  if (!fread(one, sizeof(one),1, file))
    perror ("Error reading bytorder");
  one[1] = '\0';

  string const two = "<";
  if (one != two)
    throw std::range_error("Not a version 13 dta-file.");

  fseek(file, 26, SEEK_CUR);

  // release
  string const gversion = "117";
  char release [4];
  if (!fread(release, sizeof(release), 1, file))
    perror ("Error reading release");
  release[3] = '\0';

  // check the release version. continue if "117"
  if (release != gversion)
    throw std::range_error("Not a version 13 dta-file.");

  fseek(file, 20, SEEK_CUR);

  // LSF or MSF
  char byteorder [4];
  if (!fread(&byteorder, sizeof(byteorder), 1, file))
    perror ("Error reading bytorder");
  byteorder[3] = '\0';

  fseek(file, 14, SEEK_CUR);

  // Number of Variables
  uint16_t k;
  if (!fread (&k, sizeof(k) , 1, file))
    perror ("Error reading number of variables");

  fseek(file, 7, SEEK_CUR); //</K><N>

  // Number of Observations
  uint32_t n;
  if (!fread (&n, sizeof(n), 1, file))
    perror ("Error reading number of cases");

  fseek(file, 11, SEEK_CUR); //</N><label>

  // char length dataset label
  unsigned char ndlabel;
  if (!fread (&ndlabel, sizeof(ndlabel), 1, file))
    perror ("Error reading length of dataset label");

  char datalabel [ndlabel];
  if (ndlabel>0)
  {
    if (!fread(datalabel, ndlabel, 1, file))
      perror ("Error reading dataset label");
    datalabel[ndlabel] = '\0';
  } else {
    datalabel[0] = '\0';
  };

  fseek(file, 19, SEEK_CUR); //</label><timestamp>


  // timestamp
  uint8_t ntimestamp;
  if (!fread (&ntimestamp, sizeof(ntimestamp), 1, file))
    perror ("Error reading length of timestamp");

  char timestamp [ntimestamp];
  if (ntimestamp == 17) // ntimestap is 0 or 17
  {
    if (!fread(timestamp, ntimestamp, 1, file))
      perror ("Error reading timestamp");
    timestamp[ntimestamp] = '\0';
  } else {
    timestamp[0] = '\0';
  };

  fseek(file, 26, SEEK_CUR); //</timestamp></header><map>

  // map
  NumericVector map(14);
  for (int i=0; i <14; ++i)
  {
    uint64_t nmap;
    if (!fread (&nmap, sizeof(nmap) , 1, file))
      perror ("Error reading mapping");
    map[i] = nmap;
  }

  fseek(file, 22, SEEK_CUR); //</map><variable_types>

  //vartypes
  NumericVector vartype(k);
  for (unsigned int i=0; i<k; ++i)
  {
    uint16_t nvartype;
    if (!fread (&nvartype, sizeof(nvartype), 1, file))
      perror ("Error reading vartypes");
    vartype[i] = nvartype;
  }

  fseek(file, 27, SEEK_CUR); //</variable_types><varnames>

  //varnames
  CharacterVector varnames(k);
  for (unsigned int i=0; i<k; ++i)
  {
    char nvarnames [33];
    if (fread (nvarnames, sizeof(nvarnames) ,1 , file))
      varnames[i] = nvarnames;
  }

  fseek(file, 21, SEEK_CUR); //</varnames><sortlist>

  // byte order
  string const s = "LSF";
  // byteorder == "LSF"
  if (byteorder == s)
  {
    NumericVector sortlist(k+1);
    for (unsigned int i=0; i<k+1; ++i)
    {
      uint16_t nsortlist;
      if (!fread (&nsortlist, sizeof(nsortlist), 1, file))
        perror ("Error reading sortlist");
      sortlist[i] = nsortlist;
    }
  } else {
    throw std::range_error("MSF File found, please mail authors.");
  }

  fseek(file, 20, SEEK_CUR); //</sortlist><formats>

  //formats
  CharacterVector formats(k);
  for (unsigned int i=0; i<k; ++i)
  {
    char nformats[49];
    if (fread(nformats, sizeof(nformats), 1 , file))
      formats[i] = nformats;
  }

  fseek(file, 29, SEEK_CUR); //</formats><value_label_names>

  //value_label_names
  CharacterVector valLabels(k);
  for (unsigned int i=0; i<k; ++i)
  {
    char nvalLabels[33];
    if (fread(nvalLabels, sizeof(nvalLabels), 1 , file))
      valLabels[i] = nvalLabels;
  }
  fseek(file, 37, SEEK_CUR); //</value_label_names><variable_labels>

  // variabel_labels
  CharacterVector varLabels(k);
  for (unsigned int i=0; i<k; ++i)
  {
    char nvarLabels[81];
    if (fread(nvarLabels, sizeof(nvarLabels), 1, file))
      varLabels[i] = nvarLabels;
  }

  fseek(file, 35, SEEK_CUR); //</variable_labels><characteristics>

  // characteristics
  string const c = "<ch>";

  List ch = List();
  CharacterVector chs(3);

  char tago[5];
  if (!fread (tago, sizeof(tago)-1,1, file))
    perror ("Error reading characteristics");
  tago[4] = '\0';

  while (tago == c)
  {
    uint32_t nocharacter;
    if (!fread (&nocharacter, sizeof(nocharacter), 1, file))
      perror ("Error reading length of characteristics");

    char chvarname[33];
    char chcharact[33];
    char nnocharacter[nocharacter-66];

    if(
        (!fread(&chvarname, sizeof(chvarname), 1, file)) &
          (!fread(&chcharact, sizeof(chcharact), 1, file)) &
          (!fread(&nnocharacter, sizeof(nnocharacter), 1, file))
    )
      perror ("Error reading characteristics");

    // chs vector
    CharacterVector chs(3);
    chs[0] = chvarname;
    chs[1] = chcharact;
    chs[2] = nnocharacter;

    // add characteristics to the list
    ch.push_back( chs );

    fseek(file, 5, SEEK_CUR); // </ch>

    // read next tag
    if (!fread (tago, sizeof(tago)-1,1, file))
      perror ("Error reading characteristics");
    tago[4] = '\0';
  }

  fseek(file, 20, SEEK_CUR); //aracteristics><data>

  // build list and add vector of right type for each variable
  List df(k);
  for (unsigned int i=0;i<k;++i)
  {
    if (vartype[i] > 32768)
    {
      if (vartype[i] > 65527)
        df[i] = IntegerVector(n);
      else
        df[i] = NumericVector(n);
    }
    else
      df[i] = CharacterVector(n);
  }

  // fill with data
  for(unsigned int j=0; j<n; ++j)
  {
    for (unsigned int i=0; i<k; ++i)
    {
      int const type = vartype[i];
      switch(type < 2046 ? 2045 : type)
      {
        // double
      case 65526:
      {
        double erg;
        double const dmin = -0x1.fffffffffffffp1023;
        double const dmax = 0x1.fffffffffffffp1022;
        if (fread (&erg, sizeof(double), 1, file) == 0)
          perror ("Error reading data");
        if (missing == FALSE & ((erg<dmin) | (erg>dmax)) )
          as<NumericVector>(df[i])[j] = NA_REAL;
        else
          as<NumericVector>(df[i])[j] = erg;
        break;
      }
        // float
      case 65527:
      {
        float erg;
        float const minmax = 0x1.fffffp126;
        if (fread (&erg, sizeof(float), 1, file) == 0)
          perror ("Error reading data");
        if (missing == FALSE & ((erg<(-minmax)) | (erg>minmax)) )
          as<NumericVector>(df[i])[j] = NA_REAL;
        else
          as<NumericVector>(df[i])[j] = erg;
        break;
      }
        //long
      case 65528:
      {
        int32_t erg;
        if (fread (&erg, sizeof(signed int), 1, file) == 0)
          perror ("Error reading data");
        if (missing == FALSE & ((erg<(-2147483647)) | (erg>2147483620)) )
          as<IntegerVector>(df[i])[j] = NA_REAL;
        else
          as<IntegerVector>(df[i])[j] = erg;
        break;
      }
        // int
      case 65529:
      {
        int16_t erg;
        if (fread (&erg, sizeof(short int), 1, file) == 0)
          perror ("Error reading data");
        if (missing == FALSE & ((erg<(-32767)) | (erg>32740)) )
          as<IntegerVector>(df[i])[j] = NA_REAL;
        else
          as<IntegerVector>(df[i])[j] = erg;
        break;
      }
        // byte
      case 65530:
      {
        char erg;
        if (fread (&erg, sizeof(char), 1, file) == 0)
          perror ("Error reading data");
        if (missing == FALSE & ( (erg<(-127)) | (erg>100)) )
          as<IntegerVector>(df[i])[j] = NA_REAL;
        else
          as<IntegerVector>(df[i])[j] = erg;
        break;
      }
        // strings with 2045 or fewer characters
      case 2045:
      {
        int32_t gre = vartype[i];
        char erg[gre];
        if (!fread (erg, gre, 1, file))
          perror ("Error reading data");
        erg[gre] = '\0';
        as<CharacterVector>(df[i])[j] = erg;
        break;
      }
        // string of any length
      case 32768:
      {// strL 2 4bit
        int32_t v;
        if (fread (&v, sizeof(v), 1, file) == 0)
          perror ("Error reading strl");
        int32_t o;
        if (fread (&o, sizeof(o), 1, file) == 0)
          perror ("Error reading strl");
        char erg[22];
        sprintf(erg, "%010d%010d", v, o);
        as<CharacterVector>(df[i])[j] = erg;
        break;
      }
      }
    }
  }

  // attach varnames
  df.attr("names") = varnames;
  fseek(file, 14, SEEK_CUR); //</data><strls>

  //strL
  List strlstable = List(); //put strLs into this list

  char tags[4];
  if (!fread(tags, sizeof(tags)-1, 1, file))
    perror ("Error reading tags");
  tags[3] = '\0';
  string const gso = "GSO";
  if (tags == gso)
  {
    while(tags == gso)
    {

      CharacterVector strls(2);
      int32_t v, o;

      // 2x4 bit (strl[vo1,vo2])
      if (fread(&v, sizeof(v), 1, file) == 0)
        perror ("Error reading strL v");
      if (fread(&o, sizeof(o), 1, file) == 0)
        perror ("Error reading strL o");
      char erg[22];
      sprintf(erg, "%010d%010d", v, o);

      strls(0) = erg;

      uint8_t t;
      // (129 = binary) | (130 = ascii)
      if (fread(&t, sizeof(t), 1, file) == 0)
        perror ("Error reading StrL type");

      uint32_t len;
      if (fread(&len, sizeof(len), 1, file) == 0)
        perror ("Error reading StrL length");

      if (t==129)
      {
        char strl [len];
        if (!fread (strl, len-1, 1, file))
          perror ("Error reading StrL");
        strls(1) = strl;
      } else
      {
        if (t==130)
        {
          char strl [len+1];
          if (!fread (strl, len+1-1, 1, file))
            perror ("Error reading StrL");
          strls(1) = strl;
        }
      }
      strlstable.push_back( strls );

      if (!fread(tags, sizeof(tags)-1, 1, file))
        perror ("Error reading tags");
      tags[3] = '\0';
    }
  }


  // after strls
  fseek(file, 19, SEEK_CUR); //trls><value_labels>

  // Value Labels
  List labelList = List(); //put labels into this list
  char tag[6];
  if (!fread(tag, sizeof(tag)-1, 1, file))
    perror ("Error reading label tag");
  tag[5] = '\0';
  string const lbltag = "<lbl>" ;
  if (tag == lbltag) {
    while(tag == lbltag) {

      // length of value_label_table
      int32_t nlen;
      if (fread (&nlen, sizeof(nlen), 1, file) == 0)
        perror ("Error reading  length of value_label_table");

      // name of this label set
      char nlabname[33];
      if (!fread(nlabname, sizeof(nlabname), 1, file))
        perror ("Error reading labelname");

      //padding
      fseek(file, 3, SEEK_CUR);

      // value_label_table for actual label set
      int32_t labn;
      if (fread(&labn, sizeof(labn), 1, file) == 0)
        perror ("Error reading length of label set entry");

      int32_t txtlen;
      if (fread(&txtlen, sizeof(txtlen), 1, file) == 0)
        perror ("Error reading length of label text");

      // offset for each label
      // off0 : label 0 starts at off0
      // off1 : label 1 starts at off1 ...
      NumericVector off(labn);
      for (int i=0; i < labn; ++i) {
        int32_t noff;
        if (fread(&noff, sizeof(noff), 1, file) == 0)
          perror ("Error reading label offset");
        off[i] = noff;
      }

      // needed for match
      NumericVector laborder = clone(off);
      //laborder.erase(labn+1);
      NumericVector labordersort = clone(off);
      //labordersort.erase(labn+1);
      std::sort(labordersort.begin(), labordersort.end());

      // needs txtlen for loop
      off.push_back(txtlen);

      // sort offsets so we can read labels sequentially
      std::sort(off.begin(), off.end());

      // create an index to sort lables along the code values
      // this is done while factor creation
      NumericVector indx(labn);
      indx = match(laborder,labordersort);

      // code for each label
      NumericVector code(labn);
      for (int i=0; i < labn; ++i) {
        int32_t val;
        if (fread(&val, sizeof(val), 1, file) == 0)
          perror ("Error reading label code");
        code[i] = val;
      }

      // label text
      CharacterVector label(labn);
      for (int i=0; i < labn; ++i) {
        int const lablen = off[i+1]-off[i];
        char lab[lablen];
        if (!fread(lab, lablen,1, file))
          perror ("Error reading label");
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
      code.attr("class") = "table";
      code.attr("laborder") = indx;

      // add this set to output list
      labelList[labset] = code;

      fseek(file, 6, SEEK_CUR); //</lbl>

      if (!fread(tag, sizeof(tag)-1, 1, file))
        perror ("Error reading label tag2"); // next <lbl>?
      tag[5] = '\0';
    }
  }

  // define R character vector for meta data
  CharacterVector datalabelCV(1);
  datalabelCV[0] = datalabel;

  CharacterVector timestampCV(1);
  timestampCV[0] = timestamp;

  CharacterVector version(1);
  version[0] = release;

  // convert list to data.frame
  DataFrame ddf = DataFrame::create(df,  _["stringsAsFactors"] = false );
  // assign attributes
  ddf.attr("datalabel") = datalabelCV;
  ddf.attr("time.stamp") = timestampCV;
  ddf.attr("formats") = formats;
  ddf.attr("types") = vartype;
  ddf.attr("val.labels") = valLabels;
  ddf.attr("var.labels") = varLabels;
  ddf.attr("version") = version;
  ddf.attr("label.table") = labelList;
  ddf.attr("expansion.fields") = ch;
  ddf.attr("strl") = strlstable;

  fclose(file);
  return ddf;
}
