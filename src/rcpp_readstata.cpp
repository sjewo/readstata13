#include <Rcpp.h>
#include <stdio.h>
#include <iostream>
#include "string"

using namespace Rcpp;
using namespace std;

//' Reads the binary Stata file
//'
//' @param filePath The full systempath to the dta file you want to import.
//' @import Rcpp
//' @export 
// [[Rcpp::export]]
List stata(const char * filePath)
{
  FILE *file = NULL;		// File pointer
  
  // Open the file in binary mode using the "rb" format string
  // This also checks if the file exists and/or can be opened for reading correctly
  if ((file = fopen(filePath, "rb")) == NULL)
      throw std::range_error("Could not open specified file.");
  
  // check the first byte. continue if "<"
  char one[2];
  if (fgets (one, 2 , file) == NULL) 
    perror ("Error reading bytorder");
  string const two = "<";
  if (one != two)
    throw std::range_error("Not a version 13 dta-file.");
  
  fseek(file, 27, SEEK_CUR);
  
  // release
  string const gversion = "117";
  char release [4];
  if (fgets (release, 4 , file) == NULL) 
    perror ("Error reading release");
  
  // check the release version. continue if "117"
  if (release != gversion)
    throw std::range_error("Not a version 13 dta-file.");
  
  fseek(file, 21, SEEK_CUR);
  
  // LSF or MSF
  char byteorder [4];
  if (fgets (byteorder, 4 , file) == NULL) 
    perror ("Error reading bytorder");
  
  fseek(file, 15, SEEK_CUR);
  
  // Number of Variables
  unsigned short k;
  if (fread (&k, sizeof(unsigned short) , 1, file) == 0) 
    perror ("Error reading number of variables");
  
  fseek(file, 7, SEEK_CUR); //</K><N>
  
  //Number of Observations
  unsigned int n;
  if (fread (&n, sizeof(unsigned int), 1, file) == 0) 
    perror ("Error reading number of cases");
  
  fseek(file, 11, SEEK_CUR); //</N><label>
  
  // char length dataset label
  unsigned char ndlabel;
  if (fread (&ndlabel, sizeof(unsigned char), 1, file) == 0) 
    perror ("Error reading length of dataset label");
  
  char datalabel [ndlabel+1];
  if (ndlabel>0)
  {
    if (fgets (datalabel, ndlabel+1 , file) == NULL) 
      perror ("Error reading dataset label");
  }
  
  fseek(file, 19, SEEK_CUR); //</label><timestamp>
  
  
  // timestamp
  unsigned char ntimestamp;
  if (fread (&ntimestamp, sizeof(unsigned char), 1, file) == 0) 
    perror ("Error reading length of timestamp");
  
  fseek(file, 1, SEEK_CUR); //binary zero
  
  char timestamp [ntimestamp];
  if (ntimestamp == 17) // ntimestap is 0 or 17
  {
    if (fgets (timestamp, ntimestamp , file) == NULL) 
      perror ("Error reading timestamp");
  }
  
  
  fseek(file, 26, SEEK_CUR); //</timestamp></header><map>
  
  // map
  NumericVector map(14);
  for (int i=0; i <14; i++)
  {
    long long nmap;
    if (fread (&nmap, sizeof(long long) , 1, file) == 0) 
      perror ("Error reading mapping");
    map[i] = nmap;
  }
  
  fseek(file, 22, SEEK_CUR); //</map><variable_types>
  
  //vartypes
  NumericVector vartype(k);
  for (int i=0; i<k; i++)
  {
    unsigned short nvartype;
    if (fread (&nvartype, sizeof(unsigned short), 1, file) == 0) 
      perror ("Error reading vartypes");
    vartype[i] = nvartype;
  }
  
  fseek(file, 27, SEEK_CUR); //</variable_types><varnames> 
  
  //varnames
  CharacterVector varnames(k);
  for (int i=0; i<k; i++)
  {
    char nvarnames [33+1];
    if ( fgets (nvarnames, 33+1 , file) != NULL )
      varnames[i] = nvarnames;
  }
  
  fseek(file, 21, SEEK_CUR); //</varnames><sortlist>
  
  // byte order
  string const s = "LSF";
  // byteorder == "LSF" 
  if (byteorder == s)
  {
    NumericVector sortlist(k+1);
    for (int i=0; i<k+1; i++)
    {
      unsigned short nsortlist;
      if (fread (&nsortlist, sizeof(unsigned short), 1, file) == 0) 
        perror ("Error reading sortlist");
      sortlist[i] = nsortlist;
    }
  } else {
    throw std::range_error("MSF File found, please mail authors.");
  }
  
  fseek(file, 20, SEEK_CUR); //</sortlist><formats> 
  
  //formats
  CharacterVector formats(k);
  for (int i=0; i<k; i++)
  {
    char nformats[49+1];
    if ( fgets (nformats, 49+1 , file) != NULL )
      formats[i] = nformats;
  }
  
  fseek(file, 29, SEEK_CUR); //</formats><value_label_names>
  
  //value_label_names
  CharacterVector valLabels(k);
  for (int i=0; i<k; i++)
  {
    char nvalLabels[33+1];
    if ( fgets (nvalLabels, 33+1 , file) != NULL )
      valLabels[i] = nvalLabels;
  }
  fseek(file, 37, SEEK_CUR); //</value_label_names><variable_labels>
  
  // variabel_labels
  CharacterVector varLabels(k);
  for (int i=0; i<k; i++)
  {
    char nvarLabels[81+1];
    if ( fgets (nvarLabels, 81+1 , file) != NULL )
      varLabels[i] = nvarLabels;
  }
  
  fseek(file, 35, SEEK_CUR); //</variable_labels><characteristics> 
  
  // characteristics
  string const c = "</ch";
  
  List ch = List();
  
  
  char tago[4+1];
  if (fgets (tago, 4+1 , file) == NULL) 
    perror ("Error reading characteristics");
  if (tago != c)
  {
    CharacterVector chs(3);
    unsigned int nocharacter;
    if (fread (&nocharacter, sizeof(int), 1, file) == 0) 
      perror ("Error reading length of characteristics");
    
    char nnocharacter[nocharacter+1-66];
    char chvarname[33+1];
    char chcharact[33+1];
    // FixMe: two lines of notes.
    if ( (fgets(chvarname,33+1,file) == NULL) &
         (fgets(chcharact,33+1,file) == NULL) & 
         (fgets(nnocharacter,nocharacter+1-66,file) == NULL) 
       )
      perror ("Error reading characteristics");
    chs[0] = chvarname;
    chs[1] = chcharact;
    chs[2] = nnocharacter;
    
    ch.push_front( chs );
    // 5 more bytes
    fseek(file, 5, SEEK_CUR);
    
    char tagi[4+1];
    if (fgets (tagi, 4+1 , file) == NULL)
      perror ("Error reading characteristics");
    if (tagi != c){
      if (fread (&nocharacter, sizeof(int), 1, file) == 0)
        perror ("Error reading length of characteristics");
      if ( (fgets(chvarname,33+1,file) == NULL) &
           (fgets(chcharact,33+1,file) == NULL) & 
           (fgets(nnocharacter,nocharacter+1-66,file) == NULL) 
         )
        perror ("Error reading characteristics");
      // new chs vector
      CharacterVector chs(3);
      chs[0] = chvarname;
      chs[1] = chcharact;
      chs[2] = nnocharacter;
      
      ch.push_front( chs );
      // 5 more bytes
      fseek(file, 5, SEEK_CUR);
      
      if (fgets (tagi, 4+1 , file) == NULL)
      perror ("Error reading characteristics");
    }
  }
  
  fseek(file, 20, SEEK_CUR); //aracteristics><data>
  
  // build list and add vector of right type for each variable
  List df(k);
  for (int i=0;i<k;i++)
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
  for(unsigned int j=0; j<n; j++)
  {
    for (int i=0; i<k; i++)
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
          if ((erg<dmin) | (erg>dmax))
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
          if ((erg<(-minmax)) | (erg>minmax))
            as<NumericVector>(df[i])[j] = NA_REAL;
          else
            as<NumericVector>(df[i])[j] = erg;
          break;
        } 
        //long
        case 65528:
        {
          signed int erg;
          if (fread (&erg, sizeof(signed int), 1, file) == 0) 
            perror ("Error reading data");
          if ((erg<(-2147483647)) | (erg>2147483620))
            as<IntegerVector>(df[i])[j] = NA_REAL;
          else
            as<IntegerVector>(df[i])[j] = erg;
          break;
        } 
        // int
        case 65529: 
        {
          short int erg;
          if (fread (&erg, sizeof(short int), 1, file) == 0) 
            perror ("Error reading data");
          if ((erg<(-32767)) | (erg>32740))
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
          if ((erg<(-127)) | (erg>100))
            as<IntegerVector>(df[i])[j] = NA_REAL;
          else
            as<IntegerVector>(df[i])[j] = erg;
          break;
        }
        // strings with 2045 or fewer characters
        case 2045:
        { 
          int gre = vartype[i]+1;
          char erg[gre];
          if (fgets (erg, gre , file) == NULL) 
            perror ("Error reading data");
          as<CharacterVector>(df[i])[j] = erg;
          break;
        } 
        // string of any length
        case 32768:
        {// strL 2 4bit
        int v;
        if (fread (&v, sizeof(int), 1, file) == 0) 
          perror ("Error reading strl");
        int o;
        if (fread (&o, sizeof(int), 1, file) == 0) 
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
  if (fgets(tags,4,file) == NULL) 
    perror ("Error reading tags");
  string const gso = "GSO";
  if (tags == gso)
  {
    while(tags == gso)
    {
      
      CharacterVector strls(2);
      int v, o;
      
      // 2x4 bit (strl[vo1,vo2])
      if (fread(&v, sizeof(int), 1, file) == 0) 
        perror ("Error reading strL v");
      if (fread(&o, sizeof(int), 1, file) == 0) 
        perror ("Error reading strL o");
      char erg[22];
      sprintf(erg, "%010d%010d", v, o);
      
      strls(0) = erg;
    
      unsigned char t; 
      // (129 = binary) | (130 = ascii)
      if (fread(&t, sizeof(unsigned char), 1, file) == 0)
        perror ("Error reading strL type");
      
      unsigned int len;
      if (fread(&len, sizeof(unsigned int), 1, file) == 0)
        perror ("Error reading strL length");
      
      if (t==129)
      {
        char strl [len];
        if (fgets (strl, len , file) == NULL) 
          perror ("Error reading strL");
        strls(1) = strl;
      } else
      {
        if (t==130)
        {
          char strl [len+1];
          if (fgets (strl, len+1 , file) == NULL) 
            perror ("Error reading strL");
          strls(1) = strl;
        }
      }
      
      strlstable.push_back( strls );
      
      if (fgets(tags, 4, file) == NULL) 
        perror ("Error reading tags");
    }
  }
  
  
  // after strls
  fseek(file, 19, SEEK_CUR); //trls><value_labels>
  
  // Value Labels  
  List labelList = List(); //put labels into this list
  char tag[6];
  if (fgets(tag, 6, file) == NULL) 
    perror ("Error reading label tag");
  string const lbltag = "<lbl>" ; 
  if (tag == lbltag) {
    while(tag == lbltag) {
      
      // length of value_label_table
      int nlen;
      if (fread (&nlen, sizeof(int), 1, file) == 0)
        perror ("Error reading  length of value_label_table");
      
      // name of this label set
      char nlabname[33];
      if (fgets(nlabname, 33, file) == NULL) 
        perror ("Error reading labelname");
      
      //padding
      fseek(file, 3+1, SEEK_CUR);
      
      // value_label_table for actual label set
      int labn;
      if (fread(&labn, sizeof(int), 1, file) == 0)
        perror ("Error reading length of label set entry");
      
      int txtlen;
      if (fread(&txtlen, sizeof(int), 1, file) == 0)
        perror ("Error reading length of label text");
      
      // offset for each label
      // off0 : label 0 starts at off0
      // off1 : label 1 starts at off1 ...
      NumericVector off(labn+1);
      for (int i=0; i < labn; i++) {
        int noff;
        if (fread(&noff, sizeof(int), 1, file) == 0)
          perror ("Error reading label offset");
        off[i] = noff;
      }
      off[labn] = txtlen; // needs txtlen for loop
      
      // code for each label
      NumericVector code(labn);
      for (int i=0; i < labn; i++) {
        int val;
        if (fread(&val, sizeof(int), 1, file) == 0)
          perror ("Error reading label code");
        code[i] = val;
      }    
      
      // label text
      CharacterVector label(labn);
      for (int i=0; i < labn; i++) {
        int const lablen = off[i+1]-off[i]+1;
        char lab[lablen];
        if (fgets(lab, lablen, file) == NULL) perror ("Error reading label");
        label[i] = lab;
      }
      
      // create list for actual label set
      string const labset = nlabname;
      List lset = List();
      lset["code"] = code;
      lset["label"] = label;
      
      // add this set to output list
      labelList[labset] = lset;
      
      fseek(file, 6, SEEK_CUR); //</lbl>
      
      if (fgets(tag, 6, file) == NULL) perror ("Error reading label tag"); // next <lbl>?
      
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
  //  may also include:
  //    "label.table"
  //    "expansion.table"
  ddf.attr("datalabel") = datalabelCV;
  ddf.attr("time.stamp") = timestampCV;
  ddf.attr("formats") = formats;
  ddf.attr("types") = vartype;
  ddf.attr("val.labels") = valLabels;
  ddf.attr("var.labels") = varLabels;
  ddf.attr("version") = version;
  ddf.attr("label") = labelList;
  ddf.attr("expansion.field") = ch;
  ddf.attr("strl") = strlstable;
  
  fclose(file);   
  return ddf;
}
