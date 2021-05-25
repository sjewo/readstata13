#
# Copyright (C) 2014-2021 Jan Marvin Garbuszus and Sebastian Jeworutzki
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.

#' Write Stata Binary Files
#'
#' \code{save.dta13} writes a Stata dta-file bytewise and saves the data
#' into a dta-file.
#'
#' @param file \emph{character.} Path to the dta file you want to export.
#' @param data \emph{data.frame.} A data.frame Object.
#' @param data.label \emph{character.} Name of the dta-file.
#' @param time.stamp \emph{logical.} If \code{TRUE}, add a time.stamp to the
#'  dta-file.
#' @param convert.factors \emph{logical.} If \code{TRUE}, factors will be
#'  converted to Stata variables with labels.
#'  Stata expects strings to be encoded as Windows-1252, so all levels will be
#'  recoded.  Character which can not be mapped in Windows-1252 will be saved as
#'  hexcode.
#' @param convert.dates \emph{logical.} If \code{TRUE}, dates will be converted
#'  to Stata date time format. Code from \code{foreign::write.dta}
#' @param convert.underscore \emph{logical.} If \code{TRUE}, all non numerics or
#' non alphabet characters will be converted to underscores.
#' @param tz \emph{character.} time zone specification to be used for 
#'  POSIXct values and dates (if convert.dates is TRUE). ‘""’ is the current 
#'  time zone, and ‘"GMT"’ is UTC  (Universal Time, Coordinated).
#' @param add.rownames \emph{logical.} If \code{TRUE}, a new variable rownames
#'  will be added to the dta-file.
#' @param compress \emph{logical.} If \code{TRUE}, the resulting dta-file will
#'  use all of Statas numeric-vartypes.
#' @param version \emph{numeric.} Stata format for the resulting dta-file either
#'  Stata version number (6 - 16) or the internal Stata dta-format (e.g. 117 for
#'  Stata 13). Experimental support for large datasets: Use version="15mp" to 
#'  save the dataset in the new Stata 15/16 MP file format. This feature is not
#'  thoroughly tested yet.
#' @return The function writes a dta-file to disk. The following features of the
#'  dta file format are supported:
#' \describe{
#'   \item{datalabel:}{Dataset label}
#'   \item{time.stamp:}{Timestamp of file creation}
#'   \item{formats:}{Stata display formats. May be used with
#'   \code{\link[base]{sprintf}}}
#'   \item{type:}{Stata data type (see Stata Corp 2014)}
#'   \item{var.labels:}{Variable labels}
#'   \item{version:}{dta file format version}
#'   \item{strl:}{List of character vectors for the new strL string variable
#'    type. The first element is the identifier and the second element the
#'    string.}
#' }
#' @seealso \code{\link[foreign]{read.dta}} in package \code{foreign} and
#'  \code{memisc} for dta files from Stata versions < 13 and \code{read_dta} in
#'  package \code{haven} for Stata version >= 13.
#' @references Stata Corp (2014): Description of .dta file format
#'  \url{https://www.stata.com/help.cgi?dta}
#' @examples
#' \dontrun{
#'   library(readstata13)
#'   save.dta13(cars, file="cars.dta")
#' } 
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @useDynLib readstata13
#' @export
save.dta13 <- function(data, file, data.label=NULL, time.stamp=TRUE,
                       convert.factors=TRUE, convert.dates=TRUE, tz="GMT",
                       add.rownames=FALSE, compress=FALSE, version=117,
                       convert.underscore=FALSE){


  if (!is.data.frame(data))
    stop("The object \"data\" must have class data.frame")
  if (!dir.exists13(dirname(file)))
    stop("Path is invalid. Possibly a non-existing directory.")

  # Allow writing version as Stata version not Stata format
  if (version=="15mp" | version=="16mp")
    version <- 119
  if (version==15L | version==16L)
    version <- 118
  if (version==14L)
    version <- 118
  if (version==13L)
    version <- 117
  if (version==12L)
    version <- 115
  if (version==11L | version==10L)
    version <- 114
  if (version==9L | version==8L)
    version <- 113
  if (version==7)
    version <- 110
  if (version==6)
    version <- 108

  if (version == 119)
    message("Support for Stata 15/16 MP (119) format is experimental and not thoroughly tested.")

  if (version<102 | version == 109 | version == 116 | version>119)
    stop("Version mismatch abort execution. No Data was saved.")

  sstr     <- 2045
  sstrl    <- 32768
  sdouble  <- 65526
  sfloat   <- 65527
  slong    <- 65528
  sint     <- 65529
  sbyte    <- 65530

  if (version < 117) {
    sstr    <- 244
    sstrl   <- 244
    sdouble <- 255
    sfloat  <- 254
    slong   <- 253
    sint    <- 252
    sbyte   <- 251
  }
  if (version<111 | version==112)
    sstrl   <- 80


  if(!is.data.frame(data)) {
    stop("Object is not of class data.frame.")
  }
  
  is_utf8 <- l10n_info()[["UTF-8"]]

  # Is recoding necessary?
  if (version<=117) {
    # Reencoding is always needed
    doRecode <- TRUE
    toEncoding <- "CP1252"
  } else if (!is_utf8) {
    # If R runs in a non UTF-8 locale and Stata > 13
    doRecode <- TRUE
    toEncoding <- "UTF-8"
  } else {
    # utf-8 and Stata > 13
    doRecode <- FALSE
  }


  if (add.rownames) {
    if (doRecode) {
      rwn <- save.encoding(rownames(data), toEncoding)
    } else  {
      rwn <-rownames(data)
    }

    data <- data.frame(rownames= rwn,
                       data, stringsAsFactors = F)
  }
  rownames(data) <- NULL

  if (convert.underscore) {
    names(data) <- gsub("[^a-zA-Z0-9_]", "_", names(data))
    names(data)[grepl("^[0-9]", names(data))] <-
      paste0( "_", names(data)[grepl("^[0-9]", names(data))])
  }

  filepath <- path.expand(file)

  # For now we handle numeric and integers
  vartypen <- sapply(data, class)
  names(vartypen) <- names(data)

  # Convert logicals to integers
  for (v in names(vartypen[vartypen == "logical"]))
    data[[v]] <- as.integer(data[[v]])
  vartypen <- vtyp <- sapply(data, class)

  # Identify POSIXt
  posix_datetime <- which(sapply(data, 
                         function(x) inherits(x, "POSIXt")))
  vartypen[posix_datetime] <- vtyp[posix_datetime] <- "POSIXt"

  # Change origin to 1960-01-01
  # times: seconds from 1970-01-01 + 10 years (new origin 1960-01-01) * 1000 = miliseconds
  # go back 1h
  for (v in names(vartypen[vartypen == "POSIXt"]))
    data[[v]] <- (as.double(data[[v]]) + 315622800 - 60*60)*1000

  if (convert.factors){
    if (version < 106) {

      hasfactors <- sapply(data, is.factor)

      if (any(hasfactors))
        warning(paste("dta-format < 106 can not handle factors.",
                      "Labels are not saved!"))
    }
    # If our data.frame contains factors, we create a label.table
    factors <- which(sapply(data, is.factor))
    f.names <- attr(factors,"names")

    label.table <- vector("list", length(f.names))
    names(label.table) <- f.names

    valLabel <- sapply(data, class)
    valLabel[valLabel != "factor"] <- ""

    i <- 0
    for (v in factors)  {
      i <- i + 1
      if (doRecode) {
        f.levels <- save.encoding(levels(data[[v]]), toEncoding)
      } else {
        f.levels <- levels(data[[v]])
      }
      f.labels <-  as.integer(labels(levels(data[[v]])))
      attr(f.labels, "names") <- f.levels
      f.labels <- f.labels[names(f.labels) != ".."]
      label.table[[ (f.names[i]) ]] <- f.labels

      valLabel[v] <- f.names[i]
    }
    attr(data, "label.table") <- rev(label.table)
    if (doRecode) {
      valLabel <- sapply(valLabel, save.encoding, toEncoding)
    }
    attr(data, "vallabels") <- valLabel
  } else {
    attr(data, "label.table") <- NULL
    attr(data, "vallabels") <- rep("",length(data))
  }

  if (convert.dates) {
    dates <- which(sapply(data,
                          function(x) inherits(x, "Date"))
    )
    for (v in dates)
      data[[v]] <- as.vector(
        julian(data[[v]],as.Date("1960-1-1", tz = "GMT"))
      )
  }

  # is.numeric is TRUE for integers
  ff <- sapply(data, is.numeric)
  ii <- sapply(data, is.integer)
  factors <- sapply(data, is.factor)
  empty <- sapply(data, function(x) all(is.na(x) & !is.character(x)))
  ddates <- vartypen == "Date"

  # default no compression: numeric as double; integer as long; date as date;
  # empty as byte
  if (!compress) {
    vartypen[ff] <- sdouble
    vartypen[ii] <- slong
    vartypen[factors] <- slong
    vartypen[ddates] <- -sdouble
    vartypen[empty] <- sbyte
  } else {
    varTmin <- sapply(data[(ff | ii) & !empty], function(x) min(x,na.rm=TRUE))
    varTmax <- sapply(data[(ff | ii) & !empty], function(x) max(x,na.rm=TRUE))

    # check if numerics can be stored as integers
    numToCompress <- sapply(data[ff], saveToExport)
    
    if (any(numToCompress)) {
      saveToConvert <- names(data[ff])[numToCompress]
      # replace numerics as integers
      data[saveToConvert] <- sapply(data[saveToConvert], as.integer)

      # recheck after update
      ff <- sapply(data, is.numeric)
      ii <- sapply(data, is.integer)
    }

    vartypen[ff] <- sdouble

    bmin <- -127; bmax <- 100
    imin <- -32767; imax <- 32740
    # check if integer is byte, int or long
    for (k in names(which(ii & !empty))) {
      vartypen[k][varTmin[k] < imin | varTmax[k] > imax] <- slong
      vartypen[k][varTmin[k] > imin & varTmax[k] < imax] <- sint
      vartypen[k][varTmin[k] > bmin & varTmax[k] < bmax] <- sbyte
    }

    factorlength <- sapply(data[factors & !empty], nlevels)
    for (k in names(which(factors & !empty))) {
      vartypen[factors & factorlength[k] > 0x1.000000p127] <- slong
      vartypen[factors & factorlength[k] < 0x1.000000p127] <- sint
      vartypen[factors & factorlength[k] < 101] <- sbyte
    }

    # keep dates as is
    vartypen[ddates] <- -sdouble
    # cast empty variables as byte
    vartypen[empty] <- sbyte
  }

  # recode character variables. >118 wants utf-8, so encoding may be required
  if(doRecode) {
    #TODO: use seq_len ?
    for(v in (1:ncol(data))[vartypen == "character"]) {
      data[, v] <- save.encoding(data[, v], toEncoding)
    }
  }

  # str and strL are stored by maximum length of chars in a variable
  str.length <- sapply(data[vartypen == "character"], FUN=maxchar)
  str.length[str.length > sstr] <- sstrl

  # vartypen for character
  for (v in names(vartypen[vartypen == "character"]))
  {
   # str.length[str.length > sstr] <- sstrl # no loop necessary!

    vartypen[[v]] <- str.length[[v]]
  }

  # save type bevor abs()
  formats <- vartypen

  vartypen <- abs(as.integer(vartypen))
  attr(data, "types") <- vartypen

  # ToDo: Add propper check.
  #   # value_label_names must be < 33 chars
  #   if (sapply(valLabel,FUN=maxchar) >= 33)
  #     message ("at least one variable name is to long.")

  # Resize varnames to 32. Stata requires this. It allows storing 32*4 bytes,
  # but can not work with longer variable names. Chars can be 1 - 4 bytes we
  # count the varnames in R. Get nchars and trim them.
  varnames <- names(data)
  lenvarnames <- sapply(varnames, nchar)

  maxlen <- 32
  if (version <= 108)
    maxlen <- 8
  if (version >= 118)
    maxlen <- 128
  
  if (any (lenvarnames > maxlen)) {
    message ("Varname to long. Resizing. Max size is ", maxlen, ".")
    names(data) <- sapply(varnames, strtrim, width = maxlen)
  }

  # Stata format "%9,0g" means european format
  formats <- vartypen
  formats[vtyp == "Date"]      <- "%td"
  formats[vtyp == "POSIXt"]    <- "%tc"
  formats[formats == sdouble]  <- "%9.0g"
  formats[formats == sfloat]   <- "%9.0g"
  formats[formats == slong]    <- "%9.0g"
  formats[formats == sint]     <- "%9.0g"
  formats[formats == sbyte]    <- "%9.0g"
  formats[vartypen >= 0 & vartypen <= sstr] <-
    paste0("%", formats[vartypen >= 0 & vartypen <= sstr], "s")
  formats[formats == sstrl]    <- "%9s"

  attr(data, "formats") <- formats

  # Create a datalabel
  if (is.null(data.label)) {
    attr(data, "datalabel") <- "Written by R"
  } else {
    if (version == 102L)
      warning("Format 102 does not print a data label in Stata.")
    if (doRecode) {
      data.label <- save.encoding(data.label, toEncoding)
    }
    attr(data, "datalabel") <- data.label
  }

  # Create the 17 char long timestamp. It may contain 17 char long strings
  if (!time.stamp) {
    attr(data, "timestamp") <- ""
  } else {
    lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
    attr(data, "timestamp") <- format(Sys.time(), "%d %b %Y %H:%M")
    Sys.setlocale("LC_TIME",lct)
  }

  expfield <- attr(data, "expansion.fields")
  if (doRecode) {
    expfield <- lapply(expfield, function(x) iconv(x, to=toEncoding))
  }

  attr(data, "expansion.fields") <- rev(expfield)

  attr(data, "version") <- as.character(version)
  if (version < 117)
    attr(data, "version") <- version



  # If length of varlabels differs from ncols drop varlabels. This can happen,
  # when the initial data.frame was read by read.dta13 and another variable was
  # attached. In this case the last variable label has a non existing variable
  # label which will crash our Rcpp code. Since varlabels do not respect the
  # ordering inside the data frame, we simply drop them.

  varlabels <- attr(data, "var.labels")

  if (doRecode) {
      attr(data, "var.labels") <- save.encoding(varlabels, toEncoding)
  } 
  if (!is.null(varlabels) & (length(varlabels)!=ncol(data))) {
    attr(data, "var.labels") <- NULL
    warning("Number of variable labels does not match number of variables.
            Variable labels dropped.")
  }


  if (version >= 117)
    invisible( stata_save(filePath = filepath, dat = data) )
  else
    invisible( stata_pre13_save(filePath = filepath, dat = data) )
}
