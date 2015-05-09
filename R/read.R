#
# Copyright (C) 2014-2015 Jan Marvin Garbuszus and Sebastian Jeworutzki
# Copyright (C) of 'convert.dates' and 'missing.types' Thomas Lumley
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

#' Read Stata 13 Binary Files
#'
#' \code{read.dta13} reads a Stata 13 dta file and imports the data
#' into a data.frame.
#'
#' @param file  \emph{character.} Path to the dta file you want to import.
#' @param convert.factors \emph{logical.} If \code{TRUE}, factors from Stata value labels are created.
#' @param generate.factors \emph{logical.} If \code{TRUE} and convert.factors is TRUE, missing factor labels are created from integers.
#' @param encoding \emph{character.} Strings can be converted from Windows-1252 to system encoding.
#'  Options are "CP1252" or "UTF-8" to specify target encoding explicitly.
#' @param fromEncoding \emph{character.} We expect strings to be encoded as "CP1252" for Stata Versions 13 and older. For dta files saved with Stata 14 or newer "UTF-8" is used. In some situation the used encoding can differ for Stata 14 files and must be manually set.
#' @param convert.underscore \emph{logical.} If \code{TRUE}, "_" in variable names will be changed to "."
#' @param missing.type \emph{logical.} Stata knows 27 different missing types: ., .a, .b, ..., .z. 
#' If \code{TRUE}, attribute \code{missing} will be created.
#' @param replace.strl \emph{logical.} If \code{TRUE}, replace the reference to a strL string in the data.frame with the actual value. The strl attribute will be removed from the data.frame.
#' @param convert.dates \emph{logical.} If \code{TRUE}, Stata dates are converted.
#' @param add.rownames \emph{logical.} If \code{TRUE}, the first column will be used as rownames. Variable will be dropped afterwards.
#'
#'
#' @details If the filename is a url, the file will be downloaded as a temporary file and read afterwards.
#'
#' Stata files are encoded in ansinew. Depending on your system default encoding certain characters may appear wrong.  
#' Using a correct encoding may fix these.
#'
#' Variable names stored in the dta-file will be used in the resulting data.frame. Stata types char, byte,
#' and int will become integer; float and double will become numerics. R only
#' knows a single missing type, while Stata knows 27, so all Stata missings will become NA in R.  If you need to keep track
#' of Statas original missing types, you may use \code{missing.type=TRUE}.
#'
#' Stata dates are converted to R's Date class the same way foreign handles dates.
#'
#' Stata 13 introduced a new character type called strL. strLs are able to store strings of any size up to 2 billion
#' characters.  While R is able to store strings of this size in a character, certain data.frames may appear messed, if long
#' strings are inserted default is \code{FALSE}.
#'
#' In R, you may use rownames to store characters (see for instance \code{data(swiss)}). In Stata, this is not possible and
#' rownames have to be stored as a variable.  If this is the case for your file and you want to use rownames,
#' \code{add.rownames=TRUE} will convert the first variable of the dta-file into rownames of the resulting data.frame.
#'
#' Beginning with Stata 13 (format 117), a new dta-format was introduced, therefore reading dta-files from earlier Stata
#' versions is not implemented.
#' @return The function returns a data.frame with attributes. The attributes include
#' \describe{
#'   \item{datalabel:}{Dataset label}
#'   \item{time.stamp:}{Timestamp of file creation}
#'   \item{formats:}{Stata display formats. May be used with \code{\link{sprintf}}}
#'   \item{types:}{Stata data type (see Stata Corp 2014)}
#'   \item{val.labels:}{For each variable the name of the associated value labels in "label"}
#'   \item{var.labels:}{Variable labels}
#'   \item{version:}{dta file format version}
#'   \item{label.table:}{List of value labels.}
#'   \item{strl:}{List of character vectors for the new strl string variable type. The first element is the identifier and
#'    the second element the string.}
#'   \item{expansion.fields:}{list providing variable name, characteristic name
#'    and the contents of Stata characteristic field.}
#'   \item{missing:}{List of numeric vectors with Stata missing type for each variable.}
#' }
#' @note read.dta13 uses GPL 2 licensed code by Thomas Lumley and R-core members from foreign::read.dta().
#' @seealso \code{\link{read.dta}} and \code{memisc} for dta files from Stata
#' versions < 13.
#' @references Stata Corp (2014): Description of .dta file format \url{http://www.stata.com/help.cgi?dta}
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @useDynLib readstata13
#' @export
read.dta13 <- function(file, convert.factors = TRUE, generate.factors=FALSE,
                       encoding = NULL, fromEncoding=NULL, convert.underscore = FALSE,
                       missing.type = FALSE, convert.dates = TRUE,
                       replace.strl = FALSE, add.rownames = FALSE) {
  # Check if path is a url
  if (length(grep("^(http|ftp|https)://", file))) {
    tmp <- tempfile()
    download.file(file, tmp, quiet = TRUE, mode = "wb")
    filepath <- tmp
    on.exit(unlink(filepath))
  } else {
    # construct filepath and read file
    filepath <- get.filepath(file)
  }
  if (!file.exists(filepath))
    return(message("File not found."))

  data <- stata(filepath,missing.type)

  if (convert.underscore)
    names(data) <- gsub("_", ".", names(data))

  types <- attr(data, "types")
  val.labels <- attr(data, "val.labels")
  label <- attr(data, "label.table")

  if (missing.type) {
    stata.na <- data.frame(type = 65526L:65530L,
                           min = c(101, 32741, 2147483621, 2 ^ 127, 2 ^ 1023),
                           inc = c(1, 1, 1, 2 ^ 115, 2 ^ 1011)
    )

    if (attr(data, "version") >= 117L) {
      missings <- vector("list", length(data))
      names(missings) <- names(data)
      for (v in which(types > 65525L)) {
        this.type <- 65531L - types[v]
        nas <- is.na(data[[v]]) |  data[[v]] >= stata.na$min[this.type]
        natype <- (data[[v]][nas] - stata.na$min[this.type]) /
          stata.na$inc[this.type]
        natype[is.na(natype)] <- 0L
        missings[[v]] <- rep(NA, NROW(data))
        missings[[v]][nas] <- natype
        data[[v]][nas] <- NA
      }
      attr(data, "missing") <- missings
    } else
      warning("'missing.type' only applicable to version >= 13 files")
  }

  var.labels <- attr(data, "var.labels")

  ## Encoding
  if (!is.null(encoding)) {

      # set from encoding by dta version
    if(is.null(fromEncoding)) {
      fromEncoding <- "CP1252"
      if(attr(data, "version") >= 118L)
        fromEncoding <- "UTF-8"
    }

    # varnames
    names(data) <- read.encoding(names(data), fromEncoding, encoding)

    # var.labels
    attr(data, "var.labels") <- read.encoding(var.labels, fromEncoding, encoding)

    # val.labels
    names(val.labels) <- read.encoding(val.labels, fromEncoding, encoding)
    attr(data, "val.labels") <- val.labels

    # label
    names(label) <- read.encoding(names(label), fromEncoding, encoding)

    if (length(label) > 0) {
      for (i in 1:length(label))  {
        names(label[[i]]) <- read.encoding(names(label[[i]]), fromEncoding, encoding)
      }
      attr(data, "label.table") <- label
    }

    # recode character variables
    for (v in (1:ncol(data))[types <= 2045]) {
      data[, v] <- iconv(data[, v], from=fromEncoding, sub="byte") # to=encoding?
    }

    # expansion.field
    efi <- attr(data, "expansion.fields")
    if (length(efi) > 0) {
      efiChar <- unlist(lapply(efi, is.character))
      for (i in (1:length(efi))[efiChar])  {
        efi[[i]] <- read.encoding(efi[[i]], fromEncoding, encoding)
      }
      attr(data, "expansion.fields") <- efi
    }

    #strl
    strl <- attr(data, "strl")
    if (length(strl) > 0) {
      for (i in 1:length(strl))  {
        strl[[i]] <- read.encoding(strl[[i]], fromEncoding, encoding)
      }
      attr(data, "strl") <- strl
    }
  }

  if (replace.strl) {
    strl <- do.call(rbind, attr(data,"strl"))
    for (j in seq(ncol(data))[types == 32768] ) {
      refs <- unique(data[, j])
      for (ref in refs) {
        if (length(strl[strl[,1] == ref,2]) != 0){
          data[data[, j] == ref, j] <- strl[strl[, 1] == ref, 2]
        }
      }
    }

    # recode strL 0 to void
    for (v in (1:ncol(data))[types == 32768]) {
      data[[v]] <- gsub("00000000000000000000","", data[[v]] )
    }

    # if strls are in data.frame remove attribute strl
    attr(data, "strl") <- NULL
  }


  if (convert.dates) {
    convert_dt_c <- function(x)
      as.POSIXct((x + 0.1) / 1000, origin = "1960-01-01") # avoid rounding down

    convert_dt_C <- function(x) {
      ls <- .leap.seconds + seq_along(.leap.seconds)
      z <- (x + 0.1) / 1000 # avoid rounding down
      z <- z - rowSums(outer(z, ls, ">="))
      as.POSIXct(z, origin = "1960-01-01")
    }

    ff <- attr(data, "formats")
    ## dates <- grep("%-*d", ff)
    ## Stata 12 introduced 'business dates'
    ## 'Formats beginning with %t or %-t are Stata's date and time formats.'
    ## but it seems some are earlier.
    ## The dta_115 description suggests this is too inclusive:
    ## 'Stata has an old *%d* format notation and some datasets
    ##  still have them. Format *%d*... is equivalent to modern
    ##  format *%td*... and *%-d*... is equivalent to *%-td*...'

    dates <- if (attr(data, "version") == 117L) grep("^%(-|)(d|td)", ff)
    else grep("%-*d", ff)
    ## avoid as.Date in case strptime is messed up
    base <- structure(-3653L, class = "Date") # Stata dates are integer vars
    for (v in dates) data[[v]] <- structure(base + data[[v]], class = "Date")

    for (v in grep("%tc", ff)) data[[v]] <- convert_dt_c(data[[v]])
    for (v in grep("%tC", ff)) data[[v]] <- convert_dt_C(data[[v]])
  }

  if (convert.factors) {
    vnames <- names(data)
    for (i in seq_along(val.labels)) {
      labname <- val.labels[i]
      vartype <- types[i]
      labtable <- label[[labname]]
      #don't convert columns of type double or float to factor
      if (labname %in% names(label) & vartype >= 65527) {
        # get unique values / omit NA
        varunique <- na.omit(unique(data[, i]))
        # assign label if label set is complete
        if (all(varunique %in% labtable)) {
          data[, i] <- factor(data[, i], levels=labtable,
                              labels=names(labtable))
          # else generate labels from codes
        } else if (generate.factors) {
          names(varunique) <- as.character(varunique)
          gen.lab  <- sort(c(varunique[!varunique %in% labtable], labtable))

          data[, i] <- factor(data[, i], levels=gen.lab,
                              labels=names(gen.lab))
        } else {
          warning(paste(vnames[i], "Missing factor labels - no labels assigned.
                        Set option generate.factors=T to generate labels."))
        }
      }
    }
  }

  if (add.rownames) {
    rownames(data) <- data[[1]]
    data[[1]] <- NULL
  }

  return(data)
}
