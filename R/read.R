#' Read Stata 13 Binary Files
#'
#' \code{read.dta13} reads a Stata 13 dta file bytewise and imports the data
#' into a data.frame.
#'
#' @param file  string. Path to the dta file you want to import.
#' @param convert.factors logical. If TRUE factors from Stata value labels are created.
#' @param generate.factors logical. If TRUE and convert.factors is TRUE missing factor labels are created from integers.
#' @param encoding string. By default strings will be converted from Windows-1252 to system encoding. 
#'  Options are "latin1" or "utf-8" to specify target encoding explicitly.
#' @param convert.underscore logical. Changes variable name from "_" to "."
#' @param missing.type logical. Stata knows 27 different missing types: ., .a, .b, ..., .z. If TRUE, attribute
#' "missing" will be created.
#' @param replace.strl logical. If TRUE replace the reference to a strL string in the data.frame with the actual value. The strl attribute will be removed from the data.frame.
#' @param convert.dates logical. If TRUE Stata dates are converted.
#'
#' @return The function returns a data.frame with attributes. The attributes include
#' \describe{
#'   \item{datalabel:}{Dataset label}
#'   \item{time.stamp:}{Timestamp of file creation}
#'   \item{formats:}{Stata display formats. May be used with \code{\link{sprintf}}}
#'   \item{type:}{Stata data type (see Stata Corp 2014)}
#'   \item{val.labels:}{For each variable the name of the associated value labels in "label"}
#'   \item{var.labels:}{Variable labels}
#'   \item{version:}{dta file format version}
#'   \item{label.table:}{List of value labels.}
#'   \item{strl:}{List of character vectors for the new strl string variable type. The first element is the identifier and the second element the string.}
#'   \item{expansion.fields:}{list providing variable name, characteristic name
#'    and the contents of Stata characteristic field.}
#'   \item{missing:}{List of numeric vectors with Stata missing type for each variable.}
#' }
#' @note read.dta13 uses GPL 2 licensed code by Thomas Lumley and R-core members from foreign::read.dta().
#' @seealso \code{\link{read.dta}} and \code{memisc} for dta files from Stata
#' versions < 13.
#' @references Stata Corp (2014): Description of .dta file format \url{http://www.stata.com/help.cgi?dta}
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@rub.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@rub.de}
#' @useDynLib readstata13
#' @export
read.dta13 <- function(file, convert.factors = TRUE, generate.factors=FALSE,
                       encoding = "", convert.underscore = FALSE,
                       missing.type = FALSE, convert.dates = TRUE, replace.strl = FALSE) {
  # Check if path is a url
  if(length(grep("^(http|ftp|https)://", file))) {
    tmp <- tempfile()
    download.file(file, tmp, quiet = TRUE, mode = "wb")
    filepath <- tmp
    on.exit(unlink(filepath))
  } else {
    # construct filepath and read file
    filepath <- get.filepath(file)
  }
  if(!file.exists(filepath))
    return(message("File not found."))

  data <- stata(filepath,missing.type)

  if(convert.underscore)
    names(data) <- gsub("_", ".", names(data))

  types <- attr(data, "types")
  stata.na <- data.frame(type = 65526L:65530L,
                         min = c(101, 32741, 2147483621, 2^127, 2^1023),
                         inc = c(1,1,1,2^115,2^1011)
  )

  if(missing.type)
  {
    if (as.numeric(attr(data, "version")) >= 117L) {
      missings <- vector("list", length(data))
      names(missings) <- names(data)
      for(v in which(types > 65525L)) {
        this.type <- abs(types[v] - 65530L)+1
        nas <- is.na(data[[v]]) |  data[[v]] >= stata.na$min[this.type]
        natype <- (data[[v]][nas] - stata.na$min[this.type])/stata.na$inc[this.type]
        natype[is.na(natype)] <- 0L
        missings[[v]] <- rep(NA, NROW(data))
        missings[[v]][nas] <- natype
        data[[v]][nas] <- NA
      }
      attr(data,"missing") <- missings
    } else
      warning("'missing.type' only applicable to version >= 13 files")
  }

  val.labels <- attr(data, "val.labels")
  var.labels <- attr(data, "var.labels")
  type <- attr(data, "type")
  label <- attr(data, "label.table")

  ## Encoding
  # varnames
  names(data) <- iconv(names(data), from="cp1252", to=encoding, sub="byte")

  # var.labels
  attr(data, "var.labels") <- iconv(var.labels, from="cp1252", to=encoding, sub="byte")

  # val.labels
  names(val.labels) <- iconv(val.labels, from="cp1252", to=encoding, sub="byte")
  attr(data, "val.labels") <- val.labels

  # label
  names(label) <- iconv(names(label), from="cp1252", to=encoding, sub="byte")

  if (length(label) > 0) {
    for (i in 1:length(label))  {
      names(label[[i]]) <- iconv(names(label[[i]]), from="cp1252", to=encoding, sub="byte")
    }
    attr(data, "label.table") <- label
  }

  # expansion.field
  efi <- attr(data, "expansion.fields")
  if (length(efi) > 0) {
    efiChar <- unlist(lapply(efi, is.character))
    for (i in (1:length(efi))[efiChar])  {
      efi[[i]] <- iconv(efi[[i]], from="cp1252", to=encoding, sub="byte")
    }
    attr(data, "expansion.fields") <- efi
  }

  #strl
  strl <- attr(data, "strl")
  if (length(strl) > 0) {
    for (i in 1:length(strl))  {
      strl[[i]] <- iconv(strl[[i]], from="cp1252", to=encoding, sub="byte")
    }
    attr(data, "strl") <- strl
  }


  if(replace.strl) {
    strl <- do.call(rbind, attr(data,"strl"))
    for(j in seq(ncol(data))[types==32768] )
    {
      refs <- unique(data[,j])
      for(ref in refs) {
        if(length(strl[strl[,1]==ref,2])!=0){
          data[data[,j]==ref,j]<-strl[strl[,1]==ref,2]
        }
      }
    }
    attr(data, "strl") <- NULL
  }

  ## convert dates
  convert_dt_c <- function(x)
    as.POSIXct((x+0.1)/1000, origin = "1960-01-01") # avoid rounding down

  convert_dt_C <- function(x) {
    ls <- .leap.seconds + seq_along(.leap.seconds)
    z <- (x+0.1)/1000 # avoid rounding down
    z <- z - rowSums(outer(z, ls, ">="))
    as.POSIXct(z, origin = "1960-01-01")
  }

  if (convert.dates) {
    ff <- attr(data, "formats")
    ## dates <- grep("%-*d", ff)
    ## Stata 12 introduced 'business dates'
    ## 'Formats beginning with %t or %-t are Stata's date and time formats.'
    ## but it seems some are earlier.
    ## The dta_115 description suggests this is too inclusive:
    ## 'Stata has an old *%d* format notation and some datasets
    ##  still have them. Format *%d*... is equivalent to modern
    ##  format *%td*... and *%-d*... is equivalent to *%-td*...'

    dates <- if (attr(data, "version") >= 8L) grep('^%(-|)(d|td)', ff)
    else grep("%-*d", ff)
    ## avoid as.Date in case strptime is messed up
    base <- structure(-3653L, class = "Date") # Stata dates are integer vars
    for(v in dates) data[[v]] <- structure(base + data[[v]], class = "Date")

    for(v in grep("%tc", ff)) data[[v]] <- convert_dt_c(data[[v]])
    for(v in grep("%tC", ff)) data[[v]] <- convert_dt_C(data[[v]])
  }

  if(convert.factors==T) {
    vnames <- names(data)
    for (i in seq_along(val.labels)) {
      labname <- val.labels[i]
      vartype <- type[i]
      labtable <- label[[labname]]
      #don't convert columns of type double or float to factor
      if(labname %in% names(label) & vartype>65527) {
        # get unique values / omit NA
        varunique <- na.omit(unique(data[,i]))
        # assign label if label set is complete 
        if (all(varunique%in%labtable)) {
          data[,i] <- factor(data[,i], levels=labtable,
                             labels=names(labtable))
        # else generate labels from codes
        } else if(generate.factors) {
          names(varunique) <- as.character(varunique)
          gen.lab  <- sort(c(varunique[!varunique%in%labtable], labtable))

          data[,i] <- factor(data[,i], levels=gen.lab,
                             labels=names(gen.lab))
        } else {
          warning(paste(vnames[i], "Missing factor labels - no labels assigned. Set option generate.factors=T to generate labels."))
        }
      }
    }
  }

  return(data)
}
