#' Read Stata 13 Binary Files
#'
#' \code{read.dta13} reads a Stata 13 dta file bytewise and imports the data
#' into a data.frame.
#'
#' @param path  string path to the dta file you want to import.
#' @param convert.factors logical create factors from Stata value labels.
#' @param fileEncoding string If not null, strings will be converted from fileEncoding to system encoding.
#'  Examples options are "utf8" or "latin1".
#' @param convert.underscore logical changes variable name from _ to .
#' @param missing.type logical Stata knows 27 different missing types: ., .a, .b, ..., .z. If TRUE, attributes
#' ()$missing will be created.
#' @param replace.strl logical Replace the reference to a STRL string in the data.frame with the actual value. The strl attribute will be removed from the data.frame.
#' @param convert.dates logical: Whether or not Stata dates should be converted.
#'
#' @return The function returns a data.frame with attributs. The attributes include
#' \describe{
#'   \item{datalabel}{Dataset label}
#'   \item{time.stamp}{Timestamp of file creation}
#'   \item{formats}{Stata display formats. May be used with \code{\link{sprintf}}}
#'   \item{type}{Stata data type (see Stata Corp 2014)}
#'   \item{val.labels}{For each variable the name of the associated value labels in "label"}
#'   \item{var.labels}{Variable labels}
#'   \item{version}{dta file format version}
#'   \item{lable.table}{List of value labels.}
#'   \item{strl}{List of character vectors for the new strl string variable type. The first element is the identifier and the second element the string.}
#'   \item{expansion.fields}{list providing variable name, characteristic name
#'    and the contents of stata characteristic field.}
#' }
#' @note read.dta13 uses GPL 2 licensed code by Thomas Lumley and R-core members from foreign::read.dta().
#' @seealso \code{\link{read.dta}} and \code{memisc} for dta files from Stata
#' versions < 13.
#' @references Stata Corp (2014): Description of .dta file format \url{http://www.stata.com/help.cgi?dta}
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@rub.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@rub.de}
#' @useDynLib readstata13
#' @export
read.dta13 <- function(path, convert.factors = TRUE, fileEncoding = NULL,
                       convert.underscore = FALSE, missing.type = FALSE,
                       convert.dates = TRUE, replace.strl = FALSE) {
  # Check if path is a url
  if(length(grep("^(http|ftp|https)://", path))) {
    tmp <- tempfile()
    download.file(path, tmp, quiet = TRUE, mode = "wb")
    filepath <- tmp
    on.exit(unlink(filepath))
  } else {
    # construct filepath and read file
    filepath <- get.filepath(path)
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
  type <- attr(data, "type")
  label <- attr(data, "label.table")

  if(!is.null(fileEncoding)) {
    # varnames
    Encoding(names(data)) <- fileEncoding
    names(data) <- enc2native(names(data))

    # val.lables
    Encoding(val.labels) <- fileEncoding
    names(val.labels) <- enc2native(val.labels)

    # label
    Encoding(names(label)) <- fileEncoding
    names(label) <- enc2native(names(label))

    if (length(label) > 0) {
      for (i in 1:length(label))  {
        Encoding(names(label[[i]])) <- fileEncoding
        names(label[[i]]) <- enc2native(names(label[[i]]))
      }
      attr(data, "label.table") <- label
    }

    # expansion.field
    efi <- attr(data, "expansion.fields")
    if (length(efi) > 0) {
      efiChar <- unlist(lapply(efi, is.character))
      for (i in (1:length(efi))[efiChar])  {
        Encoding(efi[[i]]) <- fileEncoding
        efi[[i]] <- enc2native(efi[[i]])
      }
      attr(data, "expansion.fields") <- efi
    }

    #strl
    strl <- attr(data, "strl")
    if (length(strl) > 0) {
      for (i in 1:length(strl))  {
        Encoding(strl[[i]]) <- fileEncoding
        strl[[i]] <- enc2native(strl[[i]])
      }
      attr(data, "strl") <- strl
    }
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
    for (i in seq_along(val.labels)) {
      labname <- val.labels[i]
      vartype <- type[i]
      #don't convert columns of type double or float to factor
      if(labname!="" & labname %in% names(label) & vartype>65527) {
        data[,i] <- factor(data[,i], levels=label[[labname]],
                           labels=names(label[[labname]]))
      }
    }
  }

  return(data)
}
