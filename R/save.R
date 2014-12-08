#' Write Stata 13 Binary Files
#'
#' \code{save.dta13} writes a Stata 13 dta file bytewise and saves the data
#' into a dta-file.
#'
#' @param path string. Path to the dta file you want to export.
#' @param data data.frame. A data.frame Object.
#' @param data.label string. Name of the dta-file.
#' @param time.stamp logical. If TRUE add a time.stamp to the dta-file.
#' @param convert.factors logical. If TRUE factors will be converted to Stata variables with labels. Stata expects strings to be encoded as Windows-1252, so all levels will be recoded.  Character which can not be mapped in Windows-1252 will be saved as hexcode.
#' @param convert.dates logical. If TRUE dates will be converted to Stata date time format. Code from foreign::write.dta()
#' @param tz string. The name of the timezone convert.dates will use.
#' @param add.rownames logical. If TRUE a new variable rownames will be added to the dta-file.
#' @param compress logical. If TRUE the resulting dta-file will use all of Statas numeric-vartypes.
#' @return The function writes a dta-file to disk. The following features of the dta file format are supported:
#' \describe{
#'   \item{datalabel:}{Dataset label}
#'   \item{time.stamp:}{Timestamp of file creation}
#'   \item{formats:}{Stata display formats. May be used with \code{\link{sprintf}}}
#'   \item{type:}{Stata data type (see Stata Corp 2014)}
#'   \item{var.labels:}{Variable labels}
#'   \item{version:}{dta file format version}
#'   \item{strl:}{List of character vectors for the new strL string variable type. The first element is the identifier and the second element the string.}
#' }
#' @seealso \code{\link{write.dta}} and \code{memisc} for dta files from Stata
#' versions < 13.
#' @references Stata Corp (2014): Description of .dta file format \url{http://www.stata.com/help.cgi?dta}
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@rub.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@rub.de}
#' @useDynLib readstata13
#' @export
save.dta13 <- function(data, file="path", data.label=NULL, time.stamp=TRUE,
                       convert.factors=FALSE, convert.dates=TRUE, tz="GMT",
                       add.rownames=FALSE, compress=FALSE){

  if(!is.data.frame(data))
    message("Object is not of class data.frame.")

  if (add.rownames)
    data <- data.frame(rownames= iconv(rownames(data), to="CP1252", sub="byte"),data, stringsAsFactors = F)

  filepath <- path.expand(file)

  # For now we handle numeric and integers
  vartypen <- sapply(data, class)

  # Convert logicals to integers
  for (v in names(vartypen[vartypen=="logical"]))
    data[[v]] <- as.integer(data[[v]])
  vartypen <- sapply(data, class)

  if(convert.factors){
    # If our data.frame contains factors, we create a label.table
    factors <- which(sapply(data, is.factor))
    f.names <- attr(factors,"names")

    label.table <- vector("list",length(f.names))
    names(label.table) <- f.names

    valLabel <- sapply(data, class)
    valLabel[valLabel!="factor"] <- ""

    i <- 0
    for (v in factors)  {
      i <- i+1
      f.levels <-  iconv(levels(data[[v]]), to="CP1252", sub="byte")
      f.labels <-  as.integer(labels(levels(data[[v]])))
      attr(f.labels, "names") <- f.levels
      f.labels <- f.labels[names(f.labels)!=".."]
      label.table[[(f.names[i])]] <- f.labels

      valLabel[v] <- f.names[i]
    }
    attr(data, "label.table") <- rev(label.table)
    attr(data, "vallabels") <-  iconv(valLabel, to="CP1252", sub="byte")
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
    dates <- which(
      sapply(data, function(x) inherits(x,"POSIXt"))
    )
    for (v in dates)
      data[[v]] <- as.vector(
        round(julian(data[[v]], ISOdate(1960, 1, 1, tz = tz)))
      )
  }

  # FixMe: what about AsIs ?
  vartypen[vartypen=="Date"] <- -65526

  # is.numeric is TRUE for integers
  ff <- sapply(data, is.numeric)
  ii <- sapply(data, is.integer)
  factors <- sapply(data, is.factor)
  if (!compress)
  {
    vartypen[ff] <- 65526
    vartypen[ii] <- 65528
    vartypen[factors] <- 65528
  } else {
    varTmin <- sapply(data[ff], function(x) min(x,na.rm=TRUE))
    varTmax <- sapply(data[ff], function(x) max(x,na.rm=TRUE))

    print(varTmin)
    print(varTmax)

    # check if numeric is float or double
    fminmax <- 1.701e+38
    for (k in names(which(ff)))
    {
      vartypen[k][varTmin[k]<(-fminmax) | varTmax[k]>fminmax] <- 65526
      vartypen[k][varTmin[k]>(-fminmax) & varTmax[k]<fminmax] <- 65527
    }

    bmin <- -127; bmax <- 100
    imin <- -32767; imax <- 32740
    # check if integer is byte, int or long
    for (k in names(which(ii))){
      vartypen[k][varTmin[k]<imin | varTmax[k]>imax] <- 65528
      vartypen[k][varTmin[k]>imin & varTmax[k]<imax] <- 65529
      vartypen[k][varTmin[k]>bmin & varTmax[k]<bmax] <- 65530
    }

    factorlength <- sapply(data[factors], nlevels)
    for ( k in names(which(factors)))
    {
      vartypen[factors & factorlength[k] > 0x1.000000p127] <- 65528
      vartypen[factors & factorlength[k] < 0x1.000000p127] <- 65529
      vartypen[factors & factorlength[k] < 101] <- 65530
    }
  }

  # recode character variables
  for(v in (1:ncol(data))[vartypen == "character"]) {
    data[,v] <- iconv(data[,v], to="CP1252", sub="byte")
  }

  # str and strL are stored by maximum length of chars in a variable
  maxchar <- function(x){max(nchar(x))+1}
  str.length <- sapply(data[vartypen=="character"], FUN=maxchar)

  for (v in names(vartypen[vartypen=="character"])) vartypen[[v]] <- str.length[[v]]
  vartypen <- abs(as.integer(vartypen))
  # str longer than 2045 chars are in Stata type strL.
  vartypen[vartypen>2045 & vartypen <65526] <- 32768

  attr(data, "types") <- vartypen

  #   # value_label_names must be < 33 chars
  #   if (sapply(valLabel,FUN=maxchar) >= 33)
  #     message ("at least one variable name is to long.")

  # Stata format "%9,0g" means european format
  formats <- vartypen
  formats[formats==-65526] <- "%td"
  formats[formats==65526] <- "%9.0g"
  formats[formats==65527] <- "%9.0g"
  formats[formats==65528] <- "%9.0g"
  formats[formats==65529] <- "%9.0g"
  formats[formats==65530] <- "%9.0g"
  formats[vartypen>=0 & vartypen <2046] <- paste0("%-",formats[vartypen>=0 & vartypen<2046],"s")

  attr(data, "formats") <- formats

  # Create a datalabel
  if (is.null(data.label)){
    attr(data, "datalabel") <- "Written by R"
  } else {
    attr(data, "datalabel") <- iconv(data.label, to="CP1252", sub="byte")
  }

  # Create the 17 char long timestamp. It may contain 17 char long strings
  if (!time.stamp){
    attr(data, "timestamp") <- ""
  } else {
    attr(data, "timestamp") <- format(Sys.time(), "%d %b %Y %H:%M")
  }

  expfield <- attr(data, "expansion.fields")
  expfield <- lapply(expfield, function(x) iconv(x, to="CP1252"))
  attr(data, "expansion.fields") <- rev(expfield)

  stataWrite(filePath = filepath, dat = data)
}
