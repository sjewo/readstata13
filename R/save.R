#' Write Stata 13 Binary Files
#'
#' \code{write.dta13} writes a Stata 13 dta file bytewise and stores the data
#' into a dta-file.
#'
#' @param path  string path to the dta file you want to exmport.
#' @param data  a data.frame Object.
#' @param data.label String Name of the dta-file.
#' @param time.stamp Whether or not a time.stamp should be added to the dta-file.
#' @return The function returns a dta-file. It includes
#' \describe{
#'   \item{datalabel}{Dataset label}
#'   \item{time.stamp}{Timestamp of file creation}
#'   \item{formats}{Stata display formats. May be used with \code{\link{sprintf}}}
#'   \item{type}{Stata data type (see Stata Corp 2014)}
#'   \item{var.labels}{Variable labels}
#'   \item{version}{dta file format version}
#'   \item{strl}{List of character vectors for the new strl string variable type. The first element is the identifier and the second element the string.}
#' }
#' @seealso \code{\link{write.dta}} and \code{memisc} for dta files from Stata
#' versions < 13.
#' @references Stata Corp (2014): Description of .dta file format \url{http://www.stata.com/help.cgi?dta}
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@rub.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@rub.de}
#' @useDynLib readstata13
#' @export
save.dta13 <- function(data, path="path", data.label=NULL, time.stamp=TRUE,
                       convert.factors=FALSE, convert.dates=TRUE){

  if(!is.data.frame(data))
    message("Object is not of class data.frame.")

  filepath <- path.expand(path)

  # For now we handle numeric and integers
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
      f.levels <-  levels(data[[v]])
      f.labels <-  as.integer(labels(levels(data[[v]])))
      attr(f.labels, "names") <- f.levels
      f.labels <- f.labels[names(f.labels)!=".."]
      label.table[[(f.names[i])]] <- f.labels

      valLabel[v] <- f.names[i]
    }
    attr(data, "label.table") <- rev(label.table)
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
    dates <- which(
      sapply(data, function(x) inherits(x,"POSIXt"))
      )
    for (v in dates)
      data[[v]] <- as.vector(
        round(julian(data[[v]], ISOdate(1960, 1, 1, tz = tz)))
        )
  }

  # FixMe: what about AsIs ?
  vartypen[vartypen=="Date"] <- 65526
  vartypen[vartypen=="factor"] <- 65528
  vartypen[vartypen=="logical"] <- 65530
  vartypen[vartypen=="numeric"] <- 65526
  vartypen[vartypen=="integer"] <- 65528

  # str and strL are stored by maximum length of chars in a variable
  maxchar <- function(x){max(nchar(x))+1}
  str.length <- sapply(data[vartypen=="character"], FUN=maxchar)
  vartypen[vartypen=="character"] <- str.length
  # str longer than 2045 chars are in Stata type strL.
  vartypen[vartypen>2045 & vartypen <65526] <- 32768

  vartypen <- as.integer(vartypen)
  attr(data, "types") <- vartypen

  #   # value_label_names must be < 33 chars
  #   if (sapply(valLabel,FUN=maxchar) >= 33)
  #     message ("at least one variable name is to long.")

  # Stata format "%9,0g" means european format
  formats <- vartypen
  formats[formats==65526] <- "%9.0g"
  formats[formats==65528] <- "%9.0g"
  formats[formats==65530] <- "%9.0g" # oder %td, wenn Date!
  formats[vartypen<2046] <- paste0("%-",formats[vartypen<2046],"s")

  attr(data, "formats") <- formats

  # Create a datalabel
  if (is.null(data.label)){
    attr(data, "datalabel") <- "Written by R"
  } else {
    attr(data, "datalabel") <- data.label
  }

  # Create the 17 char long timestamp. It may contain 17 char long strings
  if (!time.stamp){
    attr(data, "timestamp") <- ""
  } else {
    attr(data, "timestamp") <- format(Sys.time(), "%d %b %Y %H:%M")
  }

  expfield <- attr(data, "expansion.fields")
  attr(data, "expansion.fields") <- rev(expfield)

  stataWrite(filePath = filepath, dat = data)
}
