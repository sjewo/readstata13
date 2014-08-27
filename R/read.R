#' Read Stata 13 Binary Files
#'
#' \code{read.dta13} reads a Stata 13 dta file bytewise and imports the data 
#' into a data.frame. 
#'
#' @param path The dta file you want to import.
#' @param convert.factors logical Use Stata value labels to create factors? 
#' (Version 6.0 or later).
#' @return The function returns a data.frame with attributs. The attributes include
#' \describe{
#'   \item{datalabel}{Dataset label}
#'   \item{time.stamp}{Timestamp of file creation}
#'   \item{formats}{Stata display formats. May be used with \code{\link{sprintf}}}
#'   \item{type}{Stata data type (see Stata Corp 2014)}
#'   \item{val.labels}{For each variable the name of the associated value labels in "label"}
#'   \item{var.labels}{Variable labels}
#'   \item{version}{dta file format version}
#'   \item{label}{List of value labels: ["labelname"]["code"] is a numeric vector with codes and ["labelname"]["label"] ia a character vector with label text. }
#'   \item{strl}{List of character vectors for the new strl string variable type. The first element is the identifier, and the second element the string.}
#' }
#' @note If you catch a bug, please do not sue us, we do not have any money.
#' @seealso \code{\link{read.dta}} and \code{memisc} for dta files from Stata 
#' Versions < 13   
#' @references Stata Corp (2014): Description of .dta file format \url{http://www.stata.com/help.cgi?dta}
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@rub.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@rub.de} 
#' @useDynLib readstata13
#' @export
read.dta13 <- function(path, convert.factors = TRUE) {      
  
  # construct filepath and read file
  filepath <- get.filepath(path)
  if(!file.exists(filepath))
    return(message("File not found."))
  
  data <- stata(filepath)
  
  val.labels <- attr(data, "val.labels")
  type <- attr(data, "type")
  label <- attr(data, "label")
  
  if(convert.factors==T) {
    for (i in seq_along(val.labels)) {
      labname <- val.labels[i]
      vartype <- type[i]
      #don't convert columns of type double or float to factor
      if(labname!="" & labname %in% names(label) & vartype>65527) { 
        data[,i] <- factor(data[,i], levels=label[[labname]][["code"]],
                           labels=label[[labname]][["label"]])
      }
    }
  }
  
  return(data)
}