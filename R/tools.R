#' Construct File Path
#'
#' @param path path to dta file
get.filepath <- function(path=""){
      if(substring(path, 1, 1)=="~")
        filepath <- path.expand(path)
      else
          filepath <- path

      if(!file.exists(filepath))
        return("File does not exist.")

      return(filepath)
    }

#' Get original code number
#'
#' @param x factor. Factor to obtain code for
#' @param label.table table. Table with factor levels - obtained by get.labeltable()
#' @export
#' @examples
#' dat <- read.dta13("http://www.stata-press.com/data/r13/auto.dta")
#' labname <- get.label.name(dat,"foreign")
#' labtab <- get.label.table(dat, labname)
#'
#' # comparsion
#' get.origin.codes(dat$foreign, labtab)
#' as.integer(dat$foreign)
#'
get.origin.codes <- function(x,label.table) {
  if(is.factor(x)) {
    fac <- as.character(x)
    return(as.integer(label.table[fac]))
  } else {
    message("x is no factor.")
  }
}

#' Get name of Stata label set for a variable
#'
#' @param x data.frame. Imported data.frame
#' @param var.name character. Variable name
#' @export
get.label.name <- function(x, var.name) {
  attr(x, "val.lab")[grep(var.name,names(x))]
}

#' Get Stata label set for a variable
#'
#' @param x data.frame. Imported data.frame
#' @param label.name character. Name of the Stata label set
#' @export
get.label.table <- function(x, label.name) {
  attr(x, "label.table")[label.name][[1]]
}

#' Assign Stata labels to a variable
#'
#' @param x data.frame. Imported data.frame
#' @param var.name character. Name of the variable in the data.frame
#' @export
#' @examples
#' dat <- read.dta13("http://www.stata-press.com/data/r13/auto.dta",
#'                    convert.factors = FALSE)
#'
#' # compare vectors
#' set.label(dat, "foreign")
#' dat$foreign
set.label <- function(x, var.name) {
  labtable <- get.label.table(x, get.label.name(x, var.name))

  return(factor(x[,var.name], levels=labtable,
                             labels=names(labtable))
  )
}
