#
# Copyright (C) 2014-2015 Jan Marvin Garbuszus and Sebastian Jeworutzki
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

# Wrapper Around iconv Calls for Code Readability
#
# @param x element to be converted
# @param encoding encoding to be used.
# @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
# @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
read.encoding <- function(x, fromEncoding, encoding) {
  iconv(x,
        from=fromEncoding,
        to=encoding ,
        sub="byte")
}

save.encoding <- function(x, encoding) {
  iconv(x,
        to=encoding,
        sub="byte")
}

# Function to check if directory exists
# @param x file path
dir.exists13 <-function(x) {
  path <- dirname(x)
  return(file.exists(path))
}

# Construct File Path
#
# @param path path to dta file
# @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
# @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
get.filepath <- function(path=""){
  if(substring(path, 1, 1) == "~") {
    filepath <- path.expand(path)
  } else {
    filepath <- path
  }
  if(!file.exists(filepath)) {
    return("File does not exist.")
  }

  return(filepath)
}

#' Show Default Label Language
#'
#' Displays informations about the defined label languages.
#'
#' @param dat \emph{data.frame.} Data.frame created by \code{read.dta13}.
#' @param print \emph{logical.} If \code{TRUE}, print available languages and default language.
#' @return Returns a list with two components:
#' \describe{
#' \item{languages:}{Vector of label languages used in the dataset}
#' \item{default:}{Name of the actual default label language, otherwise NA}
#' }
#' @details Stata allows to define multiple label sets in different languages. This functions reports the
#' available languages and the selected default language.
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @export
get.lang <- function(dat, print=T) {
  ex <- attr(dat, "expansion.fields")

  lang <- list()
  if(length(grep("_lang_list", ex)) > 0) {
    lang$languages <- strsplit(ex[[grep("_lang_list", ex)]][3], " ")[[1]]
  } else {
    lang$languages <- NA
  }
  lang$default <- ifelse(length(grep("_lang_c", ex)) > 0,
                         ex[[grep("_lang_c", ex)]][3],
                         NA)

  if(print) {
    cat("Available languages:\n ")
    cat(paste0(lang$languages, "\n"))
    cat("\nDefault language:\n")
    cat(paste0(" ",lang$default, "\n"))
    return(invisible(lang))
  }

  return(lang)
}

#' Get Names of Stata Label Set
#'
#' Retrieves the Stata label set in the dataset for all or an vector of variable names.
#'
#' @param dat \emph{data.frame.} Data.frame created by \code{read.dta13}.
#' @param var.name \emph{character vector.} Variable names. If \code{NULL}, get names of all label sets.
#' @param lang \emph{character.} Label language. Default language defined by \code{\link{get.lang}} is used if NA
#' @return Returns an named vector of variable labels
#' @details Stata stores factor labels in variable independent labels sets.  This function retrieves the name of the label set for a variable.
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @export
get.label.name <- function(dat, var.name=NULL, lang=NA) {
  vnames  <- names(dat)
  if (is.na(lang) | lang == get.lang(dat, F)$default) {
    labelsets <- attr(dat, "val.labels")
    names(labelsets) <- vnames
  } else if (is.character(lang)) {
    ex <- attr(dat, "expansion.fields")
    varname <- sapply(ex[grep(paste0("_lang_l_", lang), ex)],
                      function(x) x[1])
    labelsets.tmp <- sapply(ex[grep(paste0("_lang_l_", lang), ex)],
                            function(x) x[3])
    names(labelsets.tmp) <- varname

    labelsets <- rep("", length(vnames))
    names(labelsets) <- vnames
    labelsets[varname] <- labelsets.tmp[varname]
  }

  if(is.null(var.name)) {
    return(labelsets)
  } else {
    return(labelsets[var.name])
  }
}

#' Get Origin Code Numbers for Factors
#'
#' Recreates the code numbers of a factor as stored in the Stata dataset.
#'
#' @param x \emph{factor.} Factor to obtain code for
#' @param label.table \emph{table.} Table with factor levels obtained by \code{\link{get.label}}.
#' @return Returns an integer with original codes
#' @details While converting numeric variables into factors, the original code numbers are lost.  This function reconstructs the codes from the attribute \code{label.table}.
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @examples
#' dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"))
#' labname <- get.label.name(dat,"type")
#' labtab <- get.label(dat, labname)
#'
#' # comparsion
#' get.origin.codes(dat$type, labtab)
#' as.integer(dat$type)
#' @export
get.origin.codes <- function(x, label.table) {
  if(is.factor(x)) {
    fac <- as.character(x)
    return(as.integer(label.table[fac]))
  } else {
    message("x is no factor.")
  }
}

#' Get Stata Label Table for a Label Set
#'
#' Retrieve the value labels for a specific Stata label set.
#'
#' @param dat \emph{data.frame.} Data.frame created by \code{read.dta13}.
#' @param label.name \emph{character.} Name of the Stata label set
#' @return Returns a named vector of code numbers
#' @details This function returns the table of factor levels which represent a Stata label set.
#' The name of a label set for a variable can be obtained by \code{\link{get.label.name}}.
#' @examples
#' dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"))
#' labname <- get.label.name(dat,"type")
#' get.label(dat, labname)
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @export
get.label <- function(dat, label.name) {
  return(attr(dat, "label.table")[label.name][[1]])
}

#' Assign Stata Labels to a Variable
#'
#' Assign value labels from a Stata label set to a variable.
#'
#' @param dat \emph{data.frame.} Data.frame created by \code{read.dta13}.
#' @param var.name \emph{character.} Name of the variable in the data.frame
#' @param lang \emph{character.} Label language. Default language defined by \code{\link{get.lang}} is used if NA
#' @return Returns a labeled factor
#' @examples
#' dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"), convert.factors=FALSE)
#'
#' # compare vectors
#' set.label(dat, "type")
#' dat$type
#'
#' # German label
#' set.label(dat, "type", "de")
#' @export
set.label <- function(dat, var.name, lang=NA) {
  if(is.factor(dat[,var.name])) {
    tmp <- get.origin.codes(dat[,var.name], get.label(dat, get.label.name(dat, var.name)))
  } else {
    tmp <- dat[,var.name]
  }

  labtable <- get.label(dat, get.label.name(dat, var.name, lang))

  return(factor(tmp, levels=labtable,
                labels=names(labtable))
  )
}

#' Get Stata Variable Labels
#'
#' Retrieve variable labels from dataset attributes.
#'
#' @param dat \emph{data.frame.} Data.frame created by \code{read.dta13}.
#' @param var.name \emph{character vector.} Variable names. If NULL, get label for all variables.
#' @param lang \emph{character.} Label language. Default language defined by \code{\link{get.lang}} is used if NA
#' @return Returns an named vector of variable labels
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @export
get.varlabel <- function(dat, var.name=NULL, lang=NA) {
  vnames <- names(dat)
  if (is.na(lang) | lang == get.lang(dat, F)$default) {
    varlabel <- attr(dat, "var.lab")
    names(varlabel) <- vnames
  } else if (is.character(lang)) {
    ex <- attr(dat, "expansion.fields")
    varname <- sapply(ex[grep(paste0("_lang_v_", lang), ex)], function(x) x[1])
    varlabel <- sapply(ex[grep(paste0("_lang_v_", lang), ex)], function(x) x[3])
    names(varlabel) <- varname
  }
  if(is.null(var.name)) {
    # order by data.frame columns and return
    return(varlabel[vnames])
  } else {
    return(varlabel[var.name])
  }
}

#' Assign Stata Language Labels
#'
#' Changes default label language for a dataset.
#'
#' @param dat \emph{data.frame.} Data.frame created by \code{read.dta13}.
#' @param lang \emph{character.} Label language. Default language defined by \code{\link{get.lang}} is used if NA
#' @param generate.factors \emph{logical.} If \code{TRUE}, missing factor levels are generated.
#' @return Returns a data.frame with value labels in language "lang".
#' @examples
#' dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"))
#' get.lang(dat)
#' get.varlabel(dat)
#'
#' # set German label
#' datDE <- set.lang(dat, "de")
#' get.lang(datDE)
#' get.varlabel(datDE)
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @importFrom stats na.omit
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
set.lang <- function(dat, lang=NA, generate.factors=FALSE) {
  if (is.na(lang) | lang == get.lang(dat, F)$default) {
    return(dat)
  } else if (is.character(lang)) {
    vnames <- names(dat)
    types <- attr(dat, "types")
    label <- attr(dat, "label.table")
    val.labels <- get.label.name(dat, NULL, lang)
    oldval.labels <- get.label.name(dat)
    oldval.labels <- oldval.labels[!is.na(oldval.labels)]
    oldlang <- get.lang(dat, F)$default

    cat("Replacing value labels. This might take some time...\n")
    pb <- txtProgressBar(min=1,max=length(val.labels)+1)

    for (i in seq_along(val.labels)) {
      if(val.labels[i]!="") {
        labname <- val.labels[i]
        vartype <- types[i]
        labtable <- label[[labname]]
        varname <- names(val.labels)[i]

        # get old codes
        if(is.factor(dat[, varname])) {
          oldlabname <- get.label.name(dat, varname)
          oldlabtab <- get.label(dat, oldlabname)
          codes <- get.origin.codes(dat[,varname], oldlabtab)
          varunique <- na.omit(unique(codes))
        } else {
          varunique <- na.omit(unique(dat[,varname]))
        }

        if(labname %in% names(label) & vartype > 65527 & is.factor(dat[,varname])) {
          # assign label if label set is complete
          if (all(varunique %in% labtable)) {

            dat[,varname] <- factor(codes, levels=labtable,
                                    labels=names(labtable))
          }
          # else generate labels from codes
        } else if(generate.factors) {
          names(varunique) <- as.character(varunique)
          gen.lab  <- sort(c(varunique[!varunique %in% labtable], labtable))

          dat[,varname] <- factor(dat[,varname], levels=gen.lab,
                                  labels=names(gen.lab))
        } else {
          warning(paste(vnames[i], "Missing factor labels - no labels assigned.
                        Set option generate.factors=T to generate labels."))
        }

        setTxtProgressBar(pb, i)
        }
    }
    close(pb)

    # Save old default labels to expansion.fields. This is necessary to save
    # original labels for further use.
    vnames <- names(oldval.labels)
    names(oldval.labels) <- NULL
    tmp <- list()
    for (i in seq_along(val.labels)){
      tmp[[i]] <- c(vnames[i],paste0("_lang_l_",oldlang), oldval.labels[i])
    }
    attr(dat, "expansion.fields") <- c(attr(dat, "expansion.fields"),tmp)

    # variable label
    old.varlabel <- attr(dat, "var.lab")
    tmp <- list()
    for (i in seq_along(old.varlabel)){
      tmp[[i]] <- c(vnames[i],paste0("_lang_v_", oldlang), old.varlabel[i])
    }
    attr(dat, "expansion.fields") <- c(attr(dat, "expansion.fields"),tmp)

    ex <- attr(dat, "expansion.fields")
    varname <- sapply(ex[grep(paste0("_lang_v_", lang), ex)], function(x) x[1])
    varlabel <- sapply(ex[grep(paste0("_lang_v_", lang), ex)], function(x) x[3])
    names(varlabel) <- varname
    varlabel.out <- as.character(varlabel[vnames])
    varlabel.out[is.na(varlabel.out)] <- ""
    attr(dat, "var.lab") <- varlabel.out

    # set new default lang and store string as default attributes
    names(val.labels) <- NULL
    attr(dat, "val.labels") <- val.labels
    attr(dat, "expansion.fields")[[
      grep("_lang_c", attr(dat, "expansion.fields"))
      ]][3] <- lang

    return(dat)
  }
  }
