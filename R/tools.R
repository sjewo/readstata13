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

# Wrapper around iconv calls for code readability
#
# @param x element to be converted
# @param encoding encoding to be used.
read.encoding <- function(x, encoding) {
  iconv(x,
        from="cp1252",
        to=encoding ,
        sub="byte")
}
save.encoding <- function(x) {
  iconv(x,
        to="CP1252",
        sub="byte")
}


# Construct File Path
#
# @param path path to dta file
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

#' Show default label language
#'
#' @param dat data.frame. Data.frame created by read.stata13.
#' @param print. If TRUE print available languages and default language.
#' @return Returns a list with two components: 
#' \describe{
#' \item{languages:}{Vector of label languages used in the dataset}
#' \item{default:}{Name of the actual default label language, otherwise NA}
#' }
#' @details Stata allows to define multiple label sets in different languages. This functions reports the 
#' available languages and the selected default language.
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

#' Get name of Stata label set for a variable
#'
#' @param dat data.frame. Data.frame created by read.stata13.
#' @param var.name character vector. Variable names. If NULL names of all label sets.
#' @param lang string. Label language. Default language defined by \code{\link{get.lang}} is used if NA
#' @return Returns an named vector of variable labels
#' @details Stata stores factor labels in variable independent labels sets.  This function retrieves the name of the label set for a variable. 
#' @export
get.label.name <- function(dat, var.name=NULL, lang=NA) {
    vnames  <- names(dat)
    if (is.na(lang) | lang == get.lang(dat, F)$default) {
    labelsets <- attr(dat, "val.lab")
    names(labelsets) <- vnames
  } else if (is.character(lang)) {
    ex <- attr(dat, "expansion.fields")
    varname <- sapply(ex[grep(paste0("_lang_l_", lang), ex)],
                      function(x) x[1])
    labelsets <- sapply(ex[grep(paste0("_lang_l_", lang), ex)],
                        function(x) x[3])
    names(labelsets) <- varname
  }

   if(is.null(var.name)) {
    return(labelsets[vnames])
   } else {
    return(labelsets[var.name])
   }
}

#' Get origin code numbers for factors
#'
#' @param x factor. Factor to obtain code for
#' @param label.table table. Table with factor levels obtained by \code{\link{get.label}}.
#' @return Returns an integer with original codes
#' @details While converting numeric variables to factors, the original code numbers get lost.  This function reconstructs the codes from the attribute "label.table".
#' @examples
#' dat <- read.dta13("http://www.stata-press.com/data/r13/auto.dta")
#' labname <- get.label.name(dat,"foreign")
#' labtab <- get.label(dat, labname)
#'
#' # comparsion
#' get.origin.codes(dat$foreign, labtab)
#' as.integer(dat$foreign)
#' @export
get.origin.codes <- function(x, label.table) {
  if(is.factor(x)) {
    fac <- as.character(x)
    return(as.integer(label.table[fac]))
  } else {
    message("x is no factor.")
  }
}

#' Get Stata label table for a label set
#'
#' @param dat data.frame. Data.frame created by read.stata13.
#' @param label.name character. Name of the Stata label set
#' @return Returns a named vector of code numbers
#' @details This function returns the table of factor levels which represent a Stata label set.  
#' The name of a label set for a variable can be obtained by \code{\link{get.label.name}}. 
#' @examples
#' dat <- read.dta13("http://www.stata-press.com/data/r13/auto.dta")
#' labname <- get.label.name(dat,"foreign")
#' get.label(dat, labname)
#' @export
get.label <- function(dat, label.name) {
  return(attr(dat, "label.table")[label.name][[1]])
}

#' Assign Stata labels to a variable
#'
#' @param dat data.frame. Data.frame created by read.stata13.
#' @param var.name character. Name of the variable in the data.frame
#' @param lang string. Label language. Default language defined by \code{\link{get.lang}} is used if NA
#' @return Returns a labeled factor
#' @examples
#' dat <- read.dta13("http://www.stata-press.com/data/r13/autofull.dta",
#'                    convert.factors = FALSE)
#'
#' # compare vectors
#' set.label(dat, "foreign")
#' dat$foreign
#' @export
set.label <- function(dat, var.name, lang=NA) {
  labtable <- get.label(dat, get.label.name(dat, var.name, lang))
  return(factor(dat[,var.name], levels=labtable,
                labels=names(labtable))
  )
}

#' Get Stata Variable labels
#'
#' @param dat data.frame. Data.frame created by read.stata13.
#' @param var.name character vector. Variable names. If NULL label for all variables.
#' @param lang string. Label language. Default language defined by \code{\link{get.lang}} is used if NA
#' @return Returns an named vector of variable labels
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
#' @param dat data.frame. Data.frame created by read.stata13.
#' @param lang string. Label language. Default language defined by \code{\link{get.lang}} is used if NA
#' @param generate.factors logical. If TRUE generates factors.
#' @return Returns a data.frame with value labels in language "lang".
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
    oldlang <- get.lang(dat, F)$default

    cat("Replacing value labels. This might take some time...\n")
    pb <- txtProgressBar(min=1,max=length(val.labels))

    for (i in seq_along(val.labels)) {
      labname <- val.labels[i]
      vartype <- types[i]
      labtable <- label[[labname]]

      # get old codes
      if(is.factor(dat[,i])) {
        oldlabname <- get.label.name(dat, names(dat)[i])
        oldlabtab <- get.label(dat, oldlabname)
        codes <- get.origin.codes(dat[,i], oldlabtab)
        varunique <- na.omit(unique(codes))
      } else {
        varunique <- na.omit(unique(dat[,i]))
      }

      if(labname %in% names(label) & vartype > 65527 & is.factor(dat[,i])) {
        # assign label if label set is complete
        if (all(varunique %in% labtable)) {

          dat[,i] <- factor(codes, levels=labtable,
                            labels=names(labtable))
        }
        # else generate labels from codes
      } else if(generate.factors) {
        names(varunique) <- as.character(varunique)
        gen.lab  <- sort(c(varunique[!varunique %in% labtable], labtable))

        dat[,i] <- factor(dat[,i], levels=gen.lab,
                          labels=names(gen.lab))
      } else {
        warning(paste(vnames[i], "Missing factor labels - no labels assigned.
                      Set option generate.factors=T to generate labels."))
      }

      setTxtProgressBar(pb, i)
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

    # set new default lang and store string as default attributes
    names(val.labels) <- NULL
    attr(dat, "val.lab") <- val.labels
    attr(dat, "expansion.fields")[[
      grep("_lang_c", attr(dat, "expansion.fields"))
      ]][3] <- lang

    return(dat)
  }
}
