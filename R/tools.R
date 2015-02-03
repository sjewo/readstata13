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

#' Wrapper around iconv calls for code readability
#'
#' @param x element to be converted
#' @param encoding encoding to be used.
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


#' Construct File Path
#'
#' @param path path to dta file
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
#' @param dat data.frame. Imported data.frame
#' @return If a language label was set, it will be returned otherwise NA.
#' @export
get.lang <- function(dat) {
  ex <- attr(dat, "expansion.fields")
  if(any(grepl("_lang_c", ex))) {
    res <- ex[[grep("_lang_c", ex)]][3]
  } else {
    res <- NA
  }
  return(res)
}

#' Get name of Stata label set for a variable
#'
#' @param dat data.frame. Imported data.frame
#' @param var.name character. Variable name
#' @param lang string. Label language. Default language defined by get.lang() is used if NA
#' @return Returns the variable name as string
#' @export
get.label.name <- function(dat, var.name, lang=NA) {
  if (is.na(lang) | lang == get.lang(dat)) {
    return(attr(dat, "val.lab")[grep(var.name, names(dat))])
  } else if (is.character(lang)) {
    ex <- attr(dat, "expansion.fields")
    varlabel <- ex[grep(paste0("_lang_v_", lang), ex)]
    return(varlabel[[grep(var.name, varlabel)]][3])
  }
}

#' Get original code number
#'
#' @param x factor. Factor to obtain code for
#' @param label.table table. Table with factor levels - obtained by get.labeltable()
#' @examples
#' dat <- read.dta13("http://www.stata-press.com/data/r13/auto.dta")
#' labname <- get.label.name(dat,"foreign")
#' labtab <- get.label.table(dat, labname)
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

#' Get Stata label set for a variable
#'
#' @param dat data.frame. Imported data.frame
#' @param label.name character. Name of the Stata label set
#' @return Returns a named vector of code numbers
#' @export
get.label.table <- function(dat, label.name) {
  return(attr(dat, "label.table")[label.name][[1]])
}

#' Assign Stata labels to a variable
#'
#' @param x data.frame. Imported data.frame
#' @param var.name character. Name of the variable in the data.frame
#' @return Returns a labeled factor
#' @examples
#' dat <- read.dta13("http://www.stata-press.com/data/r13/auto.dta",
#'                    convert.factors = FALSE)
#'
#' # compare vectors
#' set.label(dat, "foreign")
#' dat$foreign
#' @export
set.label <- function(x, var.name) {
  labtable <- get.label.table(x, get.label.name(x, var.name))

  return(factor(x[,var.name], levels=labtable,
                labels=names(labtable))
  )
}

#' List label languages
#'
#' @param dat data.frame. Imported data.frame
#' @return Returns a vector of language labels
#' @export
get.lang.list <- function(dat) {
  ex <- attr(dat, "expansion.fields")
  langs <- strsplit(ex[[grep("_lang_list", ex)]][3], " ")[[1]]
  cat("Available languages:\n ")
  cat(paste0(langs, "\n"))
  cat("\nDefault language:\n")
  cat(paste0(" ",ex[[grep("_lang_c", ex)]][3]),"\n")
  return(invisible(langs))
}

#' Get Stata Variable labels
#'
#' @param dat data.frame. Imported data.frame
#' @param lang string. Label language. Default language defined by get.lang() is used if NA
#' @return Returns an named vector of variable labels
#' @export
get.label.name.list <- function(dat, lang=NA) {
  if (is.na(lang) | lang == get.lang(dat)) {
    ex <- attr(dat, "var.lab")
    names(ex) <- names(dat)
    return(ex)
  } else if (is.character(lang)) {
    ex <- attr(dat, "expansion.fields")
    varname <- sapply(ex[grep(paste0("_lang_v_", lang), ex)], function(x) x[1])
    varlabel <- sapply(ex[grep(paste0("_lang_v_", lang), ex)], function(x) x[3])
    names(varlabel) <- varname

    # order by data.frame columns and return
    return(varlabel[names(dat)])
  }
}

#' Get Stata Label Set Names
#'
#' @param dat data.frame. Imported data.frame
#' @param lang string. Label language. Default language defined by get.lang() is used if NA
#' @return Returns an named vector of variable labels
#' @export
get.labelsets.list  <- function(dat, lang=NA) {
  if (is.na(lang) | lang == get.lang(dat)) {
    ex <- attr(dat, "val.lab")
    names(ex) <- names(dat)
    return(ex)
  } else if (is.character(lang)) {
    ex <- attr(dat, "expansion.fields")
    varname <- sapply(ex[grep(paste0("_lang_l_", lang), ex)],
                      function(x) x[1])
    labelsets <- sapply(ex[grep(paste0("_lang_l_", lang), ex)],
                        function(x) x[3])
    names(labelsets) <- varname
    return(labelsets[names(dat)])
  }
}

#' Assign Stata Language Labels
#'
#' @param dat data.frame. Imported data.frame
#' @param lang string. Label language. Default language defined by get.lang() is used if NA
#' @param generate.factors logical. If TRUE generates factors.
#' @return Returns a data.frame with value labels in language "lang".
#' @export
set.label.lang <- function(dat, lang=NA, generate.factors=FALSE) {
  if (is.na(lang) | lang == get.lang(dat)) {
    return(dat)
  } else if (is.character(lang)) {
    vnames <- names(dat)
    types <- attr(dat, "types")
    label <- attr(dat, "label.table")
    val.labels <- get.labelsets.list(dat, lang)
    oldval.labels <- get.labelsets.list(dat)
    oldlang <- get.lang(dat)

    cat("Replacing value labels. This might take some time...\n")
    pb <- txtProgressBar(min=1,max=length(val.labels))

    for (i in seq_along(val.labels)) {
      labname <- val.labels[i]
      vartype <- types[i]
      labtable <- label[[labname]]

      # get old codes
      if(is.factor(dat[,i])) {
        oldlabname <- get.label.name(dat, names(dat)[i])
        oldlabtab <- get.label.table(dat, oldlabname)
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
