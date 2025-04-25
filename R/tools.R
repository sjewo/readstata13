#
# Copyright (C) 2014-2025 Jan Marvin Garbuszus and Sebastian Jeworutzki
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
  sapply(x, function(s)
           ifelse(Encoding(s) == "unknown",
                    iconv(s,
                          to=encoding,
                          sub="byte"),
                    iconv(s,  from=Encoding(s),
                          to=encoding,
                          sub="byte")
           )
        )
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
get.filepath <- function(path="") {
  if (substring(path, 1, 1) == "~") {
    filepath <- path.expand(path)
  } else {
    filepath <- path
  }
  if (!file.exists(filepath)) {
    return("File does not exist.")
  }

  return(filepath)
}

#' Show Default Label Language
#'
#' Displays informations about the defined label languages.
#'
#' @param dat \emph{data.frame.} Data.frame created by \code{read.dta13}.
#' @param print \emph{logical.} If \code{TRUE}, print available languages and
#'  default language.
#' @return Returns a list with two components:
#' \describe{
#' \item{languages:}{Vector of label languages used in the dataset}
#' \item{default:}{Name of the actual default label language, otherwise NA}
#' }
#' @details Stata allows to define multiple label sets in different languages.
#'  This functions reports the available languages and the selected default
#'  language.
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @export
get.lang <- function(dat, print=T) {
  ex <- attr(dat, "expansion.fields")

  lang <- list()
  if (length(grep("_lang_list", ex)) > 0) {
    lang$languages <- strsplit(ex[[grep("_lang_list", ex)]][3], " ")[[1]]
  } else {
    lang$languages <- NA
  }
  lang$default <- ifelse(length(grep("_lang_c", ex)) > 0,
                         ex[[grep("_lang_c", ex)]][3],
                         NA)

  if (print) {
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
#' Retrieves the Stata label set in the dataset for all or an vector of variable
#' names.
#'
#' @param dat \emph{data.frame.} Data.frame created by \code{read.dta13}.
#' @param var.name \emph{character vector.} Variable names. If \code{NULL}, get
#'  names of all label sets.
#' @param lang \emph{character.} Label language. Default language defined by
#'  \code{\link{get.lang}} is used if NA
#' @return Returns an named vector of variable labels
#' @details Stata stores factor labels in variable independent labels sets. This
#'  function retrieves the name of the label set for a variable.
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

    has_no_label_lang <- identical(
      integer(0),
      unlist(lapply(ex, grep, pattern ="_lang_l_"))
    )

    if (has_no_label_lang) {
      return("")
    }

    varname <- sapply(ex[grep(paste0("_lang_l_", lang), ex)],
                      function(x) x[1])
    labelsets.tmp <- sapply(ex[grep(paste0("_lang_l_", lang), ex)],
                            function(x) x[3])
    names(labelsets.tmp) <- varname

    labelsets <- rep("", length(vnames))
    names(labelsets) <- vnames
    labelsets[varname] <- labelsets.tmp[varname]
  }

  if (is.null(var.name)) {
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
#' @param label.table \emph{table.} Table with factor levels obtained by
#'  \code{\link{get.label}}.
#' @return Returns an integer with original codes
#' @details While converting numeric variables into factors, the original code
#'  numbers are lost. This function reconstructs the codes from the attribute
#'  \code{label.table}.
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
  if (is.factor(x)) {
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
#' @details This function returns the table of factor levels which represent
#'  a Stata label set. The name of a label set for a variable can be obtained
#'  by \code{\link{get.label.name}}.
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

#' Get all Stata Label Sets for a Data.frame
#'
#' Retrieve the value labels for all variables.
#'
#' @param dat \emph{data.frame.} Data.frame created by \code{read.dta13}.
#' @return Returns a named list of label tables
#' @details This function returns the factor levels which represent
#'  a Stata label set for all variables.
#' @examples
#' dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"))
#' get.label.tables(dat)
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @importFrom stats setNames
#' @export
get.label.tables <- function(dat) {
  varnames <- setNames(names(dat), names(dat))
  lapply(varnames, function(varname) get.label(dat, get.label.name(dat, varname)))
}

#' Assign Stata Labels to a Variable
#'
#' Assign value labels from a Stata label set to a variable. If duplicated
#'  labels are found, unique labels will be generated according the following
#'  scheme: "label_(integer code)". Levels without labels will become <NA>.
#'
#' @param dat \emph{data.frame.} Data.frame created by \code{read.dta13}.
#' @param var.name \emph{character.} Name of the variable in the data.frame
#' @param lang \emph{character.} Label language. Default language defined by
#'  \code{\link{get.lang}} is used if NA
#' @return Returns a labeled factor
#' @examples
#' dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"),
#'                   convert.factors=FALSE)
#'
#' # compare vectors
#' set.label(dat, "type")
#' dat$type
#'
#' # German label
#' set.label(dat, "type", "de")
#' @export
set.label <- function(dat, var.name, lang=NA) {
  if (is.factor(dat[,var.name])) {
    tmp <- get.origin.codes(dat[,var.name],
                            get.label(dat, get.label.name(dat, var.name)))
  } else {
    tmp <- dat[,var.name]
  }

  labtable <- get.label(dat, get.label.name(dat, var.name, lang))

  #check for duplicated labels
  labcount <- table(names(labtable))
  if (any(labcount > 1)) {


    warning(paste0("\n  ",var.name, ":\n  Duplicated factor levels detected -",
                   "generating unique labels.\n"))
    labdups <- names(labtable) %in% names(labcount[labcount > 1])
    # generate unique labels from assigned label and code number
    names(labtable)[labdups] <- paste0(names(labtable)[labdups], "_(",
                                       labtable[labdups], ")")
  }

  return(factor(tmp, levels=labtable,
                labels=names(labtable))
  )
}

#' Get and assign Stata Variable Labels
#'
#' Retrieve or set variable labels for a dataset.
#'
#' @name varlabel
#' @rdname varlabel
#' @param dat \emph{data.frame.} Data.frame created by \code{read.dta13}.
#' @param var.name \emph{character vector.} Variable names. If NULL, get label
#'  for all variables.
#' @param lang \emph{character.} Label language. Default language defined by
#'  \code{\link{get.lang}} is used if NA
#' @param value \emph{character vector.} Character vector of size ncol(data) with variable names.
#' @return Returns an named vector of variable labels
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @aliases varlabel
#' @aliases 'varlabel<-'
#' @examples
#' dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"),
#'                   convert.factors=FALSE)
#'
#' # display variable labels 
#' varlabel(dat)
#' 
#' # display german variable labels
#' varlabel(dat, lang="de")
#' 
#' # display german variable label for brand
#' varlabel(dat, var.name = "brand", lang="de")
#' 
#' # define new variable labels
#' varlabel(dat) <- letters[1:ncol(dat)]
#'
#' # display new variable labels
#' varlabel(dat)
NULL

#' @rdname varlabel
#' @export
varlabel <- function(dat, var.name=NULL, lang=NA) {
  vnames <- names(dat)
  if (is.na(lang) | lang == get.lang(dat, F)$default) {
    varlabel <- attr(dat, "var.labels")
    names(varlabel) <- vnames
  } else if (is.character(lang)) {
    ex <- attr(dat, "expansion.fields")
    varname <- sapply(ex[grep(paste0("_lang_v_", lang), ex)], function(x) x[1])
    varlabel <- sapply(ex[grep(paste0("_lang_v_", lang), ex)], function(x) x[3])
    names(varlabel) <- varname
  }
  if (is.null(var.name)) {
    # order by data.frame columns and return
    return(varlabel[vnames])
  } else {
    return(varlabel[var.name])
  }
}

#' @rdname varlabel
#' @export
'varlabel<-' <- function(dat, value) {
  nlabs <- ncol(dat)
  if (length(value)==nlabs) {
    attr(dat, "var.labels") <- value
  } else {
      warning(paste("Vector of new labels must have", nlabs, "entries."))
    }
  dat
}


#' Assign Stata Language Labels
#'
#' Changes default label language for a dataset. 
#' Variables with generated labels (option generate.labels=TRUE) are kept unchanged.
#'
#' @param dat \emph{data.frame.} Data.frame created by \code{read.dta13}.
#' @param lang \emph{character.} Label language. Default language defined by
#'  \code{\link{get.lang}} is used if NA
#' @param generate.factors \emph{logical.} If \code{TRUE}, missing factor levels
#'  are generated.
#' @return Returns a data.frame with value labels in language "lang".
#' @examples
#' dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"))
#' get.lang(dat)
#' varlabel(dat)
#'
#' # set German label
#' datDE <- set.lang(dat, "de")
#' get.lang(datDE)
#' varlabel(datDE)
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
    oldval.labtab <- lapply(oldval.labels, function(x) get.label(dat, x))

    oldlang <- get.lang(dat, F)$default

    cat("Replacing value labels. This might take some time...\n")
    pb <- txtProgressBar(min=1,max=length(val.labels)+1)



    for (i in which(val.labels != "")) {

        labname <- val.labels[i]
        vartype <- types[i]
        labtable <- label[[labname]]
        varname <- names(val.labels)[i]

        # get old codes
        if (is.factor(dat[, varname])) {
          oldlabname <- oldval.labels[names(oldval.labels) == varname]
          oldlabtab <- oldval.labtab[[names(oldlabname)]]
          codes <- get.origin.codes(dat[,varname], oldlabtab)
          varunique <- na.omit(unique(codes))
        } else {
          varunique <- na.omit(unique(dat[,varname]))
        }

        if (labname %in% names(label) & is.factor(dat[,varname])) {
                     
          # assign label if label set is complete
          if (all(varunique %in% labtable)) {

            dat[,varname] <- factor(codes, levels=labtable,
                                    labels=names(labtable))
          }
          # else generate labels from codes
        } else if (generate.factors) {
          names(varunique) <- as.character(varunique)
          gen.lab  <- sort(c(varunique[!varunique %in% labtable], labtable))

          dat[,varname] <- factor(codes, levels=gen.lab,
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
    for (i in seq_along(val.labels)) {
      tmp[[i]] <- c(vnames[i],paste0("_lang_l_",oldlang), oldval.labels[i])
    }
    attr(dat, "expansion.fields") <- c(attr(dat, "expansion.fields"),tmp)

    # variable label
    old.varlabel <- attr(dat, "var.labels")
    tmp <- list()
    for (i in seq_along(old.varlabel)) {
      tmp[[i]] <- c(vnames[i],paste0("_lang_v_", oldlang), old.varlabel[i])
    }
    attr(dat, "expansion.fields") <- c(attr(dat, "expansion.fields"),tmp)

    ex <- attr(dat, "expansion.fields")
    varname <- sapply(ex[grep(paste0("_lang_v_", lang), ex)], function(x) x[1])
    varlabel <- sapply(ex[grep(paste0("_lang_v_", lang), ex)], function(x) x[3])
    names(varlabel) <- varname
    varlabel.out <- as.character(varlabel[vnames])
    varlabel.out[is.na(varlabel.out)] <- ""
    attr(dat, "var.labels") <- varlabel.out

    # set new default lang and store string as default attributes
    names(val.labels) <- NULL
    attr(dat, "val.labels") <- val.labels
    attr(dat, "expansion.fields")[[
      grep("_lang_c", attr(dat, "expansion.fields"))
      ]][3] <- lang

    return(dat)
  }
  }

#' Check if numeric vector can be expressed as integer vector
#'
#' Compression can reduce numeric vectors as integers if the vector does only
#' contain integer type data.
#'
#' @param x vector of data frame
saveToExport <- function(x) {
  ifelse(any(is.infinite(x)), FALSE, 
         ifelse(any(!is.na(x) & (x > .Machine$integer.max | x < -.Machine$integer.max)), FALSE, 
                isTRUE(all.equal(x, as.integer(x)))))
}


#' Check max char length of data.frame vectors
#'
#' Stata requires us to provide the maximum size of a charactervector as every
#' row is stored in a bit region of this size.
#'
#' Ex: If the max chars size is four, _ is no character in this vector:
#' 1. row: four
#' 3. row: one_
#' 4. row: ____
#'
#' If a character vector contains only missings or is empty, we will assign it a
#' value of one, since Stata otherwise cannot handle what we write.
#'
#' @param x vector of data frame
maxchar <- function(x) {
  z <- max(nchar(x, type="byte"), na.rm = TRUE)

  # Stata does not allow storing a string of size 0
  if (is.infinite(z) | (z == 0))
    z <- 1

  z
}

#' Read frames from Stata dtas files
#'
#' Stata 18 introduced framesets (file extension `.dtas`) that contain zipped `dta`
#' files. This helper functions imports those files and returns a list of data.frames.
#'
#' @param path path to .dtas file
#' @param select.frames character vector 
#' @param read.dta13.options list of parameters used in  \code{\link[readstata13]{read.dta13}}. The list must have the following structure: \code{list(framename = list(param = value))}
#' @return Returns a named list of data.frames.
#' @importFrom utils unzip
#' @export
#' @examples
#' 
#' path <- system.file("extdata", "myproject2.dtas", package="readstata13")
#' 
#' # read all frames in myproject2.dtas
#' read.dtas(path)
#' 
#' # read selected frames
#' read.dtas(path, select.frames = c("persons", "counties"))
#' 
#' # read only frame counties
#' read.dtas(path, select.frames = c("counties"))
#' 
#' # read frames with different arguments
#' read.dtas(path, 
#'           read.dta13.options = list(counties = list(select.cols = "median_income"),
#'                                      persons = list(select.cols = "income")))
#' 
read.dtas <- function(path, select.frames = NULL, read.dta13.options = NULL) {
  tmp <- tempdir()
  
  fls <- utils::unzip(path, exdir = tmp)
  
  # data name, dta file name, dta version
  frames <- strsplit(readLines(fls[grep(".frameinfo", fls)])[-c(1:2)], " ")
  frames <- as.data.frame(do.call("rbind", frames))
  
  # select frames
  if(!is.null(select.frames)) {
    frames <- frames[frames$V1 %in% select.frames, ]
  }
  
  # read dtas
  opts <- vector(mode = "list", length = length(frames$V1))
  names(opts) <- frames$V1
  
  for(f in frames$V1) {
    
    if(is.list(read.dta13.options)) {
      opts[[f]] <- read.dta13.options[[f]]
    }
    
    opts[[f]][["file"]] <- file.path(tmp, paste0(frames$V2[frames$V1 == f], ".dta"))
  }
  
  dtas <- lapply(opts, function(f) do.call(read.dta13, f))
  names(dtas) <- names(opts)
  
  return(dtas)
}

#' List frames in Stata dtas files
#'
#' Stata 18 introduced framesets (file extension `.dtas`) that contain zipped `dta`
#' files. This helper functions imports those files and returns a list of data.frames.
#'
#' @param path path to .dtas file
#' @return Returns a data.frame with frame names, internal filenames and dta file format version.
#' @export
#' @examples
#' 
#' path <- system.file("extdata", "myproject2.dtas", package="readstata13")
#' 
#' # print all frames in myproject2.dtas
#' get.frames(path)
#' 
get.frames <- function(path) {
  tmp <- tempdir()
  
  fls <- unzip(path, exdir = tmp, files = ".frameinfo")
  
  # data name, dta file name, dta version
  frames <- strsplit(readLines(fls[grep(".frameinfo", fls)])[-c(1:2)], " ")
  frames <- as.data.frame(do.call("rbind", frames))
  names(frames) <- c("name", "filename", "version")
  
  return(frames)
}
