#' Read Stata 13 Binary Files
#'
#' \code{read.dta13} reads a Stata 13 dta file bytewise and imports the data
#' into a data.frame.
#'
#' @param path  string path to the dta file you want to import
#' @param convert.factors logical create factors from Stata value labels
#' @param fileEncoding string If not null, strings will be converted from fileEncoding to system encoding
#' @param convert.underscore logical changes variable name from _ to .
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
#'   \item{expansion.table}{list providing variable name, characteristic name and the contents of stata characteristic field.}
#' }
#' @note If you catch a bug, please do not sue us, we do not have any money.
#' @seealso \code{\link{read.dta}} and \code{memisc} for dta files from Stata
#' Versions < 13
#' @references Stata Corp (2014): Description of .dta file format \url{http://www.stata.com/help.cgi?dta}
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@rub.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@rub.de}
#' @useDynLib readstata13
#' @export
read.dta13 <- function(path, convert.factors = TRUE, fileEncoding = NULL,
											 convert.underscore = FALSE) {
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

	data <- stata(filepath)

	if(convert.underscore)
		names(data) <- gsub("_", ".", names(data))

	val.labels <- attr(data, "val.labels")
	type <- attr(data, "type")
	label <- attr(data, "label.table")

	# make characteristics more usefull
	#characteristics <- do.call(rbind.data.frame, attr(data, "characteristics"))
	#names(characteristics) <- c("varname","charname","contents")
	#attr(data, "characteristics") <- characteristics

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
		efi <- attr(data, "expansion.table")
		if (length(efi) > 0) {
			efiChar <- unlist(lapply(efi, is.character))
			for (i in (1:length(efi))[efiChar])  {
				Encoding(efi[[i]]) <- fileEncoding
				efi[[i]] <- enc2native(efi[[i]])
			}
			attr(data, "expansion.table") <- efi
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