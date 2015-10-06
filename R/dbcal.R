#
# Copyright (C) 2015 Jan Marvin Garbuszus and Sebastian Jeworutzki
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


#' Parse Stata business calendar files
#'
#' Create conversion table for business calendar dates.
#'
#' @param stbcalfile \emph{stbcal-file} Stata buisness calendar file created by
#'  Stata.
#' @return Returns a data.frame with two cols:
#' \describe{
#' \item{range:}{The date matching the buisnesdate. Date format.}
#' \item{buisdays:}{The Stata business calendar day. Integer format.}
#' }
#' @details Stata 12 introduced business calender format. Business dates are
#' integer numbers in a certain range of days, weeks, months or years. In this
#' range some days are omitted (e.g. weekends or holidays). If a business
#' calendar was created, a stbcal file matching this calendar was created. This
#' file is required to read the business calendar. This parser reads the stbcal-
#' file and returns a data.frame with dates matching business calendar dates.
#'
#' A dta-file containing Stata business dates imported with read.stata13() shows
#' in formats which stdcal file is required (e.g. "%tbsp500" requires
#' sp500.stbcal).
#'
#' Stata allows adding a short description called purpose. This is added as an
#' attribute of the resulting data.frame.
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @examples
#' sp500 <- stbcal(system.file("extdata/sp500.stbcal", package="readstata13"))
#' @importFrom stats complete.cases
#' @export
stbcal <- function(stbcalfile) {

  # Otherwise localised dates will be used.
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

  # Parse full file
  stbcal <- file(stbcalfile, "rb")
  x <- readLines(stbcal, file.info(stbcalfile)$size)
  close(stbcal)

  # Dateformat can be ymd, ydm, myd, mdy, dym or dmy
  if(any(grepl("dateformat ymd", x)))
    dateformat <- "%Y%b%d"
  if(any(grepl("dateformat ydm", x)))
    dateformat <- "%Y%d%b"
  if(any(grepl("dateformat myd", x)))
    dateformat <- "%b%Y%d"
  if(any(grepl("dateformat mdy", x)))
    dateformat <- "%b%d%Y"
  if(any(grepl("dateformat dym", x)))
    dateformat <- "%b%Y%d"
  if(any(grepl("dateformat dmy", x)))
    dateformat <- "%d%b%Y"

  # Range of stbcal. Range is required, contains start and end.
  rangepos <- grep("range", x)
  range <- x[rangepos]
  range <- strsplit(range, " ")
  rangestart <- range[[1]][2]
  rangestop <- range[[1]][3]
  range <- seq(from= as.Date(rangestart, dateformat),
               to= as.Date(rangestop, dateformat), "days")

  # Centerdate of stbcal. Date that matches 0.
  centerpos <- grep("centerdate", x)
  centerdate <- x[centerpos]
  centerdate <- gsub("centerdate ","",centerdate)
  centerdate <- as.Date(centerdate, dateformat)

  # Omit Dayofweek
  omitdayofweekpos <- grep ("omit dayofweek", x)
  omitdayofweek <- x[omitdayofweekpos]

  # Mo, Tu, We, Th, Fr, Sa, Su
  daysofweek <- weekdays(as.Date(range))

  stbcal <- data.frame(range = range, daysofweek=daysofweek)

  # Weekdays every week
  if (any(grepl("Mo", omitdayofweek)))
    stbcal$daysofweek[stbcal$daysofweek=="Monday"] <- NA
  if (any(grepl("Tu", omitdayofweek)))
    stbcal$daysofweek[stbcal$daysofweek=="Tuesday"] <- NA
  if (any(grepl("We", omitdayofweek)))
    stbcal$daysofweek[stbcal$daysofweek=="Wednesday"] <- NA
  if (any(grepl("Th", omitdayofweek)))
    stbcal$daysofweek[stbcal$daysofweek=="Thursday"] <- NA
  if (any(grepl("Fr", omitdayofweek)))
    stbcal$daysofweek[stbcal$daysofweek=="Friday"] <- NA
  if (any(grepl("Sa", omitdayofweek)))
    stbcal$daysofweek[stbcal$daysofweek=="Saturday"] <- NA
  if (any(grepl("Su", omitdayofweek)))
    stbcal$daysofweek[stbcal$daysofweek=="Sunday"] <- NA

  # Special days to be omitted
  if (any(grepl("omit date", x))) {
    dates <- grep("omit date", x)

    omitdates <- x[dates]
    omitdates <- gsub("omit date ", "", omitdates)
    dates <- as.Date(omitdates, dateformat)

    stbcal$daysofweek[which(stbcal$range%in%dates)] <- NA

    # Keep only wanted days stbcal$daysofweek behalten
    stbcal <- stbcal[complete.cases(stbcal$daysofweek),]
  }

  # In case centerdate is not rangestart:
  stbcal$buisdays <- NA
  stbcal$buisdays[stbcal$range==centerdate] <- 0
  stbcal$buisdays[stbcal$range<centerdate] <- seq(from=-length(stbcal$range[stbcal$range<centerdate]), to=-1)
  stbcal$buisdays[stbcal$range>centerdate] <- seq(from=1, to=length(stbcal$range[stbcal$range>centerdate]))

  # Add purpose
  if (any(grepl("purpose", x))) {
    purposepos <- grep("purpose", x)
    purpose <- x[purposepos]
    attr(stbcal, "purpose") <- purpose
  }

  # restore locale
  Sys.setlocale("LC_TIME", lct)

  return(stbcal)
}

#' Convert Stata business calendar dates in readable dates.
#'
#' Convert Stata business calendar dates in readable dates.
#'
#' @param buisdays numeric Vector of business dates
#' @param cal data.frame Conversion table for business calendar dates
#' @param format character String with date format as in \code{\link{as.Date}}
#' @return Returns a vector of readable dates.
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#' @examples
#' # read business calendar and data
#' sp500 <- stbcal(system.file("extdata/sp500.stbcal", package="readstata13"))
#' dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"))
#'
#' # convert dates and check
#' dat$ldatescal2 <- as.caldays(dat$ldate, sp500)
#' all(dat$ldatescal2==dat$ldatescal)
#' @export
as.caldays  <- function(buisdays, cal, format="%Y-%m-%d") {
  rownames(cal) <- cal$buisdays
  dates  <- cal[as.character(buisdays), "range"]

  if(!is.null(format))
    as.Date(dates, format = format)
  return(dates)
}
