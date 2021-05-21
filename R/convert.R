#
# Copyright (C) 2014-2021 Jan Marvin Garbuszus and Sebastian Jeworutzki
# Copyright (C) of 'convert_dt_c' and 'convert_dt_C' Thomas Lumley
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

convert_dt_c <- function(x, tz) {
  as.POSIXct((x + 0.1) / 1000, # avoid rounding down
             origin = "1960-01-01",
             tz = tz)
}

convert_dt_C <- function(x, tz) {
  ls <- .leap.seconds + seq_along(.leap.seconds) + 315619200
  z <- (x + 0.1) / 1000 # avoid rounding down
  z <- z - rowSums(outer(z, ls, ">="))
  as.POSIXct(z, origin = "1960-01-01", tz = tz)
}

# Convert Stata format %tm integer to R date.
# Uses the first day of month.
#
# @param x element to be converted
# @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
# @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
convert_dt_m <- function(x) {
  z <- x / 12 # divide by 12 to create years
  mth <- x %% 12 + 1
  yr <- 1960 + floor(z)
  
  z <- paste0(yr, "-", mth, "-1")
  z <- as.Date(z, "%Y-%m-%d")
  if (any(is.na(z))) warning("conversion of %tm failed")
  z
}

# Convert Stata format %tq integer to R date.
# Uses the first month and day of quarter.
#
# @param x element to be converted
# @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
# @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
convert_dt_q <- function(x) { 
  z <- x / 4 
  yr <- 1960 + floor(z)
  
  qrt <- x %% 4 + 1
  qrt_month <- c(1, 4, 7, 10)
  
  z <- paste0(yr, "-", qrt_month[qrt], "-1")
  z <- as.Date(z, "%Y-%m-%d")
  if (any(is.na(z))) warning("conversion of %tq failed")
  z
}

# Convert Stata format %ty integer to R date
# Uses the first month and day of year.
#
# @param x element to be converted
# @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
# @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
convert_dt_y <- function(x) {
  z <- as.Date(paste0(x, "-1-1"), "%Y-%m-%d")
  if (any(is.na(z))) warning("conversion of %ty failed")
  z
}
