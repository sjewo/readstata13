
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

convert_dt_m <- function(x) {
  z <- x / 12 # divide by 12 to create months
  mth <- x %% 12 + 1
  yr <- 1960 + floor(z)
  
  z <- paste0(yr, "-", mth, "-1")
  z <- as.Date(z, "%Y-%m-%d")
  if (any(is.na(z))) warning("conversion of %tm failed")
  z
}

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

convert_dt_y <- function(x) {
  z <- as.Date(paste0(x, "-1-1"), "%Y-%m-%d")
  if (any(is.na(z))) warning("conversion of %ty failed")
  z
}