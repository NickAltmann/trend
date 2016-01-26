
is_weekend <- function(dd) {
  as.POSIXlt(as.Date(dd))$wday %in% c(0,6)
}

