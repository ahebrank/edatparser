# utility functions

.simplify_edat_data_frame <- function(df) {
  df
}

.get_numeric_header <- function(edat, property) {
  stopifnot(class(edat)=="edat")
  as.numeric(edat$header_info[property])
}
