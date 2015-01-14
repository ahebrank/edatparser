# utility functions

.simplify_data_frame <- function(df) {
  # remove any irrelevant columns
  unique_values <- (plyr::laply(df, function(x) { length(unique(x[!is.na(x)])) })) > 1
  df[,unique_values]
}

.get_numeric_header <- function(edat, property) {
  stopifnot(class(edat)=="edat")
  as.numeric(edat$header_info[property])
}
