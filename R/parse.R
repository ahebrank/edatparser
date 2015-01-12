# parse the .txt "recovery" version of edat/edat2 files
# based loosely on https://github.com/canlab/CanlabCore/blob/master/Misc_utilities/parse_edat_txt.m
# provides S3 edat class and as.data.frame method to retrive trial log (like an eDataAid export to csv)

# S3 constructor
edat <- function(txt_filename) {
  if (!file.exists(txt_filename)) {
    stop('File not found.')
  }

  parsed_edat <- .parse_file(txt_filename)
  if (!is.list(parsed_edat)) {
    stop('Unable to parse file.')
  }

  structure(parsed_edat, class="edat")
}

# return trial info as a dataframe
as.data.frame.edat <- function(edat, simplify = FALSE) {
  stopifnot(class(edat)=="edat")
  trials <- edat$trial_info
  df <- plyr::rbind.fill(lapply(trials, function(x) { data.frame(t(x)) }))
  if (simplify) {
    df <- .simplify_data_frame(df)
  }
  df
}

get_date <- function(edat) {
  stopifnot(class(edat)=="edat")
  date_array <- unlist(strsplit(edat$header_info['SessionDate'], '-'))
  # edat has date in mm-dd-yyyy format
  paste0(date_array[c(3,1,2)], collapse='-')
}

get_subject_number <- function(edat) {
  .get_numeric_header(edat, 'Subject')
}

get_session <- function(edat) {
  .get_numeric_header(edat, 'Session')
}

test3 <- edat('words_norming-953-1.txt')
test2 <- edat('scene-961-3.txt')
df <- as.data.frame(test2, simplify = TRUE)
