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
as.data.frame.edat <- function(edat) {
  stopifnot(class(edat)=="edat")
  trials <- edat$trial_info
  plyr::rbind.fill(lapply(trials, function(x) { data.frame(t(x)) }))
}

.read_text <- function(fn) {
  # never know what kind of line endings you're going to get
  # there's probably a better way to deal with this
  tryCatch(
    {scan(fn, what=character(), fileEncoding='UTF-16', sep='\r', quiet=TRUE)},
    warning = function(e) {
      scan(fn, what=character(), fileEncoding='UTF-16', sep='\n', quiet=TRUE)
    }
  )
}

.look_for <- function(strings, needle) {
  i <- grep(needle, strings)
  if (length(i)==0) {
    warning(sprintf('%s not found in file. Did experiment terminate early?', match))
  }
  i
}

.parse_pairs_vector <- function(idx, dat) {
  strings <- dat[as.numeric(idx[1]):as.numeric(idx[2])]
  named_vector <- function(x) {
    nv <- c(x[2])
    names(nv) <- c(gsub('^\\s+', '', x[1]))
    nv
  }
  unlist(lapply(strsplit(strings, ': '), named_vector))
}

.parse_pairs <- function(dat, idx) {
  if (is.data.frame(idx)) {
    # windowed definition of rows
    plyr::alply(idx, 1, .parse_pairs_vector, dat=dat)
  } else {
    # just a single start and stop index
    .parse_pairs_vector(idx, dat)
  }
}

.parse_file <- function(fn) {
  dat <- .read_text(fn)

  # header info
  header_start <- .look_for(dat, '^\\*\\*\\* Header Start')
  header_end <- .look_for(dat, '^\\*\\*\\* Header End')
  hdr_info <- .parse_pairs(dat, c(header_start + 1, header_end - 1))

  # run info
  run_edges <- data.frame(start = .look_for(dat, '^\\*\\*\\* LogFrame Start') + 1,
                          end = .look_for(dat, '^\\*\\*\\* LogFrame End') - 1)
  run_info <- .parse_pairs(dat, run_edges)

  # trial info -- we'll assume anything indented more than one level deep is a trial
  trial_edges <- data.frame(start = .look_for(dat, '^\\s+\\*\\*\\* LogFrame Start') + 1,
                            end = .look_for(dat, '^\\s+\\*\\*\\* LogFrame End') - 1)
  trial_info <- .parse_pairs(dat, trial_edges)

  list(header_info = hdr_info, run_info = run_info, trial_info = trial_info)
}
