#' Get a Picture of the Queue
#'
#' @return A \code{tibble}
#' @export
#'
#' @importFrom fansi strip_sgr
#' @examples
#' qpic()
qpic = function() {
  x = system("qpic", intern = TRUE)
  x = fansi::strip_sgr(x)
  
  xx = fansi::strip_sgr(x)
  xx = trimws(xx)
  hdr = xx[1]
  xx = xx[-1]
  hdr = sub("\t", " ", hdr)
  hdr = strsplit(hdr, split = "(-|\\|)")[[1]]
  hdr = trimws(hdr)
  hdr = c("node", hdr)
  hdr = tolower(hdr)
  hdr = gsub(" ", "_", hdr)
  
  ind = grep("^Total", xx)
  xx = xx[-(ind:length(x))]
  tfile = tempfile()
  xx = gsub("\t", " ", xx)
  xx = gsub(" : ", "  ", xx)
  xx = gsub("\\|", "", xx)
  xx = gsub("\\|", "", xx)
  xx = gsub(" - ", "  ", xx)
  squish = function(x) {
    trimws(gsub("\\+", " ", x))
  }
  xx = squish(x)
  writeLines(xx, tfile,  sep = "\n")
  res = readr::read_delim(tfile, delim = " ", col_names = hdr)
  return(res)
}
