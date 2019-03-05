#' Get Job Information on Cluster
#'
#' @param job_id Job identifier 
#' @param last_days jobs started during the last \code{last_days} days,
#' passed to \code{qacct -d} argument
#' @param verbose print diagnostic messages
#'
#' @return A \code{tibble} of information
#' @importFrom dplyr mutate select filter %>% 
#' @importFrom tidyr spread gather separate
#' @export
#'
#' @examples
#' \dontrun{
#' res = job_info("6684455")
#' }
job_info = function(job_id, last_days = 100, verbose = TRUE) {
  value = variable = marker = marker_id = NULL
  rm(list = c("value", "variable", "marker", "marker_id"))
  cmd = paste0("qacct -j ", job_id)
  
  x = system(cmd, intern = TRUE)
  status = attr(x, "status")
  if (status > 0) {
    if (verbose) {
      message("Job not found. Searching previous accounting logs")
    }
    search_str = paste0(':', job_id, ':')
    cmd2 = paste0(
      'grep -l "', search_str, 
      '" ', 
      "/cm/shared/apps/sge/var/default/common/accounting*.txt")
    files = system(cmd2, intern = TRUE)
    files = unique(files)
    if (length(files) == 0) {
      stop("No data returned for job")
    }
    ccmds = paste0("qacct -j ", job_id, " -f ", files)
    if (!is.null(last_days)) {
      if (is.finite(last_days)) {
        ccmds = paste0(ccmds, " -d ", last_days)
      }
    }
    x = lapply(ccmds, function(cmd) {
      x = system(cmd, intern = TRUE)
      status = attr(x, "status")
      return(x)
    })
    x = unlist(x)
  }
  
  job_info = tibble::tibble(x = x) %>% 
    mutate(marker = grepl("=============", x),
           nc = nchar(x)) 
  if (job_info$marker[1]) {
    job_info = job_info[-1, ]
  }
  job_info = job_info %>% 
    mutate(x = trimws(x),
           x = sub("\\s+", " ", x),
           marker_id = cumsum(marker) + 1) %>% 
    filter(!marker)
  job_info = job_info %>% 
    separate(x, into = c("variable", "value"), 
             sep = " ", 
             extra = "merge")
  job_info = job_info %>% 
    select(marker_id, variable, value) %>% 
    spread(key = variable, value = value)
  return(job_info)
}