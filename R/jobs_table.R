#' Run Table of jobs
#' @export 
jobs_table = function() {
  job = node = NULL
  rm(list = c("job", "node"))
  out = system('qstat', intern = TRUE)
  out = out[3:length(out)]
  out = gsub(" +", " ", out)
  out = trimws(out)
  df = strsplit(out, split = " ")
  
  df = lapply(df, getslot, c(3, 8))
  df = do.call(rbind, df)
  colnames(df) = c("job", "node")
  df = as.data.frame(df, stringsAsFactors = FALSE)
  df$node = sub(".*compute-(\\d*)[.].*", "\\1", df$node)
  df %>% 
    dplyr::count(job, node)
}