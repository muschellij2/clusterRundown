#' Run Table of jobs
#' 
#' @param sort sort the data
#' @export 
jobs_table = function(sort = TRUE) {
  n = job = node = NULL
  rm(list = c("job", "node", "n"))
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
  df = df %>% 
    dplyr::count(job, node)
  if (sort) {
    df = df %>% 
      dplyr::arrange(dplyr::desc(n))
  }
  df
}