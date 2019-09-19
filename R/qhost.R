#' Get host information
#'
#' @return A list of \code{data.frame}s about the nodes
#' @export
qhost = function() {
  x = NULL
  rm(list= "x")
  out = system("qhost -F", intern = TRUE)
  out = out[4:length(out)]
  
  key = value = NULL
  rm(list = c("key", "value"))
  
  df = data.frame(x = out, stringsAsFactors = FALSE)
  df$job = !grepl("^\\s.*h(l|c|s|f):", df$x)
  df$id = cumsum(df$job)
  id_df = df[ df$job, ]
  id_df$x = gsub("\\s+", " ", id_df$x)
  id_df = tidyr::separate(
    id_df, col = x, 
    into = c("node", "architecture", "number_of_cores", "load", 
             "total_memory", "used_memory", "total_swapspace", 
             "used_swapspace"), 
    sep = " ")
  for (icol in colnames(id_df)) {
    bad = id_df[[icol]] %in% c("-", "-")
    # if (any(bad)) {
      id_df[[icol]][ bad ] = NA
    # }
  }
  id_df$load[ is.na(id_df$load)] = 0
  id_df$load = as.numeric(id_df$load)
  id_df$number_of_cores = as.numeric(id_df$number_of_cores)
  
  df = df[ !df$job, ]
  
  df$job = NULL
  id_df$job = NULL
  
  df$x = trimws(df$x)
  df$x = sub("^h(l|c|s|f):", "", df$x)
  
  df = tidyr::separate(df, col = x, into = c("key", "value"), 
                       sep = "=")
  df$value[ df$value %in% c("-", "-")] = NA_character_
  df = tidyr::spread(df, key = key, value = value)
  
  df = dplyr::left_join(id_df, df)
  
  id_df$bad = id_df$load > id_df$number_of_cores
  return(list(df = df, node_df = id_df))
}