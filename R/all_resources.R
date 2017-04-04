#' @title Get Full Cluster Rundown
#' @description Get output of resources and slots begin used by cluster
#' @return \code{data.frame} of values for each person
#' @export
#' @importFrom tidyr gather
#' @importFrom dplyr starts_with select one_of
#' @importFrom data.table rbindlist
all_resources = function(){
  res = system('qstat -u "*" -j "*"', intern = TRUE)
  
  res = trimws(res)
  separator = "^======"
  df = data.frame(x = res, stringsAsFactors = FALSE)
  df$x = gsub("\\s+", " ", df$x)
  df$job = grepl(separator, df$x)
  df$index = cumsum(df$job)
  df = df[ !df$job, ]
  df$job = NULL
  rownames(df) = NULL
  
  dd = split(df, df$index)
  # idf = dd[[1]]
  
  drop_fields = c("error reason")
  i = 1
  dd = lapply(dd, function(idf){
    print(i)
    i <<- i + 1
    ss = strsplit(idf$x, ":")
    idf$fields = sapply(ss, `[`, 1)
    # idf$fields = gsub("usage .*", "usage", idf$fields)
    idf$x = sapply(ss, function(x) {
      if (length(x) > 1) {
        x = paste(x[2:length(x)], collapse = ":")
        x = trimws(x)
      } else {
        x = ""
      }
      return(x)
    })
    for (ifield in drop_fields) {
      ind = !grepl(ifield, idf$fields)
      idf = idf[ind, ]
    }
    # idf$ind = tapply(df$ind)
    rd = reshape(idf, direction = "wide", 
                 timevar = "fields",
                 idvar = "index")
    colnames(rd) = gsub("^x[.]", "", colnames(rd))
    
    return(rd)
  })
  
  alldf = data.table::rbindlist(dd, fill = TRUE)
  alldf = tidyr::gather(alldf, key = "usage_ind", 
                        value = "usage", dplyr::starts_with("usage"))
  colnames(alldf) = gsub(" ", "_", colnames(alldf))
  vars = c("index", "job_number", "owner", "group", 
            "hard_resource_list", "job_name",
           "jobshare", "parallel_environment", 
           "job-array_tasks", "reserve", 
           "jid_predecessor_list_(req)", 
           "job_args",
           "hard_queue_list","usage_ind", "usage")
  alldf = dplyr::select(alldf,  dplyr::one_of(vars))
  alldf$usage_ind = as.numeric(gsub("usage ", "", alldf$usage_ind))
  alldf$usage = gsub(", ", ";", alldf$usage)
  alldf$usage = gsub(" ", "", alldf$usage)
  alldf$hard_resource_list = gsub(" ", "", alldf$hard_resource_list)
  
  ss = strsplit(alldf$usage, split = ";")
  ss = lapply(ss, function(x) {
    names(x) = gsub("(.*)=.*", "\\1", x)
    x = gsub(".*=(.*)", "\\1", x)
    as.data.frame(t(x), stringsAsFactors = FALSE)
  })
  ss = data.table::rbindlist(ss, fill = TRUE)
  ss = as.data.frame(ss)
  svars = c("cpu", "mem", "io", "vmem", "maxvmem")
  svars = intersect(colnames(ss), svars)
  ss = ss[, svars]
  
  alldf = cbind(alldf, ss)
  
  
  ss = strsplit(alldf$hard_resource_list, split = ",")
  ss = lapply(ss, function(x) {
    names(x) = gsub("(.*)=.*", "\\1", x)
    x = gsub(".*=(.*)", "\\1", x)
    as.data.frame(t(x), stringsAsFactors = FALSE)
  })
  ss = data.table::rbindlist(ss, fill = TRUE)
  ss = as.data.frame(ss)
  svars = c("h_fsize", "h_stack", "h_vmem", 
            "mem_free")
  
  svars = intersect(colnames(ss), svars)
  ss = ss[, svars]  
  
  alldf = cbind(alldf, ss)
  
  alldf$usage = alldf$hard_resource_list = NULL
  
  return(alldf)
}