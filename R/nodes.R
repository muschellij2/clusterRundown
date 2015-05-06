#' @title Get Cluster Rundown
#'
#' @description Get output of resources and slots begin used by cluster
#' @param username Grab a user
#' @param all.q Show all queue information
#' @param std.name name for the "Standard" queue
#' @export 
#' @return List of stuff
full.rundown = function(username=NULL, all.q = TRUE, std.name = "shared"){
  out = system('qstat -u "*" -r', intern=TRUE)
  out = out[3:length(out)]
  out = gsub(" +", " ", out)
  out = str_trim(out)
  
  df = data.frame(x = out, stringsAsFactors = FALSE)
  df$job = grepl("^\\d{7}", df$x)
  df$id = cumsum(df$job)
  df = df[ !grepl("Master Queue:", df$x), ]
  df = df[ !grepl("Full jobname:", df$x), ]
  
  df = ddply(df, .(id), function(d){
    xx = d$x[1]
    ss = strsplit(xx, " ")
    d$status = sapply(ss, getslot, slot=5)
    d$user = sapply(ss, getslot, slot=4)
    d$queue = gsub(".* (.*)@.*", "\\1", xx)
    d$cores = sapply(ss, getslot, slot=9)
    if (nrow(d) > 1){
      d = d[seq(2, nrow(d)),]
    }
    d
  }) 
  
  df = df[ df$status %in% "r", ]
  
  all.ids = ddply(df, .(id), function(d){
    d$x = NULL
    d[1,, drop = FALSE]
  })
  
  #################
  # Getting Resource List
  #################  
  df$obj = gsub("(.*):.*", "\\1", df$x)
  df$obj[!grepl(":", df$x)] = NA
  df$obj = na.locf(df$obj)
  df$x = gsub("(.*):(.*)", "\\2", df$x)
  df = df[ !df$x %in% "", ]
  df$x = gsub("(0.000000)", "", df$x, fixed = TRUE)
  df$x = str_trim(df$x)
  
  df = df[ grepl("=", df$x), ]
  df$resource = gsub("(.*)=(.*)", "\\1", df$x)
  df$value = gsub("(.*)=(.*)", "\\2", toupper(df$x))
  df$q = gsub("(.*)[.]q", "\\1", df$queue)
  df = df[ df$resource != df$q, ]
  df$q = NULL
  df$x = NULL
  df$obj = NULL
  df$value[grepl("K$", df$value)] = gsub("K$", "", 
                                         df$value[grepl("M$", df$value)])
  df$value[grepl("M$", df$value)] = gsub("M$", "000", 
                                      df$value[grepl("M$", df$value)])
  df$value[grepl("G$", df$value)] = gsub("G$", "000000", 
                                      df$value[grepl("G$", df$value)])
  
  long = df

  df = reshape(df, direction = "wide", 
               idvar = "id",
               timevar = "resource", 
               times = unique(df$resource),
               v.names = "value")
  colnames(df) = gsub("value[.]", "", colnames(df))
  df = merge(all.ids, df, all.x = TRUE, sort = FALSE)
  df = df[ order(df$id), ]
  df$cores = as.numeric(df$cores)
  cn = colnames(df)
  func = function(df, cname){
    if (!(cname %in% cn)){
      df[, cname] = NA
    }
    return(df)
  }
  for (cname in c("h_fsize", 
                  "h_stack", "h_vmem", "mem_free")){
    df = func(df, cname)
  }
  xdf = df
  #### turn into Gb
  df$mem_free = as.numeric(df$mem_free) / (1000 * 1000)
  df$h_stack = as.numeric(df$h_stack) / (1000 * 1000)
  df$h_fsize = as.numeric(df$h_fsize) / (1000 * 1000)
  df$h_vmem = as.numeric(df$h_vmem) / (1000 * 1000)
  
  user = ddply(df, .(user), function(d){
    colSums()
  })
  
#   xx = ddply(df, .(queue), function(d){
#     ddply(d, .(user), function(x){
#       c(jobs = length(x$cores), cores = sum(x$cores))
#     })
#   })
#   xx = xx[ xx$jobs > 0,]
#   xx = xx[order(xx$queue, xx$cores, xx$user), ]
# 
#   ncores = ddply(df, .(user), function(x){
#     c(jobs = length(x$cores), cores = sum(x$cores))
#   })
  
  return(df)
}

