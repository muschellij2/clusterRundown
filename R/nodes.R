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
  df$value = gsub("(.*)=(.*)", "\\2", df$x)
  df$q = gsub("(.*)[.]q", "\\1", df$queue)
  df = df[ df$resource != df$q, ]
  df$q = NULL
  df$x = NULL
  df$obj = NULL
  df = reshape(df, direction = "wide", 
               idvar = "id",
               timevar = "resource", 
               times = unique(df$resource),
               v.names = "value")
  colnames(df) = gsub("value[.]", "", colnames(df))
  
  df = merge(all.ids, df, all.x = TRUE, sort = FALSE)
  df = df[ order(df$id), ]
  
  return(df)
}

