#' @title Get Full Cluster Rundown
#'
#' @description Get output of resources and slots begin used by cluster
#' @export 
#' @import stringr
#' @import zoo
#' @import reshape2
#' @import plyr
#' @return List of stuff
#' @importFrom stats reshape
full.rundown = function(blameGame = FALSE){
  out = system('qstat -u "*" -r', intern = TRUE)
  out = out[3:length(out)]
  out = gsub(" +", " ", out)
  out = str_trim(out)
  
  df = data.frame(x = out, stringsAsFactors = FALSE)
  df$job = grepl("^\\d{4,8}", df$x)
  df$id = cumsum(df$job)
  df = df[ !grepl("Master Queue:", df$x), ]
  df$is_jobname = grepl("Full jobname:", df$x)
  
  
  df$job_id = NA
  df$job_id[ df$job ] = gsub("^(\\d{4,8}).*", "\\1", df$x[df$job])
  df$job_id = na.locf(df$job_id)
  
  
  df$jobname = NA
  df$jobname[ df$is_jobname ] = gsub("Full jobname:(.*)", "\\1", 
                                     df$x[df$is_jobname])
  jn = df[ !is.na(df$jobname), c("id", "job_id", "jobname")]
  df$jobname = NULL
  
  df = df[ !df$is_jobname, ]
  df$is_jobname = NULL
  
  df = ddply(df, .(id), function(d){
    xx = d$x[1]
    ss = strsplit(xx, " ")
    d$status = sapply(ss, getslot, slot = 5)
    d$user = sapply(ss, getslot, slot = 4)
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
  df = merge(df, jn, all.x = TRUE, sort = FALSE)
  
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
  df$core_mem_free = df$cores * df$mem_free
  
  user = ddply(df, .(user), function(d){
    n = colSums(d[, c("cores", "mem_free", "h_vmem")], na.rm=TRUE)
    c(n, jobs = nrow(d))
  })
  user = user[order(user$mem_free, user$cores, decreasing = TRUE), ]
  
  if (c("shared.q") %in% df$queue){
    shared = ddply(df[ df$queue %in% c("shared.q"), , drop=FALSE], 
                   .(user), function(d){
      n = colSums(d[, c("cores", "mem_free", "h_vmem")], na.rm=TRUE)
      c(n, jobs = nrow(d))
    })
    shared = shared[order(shared$mem_free, shared$cores, decreasing = TRUE), ]
  } else {
    shared = NULL
  }
  
  ## add names
  if(blameGame) {
	  realUsers = t(sapply(user$user, unmask.bandit))
	  realUsers = as.data.frame(realUsers, stringsAsFactors=FALSE)
	  user$name = realUsers$Name[match(user$user, realUsers$UID)]
	  user$email = realUsers$Email[match(user$user, realUsers$UID)]
  }
  #   xx = ddply(df, .(queue), function(d){
#     ddply(d, .(user), function(x){
#       n = colSums(x[, c("cores", "mem_free", "h_vmem")], na.rm=TRUE)
#       c(n, jobs = nrow(x))
#     })
#   })
#   xx = xx[ xx$jobs > 0,]
#   xx$queue = gsub("[.]q", "", xx$queue)
#   xx = reshape(xx, 
#                idvar = "user",
#                direction = "wide",
#                timevar = "queue", 
#                times = unique(df$queue)),
#                v.names = "value")
# 
# 
#   ncores = ddply(df, .(user), function(x){
#     c(jobs = length(x$cores), cores = sum(x$cores))
#   })
#   
  return(list(rundown = df, user = user, shared = shared))
}

