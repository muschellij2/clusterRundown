#' @title Get Full Cluster Rundown
#'
#' @description Get output of resources and slots begin used by cluster
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
  
  
  dd = split(df, df$index)
  idf = dd[[7]]
  
  drop_fields = c("error reason")
  # i = 1 
  dd = lapply(dd, function(idf){
    # print(i)
    # i <<- i + 1
    ss = strsplit(idf$x, ":")
    idf$fields = sapply(ss, `[`, 1)
    idf$fields = gsub("usage .*", "usage", idf$fields)
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
  
  dd = do.call("rbind")
}