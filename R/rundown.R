#' @title Get a slot of a vector
#'
#' @description Returns the element denoted by slot from object x
#' @param x Vector/list
#' @param slot numeric slot to grab
#' @export
#' @return One element of vector/list
#' @examples \dontrun{
#' l = list(1:5, 1:10, 1:6, rep("hey", 10))
#' sapply(l, getslot, 5)
#'}
getslot = function(x, slot){
	x[slot]
}

#' @title Get Cluster Rundown
#'
#' @description Get output of resources and slots begin used by cluster
#' @param username Grab a user
#' @param all.q Show all queue information
#' @param std.name name for the "Standard" queue
#' @export 
#' @return List of stuff
get.rundown = function(username=NULL, all.q = TRUE, std.name = "shared"){
	out = system('qstat -u "*"', intern=TRUE)
	out = out[3:length(out)]
	out = gsub(" +", " ", out)
	out = str_trim(out)

	### keep only ones that are running
	ss = strsplit(out, " ")
	running = sapply(ss, getslot, slot=5)
	out = out[running == "r"]
	ss = strsplit(out, " ")

	### just grab username and queue
	user.q = t(sapply(ss, getslot, slot=c(4,8)))
	user.q = data.frame(user.q, stringsAsFactors=FALSE)
	colnames(user.q) = c("user", "queue")
	### don't want the node - just the queue
	ss = strsplit(user.q$queue, split="@")
	user.q$queue = sapply(ss, getslot, slot=1)

	### taking off the trailing .q
	user.q$queue = gsub("\\.q", "", user.q$queue)
	### table of number of jobs by each queue
	indiv.q = tapply(user.q$user, user.q$queue, 
		function(x) {
			sort(table(x), decreasing=TRUE)
		})
	user.table = sort(table(user.q$user), decreasing=TRUE)
	standard = user.q[user.q$queue == std.name, ]


	user.tab = NULL
	if (!is.null(username)) {
		dat = user.q
		if (all.q) dat$queue = factor(dat$queue)
		dat = dat[dat$user == username,]
		if (nrow(dat) == 0)  dat[1,] = c(username, NA)
		user.tab = table(dat$user, dat$queue, useNA='no')
	}
	return(list(user.q=user.q, 
		standard = standard, 
		q.users=indiv.q,
		user.table = user.table,
		user.tab = user.tab))
}


#' @title Get list of node resources/memory
#'
#' @description Grabs the rundown of each compute node memory and slots
#' @param username Used if you want to see a specific user usage
#' @export
#' @return List of stuff 
get.resource = function(username=NULL){
  # ridding of notes
  queue = NULL
  rm(list = "queue")
  
  if (is.null(username)) username = "*"
  out = system(paste0('qstat -u "', username, '" -F'), intern=TRUE)
  out = out[2:length(out)]
  qs = grep("-----------", out)+1
  out = cbind(out, NA)
  out[qs, 2] = out[qs,1]
  colnames(out) = c("info", "node")
  out = data.frame(out, stringsAsFactors=FALSE)
  out$node = na.locf(out$node, na.rm=FALSE)
  ss= strsplit(out$node, " ")
  out$node = sapply(ss, getslot, 1)
  out = out[ -(c(qs -1, qs)), ]
  out$queue = gsub("(.*).q@(.*)", "\\1", out$node)
  out$info = gsub("\thl:", "", out$info)
  out$info = gsub("\tqf:", "", out$info)
  out$info = gsub("\tqc:", "", out$info)

  keeprows = grepl("mem_|swap_|virtual_|^cpu", out$info)
  out = out[keeprows, ]
  out$var = gsub("(.*)=(.*)", "\\1", out$info)
  out$value = gsub("(.*)=(.*)", "\\2", out$info)

  cpu = out[ out$var == "cpu", ]
  out = out[ out$var != "cpu", ]
  out$tf = gsub("(.*)_(.*)", "\\2", out$var)
  out$var = gsub("(.*)_(.*)", "\\1", out$var)
  out$gorm =  gsub("(.*)(.)$", "\\2", out$value)
  ### T is future hopes...
  stopifnot(all(out$gorm %in% c("0", "G", "K", "M", "T")))
  ### all output is in gigabytes
  out$value = as.numeric( gsub("G|K|M", "", out$value))
  out$value[out$gorm == "M"] = out$value[out$gorm == "M"]/1024
  out$value[out$gorm == "K"] = out$value[out$gorm == "K"]/(1024*1024)
  out$gorm = out$info = NULL

	varwide = dcast(out, queue + node + var ~ tf, value.var = "value")
	tfwide = dcast(out, queue + node + tf ~ var, value.var = "value")
	wide = dcast(out, queue + node  ~ var + tf, value.var = "value")

	agg = wide[ ,!colnames(wide) %in% "node"]
	agg = ddply(.data=agg, 
		.(queue), function(x) {
			colSums(x[ ,!colnames(x) %in% "queue"], na.rm=TRUE)
		})

	return(list(out=out,
		varwide=varwide,
		tfwide = tfwide,
		wide=wide,
		byqueue = agg))
}

