##########################
#
rm(list= ls())

library(tidyr)

user = system("users", intern = TRUE)
x = system("qmem", intern = TRUE)
xx = gsub(", ", "\t", x)
xx = gsub(paste0(" ", user), 
	paste0("\t", user), xx)

ss = strsplit(xx, "\t")

mv = sapply(ss, function(x) {
	grep("maxvmem", x, value = TRUE)
})
job_names = t(sapply(ss, function(x) {
	x[c(1, length(x))]
}))
job_names = data.frame(job_names, 
	stringsAsFactors = FALSE)
colnames(job_names) = c("job_id", "job_name")
job_names = separate(job_names, job_id, 
	into = c("job_id", "array_id"), sep = "[.]")


mv = gsub('.*=(.*)', "\\1", mv)
mv2 = mv
mv2[mv2 %in% "N/A"] = NA
mv2 = gsub("M", "", mv2)
mv2[ grep("G", mv2)] = as.numeric( 
	gsub("G", "", mv2[ grep("G", mv2)])) * 1000
mv2 = as.numeric(mv2)

res = cbind(job_names, max_vmem = mv2)

con_res = res %>% group_by(job_id) %>% 
	summarize(max_vmem = max(max_vmem))