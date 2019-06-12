#' Get Tasks in Error
#'
#' @return If no tasks, \code{\link{NULL}}, otherwise a 
#' \code{data.frame} of the tasks to be removed
#' @export
#'
#' @examples
#' get_error_tasks()
get_error_tasks = function() {
  x = system("qstat", intern = TRUE)
  err = grepl("Eqw", x)
  if (!any(err)) {
    return(NULL)
  }
  
  x = x[ err ]
  x = strsplit(x, " ")
  x = sapply(x, function(x) x[ x != ""] )
  x = x[c(1,9), , drop = FALSE]
  rownames(x) = c("job_id", "tasks")
  x = t(x)
  x = tibble::as_tibble(x)
  nvars = max(sapply(strsplit(x$tasks, split = ","), length))
  x = tidyr::separate(
    x, 
    col = tasks, 
    into = paste0("task_", 1:nvars),
    sep = ",") %>% 
    tidyr::gather(key = "task_key", value = "task_id", -job_id) %>% 
    dplyr::select(-task_key) 
  x = x %>% 
    tidyr::separate(task_id, into = c("from", "to"), 
                    sep = "-",
                    extra = "merge", fill = "right")
  just_one = is.na(x$to)
  x$to[ just_one ] = paste0(x$from[ just_one ], ":1")
  x = x %>% 
    tidyr::separate(to, into = c("to", "by"), 
                    sep = ":",
                    extra = "merge", fill = "right")
  x = x %>% 
    mutate_at(.vars = dplyr::vars(from, to, by), 
              as.numeric)
  
  indices = mapply(seq.int, to = x$to, 
                   from = x$from, by = x$by)
  removers = unlist(mapply(function(x, y) {
    paste0(x, ".", y)
  }, x$job_id, indices))
  removers = unname(removers)
  x = tibble::tibble(task_id = removers)
  x = x %>% 
    tidyr::separate(task_id, into = c("job_id", "task"), remove = FALSE,
                    sep= "[.]")
  return(x)
}
