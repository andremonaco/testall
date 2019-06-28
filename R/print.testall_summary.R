#' @param x an object of class \code{testall_summary}
#' @param ... arguments to be passed to the method
#' @rdname test_all
#' @method print testall_summary
#' @export
#' @examples
#' # NOT RUN:
#' my_sug <- test_all(mean, input = list(x = c('1','2','3')))
#' my_sug
print.testall_summary <- function(x, ...) {
  sug_list <- x$suggestion

  sug_to_cons <- function(i, x) {
    cat(paste0(rep(".", options()$width), collapse = ""), "\n")
    cat(names(x)[i], "\n")
    cat(paste0(rep(".", options()$width), collapse = ""), "\n")
    cat("\n\n")

    all_call_value <- strsplit(x[[i]]$call, " = ")
    all_call <- lapply(all_call_value, "[[", 1)
    all_value <- lapply(all_call_value, function(y) {paste0(y[-1], collapse = " = ")})

    this_scenario <- unique(mapply(FUN = function(c, v) {
      # c <- all_call[[1]]
      # v <- all_value[[1]]
      min(which(lapply(suggestion$tests, "[[", c) == v))
    }, c = all_call, v = all_value))

    cat("Debug suggestion:", paste0(unique(x[[i]]), collapse = " | "), "\n")
    cat("Scenario:", paste0(this_scenario, collapse = " | "), "\n\n")
  }

  cat(rep("\t", options()$width/20), "Error Summary:\n")
  if (length(sug_list)>0) {
    suggested <- lapply(seq_len(length(sug_list)),
                        FUN = sug_to_cons,
                        x = sug_list)
  } else {
    cat(paste0(rep(".", options()$width), collapse = ""), "\n")
    cat("No error found.", "\n")
    cat(paste0(rep(".", options()$width), collapse = ""), "\n")
  }

}
