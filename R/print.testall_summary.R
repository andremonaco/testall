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
    cat("Debug suggestion:", paste0(unique(x[[i]]), collapse = " | "), "\n\n")
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
