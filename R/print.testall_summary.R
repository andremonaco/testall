print.testall_summary <- function(x, ...) {
  sug_list <- x$suggestion
  # print suggestion to console
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
