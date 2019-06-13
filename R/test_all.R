#' test_all
#' A function which stress tests an R function with user-defined inputs.
#' @param fun an R function to test
#' @param input a grid created with \code{\link{lazy_tester}}
#' @param output not implemented yet
#' @param cores an integer specifying the number of cores to use
#'
#' @return Returns a table with error information, i.e.
#' \itemize{
#' \item The function call (arguments)
#' \item In how many occurences it threw an error
#' }
#' @import dplyr purrr reshape2
#' @export
#'
#' @examples
#' # NOT RUN:
#' test_all(mean, list(x = c(1,2,3)))
test_all <- function(fun, input, output = NULL, cores = 1) {

  # checking inputs
  if (!is.function(fun)) {
    stop(paste0(sQuote('fun'), " has to be of class function."))
  }

  if (!is.list(input)) {
    stop(paste0(sQuote('input'), " has to be of class list"))
  }

  # extract function name
  fun_name <- as.character(as.list(match.call())$fun)

  # Test a single function call
  test_single <- function(fun, args) {
    tested <- tryCatch({do.call("fun", args=args)},
                       error = function(e) {
                         e <- as.character(e)
                         if (grepl("[:]", e))
                            e <- gsub(".*[[:blank:]]?[:][[:blank:]]?(.*)", "\\1", e)
                         e <- gsub("\n", "", e)
                         class(e) <- c(class(e), 'error')
                         return(e)
                         })

    if (inherits(tested, 'error')) {
      return(tested)
    } else {
      return("success")
    }
  }

  tests <- expand.grid(input)

  errors <- vector(length = nrow(tests))
  for (i in seq_len(nrow(tests))) {
    errors[i] <- test_single(fun = fun,
                             args = lapply(tests[i,], function(x) x[[1]])
                             )

  }

  # catch errors
  stats <- which(!sapply(errors, function(x) x == "success"))

  # add errors
  tests$test_all_errors <- unlist(errors)

  # characterize everything
  tests_char <- data.frame(lapply(tests, function(x) as.character(x)))

  tests_long <- suppressWarnings(reshape2::melt(tests_char, 'test_all_errors'))

  tests_long$call <- paste0(tests_long$variable, " = ", tests_long$value)

  # extract levels
  level_list <- tests_long %>%
    group_by(variable) %>%
    summarize(n = length(unique(value)))

  # extract error elements
  test_errors <- tests_long %>%
    filter(variable %in% as.character(level_list$variable[level_list$n > 1]))

  error_table <- as.data.frame.matrix(table(test_errors$call, test_errors$test_all_errors))
  error_table$argument <- gsub("(.*) = .*", "\\1", rownames(error_table))
  error_table$call <- rownames(error_table)

  # this is the error metric at this time, when the argument fails compared to
  # the relative frequency of the argument
  error_rel <- suppressMessages(error_table %>%
    group_by(argument) %>%
    mutate_if(is.numeric, funs(./sum(.)-1/n())) %>%
    ungroup())

  # extract the suggestion (error_rel > 0) from the error table
  sug_list <- error_rel %>% select_if(is.numeric) %>%
    dplyr::select(-success) %>%
    map(function(x, df) {
    which_max <- function(x) {
     which(x == max(x, na.rm = TRUE))
    }
    ind <- which_max(x)
    arguments <- df[ind, "call"]
    data.frame(arguments)
  }, df = error_rel)

  # return elements
  out <- list(tests = tests[stats,],
              suggestion = sug_list,
              fun = fun_name)

  class(out) <- 'testall_summary'
  return(out)
}
