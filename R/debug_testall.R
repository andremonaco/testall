#' debugonce_testall
#' This function consumes an object of class \code{testall_summary} with the
#' ultimate goal to debug your function with a suggested specifications
#' @param sug an object of class \code{testall_summary}
#' @param scenario an optional numeric value indicating which scenario from sug$tests
#' will be used for debugging.
#'
#' @return Will take you directly into debug mode and will not return anything
#' @export
#'
#' @examples
#' # NOT RUN:
#' suggestion <- test_all(mean, list(x = c('asd')))
#' debugonce_testall(suggestion)
debugonce_testall <- function(sug, scenario = 1) {

  # check if there are any errors
  if(dim(sug$tests)[1] == 0) {
    return(cat('found no errors - nothing to debug'))
  }

  # function name
  fun <- sug$fun

  if(!inherits(sug, 'testall_summary')) {
    stop(paste0(sQuote('sug'), " has to be of class testall_summary"))
  }

  scens <- sug$tests[, -ncol(sug$tests)]

  debug_args <- unlist(scens[scenario,], recursive = FALSE)

  debugonce(fun = fun, text = paste0('Scenario: ', paste0(paste(names(debug_args),
                                           debug_args,
                                           sep = " = "),
                                     collapse = ", "))
            )
  do.call(fun, args = debug_args)
  return(invisible())
}
