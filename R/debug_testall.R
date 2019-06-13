debugonce_testall <- function(sug, scenario = NULL) {

  # function name
  fun <- sug$fun

  if(!inherits(sug, 'testall_summary')) {
    stop(paste0(sQuote('sug'), " has to be of class testall_summary"))
  }

  scens <- sug$tests[, -ncol(sug$tests)]

  if(!is.null(scenario)) {
    scenario <- 1
  }

  debug_args <- unlist(scens[scenario,], recursive = FALSE)

  debugonce(fun = fun, text = paste0('Scenario: ', paste0(paste(names(debug_args),
                                           debug_args,
                                           sep = " = "),
                                     collapse = ", "))
            )
  do.call(fun, args = debug_args)
}
