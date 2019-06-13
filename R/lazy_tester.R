#' lazy_tester
#' Create a grid corpus for \code{\link{test_all}}
#'
#' @param FUN the function for which a grid corpus should be created
#' @param return_list a boolean which indicates whether a list should be returned
#'
#' @return Returns a grid output either in the console or as list
#' @export
#'
#' @examples
#' # create a grid
#' lazy_tester(mean)
lazy_tester <- function(FUN, return_list = FALSE) {
  formals <- formals(FUN)
  cat(paste0('test_grid <- list(\n',
             paste0("\t\t  ", names(formals), ' = list(', formals, ')', collapse = ",\n"),
             '\n)'))
}
