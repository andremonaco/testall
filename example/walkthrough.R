# testall walkthrough -----------------------------------------------------

# toy function without real objective. There are some numeric inputs, which can
# be provided by the user. The boolean arguments distinguish between
# certain cases, where the argument z will be refactored to a string.
# This function will certainly fail when both boolean arguments are set to TRUE
# and x is smaller or equal to 50, since in this case x divided by a string is not
# possible
why_test_all <- function(x = 5, z = 300, natasha_boolison, frankie_boolsen) {

  # input checking
  if (!is.logical(natasha_boolison)) {
    stop("natasha_boolison has to be of type logical.")
  }

  if (!is.logical(frankie_boolsen)) {
    stop("frankie_boolsen has to be of type logical.")
  }

  # case 1
  if (natasha_boolison) {

    # substract some numbers
    x <- x - 30
    # reformat to a string
    z <- paste0(z, " €")


    if (frankie_boolsen) {

      # add 5
      x <- x + 5

    # check magnitude
    if (x > 50) {

      # refactor as a numeric
      z <- as.numeric(gsub("€", "", z))

      # add squared z
      x <- x + z^2

    } else {

     # divide by z
     x <- x / z

    }
   }
  }

  # return x
  return(x)
}

# test the function solo
# - works (as intended)
why_test_all(x = -5, z = 30,natasha_boolison = FALSE, frankie_boolsen = TRUE)

# - does not work (as intended)
why_test_all(x = -5, z = 30,natasha_boolison = TRUE, frankie_boolsen = TRUE)

# create possible scenarios with lazy_tester
# lazy_tester(why_test_all)
# and fill with inouts
test_grid <- list(
  x = list(-50, -10, 0, 50),
  z = list(0, 200, 300),
  natasha_boolison = list(TRUE, FALSE),
  frankie_boolsen = list(TRUE, FALSE)
)

# run scenarios
suggestion <- test_all(input = test_grid, fun = why_test_all)

# print outcome
# (suggestion hints to check the boolean arguments)
suggestion

# debug this scenario (or other scenarios) with debugonce_testall
debugonce_testall(suggestion, 1)

