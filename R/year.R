#' @export
in_year <- function(x) {
  x <- vec_cast(x, integer())

  test <- function() {
    vec_in(current_year(), x)
  }

  new_schedule(test)
}

#' @export
after_year <- function(x) {
  new_year_schedule(x, expr(current_year() > x))
}

#' @export
after_or_in_year <- function(x) {
  new_year_schedule(x, expr(current_year() >= x))
}

#' @export
before_year <- function(x) {
  new_year_schedule(x, expr(current_year() < x))
}

#' @export
before_or_in_year <- function(x) {
  new_year_schedule(x, expr(current_year() <= x))
}

new_year_schedule <- function(x, predicate) {
  x <- vec_cast(x, integer())
  vec_assert(x, size = 1L)

  test <- new_function(args = list(), predicate)

  new_schedule(test)
}
