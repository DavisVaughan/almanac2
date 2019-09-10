#' @export
in_month <- function(x) {
  if (is.character(x)) {
    x <- month_normalize(x)
  }

  x <- vec_cast(x, integer())

  test <- function() {
    vec_in(current_month(), x)
  }

  new_schedule(test)
}

#' @export
after_month <- function(x) {
  new_month_schedule(x, expr(current_month() > x))
}

#' @export
after_or_in_month <- function(x) {
  new_month_schedule(x, expr(current_month() >= x))
}

#' @export
before_month <- function(x) {
  new_month_schedule(x, expr(current_month() < x))
}

#' @export
before_or_in_month <- function(x) {
  new_month_schedule(x, expr(current_month() <= x))
}

#' @export
between_months <- function(x, y) {
  if (is.character(x)) {
    x <- month_normalize(x)
  }

  if (is.character(y)) {
    y <- month_normalize(y)
  }

  x <- vec_cast(x, integer())
  vec_assert(x, size = 1L)

  y <- vec_cast(y, integer())
  vec_assert(y, size = 1L)

  test <- function() {
    month <- current_month()
    x <= month & y >= month
  }

  new_schedule(test)
}

new_month_schedule <- function(x, predicate) {
  if (is.character(x)) {
    x <- month_normalize(x)
  }

  x <- vec_cast(x, integer())
  vec_assert(x, size = 1L)

  test <- new_function(args = list(), predicate)

  new_schedule(test)
}

month_normalize <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- tolower(x)

  where <- month_match(x)

  misses <- is.na(where)
  month_matches <- !misses
  numeric_month <- unique(where[month_matches])

  if (all(month_matches)) {
    return(numeric_month)
  }

  where <- month_abbr_match(x[which(misses)])

  misses <- is.na(where)
  month_abbr_matches <- !misses
  numeric_month_abbr <- unique(where[month_abbr_matches])

  if (any(misses)) {
    abort("A character `x` must be a month name or abbreviation.")
  }

  numeric_month <- unique(c(numeric_month, numeric_month_abbr))

  numeric_month
}

month_match <- function(x) {
  vec_match(x, tolower(month.name))
}

month_abbr_match <- function(x) {
  vec_match(x, tolower(month.abb))
}
