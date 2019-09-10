#' @export
on_day <- function(x) {
  on_mday(x)
}

#' @export
on_wday <- function(x) {
  x <- vec_cast(x, integer())

  if (!all(vec_in(x, 1:7))) {
    abort("`x` must be a valid week day, `1:7`.")
  }

  test <- function() {
    vec_in(current_wday(), x)
  }

  new_schedule(test)
}

#' @export
on_yday <- function(x) {
  x <- vec_cast(x, integer())

  if (!all(vec_in(x, 1:365))) {
    abort("`x` must be a valid year day, `1:365`.")
  }

  test <- function() {
    vec_in(current_yday(), x)
  }

  new_schedule(test)
}

#' @export
on_mday <- function(x) {
  x <- vec_cast(x, integer())

  if (!all(vec_in(x, 1:31))) {
    abort("`x` must be a valid month day, `1:31`.")
  }

  test <- function() {
    vec_in(current_mday(), x)
  }

  new_schedule(test)
}

#' @export
on_qday <- function(x) {
  x <- vec_cast(x, integer())

  test <- function() {
    vec_in(current_qday(), x)
  }

  new_schedule(test)
}

#' @export
on_weekday <- function() {
  on_wday(2:5)
}

#' @export
on_weekend <- function() {
  on_wday(6:7)
}
