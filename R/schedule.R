#' @export
new_schedule <- function(test = function(x) TRUE) {
  if (!is.function(test)) {
    abort("`test` must be a function.")
  }

  if (length(fn_fmls(test)) != 0L) {
    abort("`test` must be a function with 0 arguments.")
  }

  .data <- list(test = test)

  structure(.Data = .data, class = "schedule")
}

#' @export
format.schedule <- function(x, ...) {
  "<schedule>"
}

#' @export
print.schedule <- function(x, ...) {
  cat(format(x))
}

is_schedule <- function(x) {
  inherits(x, "schedule")
}

assert_schedule <- function(x, arg = "`schedule`") {
  if (!is_schedule(x)) {
    glubort("{arg} must be a schedule.")
  }
  invisible(x)
}
