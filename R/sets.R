#' @export
schd_union <- function(...) {
  schedules <- list2(...)
  walk(schedules, assert_schedule, arg = "Each element of `...`")

  test <- function() {
    results <- map(schedules, is_event_impl)
    reduce(results, `|`)
  }

  new_schedule(test)
}

#' @export
`%u%` <- function(x, y) {
  schd_union(x, y)
}

#' @export
schd_intersect <- function(...) {
  schedules <- list2(...)
  walk(schedules, assert_schedule, arg = "Each element of `...`")

  test <- function() {
    results <- map(schedules, is_event_impl)
    reduce(results, `&`)
  }

  new_schedule(test)
}

#' @export
`%i%` <- function(x, y) {
  schd_intersect(x, y)
}

#' @export
schd_diff <- function(x, y) {
  assert_schedule(x, arg = "`x`")
  assert_schedule(y, arg = "`y`")

  test <- function() {
    is_event_impl(x) & !is_event_impl(y)
  }

  new_schedule(test)
}

#' @export
`%d%` <- function(x, y) {
  schd_diff(x, y)
}
