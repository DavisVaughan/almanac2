#' @export
is_event <- function(x, schedule) {
  x <- vec_cast(x, new_date())
  assert_schedule(schedule, "schedule")

  init_context(x)
  on.exit(reset_context(), add = TRUE)

  is_event_impl(schedule)
}

is_event_impl <- function(schedule) {
  out <- schedule$test()
  vec_assert(out, ptype = logical(), size = current_size())
  out
}
