#' @export
on_dates <- function(x) {
  x <- vec_cast(x, new_date())

  test <- function() {
    vec_in(current_date(), x)
  }

  new_schedule(test)
}
