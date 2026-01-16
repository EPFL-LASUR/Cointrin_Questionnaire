#' Extract Labels from Labelled Data
#'
#' Extracts variable labels and value labels from a data frame containing
#' haven-labelled columns and returns them in a tidy format.
#'
#' @param x A data frame containing labelled variables (created with haven package)
#' @param ... Additional arguments (currently unused, must be empty)
#' @param names_to Character string specifying the name for the variable name column.
#'   Default is "variable_name"
#' @param name Character string specifying the name for the label name column.
#'   Default is "name"
#' @param value Character string specifying the name for the label value column.
#'   Default is "value"
#'
#' @return A tibble with columns for variable names, label names, and label values.
#'   Returns an empty tibble with the specified column names if no labelled
#'   columns are found.
#'
#' @examples
#' \dontrun{
#' library(haven)
#' df <- data.frame(x = labelled(1:3, c(low = 1, med = 2, high = 3)))
#' get_labels(df)
#' }
get_labels <- function(
    x, ..., names_to = "variable_name", name = "name", value = "value") {
  assertthat::assert_that(is.data.frame(x))
  rlang::check_dots_empty()
  assertthat::assert_that(
    rlang::is_string(names_to), rlang::is_string(name), rlang::is_string(value)
  )

  x <- x |>
    dplyr::select(tidyselect::where(haven::is.labelled))

  if (ncol(x) == 0) {
    return(tibble::tibble(
      "{names_to}" := character(),
      "{name}" = character(), "{value}" = integer()
    ))
  }
}
