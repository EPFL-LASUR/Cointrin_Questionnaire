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


#' Calculate Answer Agreement Ratio Between Two Participants
#'
#' Compares responses between two participants and calculates the proportion
#' of matching answers across all comparable questions.
#'
#' @param data A data frame containing participant responses with an 'lfdn'
#'   column for participant IDs
#' @param id1 Numeric or character ID of the first participant
#' @param id2 Numeric or character ID of the second participant
#'
#' @return Numeric value between 0 and 1 representing the ratio of matching
#'   answers to total comparable answers. Returns 0 if either participant is
#'   not found or if there are no comparable responses.
#'
#' @details
#' The function excludes metadata columns (lfdn, tester, timestamp, duration,
#' lastpage, startlanguage, seed) from comparison. Only non-NA values are
#' included in the calculation.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   lfdn = c(1, 2),
#'   q1 = c("A", "A"),
#'   q2 = c("B", "C")
#' )
#' calculate_same_answers(data, 1, 2) # Returns 0.5
#' }
calculate_same_answers <- function(data, id1, id2) {
  participant1 <- data[data$lfdn == id1, ]
  participant2 <- data[data$lfdn == id2, ]

  # Check if both participants exist
  if (nrow(participant1) == 0 | nrow(participant2) == 0) return(0)

  # Get columns to compare (exclude ID and metadata columns)
  cols_to_compare <- setdiff(names(data), c("lfdn", "tester", "timestamp", "duration", "lastpage", "startlanguage", "seed"))

  # Compare values column by column
  matches <- 0
  total <- 0

  for (col in cols_to_compare) {
    val1 <- participant1[[col]][1] # Get first element
    val2 <- participant2[[col]][1] # Get first element

    # Only count if both values are not NA
    if (!is.na(val1) && !is.na(val2)) {
      if (val1 == val2) matches <- matches + 1
      total <- total + 1
    }
  }

  if (total == 0) return(0)
  ratio <- matches / total
  return(ratio)
}
