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
