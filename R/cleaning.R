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


#' Replace Special Characters in Variable Labels
#'
#' @param data A data frame with variable labels
#'
#' @return The data frame with updated variable labels
replace_special_chars <- function(data) {
  for (var in names(data)) {
    # Replace in variable label
    label <- attr(data[[var]], "label")
    if (!is.null(label)) {
      new_label <- stringr::str_replace_all(label, "\\^", "'")
      attr(data[[var]], "label") <- new_label
    }

    # Replace in value labels
    labels <- attr(data[[var]], "labels")
    if (!is.null(labels)) {
      label_names <- names(labels)
      if (!is.null(label_names)) {
        new_label_names <- stringr::str_replace_all(label_names, "\\^", "'")
        names(labels) <- new_label_names
        attr(data[[var]], "labels") <- labels
      }
    }
  }
  return(data)
}

#' Rename Quoted Labels in Variables
#'
#' @param data A data frame with variable labels
#'
#' @return The data frame with updated variable labels
rename_quoted_labels <- function(data) {
  for (var in names(data)) {
    labels <- attr(data[[var]], "labels")
    if (!is.null(labels) && length(labels) == 2) {
      label_names <- names(labels)
      if (!is.null(label_names)) {
        if (all(label_names == c("quoted", "not quoted"))) {
          names(labels) <- c("Ticked", "Unticked")[order(labels)]
        }

        if (all(label_names == c("not quoted", "quoted"))) {
          names(labels) <- c("Unticked", "Ticked")[order(labels)]
        }

        attr(data[[var]], "labels") <- labels
      }
    }
  }
  return(data)
}
