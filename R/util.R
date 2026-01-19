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

#' gets recursively all the variable names related to a special variable
#'
#' @param data dataset to look for variables
#' @param name name of the regrouped variable
#' @param index index of the var to look for
#'
#' @return array containing existing variables
get_special_variables <- function(data, name, ..., index = 1) {
  var_name <- paste0(name, "_", index)
  if (var_name %in% names(data)) {
    return(c(var_name, get_special_variables(data, name, index = index + 1)))
  } else {
    return(c())
  }
}

get_special_title <- function(name) {
  titres <- c(
    "Pourquoi avez-vous choisi d'habiter dans ce quartier ?",
    "Votre logement dispose-t-il d'un jardin ou d'un balcon?",
    "Quelles pourraient être les raisons de quitter le quartier ?",
    "Utilisez-vous votre logement (y compris de ses éventuelles annexes, garages, etc.) pour d’autres activités ?",
    "Votre jardin accueille-t-il un ou plusieurs des éléments suivants ?",
    "Pour quelles activités utilisez vous votre jardin ?",
    "Êtes-vous membre d’une association ou d’un groupe lié à votre quartier?"
  )

  index <- substr(name, 4, 5)
  index <- as.numeric(index) + 1

  if (index > length(titres)) {
    return("")
  }


  return(titres[index])
}
