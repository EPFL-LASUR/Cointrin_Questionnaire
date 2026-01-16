#' Regroup a variable into fewer categories
#'
#' Creates a new regrouped variable by combining multiple values into new categories.
#' The new variable is named with "_r" suffix and inherits the original variable's label.
#'
#' @param data A data frame containing the variable to regroup
#' @param var_name Character string of the variable name to regroup
#' @param groups A named list where names are new category labels and values are
#'   numeric vectors of original values to combine.
#'   Example: list("Group A" = c(1,2), "Group B" = c(3,4))
#' @param new_labels Optional named vector of labels for the new categories.
#'   If NULL, uses sequential integers with group names as labels
#'
#' @return The data frame with the new regrouped variable added
#'
#' @examples
#' data <- regroup_variable(
#'   data, "age",
#'   list("Young" = c(1, 2, 3), "Old" = c(4, 5))
#' )
regroup_variable <- function(data, var_name, groups, new_labels = NULL) {
  # groups is a named list: list("Meyrin" = c(1,2), "Grand-Saconnex" = c(3,4))

  # Create new variable name
  new_var_name <- paste0(var_name, "_r")

  # Get original variable label
  original_label <- attr(data[[var_name]], "label")
  if (is.null(original_label)) {
    original_label <- var_name
  }
  var_label <- paste0(original_label, " (regrouped)")

  # Build new labels from list names if not provided
  if (is.null(new_labels)) {
    new_labels <- setNames(seq_along(groups), names(groups))
  }

  # Create conditions for case_when
  conditions <- list()
  for (i in seq_along(groups)) {
    conditions[[length(conditions) + 1]] <- data[[var_name]] %in% groups[[i]]
  }

  # Apply regrouping
  new_values <- rep(NA_integer_, nrow(data))
  for (i in seq_along(groups)) {
    new_values[conditions[[i]]] <- i
  }

  data[[new_var_name]] <- new_values

  # Add labels
  data[[new_var_name]] <- haven::labelled(
    data[[new_var_name]],
    labels = new_labels,
    label = var_label
  )

  return(data)
}

#' Apply multiple variable regroupings
#'
#' Applies regrouping to multiple variables based on a dictionary specification.
#' Each variable is regrouped according to its entry in the dictionary.
#'
#' @param data A data frame containing variables to regroup
#' @param dict A named list where names are variable names and values are
#'   regrouping specifications (named lists of value combinations).
#'   Example: list("v_6" = list("Meyrin" = c(1,2), "Other" = c(3,4)))
#'
#' @return The data frame with all regrouped variables added
#'
#' @examples
#' dict <- list(
#'   "age" = list("Young" = c(1, 2), "Old" = c(3, 4)),
#'   "city" = list("Geneva" = c(1), "Other" = c(2, 3))
#' )
#' data <- regroup_data(data, dict)
regroup_data <- function(data, dict) {
  for (name in names(dict)) {
    data <- regroup_variable(data, name, dict[[name]])
  }
  return(data)
}
