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

#' Combine multiple sub-question variables into one plottable multi-response variable
#'
#' Takes multiple variables representing sub-questions and combines each user's responses
#' into a list, making it easy to analyze and plot individual or aggregate responses.
#' Each response is tagged with its sub-question label for easy plotting.
#'
#' @param data A data frame containing the sub-question variables
#' @param variables Character vector of variable names to combine
#' @param new_var_name Name for the new combined variable
#'
#' @return The data frame with the new multi-response variable added (as a list column)
#'
#' @examples
#' # Combine satisfaction sub-questions into one variable
#' data <- regroup_special_var(data, c("v_1", "v_2", "v_3"), "satisfaction_combined")
#' # Access user 1's responses: data$satisfaction_combined[[1]]
regroup_special_var <- function(data, variables, new_var_name, new_var_label) {
  # Get the first variable's labels to use as levels
  first_var_labels <- attr(data[[variables[1]]], "labels")

  # Get sub-question labels
  subquestion_labels <- sapply(variables, function(var) {
    label <- attr(data[[var]], "label")
    if (is.null(label)) return(var)
    return(label)
  })

  # For each row (user), collect their responses to all sub-questions
  user_responses <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    responses <- rep(NA_character_, length(variables)) # Initialize with NAs

    for (j in seq_along(variables)) {
      var <- variables[j]
      var_value <- data[[var]][i]

      # Skip if NULL, length 0, or NA
      if (is.null(var_value) || length(var_value) == 0 || is.na(var_value)) next

      # Get the value labels (if available)
      value_labels <- attr(data[[var]], "labels")

      if (!is.null(value_labels)) {
        # Find the label for this value
        response_label <- names(value_labels)[value_labels == var_value]
        if (length(response_label) > 0) {
          responses[j] <- response_label[1]
        } else {
          responses[j] <- as.character(var_value)
        }
      } else {
        responses[j] <- as.character(var_value)
      }
    }

    # Store this user's responses as factor with names attribute for sub-questions
    if (!is.null(first_var_labels)) {
      level_names <- names(first_var_labels)
      user_responses[[i]] <- factor(responses, levels = level_names)
    } else {
      user_responses[[i]] <- factor(responses)
    }

    # Add sub-question labels as names
    names(user_responses[[i]]) <- subquestion_labels
  }

  # Add the new list column
  data[[new_var_name]] <- user_responses
  attr(data[[new_var_name]], "label") <- new_var_label
  attr(data[[new_var_name]], "labels") <- subquestion_labels

  return(data)
}


#' Apply multi-response combination to multiple question groups
#'
#' @param data A data frame
#' @param variables_list A list of character vectors, each containing variable names to combine
#' @param starting_new_var_num Starting number for naming new variables (v_X format)
#'
#' @return The data frame with all combined variables added
regroup_special <- function(data, variables_list, starting_new_var_num, new_var_labels) {
  cnt <- 0
  for (vars in variables_list) {
    new_var_name <- paste0("v_", starting_new_var_num + cnt)
    for (autre in vars) {
      if (attr(data[[autre]], "label") == "Autre" && length(attr(data[[autre]], "labels")) == 0) {
        autre_name <- paste0(new_var_name, "_autre")
        data[[autre_name]] <- data[[autre]]

        vars <- vars[vars != autre]
      }
    }
    data <- regroup_special_var(data, vars, new_var_name, new_var_labels[cnt + 1])
    cnt <- cnt + 1
  }
  return(data)
}
