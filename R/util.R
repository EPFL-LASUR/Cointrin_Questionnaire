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
