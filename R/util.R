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

#' Creates a bar plot for a specified variable and saves it
#'
#' \code{create_bar_plot} Creates a bar plot for a specified variable and saves it
#'
#' @param data Data frame, data frame containing data to plot
#' @param variableName String, variable to consider
#' @param outFile String, name of the output file name. default: [var name]_barplot.jpg
#' @param check_labels bool, flag to check the ammount of different possible answers to exclude open text answer
#' @param remove_na bool, flag to remove NA from plot
#' @param percent bool, flag to choose data representation on plot (true: percents, false: count)
#'
#' @inheritParms rlang::args_dot_not_used
#'
#' @return Plot
#'
create_bar_plot <- function(data, variableName, ..., bar_plot_folder = file.path("..", "data", "plots", "barplot"), outFileName = "", check_labels = FALSE, remove_na = TRUE, percent = FALSE) {
  if (!dir.exists(bar_plot_folder)) {
    dir.create(bar_plot_folder, recursive = TRUE)
  }

  if (variableName %in% names(data)) {
    plot_data <- data

    plot_data[[variableName]] <- haven::as_factor(data[[variableName]])

    len_labels = length(attr(data[[variableName]], "labels"))

    if (remove_na) {
      plot_data <- plot_data |>
        dplyr::filter(!is.na(.data[[variableName]]))
    }

    if (check_labels && (len_labels >= 10 || len_labels <= 1)) {
      warning("Too many or too few labels")
      return(NULL)
    }

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[variableName]])) +
      ggplot2::geom_bar(fill = "#69b3a2", ggplot2::aes(y = if (percent) ggplot2::after_stat(count / sum(count) * 100) else ggplot2::after_stat(count))) +
      ggplot2::labs(
        title = variableName,
        y = if (percent) "Percentage (%)" else "Count"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 70, hjust = 1)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )

    if (outFileName == "") {
      outFileName <- paste0(variableName, "_barplot.png")
    }

    ggplot2::ggsave(file.path(bar_plot_folder, outFileName), plot = p)

    return(p)
  } else {
    stop(paste0(variableName, " is not a variable in the dataset provided."))
  }
}

#' Creates a heat map for two specified variables and saves it.
#'
#' \code{create_heat_map} Creates a heat map for two specified variables and saves it.
#'
#' @param data Data frame, data frame containing data to plot
#' @param variableName String, variable to consider
#' @param outFile String, name of the output file name. default: [var name]_barplot.jpg
#' @param check_labels bool, flag to check the ammount of different possible answers to exclude open text answer
#' @param remove_na bool, flag to remove NA from plot
#' @param percent bool, flag to choose data representation on plot (true: percents, false: count)
#'
#' @inheritParms rlang::args_dot_not_used
#'
#' @return Plot
#'
create_heat_map <- function(data, var1, var2, ..., cross_plot_folder = file.path("..", "data", "plots", "cross"), outFileName = "", check_labels = FALSE, remove_na = TRUE, percent = FALSE) {
  if (!dir.exists(cross_plot_folder)) {
    dir.create(cross_plot_folder, recursive = TRUE)
  }

  if (var1 %in% names(data) && var2 %in% names(data)) {
    len_labels_var1 <- length(attr(data[[var1]], "labels"))
    len_labels_var2 <- length(attr(data[[var2]], "labels"))

    if (check_labels && (len_labels_var1 >= 10 || len_labels_var1 <= 1 ||
      len_labels_var2 >= 10 || len_labels_var2 <= 1)) {
      warning("too many or too few labels")
      return(invisible(NULL))
    }

    plot_data <- data

    if (remove_na) {
      plot_data <- plot_data |>
        dplyr::filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
    }

    plot_data <- plot_data |>
      dplyr::group_by(.data[[var1]], .data[[var2]]) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
      dplyr::rename(var1_col = 1, var2_col = 2) |>
      dplyr::mutate(
        var1_col = haven::as_factor(var1_col),
        var2_col = haven::as_factor(var2_col)
      )

    plot_data <- plot_data |>
      tidyr::complete(var1_col, var2_col, fill = list(count = 0))

    if (percent) {
      plot_data <- plot_data |>
        dplyr::mutate(
          percentage = count / sum(count) * 100,
          label_text = as.character(round(percentage))
        )
      fill_var <- "percentage"
      label_var <- "label_text"
    } else {
      plot_data <- plot_data |> dplyr::mutate(label_text = as.character(count))
      fill_var <- "count"
      label_var <- "label_text"
    }

    if (nrow(plot_data) == 0) {
      warning("Invalid variable combination: no data after filtering.")
      return(invisible(NULL))
    }

    g <- ggplot2::ggplot(plot_data, ggplot2::aes(x = var1_col, y = var2_col, fill = .data[[fill_var]])) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_viridis_c(
        name = if (percent) "Percentage (%)" else "Count",
        direction = -1
      ) +
      ggplot2::coord_fixed() +
      ggplot2::theme_minimal() +
      ggplot2::xlab(attr(data[[var1]], "label")[1]) +
      ggplot2::ylab(attr(data[[var2]], "label")[1]) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid = ggplot2::element_blank()
      ) +
      ggplot2::geom_text(ggplot2::aes(label = .data[[label_var]]),
        color = "white",
        size = 4, fontface = "bold"
      )


    if (outFileName == "") {
      outFileName <- paste0(var1, "_x_", var2, ".png")
    }

    ggplot2::ggsave(file.path(cross_plot_folder, outFileName), plot = g)

    return(g)
  } else {
    stop("At least one variable is not in provided data.")
  }
}


#' Creates a map visualization showing response distribution across geographic sectors
#'
#' \code{plot_on_map} Creates a choropleth map displaying the total count of responses
#' for a given variable across different geographic sectors. The map uses a shapefile
#' to visualize five specific sectors around Cointrin area.
#'
#' @param data Data frame, data frame containing data to plot
#' @param var String, variable to consider for counting responses
#' @param outFileName String, name of the output file. Default: [var]_map.png
#' @param remove_na bool, flag to remove NA values from plot. Default: TRUE
#' @param location_var String, variable name containing location codes (v_6). Default: "v_6"
#' @param percent bool, flag to display percentages instead of counts. Default: FALSE
#'
#' @inheritParams rlang::args_dot_not_used
#'
#' @return Plot (ggplot2 object with sf geometries)
#'
plot_on_map <- function(data, var, ..., map_plot_folder = file.path("..", "data", "plots", "maps"),
                        outFileName = "", remove_na = TRUE,
                        location_var = "v_6", percent = FALSE) {
  if (!dir.exists(map_plot_folder)) {
    dir.create(map_plot_folder, recursive = TRUE)
  }

  if (!var %in% names(data)) {
    stop(paste0(var, " is not a variable in the dataset provided."))
  }

  if (!location_var %in% names(data)) {
    stop(paste0(location_var, " is not a variable in the dataset provided."))
  }

  # Read shapefile
  neigh_map <- sf::st_read("../data/raw/GEO_GIREC.shp", quiet = TRUE)

  # Keep only the five sectors
  selected_sectors <- c(
    "Cointrin - Les Sapins", "Cointrin - Les Ailes",
    "Grand-Saconnex - Marais", "Vernier - Cointrin", "Le Jonc"
  )

  neigh_map_5 <- neigh_map |>
    dplyr::filter(NOM %in% selected_sectors) |>
    dplyr::mutate(
      sector = dplyr::recode(NOM,
        "Cointrin - Les Sapins"   = "Sapins",
        "Cointrin - Les Ailes"    = "Ailes",
        "Grand-Saconnex - Marais" = "Marais",
        "Vernier - Cointrin"      = "VernierCointrin",
        "Le Jonc"                 = "Jonc"
      ),
      v_6_code = dplyr::case_when(
        sector == "Sapins" ~ 1,
        sector == "Ailes" ~ 2,
        sector == "Marais" ~ 3,
        sector == "Jonc" ~ 4,
        sector == "VernierCointrin" ~ 5
      )
    )

  # Prepare data
  plot_data <- data

  if (remove_na) {
    plot_data <- plot_data |>
      dplyr::filter(
        !is.na(.data[[var]]),
        !is.na(.data[[location_var]])
      )
  }

  # Aggregate: ONE row per sector (this prevents scrambled maps)
  summary_data <- plot_data |>
    dplyr::group_by(v_6_code = as.numeric(.data[[location_var]])) |>
    dplyr::summarise(total_count = dplyr::n(), .groups = "drop")

  # Convert to percentages if requested
  if (percent) {
    total_responses <- sum(summary_data$total_count, na.rm = TRUE)
    summary_data <- summary_data |>
      dplyr::mutate(
        percentage = 100 * total_count / total_responses,
        display_value = round(percentage, 0)
      )
  } else {
    summary_data <- summary_data |>
      dplyr::mutate(display_value = total_count)
  }

  # Join to map
  map_data <- neigh_map_5 |>
    dplyr::left_join(summary_data, by = "v_6_code") |>
    dplyr::mutate(
      total_count   = dplyr::coalesce(total_count, 0),
      display_value = dplyr::coalesce(display_value, 0)
    )

  if (percent) {
    map_data <- map_data |>
      dplyr::mutate(
        percentage = dplyr::coalesce(percentage, 0)
      )
  }


  # Plot
  fill_var <- if (percent) "percentage" else "total_count"
  legend_name <- if (percent) "Percentage (%)" else "Count"

  p <- ggplot2::ggplot(map_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[fill_var]]),
      color = "black", linewidth = 0.8
    ) +
    ggplot2::geom_sf_text(ggplot2::aes(label = display_value),
      color = "white", fontface = "bold", size = 5
    ) +
    ggplot2::scale_fill_viridis_c(
      name = legend_name,
      na.value = "grey90",
      begin = 0.1, end = 0.9, direction = -1
    ) +
    ggplot2::labs(
      title = "Distribution of Responses by Sector",
      subtitle = paste("Variable:", var)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
    )

  # Save plot
  if (outFileName == "") {
    outFileName <- paste0(var, "_map.png")
  }

  ggplot2::ggsave(file.path(map_plot_folder, outFileName),
    plot = p, width = 10, height = 8
  )

  return(p)
}
