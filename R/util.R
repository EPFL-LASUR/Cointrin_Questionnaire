processed_data_folder <- "../data/processed"
bar_plot_folder = file.path(processed_data_folder, "barplot")
cross_plot_folder = file.path(processed_data_folder, "cross")


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
#' @inheritParms rlang::args_dot_used
#'
#' @return Plot
#'
create_bar_plot <- function(data, variableName, ..., outFileName = "", check_labels = FALSE, remove_na = TRUE, percent = FALSE) {
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
        panel.background = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      ) +
      ggplot2::geom_text(ggplot2::aes(label = .data[[label_var]]),
        color = "white",
        size = 4, fontface = "bold"
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
#' @inheritParms rlang::args_dot_used
#'
#' @return Plot
#'
create_heat_map <- function(data, var1, var2, ..., outFileName = "", check_labels = FALSE, remove_na = TRUE, percent = FALSE) {
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
        option = "plasma",
        name = if (percent) "Percentage (%)" else "Count"
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
#' @param answer_filter Numeric or String, specific answer value to filter for. If NULL, counts all responses. Default: NULL
#' @param percent bool, flag to display percentages instead of counts. Default: FALSE
#'
#' @inheritParams rlang::args_dot_used
#'
#' @return Plot (ggplot2 object with sf geometries)
#'
plot_on_map <- function(data, var, ..., outFileName = "", remove_na = TRUE, location_var = "v_6", answer_filter = NULL, percent = FALSE) {
  if (!var %in% names(data)) {
    stop(paste0(var, " is not a variable in the dataset provided."))
  }

  if (!location_var %in% names(data)) {
    stop(paste0(location_var, " is not a variable in the dataset provided."))
  }

  # Read shapefile
  neigh_map <- sf::st_read("../data/raw/GEO_GIREC.shp", quiet = TRUE)

  # Filter to selected sectors
  selected_sectors <- c(
    "Cointrin - Les Sapins", "Cointrin - Les Ailes",
    "Grand-Saconnex - Marais", "Vernier - Cointrin", "Le Jonc"
  )

  neigh_map_5 <- neigh_map |>
    dplyr::filter(NOM %in% selected_sectors) |>
    dplyr::mutate(
      sector = dplyr::recode(NOM,
        "Cointrin - Les Sapins"      = "Sapins",
        "Cointrin - Les Ailes"       = "Ailes",
        "Grand-Saconnex - Marais"    = "Marais",
        "Vernier - Cointrin"         = "VernierCointrin",
        "Le Jonc"                    = "Jonc"
      ),
      v_6_code = dplyr::case_when(
        sector == "Sapins" ~ 1,
        sector == "Ailes" ~ 2,
        sector == "Marais" ~ 3,
        sector == "Jonc" ~ 4,
        sector == "VernierCointrin" ~ 5,
        TRUE ~ NA_real_
      )
    )

  # Prepare data
  plot_data <- data

  if (remove_na) {
    plot_data <- plot_data |>
      dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[location_var]]))
  }

  # Filter for specific answer if provided (using numeric code before converting to factor)
  if (!is.null(answer_filter)) {
    plot_data <- plot_data |>
      dplyr::filter(as.numeric(.data[[var]]) == answer_filter)
  }

  # Aggregate data by v_6 code
  summary_data <- plot_data |>
    dplyr::group_by(v_6_code = as.numeric(.data[[location_var]])) |>
    dplyr::summarise(total_count = dplyr::n(), .groups = "drop")

  # Calculate percentage if requested
  if (percent) {
    total_responses <- sum(summary_data$total_count)
    summary_data <- summary_data |>
      dplyr::mutate(
        percentage = (total_count / total_responses) * 100,
        display_value = round(percentage, 0)
      )
  } else {
    summary_data <- summary_data |>
      dplyr::mutate(display_value = total_count)
  }

  # Join with map data
  map_data <- neigh_map_5 |>
    dplyr::left_join(summary_data, by = "v_6_code")

  # Replace NA values with 0
  if (percent) {
    map_data <- map_data |>
      dplyr::mutate(
        total_count = ifelse(is.na(total_count), 0, total_count),
        percentage = ifelse(is.na(percentage), 0, percentage),
        display_value = ifelse(is.na(display_value), 0, display_value)
      )
  } else {
    map_data <- map_data |>
      dplyr::mutate(
        total_count = ifelse(is.na(total_count), 0, total_count),
        display_value = ifelse(is.na(display_value), 0, display_value)
      )
  }

  # Create plot with labels
  # Get the answer label for subtitle if filtering
  subtitle_text <- if (!is.null(answer_filter)) {
    answer_label <- names(attr(data[[var]], "labels"))[attr(data[[var]], "labels") == answer_filter]
    if (length(answer_label) > 0) {
      paste("Variable:", attr(data[[var]], "label"), "| Answer:", answer_label)
    } else {
      paste("Variable:", var, "| Answer code:", answer_filter)
    }
  } else {
    paste("Variable:", var)
  }

  # Determine fill variable and labels
  fill_var <- if (percent) "percentage" else "total_count"
  legend_name <- if (percent) "Percentage (%)" else "Count"

  p <- ggplot2::ggplot(map_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[fill_var]]), color = "black", linewidth = 0.8) +
    ggplot2::geom_sf_text(ggplot2::aes(label = display_value), color = "white", size = 6, fontface = "bold") +
    ggplot2::scale_fill_viridis_c(
      option = "plasma",
      name = legend_name,
      na.value = "grey90",
      begin = 0.1,
      end = 0.9
    ) +
    ggplot2::labs(
      title = paste("Distribution of Responses by Sector"),
      subtitle = subtitle_text
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
    )

  # Save plot
  if (outFileName == "") {
    if (!is.null(answer_filter)) {
      outFileName <- paste0(var, "_", answer_filter, "_map.png")
    } else {
      outFileName <- paste0(var, "_map.png")
    }
  }

  ggplot2::ggsave(file.path(processed_data_folder, outFileName), plot = p, width = 10, height = 8)

  return(p)
}
