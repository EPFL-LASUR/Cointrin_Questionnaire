processed_data_folder <- "../data/processed"
bar_plot_folder = file.path(processed_data_folder, "barplot")
cross_plot_folder = file.path(processed_data_folder, "cross")



create_bar_plot <- function(data, variableName, ..., outFileName = "", check_labels = FALSE, remove_na = TRUE, percent = FALSE) {
  if (variableName %in% names(data)) {
    plot_data <- data

    plot_data[[variableName]] <- haven::as_factor(data[[variableName]])

    len_labels = length(attr(data[[variableName]], "labels"))

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
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 70, hjust = 1))

    if (outFileName == "") {
      outFileName <- paste0(variableName, "_barplot.png")
    }

    ggplot2::ggsave(file.path(bar_plot_folder, outFileName), plot = p)
  } else {
    stop(paste0(variableName, " is not a variable in the dataset provided."))
  }
}


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
          label_text = sprintf("%.1f%%", percentage)
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
      ggplot2::geom_text(ggplot2::aes(label = .data[[label_var]]),
        color = "white",
        size = 4, fontface = "bold"
      ) +
      ggplot2::scale_fill_gradientn(
        colors = c("#adacac", "#c97f7f", "#ff0000"),
        values = scales::rescale(c(0, 0.01, max(plot_data[[fill_var]]))),
        limits = c(0, NA),
        name = if (percent) "Percentage (%)" else "Count"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::xlab(attr(data[[var1]], "label")[1]) +
      ggplot2::ylab(attr(data[[var2]], "label")[1]) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid = ggplot2::element_blank()
      ) +
      ggplot2::geom_text(ggplot2::aes(label = .data[[label_var]]),
        color = "black",
        size = 4, fontface = "bold"
      )

    if (outFileName == "") {
      outFileName <- paste0(var1, "_x_", var2, ".png")
    }

    ggplot2::ggsave(file.path(cross_plot_folder, outFileName), plot = g)
  } else {
    stop("At least one variable is not in provided data.")
  }
}
