# Data Wrangling Functions

#' Create summary statistics for individual participants
#' @param data Data frame with experimental data
#' @param group_vars Character vector of grouping variables
#' @param response_var Response variable to summarize
#' @param scale Scaling factor (e.g., 100 for percentages)
summarise_individual <- function(
  data,
  group_vars,
  response_var,
  scale = 1
) {
  data |>
    group_by(pid, across(all_of(group_vars))) |>
    summarise(
      mean_val = mean({{ response_var }}, na.rm = TRUE) * scale,
      sd_val = sd({{ response_var }}, na.rm = TRUE) * scale,
      .groups = "drop"
    )
}

#' Create summary statistics at the group level directly from raw data
#' @param data Raw data frame with trial-level data
#' @param group_vars Character vector of grouping variables
#' @param response_var Response variable to summarize
#' @param id_var Participant ID variable
#' @param scale Scaling factor (e.g., 100 for percentages)
summarise_group <- function(
  data,
  group_vars,
  response_var,
  id_var = pid,
  scale = 1
) {
  data |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      n = n_distinct({{ id_var }}),
      mean_val = mean({{ response_var }}, na.rm = TRUE) * scale,
      sd_val = sd({{ response_var }}, na.rm = TRUE) * scale,
      sem_val = sd_val / sqrt(n),
      .groups = "drop"
    )
}

#' Count trials with flexible grouping
#' @param data Data frame with trial data
#' @param ... Variables to group by
count_trials <- function(data, ...) {
  data |>
    group_by(...) |>
    tally() |>
    arrange(n)
}

# Visualization Functions

#' Create a simple, readable theme for consistent plots
create_clean_theme <- function(base_size = 28) {
  theme_bw(base_size = base_size) +
    theme(
      text = element_text(face = "bold"),
      title = element_text(face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
}

#' Create a raincloud plot using the gghalves package
#' @param data_ind Individual-level summary data
#' @param data_group Group-level summary data
#' @param x_var Variable to plot on y-axis (factors, conditions etc.)
#' @param y_var Variable to plot on x-axis (data values)
#' @param facet_var Variable to facet by (optional)
#' @param x_label Label for group categories axis
#' @param y_label Label for data values axis
#' @param title Plot title
#' @param alpha Transparency of the distributions
#' @param violin_side "l" or "r"
#' @param point_side "l" or "r"
#' @param point_spread point width or height for jitterring
#' @param flip Whether to flip the coordinates (TRUE = vertical orientation, FALSE = horizontal)
#' @param trim Whether to trim the density to the range of the data (like trim in geom_violin)
#' @param value_limits Vector of min and max values for the data value axis
#' @param value_breaks Vector of break points for the data value axis
#' @param x_spacing controls white space at the edge of the x axis
plot_rain <- function(
  data_ind,
  data_group,
  x_var = "GROUP",
  y_var = "mean_val",
  facet_var = NULL,
  x_label = "Training Condition",
  y_label = "Score",
  title = NULL,
  alpha = 0.5,
  violin_side = "r",
  point_side = "l",
  point_spread = .05,
  flip = TRUE,
  trim = TRUE,
  value_limits = NULL,
  value_breaks = NULL,
  x_spacing = c(0.2, 0.2)
) {
  # Create base plot - always in vertical orientation
  p <- ggplot(
    data_ind,
    aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[x_var]])
  )

  # Add the half_violin plot
  p <- p +
    gghalves::geom_half_violin(
      side = violin_side,
      trim = trim,
      alpha = alpha
    )

  # Add points with jitter
  p <- p +
    gghalves::geom_half_point(
      aes(colour = .data[[x_var]]),
      side = point_side,
      alpha = alpha,
      size = 2,
      transformation = position_jitter(width = point_spread, height = 0)
    )

  # Add mean points and error bars
  p <- p +
    geom_point(
      data = data_group,
      size = 3,
      colour = "black"
    ) +
    geom_errorbar(
      data = data_group,
      aes(ymin = mean_val - sem_val, ymax = mean_val + sem_val),
      colour = "black",
      width = 0.2,
      linewidth = 1
    )

  # Add a line connecting group means - modify for faceting
  if (!is.null(facet_var)) {
    p <- p +
      geom_line(
        data = data_group,
        aes(group = .data[[facet_var]]), # Group by facet variable
        linewidth = 0.7,
        color = "black"
      )
  } else {
    p <- p +
      geom_line(
        data = data_group,
        group = 1,
        linewidth = 0.7,
        color = "black"
      )
  }

  # Select color palette
  p <- p +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2")

  # Add faceting if specified
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(.data[[facet_var]]), scales = "free_x")
  }

  # Add axis scaling with limits if provided
  if (!is.null(value_limits) || !is.null(value_breaks)) {
    p <- p +
      scale_y_continuous(
        limits = value_limits,
        breaks = value_breaks,
        expand = expansion(mult = 0.05, add = c(0.1, 0.1))
      ) +
      scale_x_discrete(expand = expansion(mult = 0.01, add = x_spacing))
  } else {
    p <- p +
      scale_y_continuous(
        expand = expansion(mult = 0.05, add = c(0.1, 0.1))
      ) +
      scale_x_discrete(expand = expansion(mult = 0.01, add = x_spacing))
  }

  # Add labels
  p <- p +
    labs(
      title = title,
      x = x_label,
      y = y_label
    )

  # Apply coord_flip if needed
  if (!flip) {
    p <- p + coord_flip()
  }

  return(p)
}
