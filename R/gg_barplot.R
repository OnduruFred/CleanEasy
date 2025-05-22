#' Flexible Bar Plot Wrapper for ggplot2 extensions.
#'
#' The gg_bar() function is a custom wrapper around ggplot2 that generates a bar plot with optional features such as grouping variables, fill colors, labels, and orientation. It is designed to be flexible and user-friendly for categorical data visualization.
#'
#' @export
#' @param data Dataset (data frame or tibble)
#' @param x A categorical variable name to appear on the x axis (must be non-numeric)
#' @param grp_var Optional grouping variable for stacked/grouped bars (must be non-numeric)
#' @param alpha Controls transparency of bars (0 to 1)
#' @param color Color for bar borders
#' @param lable_size Size for value labels
#' @param fill_n Default fill color when no grouping variable is specified
#' @param base_size Base font size for the plot
#' @param orientation Plot orientation ("v" for vertical or "h" for horizontal)
#' @param legend_pos Position of legend ("right", "left", "top", "bottom", or "none")
#' @param palette Color palette number (for grouped plots)
#' @param ... Additional arguments passed
#'
#' @examples
#' \dontrun{
#' # Basic bar plot
#' gg_bar(mtcars, x = cyl)
#'
#' # Grouped bar plot
#' gg_bar(mtcars, x = cyl, grp_var = am)
#'
#' # Horizontal orientation
#' gg_bar(mtcars, x = cyl, orientation = "h")
#' }




gg_bar <- function(data, x, grp_var = NULL, alpha = 1, color = "grey30", lable_size = 3.2,
                   fill_n = "steelblue4", base_size = 11, orientation = "v",
                   legend_pos = "right", palette = 5, ...) {

  x_expr <- rlang::enquo(x)
  grp_var_expr <- rlang::enquo(grp_var)

  if(!require(tidyverse)){install.packages("tidyverse")}
  if(!require(janitor)){install.packages("janitor")}
  if(!require(ggpubr)){install.packages("ggpubr")}
  library(tidyverse, quietly = TRUE)
  library(ggpubr, quietly = TRUE)
  library(janitor, quietly = TRUE)

  df <- data |> janitor::tabyl(!!x_expr) %>% adorn_pct_formatting()

  # Check if the variables are numeric in the data
  if (is.numeric(pull(data, !!x_expr))) {
    stop("x must be non-numeric in the data.", call. = TRUE)
  } else {

    # Create aes mapping
    if (rlang::quo_is_null(grp_var_expr)) {
      gg_plt <- df %>% ggplot(aes(x = !!x_expr, y = n)) +
        geom_col(width = 0.5, alpha = alpha, fill = fill_n, color = color)

    } else {
      if(rlang::as_name(enquo(x)) == rlang::as_name(enquo(grp_var))) {
        gg_plt <- df %>% ggplot(aes(x = !!x_expr, y = n, fill = !!grp_var_expr)) +
          geom_col(width = 0.5, color = color, alpha = alpha, show.legend = F)
      } else {

        if (is.numeric(pull(data, !!grp_var_expr))) {
          stop("grp_var must be non-numeric in the data.", call. = TRUE)
        } else {
          gg_plt <- data |> count(!!x_expr, !!grp_var_expr) |> group_by(!!grp_var_expr) %>%
            mutate(percent = paste0(round(n/sum(n)*100, 2), "%")) %>% ungroup() %>%
            ggplot(aes(x = !!x_expr, y = n, fill = !!grp_var_expr)) +
            geom_col(width = 0.5, color = color, alpha = alpha) +
            geom_text(aes(label = str_wrap(paste0(n, " (", percent, ")"), width = 2)),
                      position = position_stack(0.5),
                      size = lable_size) +
            theme_light(base_size = base_size) +
            scale_fill_brewer(palette = palette) +
            theme(legend.position = legend_pos)

          if (orientation == "h") {
            return(gg_plt + coord_flip())
          }
          if (orientation == "v") {
            return(gg_plt)
          } else {
            stop("Only allows 'h' and 'v' as orientation ('horizontal' and 'vertical')", call. = TRUE)
          }
        }
      }
    }
  }

  bar_plot <-
    gg_plt +
    geom_label(aes(label = str_wrap(paste0(n, " (", percent, ")"), width = 1)),
               size = lable_size, fill = "aliceblue") +
    theme_light(base_size = base_size) +
    scale_fill_brewer(palette = "Spectral")

  if (orientation == "h") {
    return(bar_plot + coord_flip())
  }
  if (orientation == "v") {
    return(bar_plot)
  } else {
    stop("Only allows 'h' and 'v' as orientation ('horizontal' and 'vertical')", call. = TRUE)
  }
}

