#' Flexible Scatter Plot Wrapper for ggplot2 extensions.
#'
#' The gg_scatter() function is a custom wrapper around ggplot2 that generates a scatter plot with optional features such as color fill, point size, correlation test, and faceting. It is designed to be flexible and user-friendly for exploratory data analysis.,
#'
#'
#' @export
#' @param  data : Dataset (data frame or tibble)
#' @param  x : a variable name to appear on the x axis (must be numeric).
#' @param  y : a variable name to appear on the y axis (must be numeric).
#' @param  fill : Variable to map to point fill color (optional).
#' @param  size: Variable to map to point size (optional).
#' @param  shape: Controls for point shape, border color, and transparency.
#' @param  color : Controls for point border color, and transparency.
#' @param  alpha : Controls for point transparency.
#' @param  size_n : Default size when size is not specified.
#' @param  fill_n : Default fill when fill color is not specified.
#' @param  cor_test : If TRUE, adds a Pearson correlation coefficient to the plot
#' @param  facet_wrap : If TRUE, facets the plot using a categorical variable
#' @param  wrap_with : Variable used to facet the plot.
#' @param  ncol, scale: Faceting controls.
#' @param  facet_legend: Whether to show the legend in faceted plots
#' @param ... : Additional arguments passed
#'
#'
#'
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("gapminder", quietly = TRUE)) {
#'   library(gapminder)
#'   library(ggplot2)
#'
#'   # Basic scatter plot
#'   gg_scatter(gapminder, x = gdpPercap, y = lifeExp)
#'
#'   # With fill color
#'   gg_scatter(gapminder, x = gdpPercap, y = lifeExp, fill = continent)
#'
#'   # With size and fill
#'   gg_scatter(gapminder, x = gdpPercap, y = lifeExp, fill = continent, size = pop)
#'
#'   # With correlation test
#'   gg_scatter(gapminder, x = gdpPercap, y = lifeExp, cor_test = TRUE)
#'
#'   # With faceting
#'   gg_scatter(gapminder, x = gdpPercap, y = lifeExp, fill = continent,
#'              facet_wrap = TRUE, wrap_with = continent, facet_legend = TRUE)
#' }
#' }





gg_scatter <- function(data, x, y, fill = NULL, size = NULL, alpha = 1, shape = 21, color = "white",cor_test = FALSE,
                       size_n = 3, fill_n = 3, facet_wrap = FALSE, wrap_with = NULL, ncol = 3, scale = "free",facet_legend = FALSE,...) {
  x_expr <- rlang::enquo(x)
  y_expr <- rlang::enquo(y)
  fill_expr <- rlang::enquo(fill)
  size_expr <- rlang::enquo(size)

  if(!require(tidyverse)){install.packages("tidyverse")}
  if(!require(ggpubr)){install.packages("ggpubr")}
  library(tidyverse)
  library(ggpubr)


  # Check if the variables are numeric in the data
  if (!is.numeric(pull(data, !!x_expr)) || !is.numeric(pull(data, !!y_expr))) {
    stop("Both x and y must be numeric columns in the data.", call. = TRUE)
  }else{

    # Create aes mapping
    if (rlang::quo_is_null(fill_expr)) {
      if (rlang::quo_is_null(size_expr)) {
        gg_plt <-  ggplot(data, aes(x = !!x_expr, y = !!y_expr)) +
          geom_point(shape = shape, color = color,size = size_n, fill = fill_n, alpha = alpha, show.legend = T)
      } else {
        gg_plt <-  ggplot(data, aes(x = !!x_expr, y = !!y_expr, size = !!size_expr)) +
          geom_point(shape = shape, color = color,fill = fill_n, alpha = alpha, show.legend = T)
      }
    } else {
      if (rlang::quo_is_null(size_expr)) {
        gg_plt <-  ggplot(data, aes(x = !!x_expr, y = !!y_expr, fill = !!fill_expr)) +
          geom_point(shape = shape, color = color, size = size_n, alpha = alpha, show.legend = T)
      } else {
        gg_plt <-  ggplot(data, aes(x = !!x_expr, y = !!y_expr, fill = !!fill_expr, size = !!size_expr)) +
          geom_point(shape = shape, color = color, alpha = alpha, show.legend = T)
      }
    }

    scatt_plot = gg_plt +
      geom_point(shape = shape, color = color, alpha = alpha, show.legend = T) +
      theme_minimal() +
      labs(
        title = paste(rlang::as_name(enquo(x)), "vs", rlang::as_name(enquo(y))),
        y = rlang::as_name(enquo(y)),
        x = rlang::as_name(enquo(x)),
        #fill = if (!rlang::quo_is_null(fill_expr)) rlang::as_name(enquo(fill)) else NULL
      )

    if (cor_test == TRUE) {
      plt_gg = scatt_plot +
        ggpubr::stat_cor(aes(fill = NULL,size = NULL),p.accuracy = 0.05,size = 3.2)
    }else {
      plt_gg = scatt_plot
    }

    if(facet_wrap == TRUE){
      if(facet_legend == TRUE){
        return(
          plt_gg +
            facet_wrap(paste("~",rlang::as_name(enquo(wrap_with))), ncol = ncol, scale = scale)
        )
      }else{
        return(
          plt_gg +
            facet_wrap(as.formula(paste("~",rlang::as_name(enquo(wrap_with)))), ncol = ncol, scale = scale) +
            theme(legend.position = "none")
        )
      }

    }else{
      return(plt_gg)
    }
  }
}

