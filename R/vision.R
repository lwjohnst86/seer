#' Vision experience (aka plot theme).
#'
#' Describe the vision, also known as setting the theme for your plot.
#' @param base_plot The base_plot sent from the \code{\link{visualize}} chain
#' @param base_size Font size
#' @param base_family Font family
#'
#' @return Customizes the appearance of the plot.
#' @name vision
#'
NULL


#' @rdname vision
#' @export
vision_sparse <-
    function(base_plot, base_size = 12, base_family = "Helvetica") {
        base_plot %>%
            magrittr::add(ggplot2::"%+replace%"(
                ggplot2::theme_bw(base_size = base_size, base_family = base_family),
                ggplot2::theme(
                    rect = ggplot2::element_blank(),
                    line = ggplot2::element_blank(),
                    text = ggplot2::element_blank(),
                    axis.ticks.margin = grid::unit(0, "lines"),
                    legend.position = 'none'
                )
            ))
    }

#' @rdname vision
#' @export
vision_simple <-
    function(base_plot, base_size = 12, base_family = "Helvetica") {
        base_plot %>%
            magrittr::add(ggplot2::"%+replace%"(
                ggthemes::theme_tufte(base_size = base_size, base_family = base_family),
                ggplot2::theme(
                    legend.key.width = grid::unit(0.7, "line"),
                    legend.key.height = grid::unit(0.7, "line"),
                    strip.background = ggplot2::element_rect(fill = 'grey95', colour = 'grey95'),
                    plot.margin = grid::unit(c(0.5, 0, 0, 0), "cm"),
                    legend.position = 'bottom'
                )
            ))
    }
