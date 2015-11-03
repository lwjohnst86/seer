#' Visualize the results.
#'
#' After entering a trance, visualize the results of the analysis.
#' @param data Input data frame, sent from the \code{\link{trance}} command
#' @param ... Additional parameters
#'
#' @return Create a high level graphic.
#' @export
visualize <- function(data, ...) {
    UseMethod('visualize', data)
}

#' @rdname visualize
#' @param groups Groups to facet the graph into (see
#'   \code{\link[ggplot2]{facet_grid}}). Usage: 'horizontal~vertical',
#'   'horizontal~.', or '~vertical'. So if a variable \code{Var} is set as
#'   'Var~.' the variable will be set horizontally, while '~Var' will be set
#'   vertically.
#' @param ylab Y-axis label
#' @param xlab X-axis label
#' @param center.line Where the dotted line is set
#'
#' @export
visualize.main_reg <- function(data,
                               groups = '~Yterms',
                               ylab = 'Exposures',
                               xlab = 'Beta estimates',
                               center.line = c('0', '1', NULL)) {
    p <- data %>%
        dplyr::mutate(
            p.value = cut(
                p.value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                labels = c('<0.001', '<0.01', '<0.05', '>0.05'),
                ordered_result = TRUE
            ) %>%
                factor(., levels = rev(levels(.))),
            Yterms = Yterms %>%
                factor(., levels = unique(.)),
            Xterms = Xterms %>%
                factor(., levels = unique(.))
        ) %>%
        ggplot2::ggplot(ggplot2::aes(x = estimate, y = Xterms)) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low,
                                             xmax = conf.high,
                                             alpha = p.value),
                                height = 0) +
        ggplot2::geom_point(ggplot2::aes(size = p.value,
                                         alpha = p.value)) +
        ggplot2::scale_alpha_discrete(name = 'P-value') +
        ggplot2::scale_size_discrete(name = 'P-value') +
        ggplot2::labs(x = xlab, y = ylab) +
        ggplot2::theme(legend.position = 'bottom')

    # Add a dashed zero line if set.
    if (!is.null(center.line)) {
        center.line <- as.numeric(match.arg(center.line))
        p <- p + ggplot2::geom_vline(xintercept = center.line,
                                     linetype = 'dashed')
    }

    # Split the plot up by a group variable.
    if (!is.null(groups)) {
        p <- p + ggplot2::facet_grid(as.formula(groups))
    }

    return(p)
}

