
#' @export
view_main_effect <-
    function(data,
             graph.options = c('default', 'dot.size', 'grey.scale', 'dot.grey'),
             ylab = 'Exposures',
             xlab = 'Beta estimates',
             groups = '~Yterms',
             estimate.type = c('beta', 'OR'),
             center.line = TRUE,
             legend.title = 'P-value',
             dot.size.range = c(0.75, 4),
             dot.colour = "black",
             grey.scale.range = c(0, 0.75),
             group.label.switch = c(NULL, 'both', 'x', 'y')) {

    .is_df(data)
    .is_logic(center.line)
    .is_length(dot.size.range, 2)
    .is_length(grey.scale.range, 2)
    estimate.type <- match.arg(estimate.type)
    label.switch <- match.arg(group.label.switch)

    graph.options <- match.arg(graph.options)
    switch(graph.options,
           default = {
               p <- .graph_main_effect_default(data, dot.colour = dot.colour)
           },
           dot.size = {
               data <- .convert_pvalue_to_discrete(data)
               p <- suppressWarnings(
                   .graph_main_effect_dot.size(data, legend.title, dot.size.range)
                   )
           },
           grey.scale = {
               data <- .convert_pvalue_to_discrete(data)
               p <- .graph_main_effect_grey.scale(data, legend.title, grey.scale.range)
           },
           dot.grey = {
               data <- .convert_pvalue_to_discrete(data)
               p <-
                   suppressWarnings(
                       .graph_main_effect_dot_grey(
                           data,
                           legend.title,
                           grey.scale.range = grey.scale.range,
                           dot.size.range = dot.size.range
                       )
                   )
           })

    # Add a dashed zero line if set.
    if (center.line) {
        switch(estimate.type,
               beta = {
                   center.line <- 0
               },
               OR = {
                   center.line <- 1
               })
        p <- p + ggplot2::geom_vline(xintercept = center.line,
                                     linetype = 'dashed')
    }

    p <- p + ggplot2::labs(x = xlab, y = ylab)

    # Split the plot up by a group variable.
    p <- facet_groups_fun(p, facet_groups = groups, facet_switch = label.switch)

    return(p)
}

.convert_pvalue_to_discrete <- function(data, p.value.variable = 'p.value') {
    if (! p.value.variable %in% names(data)) {
        stop('Please make sure there is a variable called p.value in the dataset.')
    }
        data %>%
            dplyr::mutate(
                p.value =
                    cut(
                        p.value,
                        breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                        labels = c('<0.001', '<0.01', '<0.05', '>0.05'),
                        ordered_result = TRUE
                    ) %>%
                    factor(., levels = rev(levels(.)))
            )
}

.graph_main_effect_default <- function(data, dot.colour = "black") {
    data %>%
        ggplot2::ggplot(ggplot2::aes(x = estimate, y = Xterms)) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low,
                                             xmax = conf.high),
                                height = 0, colour = dot.colour) +
        ggplot2::geom_point(colour = dot.colour)
}

.graph_main_effect_dot.size <- function(data, legend.title, dot.size.range, dot.colour = "black") {
    data %>%
        ggplot2::ggplot(ggplot2::aes(x = estimate, y = Xterms)) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low,
                                             xmax = conf.high),
                                height = 0, colour = dot.colour) +
        ggplot2::geom_point(ggplot2::aes(size = p.value), colour = dot.colour) +
        ggplot2::scale_size_discrete(name = legend.title,
                                     range = dot.size.range)
}

.graph_main_effect_grey.scale <- function(data, legend.title, grey.scale.range) {
    data %>%
        ggplot2::ggplot(ggplot2::aes(x = estimate, y = Xterms)) +
        ggplot2::geom_errorbarh(ggplot2::aes(
            xmin = conf.low,
            xmax = conf.high,
            colour = p.value
        ),
        height = 0) +
        ggplot2::geom_point(ggplot2::aes(colour = p.value)) +
        ggplot2::scale_color_grey(start = grey.scale.range[2],
                                  end = grey.scale.range[1],
                                  name = legend.title)
}

.graph_main_effect_dot_grey <- function(data, legend.title, dot.size.range, grey.scale.range) {
    data %>%
        ggplot2::ggplot(ggplot2::aes(x = estimate, y = Xterms)) +
        ggplot2::geom_errorbarh(ggplot2::aes(
            xmin = conf.low,
            xmax = conf.high,
            colour = p.value
        ),
        height = 0) +
        ggplot2::geom_point(ggplot2::aes(size = p.value,
                                         colour = p.value)) +
        ggplot2::scale_size_discrete(name = legend.title,
                                     range = dot.size.range) +
        ggplot2::scale_color_grey(start = grey.scale.range[2],
                                  end = grey.scale.range[1],
                                  name = legend.title)
}
