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

#' For 'main_effect', useful for presenting beta-coefficient results from
#' regression type analyses (eg. linear or logistic regression, GEE, mixed
#' effect, etc).
#'
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
visualize.main_effect <- function(data,
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
    p <- .plot_groups(p, groups)

    return(p)
}

#' For 'boxes_dots', useful for presenting multiple variables with a similar
#' scale or unit.
#'
#' @rdname visualize
#' @param box_groups Grouping by boxplot
#' @param dots Whether or not to include the dots on the chart
#' @param ylab Y-axis label
#' @param xlab X-axis label
#' @param groups Grouping by faceting
#'
#' @export
#' @examples
#'
#' ds <- data.frame(state.region, state.x77) %>% tbl_df()
#' ds %>%
#'     select(state.region, Illiteracy, Life.Exp, Murder, HS.Grad) %>%
#'     mutate(HS.Grad = as.numeric(HS.Grad < 50)) %>%
#'     trance('continuous_distrib') %>%
#'     visualize(box_groups = 'HS.Grad', groups = '~state.region')
#' ds %>%
#'     select(state.region, Illiteracy, Life.Exp, Murder, HS.Grad) %>%
#'     trance('continuous_distrib') %>%
#'     visualize(box_groups = 'state.region')
#' ds %>%
#'     select(state.region, Illiteracy, Life.Exp, Murder, HS.Grad) %>%
#'     trance('continuous_distrib') %>%
#'     visualize(groups = 'state.region ~ .')
#' ds %>%
#'     select(Illiteracy, Life.Exp, Murder, HS.Grad) %>%
#'     trance('continuous_distrib') %>%
#'     visualize(dots = FALSE)
#'
visualize.boxes_dots <-
    function(data,
             box_groups = NULL,
             groups = NULL,
             dots = TRUE,
             ylab = 'Variable',
             xlab = 'Value') {

        groups.name <- gsub(' +', '', groups) %>%
            gsub('~', '', .) %>%
            gsub('\\.$', '', .)

        p.df <- data %>%
            tidyr::gather_('Measure', 'Value', names(.) %>%
                               .[which(!. %in% c(box_groups, groups.name),
                                       arr.ind = TRUE)]) %>%
            dplyr::mutate(Measure = Measure %>%
                              factor(., levels = unique(.)))

        if (!is.null(box_groups) & is.null(groups)) {
            p <- p.df %>%
                ggplot2::ggplot(ggplot2::aes_string('Measure', 'Value', fill = box_groups))
        } else {
            p <- p.df %>%
                ggplot2::ggplot(ggplot2::aes_string('Measure', 'Value'))
        }

        if (dots)
            p <- p + ggplot2::geom_jitter()

        p <- p +
            ggplot2::geom_boxplot(outlier.shape = NA) +
            ggplot2::coord_flip() +
            ## Axis labels are reversed because of flip
            ggplot2::ylab(xlab) +
            ggplot2::xlab(ylab)

        p <- .plot_groups(p, groups)

        return(p)
    }



#' For 'heatmap', can create a matrix or non-matrix style. Useful when
#' presenting correlation coefficients.
#'
#' @rdname visualize
#' @param colours The spectrum of colours for the heatmap, as a vector between
#'   the lowest (negative) value and the highest (positive) value
#' @param number.colours Number of colour spectrums to use.
#' @param values Whether or not the values are put on the plot.
#' @param values.size Font size of the values.
#' @param ylab Y-axis label.
#' @param xlab X-axis label.
#' @param legend.title Title of the legend.
#' @return Creates a heatmap plot
#' @export
#' @examples
#' ## Correlation heatmap
#' ds <- data.frame(state.region, state.x77) %>% tbl_df()
#' design(ds, 'cor') %>%
#'     lay_base() %>%
#'     build() %>%
#'     polish() %>%
#'     trance('heatmap') %>%
#'     visualize() %>%
#'     vision_simple(legend.position = 'right')
#' design(ds, 'cor') %>%
#'     lay_base(c('Income', 'Population'), c('Murder', 'Illiteracy')) %>%
#'     build() %>%
#'     polish() %>%
#'     trance('heatmap') %>%
#'     visualize() %>%
#'     vision_simple(legend.position = 'right')
#'
visualize.heatmap <-
    function(data, colours = c('darkorange2', 'skyblue4'), number.colours = 1,
             values = TRUE, values.size = 5, ylab = '', xlab = '',
             legend.title = expression("Correlation" * ~ rho)) {

    if (!any(class(data) %in% 'cor_df'))
        stop('Please pass only a "cor_df" dataset, from the mason package.')

    ## Color Palette for the heatmap
    ltom <- colorRampPalette(c(colours[1], "white"))
    mtoh <- colorRampPalette(c("white", colours[2]))

    data <- data %>%
        mutate(
            Value = round(Value, 2),
            Yvar = factor(Yvar, levels = unique(Yvar)),
            Xvar = factor(Xvar, levels = rev(unique(Xvar)))
        )

    ## Main plot
    p <- ggplot2::ggplot(data, ggplot2::aes(Xvar, Yvar)) +
        ggplot2::geom_tile(ggplot2::aes(fill = Value)) +
        ggplot2::scale_fill_gradient(
            name = legend.title,
            low = ltom(number.colours),
            high = mtoh(number.colours),
            breaks = seq(-1.0, 1.0, by = 0.5),
            limits = c(-1.0, 1.0)
        ) +
        ggplot2::scale_x_discrete(expand = c(0, 0)) +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggplot2::labs(x = xlab, y = ylab)

    ## Add the correlation values to the plot if TRUE.
    if (values)
        p <- p +
        ggplot2::geom_text(ggplot2::aes(label = Value),
                           color = "#073642", size = values.size)

    return(p)
}

#' Generates a plot similar to the GWAS Manhattan plots, which are
#' useful to show significance across multiple significance testings.
#'
#' See the example for a better idea of how to use the function.
#' This style of plot is really useful to use when you have run many
#' eg. interaction testing in a regression analysis and you want to
#' see which variables are barely significant vs very significant,
#' etc.  Thus, multiple comparison problems can be dealt with as the
#' plot shows how significant a variable is compared to the rest of
#' the significance tests.  This is generally the same reason why
#' GWAS studies use Manhattan plots.
#'
#' @rdname visualize
#' @param data Dataset from a regression with the p-values
#' @param groups The column that splits the tests up, usually is the
#' dependent variable if the data has been looped through a
#' regression test (eg. see \code{\link[mason]{build}}).
#' @param ylab The label for the y-axis.
#' @export
#' @examples
#'
#' \dontrun{
#' ## Interaction, GEE
#' library(mason) ## install_github('lwjohnst86/mason')
#' ds <- data.frame(state.region, state.x77) %>% tbl_df()
#' gee.df <- design(ds, 'gee') %>%
#'     lay_base('state.region', c('Income', 'Frost'),
#'              c('Population', 'Murder'), 'Life.Exp',
#'              intvar = 'Life.Exp') %>%
#'     build() %>%
#'     polish(':', TRUE)
#' gee.df %>% trance('interaction') %>% visualize()
#' gee.df %>% trance('interaction') %>% visualize(groups = 'Yterms~.')
#' gee.df %>% trance('interaction') %>% visualize() %>% vision_simple()
#' gee.df %>% trance('interaction') %>% visualize() %>% vision_sparse()
#' }
#'
visualize.interaction <-
    function(data, groups = '~Yterms', ylab = 'Exposure') {
        p <- data %>%
            dplyr::rename_('P' = 'p.value') %>%
            ## -log10(P) is typically used for Manhattan Plots in GWAS.
            ggplot2::ggplot(ggplot2::aes(-log10(P), Xterms)) +
            ggplot2::geom_point() +
            ggplot2::geom_vline(xintercept = -log10(0.05), linetype = 'dotted') +
            ggplot2::geom_vline(xintercept = -log10(0.001), linetype = 'dashed') +
            ggplot2::coord_cartesian(xlim = c(0,-log10(0.001) + 0.5)) +
            ggplot2::geom_segment(ggplot2::aes(
                x = 0,
                xend = -log10(P),
                y = Xterms,
                yend = Xterms
            )) +
            ggplot2::labs(y = ylab,
                          x = '-log10(P)\nDotted line: p<0.05\nDashed line: p<0.001') +
            ggplot2::theme(legend.position = 'none')

        p <- .plot_groups(p, groups)

        return(p)
    }