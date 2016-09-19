#' Generates a plot similar to the GWAS Manhattan plots, which are useful to
#' show significance across multiple significance testings.
#'
#' See the example for a better idea of how to use the function. This style of
#' plot is really useful to use when you have run many eg. interaction testing
#' in a regression analysis and you want to see which variables are barely
#' significant vs very significant, etc.  Thus, multiple comparison problems can
#' be dealt with as the plot shows how significant a variable is compared to the
#' rest of the significance tests.  This is generally the same reason why GWAS
#' studies use Manhattan plots.
#'
#' @param data Dataset from a regression with the p-values
#' @param groups The column that splits the tests up, usually is the dependent
#'   variable for regression type analyses.
#' @param ylab The label for the y-axis.
#'
#' @export
view_interaction <-
    function(data, groups = '~Yterms', ylab = 'Exposure') {
        .is_df(data)

        if (!'p.value' %in% names(data)) {
            stop('Please make sure there is a variable called p.value in the dataset.')
        }

        p <- data %>%
            dplyr::rename_('P' = 'p.value') %>%
            ## -log10(P) is typically used for Manhattan Plots in GWAS.
            ggplot(ggplot2::aes(-log10(P), Xterms)) +
            geom_point() +
            geom_vline(xintercept = -log10(0.05), linetype = 'dotted') +
            geom_vline(xintercept = -log10(0.001), linetype = 'dashed') +
            coord_cartesian(xlim = c(0, -log10(0.001) + 0.5)) +
            geom_segment(ggplot2::aes(
                x = 0,
                xend = -log10(P),
                y = Xterms,
                yend = Xterms
            )) +
            labs(y = ylab,
                 x = '-log10(P)\nDotted line: p<0.05\nDashed line: p<0.001') +
            theme(legend.position = 'none')

        p <- .plot_groups(p, groups)

        return(p)
    }
