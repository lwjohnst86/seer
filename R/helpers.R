.plot_groups <- function(p, groups = NULL) {
    if (!is.null(groups))
        p <- p + ggplot2::facet_grid(as.formula(groups))
    return(p)
}