#' Plot the x loadings by components for PLS-DA models.
#'
#' Show the correlation circle plot of the loadings by component from the
#' partial least squares discriminant analysis.
#'
#' @param model The PLS-DA model generated from \code{\link[caret]{plsda}}.
#' @inheritParams view_pls_xloadings
#'
#' @return Prints the loadings of the X in PLS-DA by two components, with
#'  circles indicating 50% (dashed) and 100% (solid) explained variance.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # most of this example data is taken from the ?caret::plsda help.
#' library(caret)
#' data(mdrr)
#' set.seed(1)
#' inTrain <- sample(seq(along = mdrrClass), 450)
#' nzv <- nearZeroVar(mdrrDescr)
#' filteredDescr <- mdrrDescr[, -nzv]
#' training <- filteredDescr[inTrain,]
#' trainMDRR <- mdrrClass[inTrain]
#' preProcValues <- preProcess(training)
#' trainDescr <- predict(preProcValues, training)
#' fit   <- plsda(trainDescr, trainMDRR, ncomp = 5,
#'                     probMethod = "Bayes")
#' view_plsda_xloadings(fit)
#' view_plsda_xloadings(fit, title = 'Multidrug Resistance Reversal (MDRR) Agent Data: PLS-DA results')
#' view_plsda_xloadings(fit, title = 'Near-infrared radiation',
#'  renaming.x = function(x) gsub('RB', 'Protein ', x))
#' }
view_plsda_xloadings <- function(model, comps = 1:2, renaming.x = function(x) x, title = NULL) {
    .is_class(model, 'mvr')
    .is_class(model, 'plsda')
    fit <- model
    xloadings <-
        cor(model.matrix(fit), pls::scores(fit)[, comps, drop = FALSE]) %>%
        as.data.frame() %>%
        dplyr::add_rownames() %>%
        setNames(c('pred', 'x', 'y')) %>%
        dplyr::mutate(pred = renaming.x(pred))

    # circle_data in view_pls_xloadings
    circle_outer <- .circle_data(1)
    circle_inner <- .circle_data(sqrt(1/2))

    explained.var <- round((fit$Xvar / fit$Xtotvar) * 100, 1)

    xloadings %>%
        ggplot(aes(x = x, y = y)) +
        geom_segment(aes(x = -1, y = 0, xend = 1, yend = 0), colour = 'grey90') +
        geom_segment(aes(x = 0, y = -1, xend = 0, yend = 1), colour = 'grey90') +
        geom_path(data = circle_outer, aes(x = x, y = y)) +
        geom_path(data = circle_inner, aes(x = x, y = y), linetype = 'dotted') +
        geom_point() +
        geom_text(aes(label = pred), hjust = 0.5, vjust = 0, size = 4) +
        labs(
            x = paste0('Component 1 (', explained.var[1], '%)'),
            y = paste0('Component 2 (', explained.var[2], '%)'),
            title = title
        )
}
