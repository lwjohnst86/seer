#' Plot the x loadings by components for PLS models.
#'
#' @param pls.model The PLS model generated from \code{\link[pls]{plsr}}.
#' @param comps The component numbers, e.g. for the first 2 it would be '1:2',
#'   for the first and third, 'c(1,3)', and so on.
#' @param renaming.x A function to renaming the x variables for the PLS model.
#' @param title
#'
#' @return Prints the loadings of the x in PLS by two components, with circles indicating
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(pls)
#' data(yarn)
#'
#' fit <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
#' view_pls_xloadings(fit)
#'
#' fit <- plsr(density ~ NIR, 6, data = yarn)
#' view_pls_xloadings(fit)
#' view_pls_xloadings(fit, title = 'Near-infrared radiation')
#' view_pls_xloadings(fit, title = 'Near-infrared radiation',
#'  renaming.x = function(x) gsub('NIR', 'Wave ', x))
#' }
view_pls_xloadings <- function(pls.model, comps = 1:2, renaming.x = function(x) x, title = NULL) {
    .is_class(pls.model, 'mvr')
    fit <- pls.model
    xloadings <-
        cor(model.matrix(fit), pls::scores(fit)[, comps, drop = FALSE]) %>%
        as.data.frame() %>%
        dplyr::add_rownames() %>%
        setNames(c('pred', 'x', 'y')) %>%
        dplyr::mutate(pred = renaming.x(pred))

    circle_outer <- .circle_data(1)
    circle_inner <- .circle_data(sqrt(1/2))

    xloadings %>%
        ggplot(aes(x = x, y = y)) +
        geom_segment(aes(x = -1, y = 0, xend = 1, yend = 0), colour = 'grey90') +
        geom_segment(aes(x = 0, y = -1, xend = 0, yend = 1), colour = 'grey90') +
        geom_path(data = circle_outer, aes(x = x, y = y)) +
        geom_path(data = circle_inner, aes(x = x, y = y), linetype = 'dotted') +
        geom_point() +
        geom_text(aes(label = pred), hjust = 0.5, vjust = 0, size = 4) +
        labs(
            x = paste0('Component 1 (', round(pls::explvar(fit)[1], 1), '%)'),
            y = paste0('Component 2 (', round(pls::explvar(fit)[2], 1), '%)'),
            title = title
        )
}

.circle_data <-
    function(radius = 1,
             center = c(0, 0),
             npoints = 100) {
        #r = diameter / 2
        r <- radius
        tt <- seq(0, 2 * pi, length.out = npoints)
        xx <- center[1] + r * cos(tt)
        yy <- center[2] + r * sin(tt)
        return(data.frame(x = xx, y = yy))
    }