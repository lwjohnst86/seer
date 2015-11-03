#' Enter a trance.
#'
#' Start the visualization process by indicating the type of trance to enter,
#' ie. which type of visualization you would like.
#' @param data Input data set to create graphs from
#' @param type 'Trance' (visualization) type (eg. 'main_reg' for main effect results
#'   for regression)
#'
#' @return Output the input dataframe with a new class added, to be used by
#'   \code{\link{visualize}}.
#'
#' @export
trance <- function(data,
                   type = c('main_reg', 'interaction', 'cor_df')) {
    class(data) <- c(match.arg(type), class(data))
    return(data)
}
