#' Create a heatmap of values (usually correlation coefficients).
#'
#' @param data The correlation dataset.
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
#'
#' \dontrun{
#' ds <- data.frame(Var1 = rep(letters[1:6], times = 6),
#'      Var2 = rep(letters[1:6], each = 6),
#'      Values = round(runif(36, min = -1, max = 1), 2))
#'
#' view_heatmap(ds)
#' view_heatmap(ds, 'Var1', 'Var2', 'Values')
#' view_heatmap(ds, colours = c('darkblue', 'darkred'), values.text = FALSE)
#' view_heatmap(ds, 'Var1', 'Var2', 'Values', number.colours = 100)
#' view_heatmap(ds, 'Var1', 'Var2', 'Values', number.colours = 100, values.size = 10)
#' view_heatmap(ds, 'Var1', 'Var2', 'Values', number.colours = 100, values.size = 10)
#' }
#'
view_heatmap <-
    function(data, y = names(data[1]), x = names(data[2]), value = names(data[3]),
             colours = c('skyblue4', 'darkorange2'), number.colours = 2,
             values.text = TRUE, values.size = 5, ylab = '', xlab = '',
             legend.title = expression("Correlation" * ~ rho)) {
        .is_df(data)

        ## Color Palette for the heatmap
        ltom <- grDevices::colorRampPalette(c(colours[1], "white"))
        mtoh <- grDevices::colorRampPalette(c("white", colours[2]))

        ## Main plot
        p <- ggplot2::ggplot(data, ggplot2::aes_string(x, y)) +
            ggplot2::geom_tile(ggplot2::aes_string(fill = value)) +
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
        if (values.text)
            p <- p +
            ggplot2::geom_text(ggplot2::aes_string(label = value),
                               color = "#073642", size = values.size)

        return(p)
}

heatmap <- function(data, y, x, values, settings = list()) {

}

vision_opts <- list()

heatmap_settings <- list(
    # colour = list(
    #     low =
    # )
    low_colour = 'skyblue4',
    high_colour = 'darkorange2',
    number.colours = 2,
    text = TRUE,
    text_size = 5, ylab = '', xlab = '',
             legend.title = expression("Correlation" * ~ rho)

)
