#' For 'boxes_dots', useful for presenting multiple variables with a similar
#' scale or unit.
#'
#' @param box.groups Grouping by boxplot
#' @param dots Whether or not to include the dots on the chart
#' @param ylab Y-axis label
#' @param xlab X-axis label
#' @param groups Grouping by faceting
#'
#' @return Creates a plot with boxplots and dots.
#' @export
#' @examples
#'
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#' ds <- data.frame(state.region, state.x77) %>% tbl_df()
#' ds2 <- ds %>%
#'     mutate(HS.Grad = factor(HS.Grad < 50, label = c('Yes', 'No'))) %>%
#'     gather(Measure, Value, -state.region, -HS.Grad) %>%
#'     group_by(Measure) %>%
#'     mutate(Value = scale(Value))
#'
#' view_boxplots(ds2, 'Measure', 'Value', dots = FALSE, groups = '~HS.Grad')
#' view_boxplots(ds2, 'Measure', 'Value', dots = FALSE, groups = '~HS.Grad',
#'  group.label.pos = NULL)
#' view_boxplots(ds2, 'Measure', 'Value', dots = FALSE, groups = 'HS.Grad~.')
#' view_boxplots(ds2, 'Measure', 'Value', box.groups = 'HS.Grad')
#' }
#'
view_boxplots <-
    function(data, variables, values,
             box.groups = NULL,
             groups = NULL,
             dots = TRUE,
             ylab = 'Variables',
             xlab = 'Value',
             group.label.pos = 'both') {

        if (!is.null(box.groups) & is.null(groups)) {
            p <- ggplot(data = data, aes_string(x = variables, y = values, fill = box.groups))
        } else {
            p <- ggplot(data = data, aes_string(x = variables, y = values))
        }

        if (dots)
            p <- p + geom_jitter()

        p <- p +
            geom_boxplot(outlier.shape = NA) +
            coord_flip() +
            ## Axis labels are reversed because of flip
            ylab(xlab) +
            xlab(ylab)

        p <- .plot_groups(p, groups, label.switch = group.label.pos)

        return(p)
    }

