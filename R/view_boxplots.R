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
#' view_boxplots(ds2, 'Measure', 'Value', dots = FALSE, facet_groups = '~HS.Grad')
#' view_boxplots(ds2, 'Measure', 'Value', dots = FALSE, groups = '~HS.Grad',
#'  group.label.pos = NULL)
#' view_boxplots(ds2, 'Measure', 'Value', dots = FALSE, groups = 'HS.Grad~.')
#' view_boxplots(ds2, 'Measure', 'Value', box.groups = 'HS.Grad')
#' }
#'
view_boxplots <-
    function(data, variables, values,
             box_group = NULL,
             facet_groups = NULL,
             dots = TRUE,
             dot_colour = 'grey50',
             group.label.pos = 'both') {

        if (!is.null(box_group) & is.null(facet_groups)) {
            p <- ggplot2::ggplot(data = data, aes_string(x = variables, y = values, fill = box_group))
        } else {
            p <- ggplot(data = data, aes_string(x = variables, y = values))
        }

        if (dots)
            p <- p + geom_jitter(colour = dot_colour)

        p <- p +
            geom_boxplot(outlier.shape = NA) +
            ## Axis labels are reversed because of flip
            coord_flip()

        p <- facet_groups_fun(p, facet_groups = facet_groups, facet_switch = group.label.pos)

        return(p)
    }

