context("boxplot")

library(dplyr)
library(tidyr)

ds <- data.frame(state.region, state.x77) %>% tbl_df()
ds2 <- ds %>%
    mutate(HS.Grad = factor(HS.Grad < 50, label = c('Yes', 'No'))) %>%
    gather(Measure, Value, -state.region, -HS.Grad) %>%
    group_by(Measure) %>%
    mutate(Value = scale(Value))

test_that("multiplication works", {
    p <- view_boxplots(ds2, 'Measure', 'Value', dots = FALSE, groups = '~HS.Grad')
view_boxplots(ds2, 'Measure', 'Value', ylab = NULL, xlab = NULL, dots = FALSE, groups = '~HS.Grad')
    #p$layers[[1]]$geom$default_aes
    expect_is(p$coordinates, 'CoordFlip'
    p$coordinates, 'CoordCartesian'

    p$facet, 'Facet'
    p$facet, 'FacetNull'

    p$layers[[1]]$geom, 'GeomBoxPlot'

    expect_is(lyr, 'ggproto')
    class(p$layers[[1]]$geom)
    p$layers[[1]]$geom$required_aes
    p$labels
})
