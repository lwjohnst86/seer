library(ggplot2)
facet_groups_fun <- function(plot, facet_groups = NULL, facet_switch = 'both', facet_scales = 'free',
                         facet_space = 'free') {
    if (!is.null(facet_groups))
        plot <- plot + ggplot2::facet_grid(as.formula(facet_groups), scales = facet_scales,
                                     switch = facet_switch, space = facet_space)
    return(plot)
}

.is_logic <- function(x) {
    assertive::assert_is_logical(x)
}

.is_integer <- function(x) {
    assertive::assert_is_integer(x)
}

.is_length <- function(x, n) {
    assertive::assert_is_of_length(x, n)
}

.is_list <- function(list.object) {
    assertive::assert_is_list(list.object)
}

.is_vector <- function(value) {
    assertive::assert_is_vector(value)
}

.is_tbl_df <- function(data) {
    assertive::assert_is_tbl_df(data)
}

.is_df <- function(data) {
    assertive::assert_is_data.frame(data)
}

.is_string <- function(value) {
    assertive::assert_is_a_string(value)
}

.is_numeric <- function(value) {
    assertive::assert_is_numeric(value)
}

.is_character <- function(value) {
    assertive::assert_is_character(value)
}

.is_class <- function(obj, cl) {
    assertive::assert_is_inherited_from(obj, cl)
}
