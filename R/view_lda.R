
#' @export
view_lda <- function(data, type = c('default', 'means', 'scaling'), orig.data = NULL,
                          group.name = NULL, label.rename.fun = function(x) x,
                          ordering.fun = function(x) x) {
    type <- match.arg(type)

    if (!requireNamespace('MASS', quietly = TRUE))
        stop('Please install or load the MASS package.')

    if (suppressWarnings(!any(is.na(as.numeric(data$lev)))))
        stop('Please convert the group variable to word strings ("yes", "no") rather than numeric strings ("0", "1").')


    if (type == 'default') {
        if (is.null(orig.data) & is.null(group.name) &
            (!is.data.frame(orig.data) | !is.character(group.name))) {
            stop('Please provide the original data used for the LDA and the name of the group variable.')
        }
        prop.lda <- round(data$svd^2/sum(data$svd^2) * 100, 2)
        p <-
            data.frame(Group = orig.data[, group.name],
                       lda = predict(object = data, newdata = orig.data)$x) %>%
            dplyr::rename(LD1 = lda.LD1, LD2 = lda.LD2) %>%
            dplyr::rename_('Group' = group.name) %>%
            ggplot2::ggplot(ggplot2::aes(LD1, LD2, group = Group)) +
            ggplot2::geom_vline(xintercept = 0, linetype = 'dotted') +
            ggplot2::geom_hline(yintercept = 0, linetype = 'dotted') +
            ggplot2::stat_density2d(ggplot2::aes(alpha = ..level.., colour = Group), size = 2) +
            ggplot2::labs(x = paste0("LD1 (", prop.lda[1], "%)"),
                 y = paste0('LD2 (', prop.lda[2], '%)')) +
            ggplot2::scale_alpha(guide = 'none')
    } else {
        if (type == 'means') {
            p <- .graph_lda_means(data[[type]], label.rename.fun, ordering.fun)
        } else if (type == 'scaling') {
            p <- .graph_lda_scaling(data[[type]], label.rename.fun)
        }
    }

    class(p) <- c(class(p), 'lda_plot')
    return(p)
}

.graph_lda_means <- function(data, label.rename.fun = function(x) x,
                             ordering.fun = function(x) x) {
    p.data <- data %>%
        as.data.frame() %>%
        dplyr::add_rownames('Group') %>%
        tidyr::gather(Variables, Mean, -Group) %>%
        dplyr::mutate(Group = Group %>%
                          ordering.fun) %>%
        dplyr::arrange(Group, Mean) %>%
        dplyr::mutate(Variables = paste0(Group, Variables) %>%
                   factor(., unique(.)))
    Var.label <- p.data$Variables %>% {
        group.levels <- unique(p.data$Group)
        for (ch in 1:length(group.levels)) {
            . <- gsub(group.levels[ch], '', .)
        }
        .
    } %>%
        label.rename.fun(.) %>%
        as.character()
    p.data %>%
        ggplot2::ggplot(ggplot2::aes(x = Mean, y = Variables)) +
        ggplot2::geom_point() +
        ggplot2::geom_vline(xintercept = 0) +
        ggplot2::facet_wrap( ~ Group, scale = 'free_y') +
        ggplot2::scale_y_discrete(breaks = as.character(p.data$Variables),
                                  label = Var.label)
}

.graph_lda_scaling <- function(data, label.rename.fun = function(x) x) {
    p.data <- data %>%
        as.data.frame() %>%
        dplyr::add_rownames('Variable') %>%
        tidyr::gather(LD, Value,-Variable) %>%
        dplyr::arrange(LD, Value) %>%
        dplyr::mutate(Variable = paste0(LD, Variable) %>%
                   factor(., unique(.)))
    Var.label <- p.data$Variable %>% {
        group.levels <- unique(p.data$LD)
        for (ch in 1:length(group.levels)) {
            . <- gsub(group.levels[ch], '', .)
        }
        .
    } %>%
        label.rename.fun(.) %>%
        as.character()
    p.data %>%
        ggplot2::ggplot(ggplot2::aes(x = Value, y = Variable)) +
        ggplot2::geom_point() +
        ggplot2::geom_vline(xintercept = 0) +
        ggplot2::facet_wrap(~ LD, scale = 'free_y') +
        ggplot2::scale_y_discrete(breaks = as.character(p.data$Variable),
                                  label = Var.label)
}