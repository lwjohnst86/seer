
# based on knitr's system of doing options.
vision_defaults <- function(value = list()) {
    defaults <- value

    get <- function(name) {
        if (missing(name))
            defaults
        else if (length(name) == 1)
            defaults[[name]]
        else
            warning("Please supply one default name")
    }

    set <- function(...) {
        dots = list(...)
        if (length(dots) == 0)
            return()
        if (is.null(names(dots)) &&
            length(dots) == 1 && is.list(dots[[1]]))
            if (length(dots <- dots[[1]]) == 0)
                return()
        defaults <<- merge(dots)
        invisible(NULL)
    }

    merge <- function() {

    }


}