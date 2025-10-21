#' Generate axis breaks and limits for numeric data
#'
#' This function calculates nicely rounded axis breaks and limits for a given numeric vector.
#' It is designed to be used in plots where automatic and visually appealing axis scaling is desired.
#'
#' @param values Numeric vector. The data values for which axis breaks or limits are calculated.
#' @param breaks Logical. If TRUE, the function returns a sequence of breaks; if FALSE, it returns only the bottom and top limits.
#' @param top_ratio Numeric. A multiplier to extend the top of the axis range beyond the maximum data value (default is 1, no extension).
#' @param seq_length Numeric or NULL. Optional manual step size for axis breaks. If NULL, the function chooses a reasonable step automatically.
#' @param max_n_breaks Numeric. Maximum number of breaks (default is 16).
#'
#' @return If `breaks = TRUE`, a numeric vector of axis breaks. If `breaks = FALSE`, a numeric vector of length 2 with the bottom and top axis limits.
#'
#' @examples
#' set_breaks(c(2, 7, 15))
#' set_breaks(c(2, 7, 15), top_ratio = 1.2, breaks = FALSE)
#'
#' @export

set_breaks <- function(values, breaks = TRUE, top_ratio = 1, seq_length = NULL, max_n_breaks = 12) {

    min_n <- min(values, na.rm = TRUE)-0.02
    max_n <- max(values, na.rm = TRUE)+0.02

    max_n <- min_n + (max_n - min_n) * top_ratio


    if (is.null(seq_length)) {
        my_length <- c(.1, .2, .5, 1, 2, 4, 5, 8, 10, 15, 20, 25, 30, 40, 50, 100,
                       200, 250, 500, 1000, 2e3, 2.5e3, 3e3, 4e3, 5e3, 1e4, 2e4, 2.5e4,
                       3e4, 4e4, 5e4, 1e5, 2e5, 5e5, 1e6, 1e7, 1e8, 1e9, 1e10)

        diff <- (max_n - min_n) / max_n_breaks  - my_length

        seq_length <- my_length[diff == max(diff[diff < 0])][1]
    }


    top <- (max_n %/% seq_length + 1) * seq_length
    bottom <- (min_n %/% seq_length) * seq_length

    if (all(values > 0)) {
        bottom = max(bottom, 0)
    }

    if (breaks) {
        return(seq(bottom, top, seq_length))
    }else{
        return(c(bottom, top))
    }

}
