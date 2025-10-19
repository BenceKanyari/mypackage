#' Get a Contrasting Color for a Given Hex Color
#'
#' This function computes whether black or white would provide better contrast
#' against a specified hex color. It is useful for labeling or text placement
#' on colored backgrounds to ensure readability.
#'
#' @param hex_color Character. A hex color code (e.g., `"#FF0000"` or `"#00FF00"`).
#'
#' @return Character. Returns `"black"` if the input color is light, and `"white"` if the input color is dark.
#'
#' @examples
#' get_contrast_color("#FF0000")  # returns "white"
#' get_contrast_color("#FFFF00")  # returns "black"
#'
#' @export
get_contrast_color <- function(hex_color) {
    rgb <- grDevices::col2rgb(hex_color) / 255
    luminance <- 0.2126 * rgb[1,] + 0.7152 * rgb[2,] + 0.0722 * rgb[3,]
    ifelse(luminance > 0.5, "black", "white")
}
