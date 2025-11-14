#' Custom Color Palette Selector
#'
#' @description
#' Returns a vector of color hex codes from predefined palettes.
#' The function supports several color sets designed for plots and visual consistency.
#'
#' @param x Integer vector specifying which colors to return (default: `1:12`).
#' @param palette Character string specifying which palette to use.
#'
#' @details
#' If an unrecognized palette name is provided, the function defaults to `"default"`
#' and issues a message.
#'
#' @return A character vector of hex color codes.
#'
#' @examples
#' my_colors()                   # first 12 colors from the default palette
#' my_colors(1:5, "distinct")    # first 5 distinct colors
#' my_colors(c(1, 2), "pos_neg") # positive/negative color pair
#'
#' @export
my_colors <- function(x = 1:12, palette = "default") {

    if (palette == "default") {
        mycolors <- c("#E22222", "#550F13FF", "#A10000", "grey55", "#C0C0C0",
                      "#3D5481",  "#A3C1E1", "#303030", "#F28E8E", "grey35",
                      "#30633E", "#92BB73","#469059", "#CAD589", "#A23F3B", "#D6D7DB",
                      "#6363F4", "#9696F8", "#A9A9A9", "#F19B99", "#ED6E69", "#A869AF")
    } else if (palette == "distinct") {
        mycolors <- c(
            "#EE0011FF", "#0C1BB9FF", "#A1C720FF", "#FFD320FF", "#AA1AE8", "#FF950EFF",
            "#149BEDFF","#EC579AFF","#16A08CFF","#9A703EFF","#7E0021FF","#B5B5B5"
        )
    } else if (palette == "mol") {
        mycolors <- c(
            "#E22222", "#2D2D2D", "#A20000", "#7F7F7F", "#F28E8E","#C0C0C0",
            "#228B22", "#A3C1E1", "#90EE90", "#3D5481", "#6B8E23", "#1113E1"
        )
    } else if (palette == "pos_neg") {
        mycolors <- c(
            "#088158FF",
            "#BA2F2AFF"
        )
    } else {
        message("Unrecognized palette. Defaulting to 'default'.")
        mycolors <- c("#E22222", "#550F13FF", "#A10000", "grey55", "#C0C0C0", "#3D5481",
                      "#A3C1E1", "#303030", "#F28E8E", "grey35", "#30633E", "#469059",
                      "#92BB73", "#CAD589", "#A23F3B", "#D6D7DB")
    }

    as.character(mycolors[x])

}
