#' Export a ggplot to PowerPoint
#'
#' This function exports a `ggplot2` plot to a PowerPoint (`.pptx`) file.
#' If the file already exists, the plot is added as a new slide; otherwise, a new
#' presentation is created. The plot can be sized automatically to fill the slide
#' or manually by specifying width and height.
#'
#' @param plot A `ggplot` object to export. Defaults to the last plot produced (`ggplot2::last_plot()`).
#' @param file_name Character. Name of the PowerPoint file. Defaults to `"plot.pptx"`.
#' @param width Numeric. Optional width of the plot on the slide in inches. If `NULL`, uses slide width.
#' @param height Numeric. Optional height of the plot on the slide in inches. If `NULL`, uses slide height.
#'
#' @return Returns the original `ggplot` object invisibly, allowing for piping or further modifications.
#'
#' @details
#' - Uses the `officer` package to manipulate PowerPoint files and `rvg` to convert
#'   `ggplot2` objects into editable vector graphics.
#' - The slide layout is set to `"Title and Content"` and the master to `"Office Theme"`.
#' - If both `width` and `height` are `NULL`, the plot fills the entire slide.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' plot_to_ppt(p, "myplot.pptx")
#' }
#'
#' @export
plot_to_ppt <- function (plot = ggplot2::last_plot(), file_name = "plot.pptx",
          width = NULL, height = NULL){

    if (file_name %in% list.files()) {
        doc <- officer::read_pptx(file_name)
    }
    else {
        doc <- officer::read_pptx()
    }
    x <- rvg::dml(ggobj = plot)
    doc <- officer::add_slide(doc, layout = "Title and Content",
                              master = "Office Theme")
    if (is.null(width) & is.null(height)) {
        doc <- officer::ph_with(doc, x, location = officer::ph_location_fullsize())
        print(doc, target = file_name)
    }
    else {
        sz <- officer::slide_size(doc)
        if (is.null(width)) {
            width <- sz$width
        }
        if (is.null(height)) {
            height <- sz$height
        }
        doc <- officer::ph_with(doc, x, location = officer::ph_location(left = 0,
                                                                        top = 0, width = width, height = height))
        print(doc, target = file_name)
    }
    plot
}
