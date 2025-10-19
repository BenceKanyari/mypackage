#' Custom ggplot2 theme with optional automatic axis scaling
#'
#' This function defines a custom ggplot2 theme based on `ggthemes::theme_calc()`
#' with additional formatting for text, axes, panels, and legends. When used
#' with `ggplot_add()`, it can automatically adjust the y-axis (and optionally x-axis)
#' limits and breaks per panel in faceted plots, allowing for flexible autoscaling
#' while optionally enforcing a zero baseline.
#'
#' @param top_ratio numeric. Ratio to extend the top of the axis in autoscale mode.
#'   Useful for adding padding above the maximum value.
#' @param autoscale logical. If TRUE, automatically adjusts y-axis (and x-axis, if `cont_x = TRUE`)
#'   limits and breaks based on the data and facets.
#' @param zero_y_min logical. If TRUE, sets the y-axis minimum to zero.
#' @param zero_x_min logical or NULL. If TRUE, sets the x-axis minimum to zero.
#'   If NULL, the default is inferred based on the data type.
#' @param cont_x logical. If TRUE, enforces continuous scaling and autoscaling for the x-axis.
#' @param date_x logical. If TRUE, enforces date scaling by years for the x-axis.
#' @param n_breaks integer. Approximate number of breaks to generate for axes.
#'
#' @return A `my_theme` object (a list with class `"my_theme"`) that can be added to a ggplot
#'   object using the `+` operator.
#' @export
my_theme <- function(top_ratio = 1, autoscale = TRUE, zero_y_min = FALSE,
                     zero_x_min = NULL, cont_x = FALSE, date_x = FALSE,
                     n_breaks = 16) {
    structure(list(top_ratio = top_ratio, autoscale = autoscale,
                   zero_y_min = zero_y_min, zero_x_min = zero_x_min,
                   cont_x = cont_x, date_x = date_x, n_breaks = n_breaks),
              class = "my_theme")
}



#' @rdname my_theme
#' @export
#' @importFrom ggplot2 ggplot_add
#' @importFrom ggh4x facetted_pos_scales
#' @import tidyverse
#' @method ggplot_add my_theme
ggplot_add.my_theme <- function(object, plot, object_name) {
    top_ratio <- object$top_ratio
    autoscale <- object$autoscale
    zero_y_min <- object$zero_y_min
    zero_x_min <- object$zero_x_min
    cont_x <- object$cont_x
    date_x <- object$date_x
    n_breaks <- object$n_breaks

    # Base theme (always applied)
    base_theme <- ggthemes::theme_calc() +
        theme(
            line = element_line(linetype = 1, colour = "black"),
            rect = element_rect(fill = "white", linetype = 0, colour = NA),
            text = element_text(colour = "black"),
            title = element_text(size = rel(2)),
            panel.background = element_rect(fill = "white", colour = NA),
            plot.background = element_rect(fill = "white", colour = NA),
            panel.grid = element_blank(),
            panel.spacing.x = unit(25, "pt"),
            panel.spacing.y = unit(10, "pt"),
            plot.margin = unit(c(.5, .5, .5, .5), "lines"),
            axis.title = element_blank(),
            axis.text = element_text(face = "bold", size = 12),
            axis.line = element_line(),
            axis.line.y = element_line(),
            legend.position = "inside",
            legend.position.inside = c(.05, .95),
            legend.direction = "horizontal",
            legend.box = "horizontal",
            legend.background = element_rect(fill = NA, colour = NA),
            legend.box.margin = unit(c(0, 1, -.5, 1), "lines"),
            legend.justification = "left",
            legend.text = element_text(size = 12),
            legend.title = element_blank(),
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(color = "black", size = 14, face = "bold"),
            plot.title = element_text(hjust = .5, face = "bold", size = 16),
            plot.subtitle = element_text(size = 14, hjust = .5, margin = margin(1, 3, 10, 3)),
            plot.caption = element_text(hjust = 0, size = 8),
            plot.caption.position = "plot",
            plot.title.position = "plot"
        )

    if (!autoscale) return(plot + base_theme)

    plot0 <- ggplot2::ggplot_build(plot)

    if (plot0$layout$panel_params[[1]]$y$is_discrete()) {

        if (is.null(zero_x_min)) {
            zero_x_min <- TRUE
        }

        plot0 <- ggplot2::ggplot_build(plot + scale_x_continuous(expand = c(0,0)))

        n_breaks <- n_breaks/2

        if (inherits(plot$facet, "FacetWrap") || inherits(plot$facet, "FacetGrid")) {


            ncols <- dplyr::n_distinct(plot0$layout$layout$COL)

            max_n_breaks <-  ceiling(n_breaks/ncols/1.1)

            n_panels <- length(plot0$layout$panel_params)
            scales <- vector("list", n_panels)


            if (zero_x_min) {
                for (i in seq_len(n_panels)) {
                    yr <- plot0$layout$panel_params[[i]]$x.range
                    scales[[i]] <- scale_x_continuous(
                        limits = c(0,set_breaks(yr, breaks = FALSE, max_n_breaks = max_n_breaks)[2]),
                        breaks = set_breaks(yr, breaks = TRUE, max_n_breaks = max_n_breaks),
                        expand = c(0, 0),
                        sec.axis = dup_axis()
                    )
                }
            }else{
                for (i in seq_len(n_panels)) {
                    yr <- plot0$layout$panel_params[[i]]$x.range
                    scales[[i]] <- scale_x_continuous(
                        limits = set_breaks(yr, breaks = FALSE, max_n_breaks = max_n_breaks),
                        breaks = set_breaks(yr, breaks = TRUE, max_n_breaks = max_n_breaks),
                        expand = c(0, 0),
                        sec.axis = dup_axis()
                    )
                }
            }

            plot_out <- plot +
                base_theme +
                theme(
                    plot.margin = unit(c(.5, 1.5, .5, .5), "lines"),
                    panel.spacing.x = unit(40, "pt"),
                    panel.spacing.y = unit(20, "pt"),
                ) +
                ggh4x::facetted_pos_scales(x = scales)

        }else{
            x_range <- plot0$layout$panel_params[[1]]$x.range

            if (zero_x_min) {
                limits_x <- c(0,set_breaks(x_range, breaks = FALSE,
                                           top_ratio = top_ratio, max_n_breaks = n_breaks)[2])
            }else{
                limits_x <- set_breaks(x_range, breaks = FALSE,
                                       top_ratio = top_ratio, max_n_breaks = n_breaks)
            }

            breaks_x <- set_breaks(x_range, top_ratio = top_ratio, max_n_breaks = n_breaks)

            plot_out <- plot +
                base_theme +
                theme(
                    plot.margin = unit(c(.5, 1.5, .5, .5), "lines"),
                    panel.spacing.x = unit(40, "pt"),
                    panel.spacing.y = unit(20, "pt"),
                ) +
                scale_x_continuous(
                    sec.axis = dup_axis(),
                    limits = limits_x,
                    breaks = breaks_x,
                    expand = expansion(mult = c(0, 0))
                )
        }


    }else{

        if (is.null(zero_x_min)) {
            zero_x_min <- FALSE
        }

        plot0 <- ggplot2::ggplot_build(plot + scale_y_continuous(expand = c(0,0)))

        if (inherits(plot$facet, "FacetWrap") || inherits(plot$facet, "FacetGrid")) {


            nrows <- dplyr::n_distinct(plot0$layout$layout$ROW)

            max_n_breaks <-  ceiling(n_breaks/nrows/1.1)

            n_panels <- length(plot0$layout$panel_params)
            scales <- vector("list", n_panels)


            if (zero_y_min) {
                for (i in seq_len(n_panels)) {
                    yr <- plot0$layout$panel_params[[i]]$y.range
                    scales[[i]] <- scale_y_continuous(
                        limits = c(0,set_breaks(yr, breaks = FALSE, max_n_breaks = max_n_breaks)[2]),
                        breaks = set_breaks(yr, breaks = TRUE, max_n_breaks = max_n_breaks),
                        expand = c(0, 0),
                        sec.axis = dup_axis()
                    )
                }
            }else{
                for (i in seq_len(n_panels)) {
                    yr <- plot0$layout$panel_params[[i]]$y.range
                    scales[[i]] <- scale_y_continuous(
                        limits = set_breaks(yr, breaks = FALSE, max_n_breaks = max_n_breaks),
                        breaks = set_breaks(yr, breaks = TRUE, max_n_breaks = max_n_breaks),
                        expand = c(0, 0),
                        sec.axis = dup_axis()
                    )
                }
            }

            plot_out <- plot +
                base_theme +
                ggh4x::facetted_pos_scales(y = scales)

        }else{
            y_range <- plot0$layout$panel_params[[1]]$y.range

            if (zero_y_min) {
                limits_y <- c(0,set_breaks(y_range, breaks = FALSE,
                                           top_ratio = top_ratio, max_n_breaks = n_breaks)[2])
            }else{
                limits_y <- set_breaks(y_range, breaks = FALSE,
                                       top_ratio = top_ratio, max_n_breaks = n_breaks)
            }

            breaks_y <- set_breaks(y_range, top_ratio = top_ratio, max_n_breaks = n_breaks)

            plot_out <- plot +
                base_theme +
                scale_y_continuous(
                    sec.axis = dup_axis(),
                    limits = limits_y,
                    breaks = breaks_y,
                    expand = expansion(mult = c(0, 0))
                )
        }




        if (plot0$layout$panel_params[[1]]$x$is_discrete()) {

            plot_out <- plot_out +
                scale_x_continuous(
                    expand = expansion(mult = c(0.01, 0.01))
                )

        }else if (cont_x) {

            plot0 <- ggplot2::ggplot_build(plot + scale_x_continuous(expand = c(0,0)))

            n_breaks <- n_breaks/1.2

            if (inherits(plot$facet, "FacetWrap") || inherits(plot$facet, "FacetGrid")) {


                ncols <- dplyr::n_distinct(plot0$layout$layout$COL)

                max_n_breaks <-  ceiling(n_breaks/ncols/1.1)

                n_panels <- length(plot0$layout$panel_params)
                scales <- vector("list", n_panels)


                if (zero_x_min) {
                    for (i in seq_len(n_panels)) {
                        yr <- plot0$layout$panel_params[[i]]$x.range
                        scales[[i]] <- scale_x_continuous(
                            limits = c(0,set_breaks(yr, breaks = FALSE, max_n_breaks = max_n_breaks)[2]),
                            breaks = set_breaks(yr, breaks = TRUE, max_n_breaks = max_n_breaks),
                            expand = c(0, 0),
                            sec.axis = dup_axis()
                        )
                    }
                }else{
                    for (i in seq_len(n_panels)) {
                        yr <- plot0$layout$panel_params[[i]]$x.range
                        scales[[i]] <- scale_x_continuous(
                            limits = set_breaks(yr, breaks = FALSE, max_n_breaks = max_n_breaks),
                            breaks = set_breaks(yr, breaks = TRUE, max_n_breaks = max_n_breaks),
                            expand = c(0, 0),
                            # sec.axis = dup_axis()
                        )
                    }
                }

                plot_out <- plot_out +
                    theme(
                        axis.title = element_text(size = 12, face = "bold")
                    ) +
                    ggh4x::facetted_pos_scales(x = scales)

            }else{
                x_range <- plot0$layout$panel_params[[1]]$x.range

                if (zero_x_min) {
                    limits_x <- c(0,set_breaks(x_range, breaks = FALSE,
                                               top_ratio = top_ratio, max_n_breaks = n_breaks)[2])
                }else{
                    limits_x <- set_breaks(x_range, breaks = FALSE,
                                           top_ratio = top_ratio, max_n_breaks = n_breaks)
                }

                breaks_x <- set_breaks(x_range, top_ratio = top_ratio, max_n_breaks = n_breaks)

                plot_out <- plot_out +
                    theme(
                        axis.title = element_text(size = 12, face = "bold")
                    ) +
                    scale_x_continuous(
                        # sec.axis = dup_axis(),
                        limits = limits_x,
                        breaks = breaks_x,
                        expand = expansion(mult = c(0, 0))
                    )
            }

        }else if(date_x){

            plot0 <- ggplot2::ggplot_build(plot + scale_x_continuous(expand = c(0,0)))

            x_range <- plot0$layout$panel_params[[1]]$x.range

            x_diff <- diff(x_range)

            date_breaks <- case_when(
                x_diff <= 366*10 ~ "-1 years",
                x_diff <= 366*20 ~ "-2 years",
                x_diff <= 366*50 ~ "-5 years",
                .default = "-10 years"
            )

            plot_out <- plot_out +
                scale_x_date(
                    breaks = seq.Date(floor_date(as.Date(x_range[2]), "years"), as.Date(x_range[1]), by = date_breaks),
                    date_labels = "%Y",
                    expand = expansion(mult = c(0.01, 0.01))
                )

        }

    }


    plot_out


}


