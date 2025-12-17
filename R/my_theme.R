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
#' @param datetime_x logical. If TRUE, enforces datetime scaling by years for the x-axis.
#' @param n_breaks integer. Approximate number of breaks to generate for axes.
#'
#' @return A `my_theme` object (a list with class `"my_theme"`) that can be added to a ggplot
#'   object using the `+` operator.
#' @export
my_theme <- function(top_ratio = 1, autoscale = TRUE, zero_y_min = FALSE,
                     zero_x_min = NULL, cont_x = FALSE, date_x = FALSE, datetime_x = FALSE,
                     n_breaks = 12, sec_y_axis = NULL, seq_y_length = NULL) {
    structure(list(top_ratio = top_ratio, autoscale = autoscale,
                   zero_y_min = zero_y_min, zero_x_min = zero_x_min,
                   cont_x = cont_x, date_x = date_x, datetime_x = datetime_x,
                   n_breaks = n_breaks, sec_y_axis = sec_y_axis, seq_y_length = seq_y_length),
              class = "my_theme")
}


.freq_recognition <- function(x) {
    items <- sort(unique(x)) |>
        head(20) # should be enough, but many tests

    Ds <- base::diff(items) # Time differences in days

    recognised_freq <- dplyr::case_when(
        all(Ds > 0 & Ds <= 4) ~ "daily",
        all(Ds >= 360 & Ds <= 370) ~ "annual",
        all(Ds >= 28 & Ds <= 32) ~ "monthly",
        all(Ds >= 84 & Ds <= 93) ~ "quarterly",
        all(Ds >= 6 & Ds <= 8) ~ "weekly",
        all(Ds >= 12 & Ds <= 16) ~ "biweekly", # unpausables
        all(Ds >= 56 & Ds <= 64) ~ "bimonthly",
        all(Ds >= 180 & Ds <= 184) ~ "biannual",
        .default = "annual"
    )

    recognised_freq
}


#' @rdname my_theme
#' @export
#' @importFrom ggplot2 ggplot_add
#' @importFrom ggh4x facetted_pos_scales
#' @importFrom lubridate as_datetime
#' @method ggplot_add my_theme
ggplot_add.my_theme <- function(object, plot, object_name) {
    top_ratio <- object$top_ratio
    autoscale <- object$autoscale
    zero_y_min <- object$zero_y_min
    zero_x_min <- object$zero_x_min
    cont_x <- object$cont_x
    date_x <- object$date_x
    datetime_x <- object$datetime_x
    n_breaks <- object$n_breaks
    sec_y_axis <- object$sec_y_axis
    seq_y_length <- object$seq_y_length

    if (is.null(sec_y_axis)) {
        sec_y_axis <- sec_y_axis <- function(orig_breaks = breaks_y) {
            dup_axis()
        }
    }else if(sec_y_axis == FALSE){
        sec_y_axis <- sec_y_axis <- function(orig_breaks = breaks_y) {
            ggplot2::waiver()
        }
    }else{

        formula <- sec_y_axis
        sec_y_axis <- function(orig_breaks = breaks_y) {
            sec_axis(formula, breaks = with(list(. = orig_breaks), eval(formula[[2]])))
        }
    }

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
            axis.text.x.bottom = element_text(margin = margin(2.5,0,0,0)),
            axis.text.x.top = element_text(margin = margin(0,0,2.5,0)),
            axis.text.y.left = element_text(margin = margin(0,2.5,0,0)),
            axis.text.y.right = element_text(margin = margin(0,0,0,2.5)),
            axis.ticks.length.x = unit(4, "pt"),
            axis.ticks.length.y = unit(2.5, "pt"),
            legend.position = "inside",
            legend.position.inside = c(0.05, 1),
            legend.justification = c(0, 1),
            legend.margin = margin(0,0,0,0),
            legend.direction = "horizontal",
            legend.box = "horizontal",
            legend.background = element_rect(fill = NA, colour = NA),
            legend.box.margin = unit(c(0, 1, -.5, 1), "lines"),
            legend.text = element_text(size = 12),
            legend.title = element_blank(),
            legend.key = element_rect(fill = NA),
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(color = "black", size = 14, face = "bold"),
            plot.title = element_text(hjust = .5, face = "bold", size = 16,  margin = margin(1, 1, 12, 1, "pt")),
            plot.subtitle = element_text(size = 14, hjust = .5, margin = margin(-9, 3, 12, 3, "pt")),
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


            for (i in seq_len(n_panels)) {
                xr <- plot0$layout$panel_params[[i]]$x.range

                if (zero_x_min & min(xr) >= 0) {
                    scales[[i]] <- scale_x_continuous(
                        limits = c(0,set_breaks(xr, top_ratio = top_ratio, breaks = FALSE, max_n_breaks = max_n_breaks)[2]),
                        breaks = set_breaks(xr, top_ratio = top_ratio, breaks = TRUE, max_n_breaks = max_n_breaks),
                        expand = c(0, 0),
                        sec.axis = dup_axis()
                    )
                } else{
                    scales[[i]] <- scale_x_continuous(
                        limits = set_breaks(xr, top_ratio = top_ratio, breaks = FALSE, max_n_breaks = max_n_breaks),
                        breaks = set_breaks(xr, top_ratio = top_ratio, breaks = TRUE, max_n_breaks = max_n_breaks),
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
                    legend.position = "top",
                    legend.direction = "horizontal",
                ) +
                scale_y_discrete(expand = expansion(add = .5)) +
                ggh4x::facetted_pos_scales(x = scales)

        }else{
            x_range <- plot0$layout$panel_params[[1]]$x.range

            if (zero_x_min & (min(x_range) >= 0 | max(x_range) <= 0)) {
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
                scale_y_discrete(expand = expansion(add = .5)) +
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

            for (i in seq_len(n_panels)) {
                yr <- plot0$layout$panel_params[[i]]$y.range

                breaks_y <- set_breaks(yr, top_ratio = top_ratio, breaks = TRUE, seq_length = seq_y_length, max_n_breaks = max_n_breaks)

                if (zero_y_min & (min(yr) >= 0 | max(yr) <= 0)) {
                    scales[[i]] <- scale_y_continuous(
                        limits = c(0,set_breaks(yr, top_ratio = top_ratio, breaks = FALSE, seq_length = seq_y_length, max_n_breaks = max_n_breaks)[2]),
                        breaks = breaks_y,
                        expand = c(0, 0),
                        sec.axis = sec_y_axis(breaks_y)
                    )
                } else{
                    scales[[i]] <- scale_y_continuous(
                        limits = set_breaks(yr, top_ratio = top_ratio, breaks = FALSE, seq_length = seq_y_length, max_n_breaks = max_n_breaks),
                        breaks = breaks_y,
                        expand = c(0, 0),
                        sec.axis = sec_y_axis(breaks_y)
                    )
                }
            }

            plot_out <- plot +
                ggh4x::facetted_pos_scales(y = scales) +
                base_theme +
                theme(
                    legend.position = "top",
                    legend.direction = "horizontal",
                )

        }else{
            y_range <- plot0$layout$panel_params[[1]]$y.range

            if (zero_y_min & (min(y_range) >= 0 | max(y_range) <= 0)) {
                limits_y <- c(0,set_breaks(y_range, breaks = FALSE,
                                           top_ratio = top_ratio, seq_length = seq_y_length, max_n_breaks = n_breaks)[2])
            }else{
                limits_y <- set_breaks(y_range, breaks = FALSE,
                                       top_ratio = top_ratio, seq_length = seq_y_length, max_n_breaks = n_breaks)
            }

            breaks_y <- set_breaks(y_range, top_ratio = top_ratio, seq_length = seq_y_length, max_n_breaks = n_breaks)

            plot_out <- plot +
                base_theme +
                scale_y_continuous(
                    sec.axis = sec_y_axis(breaks_y),
                    limits = limits_y,
                    breaks = breaks_y,
                    expand = expansion(mult = c(0, 0))
                )
        }




        if (plot0$layout$panel_params[[1]]$x$is_discrete()) {

            plot_out <- plot_out +
                scale_x_discrete(
                    expand = expansion(add = .5)
                )

        }else if (cont_x) {

            plot0 <- ggplot2::ggplot_build(plot + scale_x_continuous(expand = c(0,0)))

            n_breaks <- n_breaks/1.2

            if (inherits(plot$facet, "FacetWrap") || inherits(plot$facet, "FacetGrid")) {


                ncols <- dplyr::n_distinct(plot0$layout$layout$COL)

                max_n_breaks <-  ceiling(n_breaks/ncols/1.1)

                n_panels <- length(plot0$layout$panel_params)
                scales <- vector("list", n_panels)


                for (i in seq_len(n_panels)) {
                    xr <- plot0$layout$panel_params[[i]]$x.range

                    breaks_x <- set_breaks(xr, breaks = TRUE, max_n_breaks = max_n_breaks)

                    if (zero_x_min & (min(xr) >= 0 | max(xr) <= 0)) {
                        scales[[i]] <- scale_x_continuous(
                            limits = c(0,set_breaks(xr, breaks = FALSE, max_n_breaks = max_n_breaks)[2]),
                            breaks = breaks_x,
                            expand = c(0, 0)
                        )
                    } else{
                        scales[[i]] <- scale_x_continuous(
                            limits = set_breaks(xr, breaks = FALSE, max_n_breaks = max_n_breaks),
                            breaks = breaks_x,
                            expand = c(0, 0)
                        )
                    }
                }


                plot_out <- plot_out +
                    theme(
                        axis.title = element_text(size = 12, face = "bold")
                    ) +
                    ggh4x::facetted_pos_scales(x = scales)  +
                    theme(
                        legend.position = "top",
                        legend.direction = "horizontal",
                    )

            }else{
                x_range <- plot0$layout$panel_params[[1]]$x.range

                if (zero_x_min & (min(x_range) >= 0 | max(x_range) <= 0)) {
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
                        limits = limits_x,
                        breaks = breaks_x,
                        expand = expansion(mult = c(0, 0))
                    )
            }

        }else if(date_x){

            plot0 <- ggplot2::ggplot_build(plot)

            x_time <- unique(unlist(lapply(plot0$data, function(d) d$x)))

            x_time <- sort(as.Date(x_time))


            # Detect frequency (assuming you have a helper like .freq_recognition)
            freq <- .freq_recognition(x_time)

            freq_n <- case_when(
                freq == "daily" ~ 1,
                freq == "weekly" ~ 2,
                freq == "biweekly" ~ 3,
                freq == "monthly" ~ 4,
                freq == "bimonthly" ~ 5,
                freq == "quarterly" ~ 6,
                freq == "biannual" ~ 7,
                .default = 8
            )

            # Years covered
            years_covered <- as.numeric(difftime(max(x_time), min(x_time), units = "days")) / 365


            by <- case_when(
                years_covered >= 50 ~ "10 years",
                years_covered >= 20 ~ "5 years",
                years_covered >= 10 ~ "2 years",
                years_covered >= 4 ~ "1 year",

                years_covered < .02 & freq_n <= 1 ~ "1 day",
                years_covered < .18 & freq_n <= 2 ~ "1 week",
                years_covered < .67 & freq_n <= 4 ~ "1 month",
                years_covered < 1.34 & freq_n <= 5 ~ "2 months",
                years_covered < 2 & freq_n <= 6 & freq != 5 ~ "3 months",
                freq_n <= 7 ~ "6 months",
            )


            # Compute breaks
            x_breaks <- seq(from = as.Date(max(x_time)), to = as.Date(min(x_time)), by = paste0("-",by))

            # Avoid too fine breaks for coarser frequencies
            if (str_detect(by, "year")) {
                x_breaks <- unique(as.Date(paste0(format(x_breaks, "%Y"), "-01-01")))
            }


            if (by %in% c("1 day", "1 week")) {
                x_labels <- format(x_breaks, "%d %b")
            } else if (by %in% c("1 month", "2 months")) {
                x_labels <- format(x_breaks, "%b %y")
            } else if (by %in% c("3 months", "6 months") & freq_n <= 4) {
                x_labels <- format(x_breaks, "%b %y")
            } else if (by %in% c("3 months", "6 months") & freq == "quarterly") {
                x_labels <- paste0(format(x_breaks, "%y"), "Q", ceiling(as.numeric(format(x_breaks, "%m")) / 3))
            } else if (by == "6 months" & freq == "biannual") {
                x_labels <- paste0(format(x_breaks, "%y"), "-S", ceiling(as.numeric(format(x_breaks, "%m")) / 6))
            } else {
                x_labels <- format(x_breaks, "%Y")
            }

            plot_out <- plot_out +
                scale_x_date(
                    breaks = x_breaks,
                    labels = x_labels,
                    expand = expansion(mult = c(0.01, 0.01))
                )

        }else if(datetime_x){

            plot0 <- ggplot2::ggplot_build(plot)

            x_time <- unique(unlist(lapply(plot0$data, function(d) d$x)))

            x_time <- sort(lubridate::as_datetime(x_time))


            # Detect frequency (assuming you have a helper like .freq_recognition)
            freq <- .freq_recognition(x_time)

            freq_n <- case_when(
                freq == "daily" ~ 1,
                freq == "weekly" ~ 2,
                freq == "biweekly" ~ 3,
                freq == "monthly" ~ 4,
                freq == "bimonthly" ~ 5,
                freq == "quarterly" ~ 6,
                freq == "biannual" ~ 7,
                .default = 8
            )

            # Years covered
            years_covered <- as.numeric(difftime(max(x_time), min(x_time), units = "days")) / 365


            by <- case_when(
                years_covered >= 50 ~ "10 years",
                years_covered >= 20 ~ "5 years",
                years_covered >= 10 ~ "2 years",
                years_covered >= 5 ~ "1 year",

                years_covered < .02 & freq_n <= 1 ~ "1 day",
                years_covered < .18 & freq_n <= 2 ~ "1 week",
                years_covered < .67 & freq_n <= 4 ~ "1 month",
                years_covered < 1.34 & freq_n <= 5 ~ "2 months",
                years_covered < 2 & freq_n <= 6 & freq != 5 ~ "3 months",
                freq_n <= 7 ~ "6 months",
            )


            # Compute breaks
            x_breaks <- seq(from = as.Date(max(x_time)), to = as.Date(min(x_time)), by = paste0("-",by))

            # Avoid too fine breaks for coarser frequencies
            if (str_detect(by, "year")) {
                x_breaks <- unique(as.Date(paste0(format(x_breaks, "%Y"), "-01-01")))
            }


            if (by %in% c("1 day", "1 week")) {
                x_labels <- format(x_breaks, "%d %b")
            } else if (by %in% c("1 month", "2 months")) {
                x_labels <- format(x_breaks, "%b %y")
            } else if (by %in% c("3 months", "6 months") & freq_n <= 4) {
                x_labels <- format(x_breaks, "%b %y")
            } else if (by %in% c("3 months", "6 months") & freq == "quarterly") {
                x_labels <- paste0(format(x_breaks, "%y"), "Q", ceiling(as.numeric(format(x_breaks, "%m")) / 3))
            } else if (by == "6 months" & freq == "biannual") {
                x_labels <- paste0(format(x_breaks, "%y"), "-S", ceiling(as.numeric(format(x_breaks, "%m")) / 6))
            } else {
                x_labels <- format(x_breaks, "%Y")
            }

            plot_out <- plot_out +
                scale_x_datetime(
                    breaks = lubridate::as_datetime(x_breaks),
                    labels = x_labels,
                    expand = expansion(mult = c(0.01, 0.01))
                )

        }

    }


    plot_out


}


