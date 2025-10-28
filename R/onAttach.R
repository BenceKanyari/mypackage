
.onAttach <- function(...) {

    ggplot2::update_geom_defaults("point", list(size = 1.6, color = my_colors(1)))
    ggplot2::update_geom_defaults("line", list(linewidth = 1, color = my_colors(1)))
    ggplot2::update_geom_defaults("tile", list(color = "black"))
    ggplot2::update_geom_defaults("col", list(fill = my_colors(1)))
    options(ggplot2.discrete.colour = my_colors(1:16))
    options(ggplot2.discrete.fill = my_colors(1:16))

    ggplot2::theme_set(
        ggthemes::theme_calc() +
            ggplot2::theme(
                line = ggplot2::element_line(linetype = 1, colour = "black"),
                rect = ggplot2::element_rect(fill = "white", linetype = 0, colour = NA),
                text = ggplot2::element_text(colour = "black"),
                title = ggplot2::element_text(size = ggplot2::rel(2)),
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.grid = ggplot2::element_blank(),
                panel.spacing.x = ggplot2::unit(25, "pt"),
                panel.spacing.y = ggplot2::unit(10, "pt"),
                plot.margin = ggplot2::unit(c(.5, .5, .5, .5), "lines"),
                axis.title = ggplot2::element_blank(),
                axis.text = ggplot2::element_text(face = "bold", size = 12),
                axis.line = ggplot2::element_line(),
                axis.line.y = ggplot2::element_line(),
                axis.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(2.5,0,0,0)),
                axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(0,0,2.5,0)),
                axis.text.y.left = ggplot2::element_text(margin = ggplot2::margin(0,2.5,0,0)),
                axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(0,0,0,2.5)),
                axis.ticks.length.x = ggplot2::unit(4, "pt"),
                axis.ticks.length.y = ggplot2::unit(2.5, "pt"),
                legend.position = "top",
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.background = ggplot2::element_rect(fill = NA, colour = NA),
                legend.box.margin = ggplot2::unit(c(0, 1, -.5, 1), "lines"),
                legend.justification = "left",
                legend.text = ggplot2::element_text(size = 12),
                legend.title = ggplot2::element_blank(),
                strip.background = ggplot2::element_blank(),
                strip.placement = "outside",
                strip.text = ggplot2::element_text(color = "black", size = 14, face = "bold"),
                plot.title = ggplot2::element_text(hjust = .5, face = "bold", size = 16, margin = ggplot2::margin(1, 1, 12, 1, "pt")),
                plot.subtitle = ggplot2::element_text(size = 14, hjust = .5, margin = ggplot2::margin(-13, 3, 12, 3)),
                plot.caption = ggplot2::element_text(hjust = 0, size = 8),
                plot.caption.position = "plot",
                plot.title.position = "plot"
            )
    )

    packageStartupMessage("Theme updated")
}

