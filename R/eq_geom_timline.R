#' @title Plot a timeline of earthquakes
#'
#' @description This geom plots a time line of earthquakes with a point for each earthquake.
#' Optional aesthetics include color, size, and alpha. The x aesthetic is a date when the earthquake occurred.
#' This geom is the prototype required for \code{geom_timeline()} function to work.
#'
#' @importFrom ggplot2 ggproto aes draw_key_text
#' @importFrom grid circleGrob segmentsGrob gTree gList
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                         required_aes = c("x"),
                         default_aes = ggplot2::aes(y = 0.2, alpha = 0.5),
                         draw_key = ggplot2::draw_key_text,
                         draw_panel = function(data, panel_scales, coord) {
                             coords <- coord$transform(data, panel_scales)
                             coords$size <- coords$size*0.01 # Adjust the size of the circle Grob
                             # Draw the timeline
                             g1 <- grid::segmentsGrob(
                                 default.units="npc",
                                 x0 = coords$x[1],
                                 y0 = coords$y[1],
                                 x1 = coords$x[length(coords$x)],
                                 y1 = coords$y[1],
                                 gp = gpar(lwd = 3, alpha = 0.5, col = "grey"))
                             # Add data points
                             g2 <- grid::circleGrob(
                                 x = coords$x,
                                 y = coords$y,
                                 r = coords$size,
                                 default.units="npc",
                                 gp = grid::gpar(
                                     colour = coords$colour,
                                     alpha = coords$alpha,
                                     fill = coords$fill)
                                 )
                             grid::gTree(children = grid::gList(g1, g2))
                         })

#' @title Plot a timeline of earthquakes within a given time frame
#'
#' @description This function utilizes the \code{GeomTimeline} geom to plot the earthquake events.
#' In the provided example, the \code{color} aes reflects the number of deaths resulted from the earthquake
#' with grey ones indicating no data available. The \code{size} aes reflects the magnitude of the eqrthquake.
#' The X-axis is the date.
#'
#' @importFrom ggplot2 layer
#'
#' @param mapping Aesthetic mappings created by aes
#' @param data Dataset from NOAA website
#' @param na.rm  Remove the NA values from the data frame
#' @param show.legend Legend
#' @param stat The statistical transformation used
#' @param position Position adjustment function
#' @param inherit.aes Default aesthetics
#' @param ... Other arguments
#'
#' @return The timeline plot.
#'
#' @examples #Pick data points ranging from date \code{xmin} to \code{xmax} from the cleaned dataset to plot
#'            \dontrun{eq_tidy_data <- eq_clean_data(eq_download_data())}
#'            \dontrun{xmin <- as.Date("1999-01-01")}
#'            \dontrun{xmax <- as.Date("2000-01-01")}
#'            \dontrun{sample_data <- filter(eq_tidy_data, DATE >= xmin & DATE <= xmax)}
#'            \dontrun{ggplot(sample_data,
#'                    aes(x = DATE, size = EQ_PRIMARY, fill = DEATHS, color = DEATHS)) +
#'                    geom_timeline() +
#'                    theme_minimal()}
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = TRUE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = GeomTimeline, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
