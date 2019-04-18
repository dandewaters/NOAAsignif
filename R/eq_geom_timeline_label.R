#' @title Plot a timeline of earthquakes with labels
#'
#' @description This geom plots a time line of earthquakes with a point for each earthquake.
#' This geom adds a vertical line to each data point with a text annotation (the location of the earthquake)
#' attached to each line. There is an option to subset to n_max number of earthquakes, where we take the n_max
#' largest (by magnitude) earthquakes. Aesthetics are x, which is the date of the earthquake and label which takes
#' the location column from which annotations will be obtained.
#' This geom is the prototype required for \code{geom_timeline_label()} function to work.
#'
#' @importFrom ggplot2 ggproto aes draw_key_text
#' @importFrom dplyr arrange
#' @importFrom grid segmentsGrob circleGrob textGrob gTree gList
#'
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLable", ggplot2::Geom,
                                      required_aes = c("x", "label"),
                                      default_aes = ggplot2::aes(y = 0.2, alpha = 0.5, n_max = 5),
                                      draw_key = ggplot2::draw_key_text,
                                      draw_panel = function(data, panel_scales, coord) {
                                          coords <- coord$transform(data, panel_scales)
                                          # Remove labels from cases other than top n_max earthquakes
                                          n_max = coords$n_max[1]
                                          coords <- dplyr::arrange(coords, desc(size))
                                          coords$label[(n_max + 1):length(coords$label)] <- ""
                                          # Adjust the size of the circle Grob
                                          coords$size <- coords$size * 0.01
                                          # Draw the timeline
                                          g1 <- grid::segmentsGrob(
                                              default.units="npc",
                                              x0 = 0,
                                              y0 = coords$y[1],
                                              x1 = 1,
                                              y1 = coords$y[1],
                                              gp = gpar(lwd = 3, alpha = 0.5, col = "grey")
                                              )
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
                                          # Add labels
                                          g3 <- grid::textGrob(
                                              label = coords$label,
                                              x = coords$x,
                                              y = coords$y + 0.1,
                                              default.units = "npc",
                                              rot = 50,
                                              just = "left",
                                              gp = grid::gpar(fontsize = 10)
                                              )
                                          grid::gTree(children = grid::gList(g1, g2, g3))
                                      })

#' @title Plot a labeled timeline of earthquakes within a given time frame
#'
#' @description This function utilizes the \code{GeomTimelineLable} geom to plot the earthquake events with
#' labels (location of the earthquake). There is an option to subset to n_max number of earthquakes, where we take the n_max
#' largest (by magnitude) earthquakes. In the provided example, the \code{color} aes reflects the number of deaths resulted from
#' the earthquake with grey ones indicating no data available. The \code{size} aes reflects the magnitude of the eqrthquake.
#' The X-axis is the date. n_max can be customized, the default value is 5.
#'
#' @importFrom ggplot2 layer
#' @param mapping Aesthetic mappings created by aes
#' @param data Dataset from NOAA website
#' @param na.rm  Remove the NA values from the data frame
#' @param show.legend Legend
#' @param stat The statistical transformation used
#' @param position Position adjustment function
#' @param inherit.aes Default aesthetics
#' @param ... Other arguments
#'
#' @return The labeled timeline plot.
#'
#' @examples #Pick data points ranging from date \code{xmin} to \code{xmax} from the cleaned dataset to plot
#'            \dontrun{eq_tidy_data <- eq_clean_data(eq_download_data())}
#'            \dontrun{xmin <- as.Date("1999-01-01")}
#'            \dontrun{xmax <- as.Date("1999-07-01")}
#'            \dontrun{sample_data <- filter(eq_tidy_data, DATE >= xmin & DATE <= xmax)}
#'            \dontrun{ggplot(sample_data,
#'                    aes(x = DATE, label = LOCATION, size = EQ_PRIMARY, fill = DEATHS,
#'                    color = DEATHS, n_max = 4)) +
#'                    geom_timeline_label() +
#'                    theme_minimal()}
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = TRUE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = GeomTimelineLabel, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
