#' @title Map the earthquake epicenters with pup-up labels
#'
#' @description This function takes an argument data containing the filtered data frame with earthquakes to visualize.
#' The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each point with in pop up window containing
#' annotation data stored in a column of the data frame. The user is able to choose which column is used for the annotation
#' in the pop-up with a function argument named annot_col.
#'
#' @param df The cleaned data set by applying \code{eq_clean_data()} function to the raw data
#' @param annot_col The column user wish to use for annotation, default value is "DATE"
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
#' @examples  \dontrun{eq_clean_data(eq_download_data()) %>%
#'                     dplyr::filter(COUNTRY == "Mexico:" & lubridate::year(DATE) >= 2000) %>%
#'                     eq_map(annot_col = "DATE")}
#'
#' @export
eq_map <- function(df, annot_col = "DATE"){
    plot_df <- df %>%
        dplyr::select(.data$LATITUDE, .data$LONGITUDE, .data$EQ_PRIMARY, annot_col)
    plot_df %>%
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers(lng = ~ LONGITUDE,
                         lat = ~ LATITUDE,
                         color = "red",
                         radius = ~ EQ_PRIMARY,
                         popup = ~ paste(get(annot_col)))
}


#' @title Create labels for earthquake epicenter mapping
#'
#' @description This function takes the dataset as an argument and creates an HTML label that can be used as the
#' annotation text in the leaflet map. This function puts together a character string for each earthquake that will
#' show the cleaned location, the magnitude (EQ_PRIMARY), and the total number of deaths (TOTAL_DEATHS), with boldface
#' labels for each ("Location", "Total deaths", and "Magnitude"). If an earthquake is missing values for any of these,
#' both the label and the value will be skipped for that element of the tag.
#'
#' @param df The cleaned data set by applying \code{eq_clean_data()} function to the raw data
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#' @examples  \dontrun{eq_clean_data(eq_download_data()) %>%
#'                     dplyr::filter(COUNTRY == "Mexico:" & lubridate::year(DATE) >= 2000) %>%
#'                     eq_create_label() %>%
#'                     eq_map(annot_col = "popup_text")}
#'
#' @export
eq_create_label <- function(df){
    popup <- df %>%
        dplyr::mutate(popup_LOCATION = ifelse(is.na(.data$LOCATION), "",
                                              paste0("<b>Location:</b>", .data$LOCATION, "<br>")),
                      popup_EQ_PRIMARY =  ifelse(is.na(.data$EQ_PRIMARY), "",
                                                 paste0("<b>Magnitude: </b>", .data$EQ_PRIMARY, "<br>")),
                      popup_DEATHS =  ifelse(is.na(.data$TOTAL_DEATHS), "",
                                             paste0("<b>Total deaths: </b>", .data$TOTAL_DEATHS, "<br>")),
                      popup_text = paste0(.data$popup_LOCATION, .data$popup_EQ_PRIMARY, .data$popup_DEATHS))
    return(popup)
}
