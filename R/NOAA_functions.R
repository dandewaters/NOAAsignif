#' @title Download data from NOAA website
#'
#' @description This function downloads required dataset from NOAA website.
#'
#' @importFrom readr read_tsv
#'
#' @return This function returns the downloaded dataset in a tibble.
#'
#' @export
eq_download_data <- function(){
    eq_data <- read_tsv("https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt")
    return(eq_data)
}

#' @title Converting BC dates
#'
#' @description This function helps properly format BC dates in the dataset.
#'
#' @param y year
#' @param m month
#' @param d day
#'
#' @importFrom lubridate ymd years
#'
#' @return This function returns the properly formatted and combined date of year, month and day
#' for both BC or AD dates
#'
#' @examples \dontrun{eq_convert_date(-2150, 1, 1)}
#'
#' @export
eq_convert_date <- function(y, m, d){
    if(y >= 0){
        dat <- lubridate::ymd(paste(formatC(y, width = 4, flag = 0),
                                    m, d, sep = "-"))
    }
    else{
        dat <- lubridate::ymd("0000-01-01") + lubridate::years(y)
    }
    return(dat)
}


#' @title Tidying the raw NOAA dataset
#'
#' @description This function takes raw NOAA data frame and returns a clean data frame.
#' The clean data frame have the following:
#' 1. A date column created by uniting the year, month, day and converting it to the \code{Date} class
#' 2. \code{LATITUDE} and \code{LONGITUDE} columns converted to \code{numeric} class
#'
#' @param df The raw dataset from NOAA website.
#'
#' @importFrom dplyr mutate rowwise
#' @importFrom tidyr replace_na
#' @importFrom magrittr "%>%"
#'
#' @examples \dontrun{eq_tidy_data <- eq_clean_data(eq_download_data())}
#'
#' @return This function returns the cleaned dataset as in the description.
eq_clean_data <- function(df){
    tidy_df <- df %>%
        tidyr::replace_na(list(MONTH = 1, DAY = 1)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(DATE = eq_convert_date(YEAR, MONTH, DAY))
    return(tidy_df)
}

#' @title Cleaning LOCATION_NAME column of the raw NOAA dataset
#'
#' @description This function cleans the LOCATION_NAME column by stripping out the country name (including the colon)
#' and converts names to title case (as opposed to all caps). This will be needed later for annotating visualizations.
#'
#' @param df The raw dataset from NOAA website.
#'
#' @importFrom dplyr mutate rowwise
#' @importFrom stringr str_to_title
#' @importFrom magrittr "%>%"
#'
#' @examples \dontrun{eq_tidy_data <- eq_location_clean(eq_download_data())}
#'
#' @return This function returns the cleaned dataset as in the description.
eq_location_clean <- function(df){
    tidy_df <- df %>%
        dplyr::rowwise() %>%
        dplyr::mutate(COUNTRY = stringr::str_to_title(paste0(strsplit(LOCATION_NAME, ":")[[1]][1], ":"))) %>%
        dplyr::mutate(LOCATION = stringr::str_to_title(strsplit(LOCATION_NAME, ":")[[1]][2]))
    return(tidy_df)
}
