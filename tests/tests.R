library(NOAAsignif)
library(testthat)
library(dplyr)
library(ggplot2)
library(leaflet)
library(rlang)

expect_that(eq_download_data(), is_a("data.frame"))
expect_that(eq_clean_data(eq_download_data()), is_a("data.frame"))
expect_that(eq_location_clean(eq_download_data()), is_a("data.frame"))
expect_that(eq_convert_date(1999, 1, 1), is_a("Date"))

tidy_data <- eq_download_data() %>%
    eq_clean_data() %>%
    eq_location_clean()
xmin <- as.Date("1999-01-01")
xmax <- as.Date("2000-01-01")
sample_data <- dplyr::filter(tidy_data, .data$DATE >= xmin & .data$DATE <= xmax)
g1 <- ggplot(sample_data,
       aes(x = DATE, size = EQ_PRIMARY, fill = DEATHS, color = DEATHS)) +
    geom_timeline() +
    theme_minimal()
expect_that(g1, is_a("ggplot"))

g2 <- ggplot(sample_data,
       aes(x = DATE, label = LOCATION, size = EQ_PRIMARY, fill = DEATHS,
           color = DEATHS, n_max = 4)) +
    geom_timeline_label() +
    theme_minimal()
expect_that(g2, is_a("ggplot"))

mexico_data <- tidy_data %>%
    dplyr::filter(COUNTRY == "Mexico:" & lubridate::year(DATE) >= 2000)
expect_that(eq_create_label(mexico_data), is_a("data.frame"))
im <- mexico_data %>%
    eq_create_label() %>%
    eq_map()
expect_that(im, is_a("leaflet"))
