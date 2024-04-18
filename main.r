rm(list=ls())
library(tidyverse)
library(rvest)

base_url <- "https://www.tank-mart.com/"
water_tank_url <- "plastic-tanks/water-storage-tanks/"
vertical_tank_url <- "plastic-tanks/vertical-storage-tanks/"
rv_tank_url <- "rv-marine-tanks/rv-water-tanks/"

ingest_page <- function(rel_url){
  page <- read_html(paste0(base_url, rel_url))
  data <- html_table(page)[[1]] %>% 
    as_tibble() %>%
    mutate(price = str_remove(Price, '\\$') %>%
             as.numeric(),
           capacity = str_remove(Capacity, 'Gallons') %>%
             as.numeric(),
           length = str_remove(Length, '\'\'') %>%
             as.numeric(),
           width = str_remove(Width, '\'\'') %>%
             as.numeric(),
           height = str_remove(Height, '\'\'') %>%
             as.numeric(),
           diameter = str_remove(Diameter, '\'\'') %>%
             as.numeric(),
           gal_per_dol = capacity / price,
           dol_per_gal = price / capacity,
           floor_space = ifelse(is.na(length),
                                      pi * (diameter/2)^2,
                                      length * width)
    )
}

cylindrical_tanks <- ingest_page(water_tank_url)
vertical_tanks <- ingest_page(vertical_tank_url)
rv_tanks <- ingest_page(rv_tank_url)
