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
           # extra href from 'Name' text.
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
for(page in 2:4){
  rv_tanks <- rv_tanks %>%
    full_join(ingest_page(
      paste0(rv_tank_url, "?page=", page)
    ))
}


tanks <- full_join(cylindrical_tanks, vertical_tanks) %>%
  full_join(rv_tanks) %>%
  write_csv('tank_prices.csv')


tanks %>%
  filter(capacity <= 150,
         capacity > 30,
         floor_space > 0,
#         height > 15,
         width < 20) %>%
  select(10:17) %>% 
#  sample_n(17) %>%
  arrange(dol_per_gal)
# TODO:
# * pull product URLs while ingesting data. 
# * add product URL links to Shiny visualization.