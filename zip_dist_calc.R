library(readr)
library(tidyverse)
library(zipcodeR)
library(data.table)
library(geodist)

raw_zipcode<- read_csv("zipcode_dat.csv")

df <- raw_zipcode %>% 
  drop_na()%>% 
  mutate(ZIP_END = "97218")


#have the method and data part

## Convert the zip codes to data.table so we can join on them
## I'm using the centroid of the zipcodes (lng and lat).
## If you want the distance to the endge of the zipcode boundary you'll
## need to convert this into a spatial data set
dt_zips <- as.data.table( zip_code_db[, c("zipcode", "lng", "lat")])

## convert the input data.frame into a data.table
setDT(df)

## the postcodes need to be characters
df[
  , `:=`(
    ZIP = as.character(ZIP)
    , ZIP_END = as.character(ZIP_END)
  )
]

## Attach origin lon & lat using a join
df[
  dt_zips
  , on = .(ZIP = zipcode)
  , `:=`(
    lng_start = lng
    , lat_start = lat
  )
]

## Attach destination lon & lat using a join
df[
  dt_zips
  , on = .(ZIP_END = zipcode)
  , `:=`(
    lng_end = lng
    , lat_end = lat
  )
]

## calculate the distance
df[
  , distance_metres := geodist::geodist_vec(
    x1 = lng_start
    , y1 = lat_start
    , x2 = lng_end
    , y2 = lat_end
    , paired = TRUE
    , measure = "haversine"
  )
]

#City of Portland Source
#https://www.portlandoregon.gov/revenue/article/373203
portland_zipcode <- read_csv("portland_zipcode.csv",  col_types = cols(.default = col_character()))

# RPK : Revenue Passenger Kilometers average at 2019 : 90g of CO2
# https://theicct.org/sites/default/files/publications/CO2-commercial-aviation-oct2020.pdf
df <- df %>% 
  mutate(zip_tot_dist = round(NUMB*distance_metres/1000, digit = 2)) %>% 
  mutate(distance_reed_pdx = "20.9215") %>% 
  mutate(RPK = "90") %>% 
  mutate(portland_native = ifelse(df$ZIP == portland_zipcode$zipcode, "yes","no")) %>% 
  mutate(num = c(1:1079)) 
  
  


intl_flight <- read_csv("Ahn-Data_intl_flight.csv")

intl_flight <- intl_flight %>% 
  drop_na() %>% 
  mutate(num_flight = ...1) %>% 
  select(-c(...1,...2,portland_native)) %>% 
  drop_na()

  

df <- df %>% 
  full_join(intl_flight, by = c("num" = "num_flight"), keep = TRUE) %>% 
  select(-num_flight)
#   
# airport_distance(paste0('"',ACC,'"'), paste0('"',PDX,'"'))
# print(paste0('`',intl_flight$international[1],'`'))
# 
# test = airport_distance("ACC", "PDX")
# 
# paste0("`",intl_flight$international,"`")
# quote(print(test))
# # test
# test$distance
# # [1] "484.6"
# 
# for (i in 1:2) {
#   from = intl_flight[i, "foreign_airport"]
#   to = intl_flight[i, "pdx_airport"]
#   intl_flight[i, "distance"] <- airport_distance(paste0("`",from,"`"), paste0("`",to,"'"))$distance
# }
