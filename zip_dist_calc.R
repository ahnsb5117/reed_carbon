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

## convert the input data.frame into a data.talbe
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
  mutate(zip_tot_dist = NUMB*distance_metres) %>% 
  mutate(distance_reed_pdx = "20.9215") %>% 
  mutate(RPK = "90") %>% 
  mutate(portland_native = ifelse(df$ZIP == portland_zipcode$zipcode, "yes","no"))
  

df <- read_csv("Ahn-Data_intl_flight.csv")




library(airportr)
airport_distance("YVR", "PDX")


write.csv(df,"Ahn-Data.csv")

  

