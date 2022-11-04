library(readr)
library(tidyverse)
library(zipcodeR)
library(data.table)
library(geodist)

### IMPORT DATA
raw_zipcode<- read_csv("zipcode_dat.csv") # Instutional Resource at Reed

zip_div_state <- read_csv("zip_div_state.csv") # State name 
#https://data.opendatasoft.com/explore/dataset/georef-united-states-of-america-zc-point%40public/table/

portland_zipcode <- read_csv("portland_zipcode.csv",  col_types = cols(.default = col_character()))
#City of Portland Source
#https://www.portlandoregon.gov/revenue/article/373203

###DATA WRANGLING

df <- raw_zipcode %>% 
  drop_na()%>% 
  mutate(ZIP_END = "97218") #PDX ZIPCODE


#Have the method and data part

### Distance from Zipcode
dt_zips <- as.data.table( zip_code_db[, c("zipcode", "lng", "lat")])

# convert the input data.frame into a data.table
setDT(df)

# the postcodes need to be characters
df[
  , `:=`(
    ZIP = as.character(ZIP)
    , ZIP_END = as.character(ZIP_END)
  )
]

# Attach origin lon & lat using a join
df[
  dt_zips
  , on = .(ZIP = zipcode)
  , `:=`(
    lng_start = lng
    , lat_start = lat
  )
]

# Attach destination lon & lat using a join
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


# RPK : Revenue Passenger Kilometers average at 2019 : 90g of CO2
# https://theicct.org/sites/default/files/publications/CO2-commercial-aviation-oct2020.pdf
# All in Kilometers or converted to Kilometers
df <- df %>% 
  mutate(zip_tot_dist = round(NUMB*distance_metres/1000, digit = 2)) %>% 
  mutate(distance_reed_pdx = 20.9215) %>% # From Google maps
  mutate(co2_emission_air_km = 90) %>%  # RPK
  mutate(portland_native = ifelse(df$ZIP == portland_zipcode$zipcode, "yes","no")) %>% 
  mutate(num = c(1:1079)) %>% 
  mutate(car_fuel_econ = 10.93) # KM per L from US GOVERN EPA converted

  
#2022 Fuel Economy 
intl_flight <- read_csv("Ahn-Data_intl_flight.csv")

intl_flight <- intl_flight %>% 
  mutate(num_flight = ...1) %>% 
  select(-c(...1,...2,portland_native))


df <- df %>% 
  full_join(intl_flight, by = c("num" = "num_flight"), keep = TRUE) %>% 
  select(-num_flight)
  
df <- df %>% 
  select(-airplane_distance) %>% 
  mutate(co2_emission_car_km = 251.03) %>% 
#epa sources pdf and site : https://www.epa.gov/greenvehicles/greenhouse-gas-emissions-typical-passenger-vehicle
  mutate(tot_co2_output = (distance_km * co2_emission_air_km + co2_emission_car_km * distance_reed_pdx)) %>% 
  mutate(everyones_output = sum(tot_co2_output , na.rm = TRUE)) %>% 
  drop_na()

df <- df %>% 
  mutate(ZIP = as.double(ZIP)) %>% 
  full_join(zip_div_state, by = c("ZIP" = "ZIP"), keep = TRUE) %>% 
  select(-c(ZIP.y,`ZCTA parent`)) %>% 
  drop_na()
  
state_df <- df %>% 
  group_by(state) %>% 
  summarise(state_emission = sum(distance_km * co2_emission_air_km/1000000  + co2_emission_car_km * distance_reed_pdx))

percap_state <- df %>% 
  group_by(state) %>% 
  summarise(state_emission = sum((distance_km * co2_emission_air_km/1000000  + co2_emission_car_km * distance_reed_pdx))/(sum(NUMB)))

### GRAPH

p_state_df<-ggplot(data=percap_state, aes(x=reorder(state, state_emission), y=state_emission)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  ylab("CO2 emission in tonnes")+
  xlab("State")+
  coord_flip()
p_state_df

p_percap_state<-ggplot(data=percap_state, aes(x=reorder(state, state_emission), y=state_emission)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  ylab("CO2 emission in tonnes per capita")+
  xlab("State")+
  coord_flip()
p_percap_state

# We can see that Oregon and Washington state has the lowest per capita emission,
# Maine and Hawaii has the highest CO2 emission in grams 

write_csv(df,"VanLandSchoot_Ahn-Data.csv")
