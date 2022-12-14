library(readr)
library(tidyverse)
library(zipcodeR)
library(data.table)
library(geodist)
#No changes 

### IMPORT DATA
raw_zipcode<- read_csv("data/reed_zipcode.csv") # Instutional Resource at Reed

zip_div_state <- read_csv("data/zip_div_state.csv") # State name 
#https://data.opendatasoft.com/explore/dataset/georef-united-states-of-america-zc-point%40public/table/

portland_zipcode <- read_csv("data/portland_zipcode.csv",  col_types = cols(.default = col_character()))
#City of Portland Source
#https://www.portlandoregon.gov/revenue/article/373203

intl_flight <- read_csv("data/intl_flight.csv") %>% 
  mutate(num_flight = ...1) %>% 
  select(-c(...1,...2,portland_native))

###DATA WRANGLING

datr <- raw_zipcode %>% 
  drop_na()%>% 
  mutate(ZIP_END = "97218") #PDX ZIPCODE


datr$distance <- (zip_distance(97202, datr$ZIP, units = "meters")$distance)
datr$distance_air <- (zip_distance(datr$ZIP_END, datr$ZIP, units = "meters")$distance)

stat_zip <- zip_code_db %>% select(zipcode, state) %>%
  rename(ZIP = zipcode)

datr <- left_join(datr, stat_zip, by = "ZIP") 

df <- datr %>%
  mutate(num = 1:1079) %>%
  left_join(intl_flight, by = c("num" = "num_flight"), keep = TRUE) %>% 
  mutate(ZIP = case_when(international == "yes" ~ "International",
                         TRUE ~ ZIP),
         state = case_when(international == "yes" ~ "INTL",
                           TRUE ~ state)) %>%
  mutate(distance = distance/1000,
         distance_air = distance_air/1000,
         mode = case_when(distance < 1000 ~ "Drive",
                          distance >= 1000 ~ "Fly",
                          international == "yes" ~ "Fly"),
         distance_car = case_when(mode == "Fly" ~ 19.3121,
                                  international == "yes" ~ 19.3121,
                                  mode == "Drive" ~ distance),
         distance_air = case_when(mode == "Drive" ~ 0,
                                  international == "yes" ~ distance_km,
                                  mode == "Fly" ~ distance_air),
         portland_native = case_when(ZIP %in% portland_zipcode$zipcode ~ "Native",
                                     TRUE ~ "Non-Native")) %>%
  select(-c(distance, pdx_airport, airplane_distance, ZIP_END, num, num_flight)) %>% 
  mutate(total_emission_air = case_when(international == "no" ~ 4 * distance_air * 0.000099208,
                                        international == "yes" ~ 2 * distance_air * 0.000099208), 
         # 90g of CO2 equivalent per km to tonnes, assuming 2 round-trips per year for non-intl student and 1 for intl students 
         # https://theicct.org/sites/default/files/publications/CO2-commercial-aviation-oct2020.pdf
         total_emission_car = case_when(international == "no" ~ 4 * distance_car * 0.000251034585607,
                                        international == "yes" ~ 2 * distance_car * 0.000251034585607),
         # 251g of CO2 equiv per km to tonnes, , assuming 2 round-trips per year for non-intl student and 1 for intl students 
         # https://www.epa.gov/greenvehicles/greenhouse-gas-emissions-typical-passenger-vehicle
         ind_emission_gas = (0.0004127687 * 293.071 * (63367 + 58921 + 59168)) / (1471	+ 1385	+ 1566),
         # (0.0004127687 tonnes of CO2/kwh * 293.071 kwh/MMBTU * (63,367 + 58,921 + 59,168)MMBTU) / (1,471	+ 1,385	+ 1,566) students = tons of CO2 per student
         # https://www.eia.gov/tools/faqs/faq.php?id=74&t=11
         ind_emission_electric = (0.0004490564 * (11135877 + 10159517 + 10561212)) / (1471	+ 1385	+ 1566)
         # (0.0004490564 tonnes of CO2 / kWh * (11,135,877 + 10,159,517 + 10,561,212)) / (1,471	+ 1,385	+ 1,566) students = tons of CO2 per student
         # https://www.eia.gov/tools/faqs/faq.php?id=74&t=11 
         ) %>% 
  group_by(state) %>%
  mutate(state_emission = sum(total_emission_air + total_emission_car, na.rm = T) + 8.2 *sum(NUMB),
         state_emission_per_cap = sum(total_emission_air + total_emission_car, na.rm = T)/sum(NUMB) + 8.2,
         travel_emission_per_cap = sum(total_emission_air + total_emission_car, na.rm = T)/sum(NUMB)) %>%
  filter(state != "NA") %>%
  ungroup() %>%
  rename(zip = ZIP,
         num = NUMB) %>% 
  mutate(SCC_IWG = "53") %>% 
  mutate(SCC_state_emission_per_cap = as.numeric(SCC_IWG) * as.numeric(state_emission_per_cap)) %>% 
  mutate(SCC_state_emission = as.numeric(SCC_IWG) * as.numeric(state_emission)) %>% 
  mutate(mean_total_emission_per_cap = (ind_emission_gas + ind_emission_electric + mean(state_emission_per_cap))) %>% 
  mutate(total_emission_per_cap = (ind_emission_gas + ind_emission_electric + state_emission_per_cap)) %>% 
  mutate(pct_ind_emission_gas = ind_emission_gas/mean_total_emission_per_cap) %>% 
  mutate(pct_ind_emission_electric = ind_emission_electric/mean_total_emission_per_cap) %>% 
  mutate(pct_state_emission_per_cap = state_emission_per_cap/mean_total_emission_per_cap) %>% 
  mutate(SCC_total_emission_per_cap = as.numeric(SCC_IWG) * mean_total_emission_per_cap) %>% 
  mutate(mean_pct_ind_emission_gas = mean(ind_emission_gas/mean_total_emission_per_cap)) %>% 
  mutate(mean_pct_ind_emission_electric = mean(ind_emission_electric/mean_total_emission_per_cap)) %>% 
  mutate(mean_pct_state_emission_per_cap = mean(state_emission_per_cap/mean_total_emission_per_cap))

a <- df %>% 
  mutate(s = mean(state_emission_per_cap)*1565) %>% 
  mutate(l = (ind_emission_gas + ind_emission_electric)*1565) %>% 
  select(s,l)

### calculate percentage of each gas and electrical usage, road and air travel

df %>% 
  mutate(pct_air_car_emission = (total_emission_air + total_emission_car)/state_emission)


### GRAPH in tonnes

df %>% # CO2 emission in tonnes
  select(state, state_emission) %>%
  unique() %>%
  ggplot(aes(x = reorder(state, state_emission), y = state_emission)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylab("CO2e emission in tonnes") +
  xlab("State") +
  coord_flip()

# df %>%
#   pivot_longer(10:11) %>%
#   ggplot(aes(x = state, y = value, fill = name)) +
#   geom_bar(stat = "identity") +
#   theme_minimal() +
#   ylab("CO2 emission in tonnes") +
#   xlab("State") +
#   coord_flip()
ggsave("graphs/state_emissions.png")


df %>% # CO2 emission in tonnes per capita
  select(state, state_emission_per_cap) %>%
  unique() %>%
  ggplot(aes(y = reorder(state, state_emission_per_cap), x = state_emission_per_cap)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  xlab("CO2e emission in tonnes per capita") +
  ylab("State") +
  coord_cartesian(xlim = c(8.2,10))

ggsave("graphs/state_emissions_per_cap.png")

df %>% # CO2 emission from travel in tonnes per capita
  select(state, travel_emission_per_cap) %>%
  unique() %>%
  ggplot(aes(x = reorder(state, travel_emission_per_cap), y = travel_emission_per_cap)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylab("CO2e emissions from travel in tonnes per capita") +
  xlab("State") +
  coord_flip()

ggsave("graphs/travel_emissions_per_cap.png")

### GRAPH in SCC

df %>%
  select(state, SCC_state_emission) %>%
  unique() %>%
  ggplot(aes(x = reorder(state, SCC_state_emission), y = SCC_state_emission)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylab("CO2e emission in Social Cost of Carbon ($)") +
  xlab("State") +
  coord_flip()

ggsave("graphs/SCC_state_emissions.png")

df %>%
  select(state, SCC_state_emission_per_cap) %>%
  unique() %>%
  ggplot(aes(x = reorder(state, SCC_state_emission_per_cap), y = SCC_state_emission_per_cap)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylab("CO2e emission in Social Cost of Carbon per capita ($)") +
  xlab("State") +
  coord_flip()

ggsave("graphs/SCC_state_emissions_per_cap.png")


# We can see that Oregon and Washington state has the lowest per capita emission,
# Maine and Hawaii has the highest CO2 emission in grams 

write_csv(df,"data/VanLandschoot_Ahn_Data.csv")

library("xlsx")
write.xlsx(df,"data/VanLandschoot_Ahn_Data.xlsx")
