## Spread of SARS-CoV-2

# UK graph

library(dygraphs)
library(xts)
library(lubridate)
library(dplyr)

covid <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"))

uk_covid <- covid %>%
  filter(location == "United Kingdom")

uk_covid_sub <- subset(uk_covid, select = -c(1:3, 5, 7:59))

uk_covid_sub$date <- ymd(uk_covid_sub$date)

str(uk_covid_sub)

data <- xts(x = uk_covid_sub$new_cases, order.by = uk_covid_sub$date)
names(data) <- c("New cases")

dygraph(data, main = "Spread of SARS-CoV-2 in the UK") %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.5, colors = "#00C5FF") %>%
  dyCrosshair(direction = "vertical") %>%
  dyLegend(show = "always") %>%
  dyRangeSelector() %>%
  dyCSS("dygraph.css")


# London map

library(maptools)
library(rgeos)
library(dplyr)
library(rgdal)
library(ggthemes)
library(sf)
library(purrr)
library(tidyverse)

covid <- read.csv("https://data.london.gov.uk/download/coronavirus--covid-19--cases/151e497c-a16e-414e-9e03-9e428f555ae9/phe_cases_london_boroughs.csv")

ldn <- st_read("ESRI/London_Borough_Excluding_MHW.shp", stringsAsFactors = FALSE, quiet = TRUE) %>% 
  st_transform(4326) %>% 
  select(GSS_CODE)

sf_data <- left_join(covid, ldn,  by = c("area_code"="GSS_CODE")) %>% 
  st_as_sf() 

random_round <- function(x) {
  v = as.integer(x)
  r = x-v
  test = runif(length(r), 0.0, 1.0)
  add = rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value = v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

sf_data_sub <- sf_data %>%
  filter(date == "2021-03-06")

num_dots <- as.data.frame(sf_data_sub) %>% 
  select(new_cases) %>% 
  mutate_all(funs(. / 1)) %>% 
  mutate_all(random_round)

sf_dots <- map_df(names(num_dots), 
                  ~ st_sample(sf_data_sub, size = num_dots[,.x], type = "random") %>% 
                    st_cast("POINT") %>%                                          
                    st_coordinates() %>%                                          
                    as_tibble() %>%                                               
                    setNames(c("lon","lat")) %>%                                  
                    mutate(newcases = .x)) %>% 
  slice(sample(1:n())) 

pal <- c("new_cases" = "#00C5FF")

p <- ggplot() +
  geom_sf(data = sf_data_sub, fill = "transparent", colour = "#e5e5e5", size = 1) +
  geom_point(data = sf_dots, aes(lon, lat, colour = newcases), size = 5) +
  scale_colour_manual(values = pal) +
  coord_sf(crs = 4326, datum = NA) +
  theme_void(base_size = 48) +
  labs(x = NULL, y = NULL,
       title = "Spread of SARS-CoV-2 in London",
       subtitle = "Date: 06/03/2021 \n 1 dot = 1 new case",
       caption = "Data source: PHE") +
  guides(colour = guide_legend(override.aes = list(size = 18))) +
  theme(plot.background = element_rect(fill = "black", color = NA), 
        panel.background = element_rect(fill = "black", color = NA),
        text = element_text(color = "white", hjust = 0.5),
        title = element_text(color = "white", hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 47, hjust = 0.5, vjust = 0.7, color = "white"),
        plot.caption = element_text(size = 40, hjust = 0.99, vjust = 3.5, face = "italic", color = "white"),
        legend.title = element_blank(),
        legend.position = "none")

ggsave("377.png", plot = p, dpi = 72, width = 100.39, height = 86.49, units = "cm")

#The images were resized to 2842x2449px and the video was rendered in Photoshop.

# London graph

library(dygraphs)
library(xts)
library(lubridate)
library(dplyr)

ldn <- read.csv("https://data.london.gov.uk/download/coronavirus--covid-19--cases/151e497c-a16e-414e-9e03-9e428f555ae9/phe_cases_london_boroughs.csv")

ldn_sub <- subset(ldn, select = -c(1:2, 5))

ldn_sum <- ldn_sub %>%
  group_by(date) %>%
  summarize(sum = sum(new_cases))

ldn_sum$date <- ymd(ldn_sum$date)

str(ldn_sub)

ldn_data <- xts(x = ldn_sum$sum, order.by = ldn_sum$date)

names(ldn_data) <- c("New cases")

dygraph(ldn_data, main = "Spread of SARS-CoV-2 in London") %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.5, colors = "#00C5FF") %>%
  dyCrosshair(direction = "vertical") %>%
  dyLegend(show = "always") %>%
  dyRangeSelector() %>%
  dyCSS("dygraph.css")
