## Spread of SARS-CoV-2 in London

library(maptools)
library(rgeos)
library(tidyverse)
library(rgdal)
library(ggthemes)
library(sf)

covid <- read_csv("https://data.london.gov.uk/download/coronavirus--covid-19--cases/151e497c-a16e-414e-9e03-9e428f555ae9/phe_cases_london_boroughs.csv")

#https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london

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
  filter(date == "2021-03-01")

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

pal <- c("new_cases" = "#005CE6")

p <- ggplot() +
  geom_sf(data = sf_data_sub, fill = "transparent", colour = "white") +
  geom_point(data = sf_dots, aes(lon, lat, colour = newcases, size = 10)) +
  scale_colour_manual(values = pal) +
  coord_sf(crs = 4326, datum = NA) +
  theme_void(base_size = 48) +
  labs(x = NULL, y = NULL,
       title = "Spread of SARS-CoV-2 in London",
       subtitle = "Date: 01/03/2021 \n 1 dot = 1 new case",
       caption = "Data source: PHE") +
  guides(colour = guide_legend(override.aes = list(size = 18))) +
  theme(plot.background = element_rect(fill = "#212121", color = NA), 
        panel.background = element_rect(fill = "#212121", color = NA),
        text = element_text(color = "white", hjust = 0.5),
        title = element_text(color = "white", hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 47, hjust = 0.5, vjust = 0.7, color = "white"),
        plot.caption = element_text(size = 40, hjust = 0.98, vjust = 1, face = "italic", color = "white"),
        legend.title = element_blank(),
        legend.position = "none")

ggsave("map.png", plot = p, dpi = 300, width = 95, height = 91, units = "cm")
