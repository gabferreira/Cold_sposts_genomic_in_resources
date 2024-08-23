# Toward a global science of conservation genomics: coldspots in genomic resources highlight a need for equitable collaborations and capacity building 
# Code for Fig S7: Map collaborations between countries
# R version 4.3.2

# packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggforce)

# path
setwd("E:/Manuscritos_em_producao/Manuscrito_Kelly_Zamudio_Cold_spots/data")

# load data
data <- read.csv("Final_postsubmission_Phase_2_Filtering.csv", sep = ",", fileEncoding="latin1")
head(data)

# subset yes only data
data <- subset(data, Passed.Filter.2 == "Yes")
data <- subset(data, Wild.or.Captive == "Wild")
length(unique(data$ï..Study.ID))
names(data)[1] <- "Study.ID"

# remove duplicates
data <- data %>%
  distinct(Study.ID, Country, .keep_all = T)

# load mapa mundi
world <- ne_countries(scale = "medium", returnclass = "sf")

# get centroid for each country
country_points <- world %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  cbind(world %>% st_drop_geometry() %>% select(admin)) %>%
  rename(long = X, lat = Y)

# get connections between countries
edges <- data %>%
  filter(!is.na(X1st.Author.Origin.Country) & !is.na(Country)) %>%
  select(from = X1st.Author.Origin.Country, to = Country) %>%
  group_by(from, to) %>%
  summarise(weight = n(), .groups = 'drop') %>%
  left_join(country_points, by = c("from" = "admin")) %>%
  rename(from_long = long, from_lat = lat) %>%
  left_join(country_points, by = c("to" = "admin")) %>%
  rename(to_long = long, to_lat = lat) %>%
  filter(!is.na(from_long) & !is.na(from_lat) & !is.na(to_long) & !is.na(to_lat)) %>%
  filter(!(from_long == to_long & from_lat == to_lat)) # Filtrar conexões entre o mesmo país

# number of studies by country
country_studies <- data %>%
  filter(!is.na(X1st.Author.Origin.Country)) %>%
  group_by(X1st.Author.Origin.Country) %>%
  summarise(study_count = n())

# join tables
country_points_filtered <- country_points %>%
  left_join(country_studies, by = c("admin" = "X1st.Author.Origin.Country")) %>%
  filter(admin %in% unique(edges$from) | admin %in% unique(edges$to)) %>%
  replace_na(list(study_count = 0)) # Substituir NA por 0

# plot the map
x11()
redes <- ggplot(data = world) +
  geom_sf(fill = "gray50", color = "white") +
  geom_point(data = country_points_filtered, aes(x = long, y = lat, size = study_count), color = "black") +
  geom_curve(data = edges, aes(x = from_long, y = from_lat, xend = to_long, yend = to_lat, linewidth = weight, color = "orangered"), 
             curvature = 0.3, alpha = 0.3, arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  theme_void() +
  scale_linewidth_continuous("Weight", range = c(0.5, 3)) +
  scale_size_continuous(name = "Number of Connections", range = c(2, 10)) + # Ajustar o tamanho dos pontos
  theme(
    plot.title = element_text(hjust = 0.5)
  )

print(redes)

# save the figure
setwd("E:/Manuscritos_em_producao/Manuscrito_Kelly_Zamudio_Cold_spots/resu/final/final_V2")
ggsave("conections_countries_new.pdf", plot = redes, width = 10, height = 5, dpi = 300, bg = "white")
