# Toward a global science of conservation genomics: coldspots in genomic resources highlight a need for equitable collaborations and capacity building 
# Code for Fig 5: Graph collaborations between countries
# R version 4.3.2

# packages
library(tidyverse)
library(igraph)
library(ggraph)
library(countrycode)
library(dplyr)

# path
setwd("F:/Manuscritos_em_producao/Manuscrito_Kelly_Zamudio_Cold_spots/data")

# load data
data <- read.csv("Final_postsubmission_Phase_2_Filtering.csv", sep = ",", fileEncoding = "latin1")
data <- subset(data, Passed.Filter.2 == "Yes" & Wild.or.Captive == "Wild")
names(data)[1] <- "Study.ID"

# remove duplicates
data <- data %>%
  distinct(Study.ID, Country, .keep_all = TRUE)

write.csv(data, "Final_postsubmission_Phase_2_Filtering_TESTING.csv")
# connections
edges <- data %>%
  filter(!is.na(X1st.Author.Origin.Country) & !is.na(Country)) %>%
  select(from = X1st.Author.Origin.Country, to = Country)

# count the number of unique studies by country
nodes <- data %>%
  group_by(Country) %>%
  filter(!is.na(Country) & !is.na(X1st.Author.Origin.Country)) %>%
  summarise(count = n(), global_north_south = first(Species.Global.North.or.South))

# add countries that are in the edge table but not in the node table
all_countries <- unique(c(edges$from, edges$to))
missing_countries <- setdiff(all_countries, nodes$Country)
nodes <- nodes %>%
  bind_rows(data.frame(Country = missing_countries, count = 0, global_north_south = NA))

# nodes[142, 3] <- "Global South"
# nodes[143, 3] <- "Global North"

# remove duplicates on edges
edges <- distinct(edges)

# reclassify the node size
nodes <- nodes %>%
  mutate(size_class = case_when(
    count >= 0 & count <= 20 ~ "1-20",
    count >= 21 & count <= 50 ~ "21-50",
    count >= 51 & count <= 100 ~ "51-100",
    count > 100 ~ ">100",
    TRUE ~ "0"
  ))

# factor and reorder
nodes$size_class <- factor(nodes$size_class, levels = c("1-20", "21-50", "51-100", ">100"))

# create the graph
graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
plot(graph)
###########################################################
# Degree centrality
degree_centrality <- degree(graph, mode = "all")  # "all" considera todas as conexões (entrada + saída)
degree_centrality_in <- degree(graph, mode = "in")  # Apenas conexões de entrada
degree_centrality_out <- degree(graph, mode = "out")  # Apenas conexões de saída

# Add to the dataframe of nodes
nodes <- nodes %>%
  mutate(degree_centrality = degree_centrality[Country],
         degree_in = degree_centrality_in[Country],
         degree_out = degree_centrality_out[Country])

# Check the five higher values
head(nodes[order(-nodes$degree_centrality), ], 5)

# change the column names
names(nodes)[2] <- "county_unique_studies"

# write csv
write.csv(nodes, "F:/Manuscritos_em_producao/Manuscrito_Kelly_Zamudio_Cold_spots/resu/final/degree_centrality.csv")
