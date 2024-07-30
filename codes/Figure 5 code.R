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
setwd("E:/Manuscritos_em_producao/Manuscrito_Kelly_Zamudio_Cold_spots/data")

# load data
data <- read.csv("Phase_2_Filtering_updated.csv", sep = ",", fileEncoding = "latin1")
data <- subset(data, Passed.Filter.2 == "Yes" & Wild.or.Captive == "Wild")

# remove duplicates
data <- data %>%
  distinct(Study.ID, Country, .keep_all = TRUE)

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

nodes[142, 3] <- "Global South"
nodes[143, 3] <- "Global North"

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

# colors based on global_north_south
node_colors <- ifelse(nodes$global_north_south == "Global North", "darkgreen", "lightgreen")

# Plot
g <- ggraph(graph, layout = "fr") + 
  geom_edge_link(aes(), arrow = arrow(length = unit(6, 'mm')), 
                 color = "grey", alpha = 0.6) + # Adiciona setas nas arestas
  geom_node_point(aes(size = size_class, color = global_north_south)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) + # Define o tamanho do texto
  scale_size_manual(name = "Number of Connections",
                    values = c("1-20" = 3, "21-50" = 6, "51-100" = 9, ">100" = 12),
                    breaks = c("1-20", "21-50", "51-100", ">100")) + # Ajusta o tamanho dos pontos e a ordem na legenda
  scale_color_manual(name = NULL, values = c("Global North" = "darkgreen", "Global South" = "lightgreen")) + # Adiciona a legenda de cor
  theme_void()

x11()
print(g)

# saving
setwd("E:/Manuscritos_em_producao/Manuscrito_Kelly_Zamudio_Cold_spots/resu/final")
ggsave("graph_conections_global_south_north2.png", plot = g, width = 18, height = 10, dpi = 300, bg = "white")
