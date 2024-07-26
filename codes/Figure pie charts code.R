# Toward a global science of conservation genomics: coldspots in genomic resources highlight a need for equitable collaborations and capacity building 
# Code for Fig S3: Pie charts scope level for reptilia, amphibia, wild and captive pop
# R version 4.3.2

# packages
library(dplyr)
library(ggplot2)

# path
setwd("E:/Manuscritos_em_producao/Manuscrito_Kelly_Zamudio_Cold_spots/data")

# load data
data_raw <- read.csv("Phase_2_Filtering_updated.csv", 
                     head=T, sep= ",")
head(data_raw)

# subset yes only data
data <- subset(data_raw, Passed.Filter.2 == "Yes")

# pie chart
# remove duplicates
unique_studies_scope <- data %>%
  distinct(Study.ID, Scope, .keep_all = T)

# filtering according to your interest 
unique(data$Class)
dados_amphibia <- filter(unique_studies_scope, Class == "Amphibia")
dados_reptilia <- filter(unique_studies_scope, Class == "Reptilia")
dados_wild <- filter(unique_studies_scope, Wild.or.Captive == "Wild")
dados_captive <- filter(unique_studies_scope, Wild.or.Captive == "Captive")

# how many unique studies by each filter
length(unique(dados_amphibia$Study.ID))
length(unique(dados_reptilia$Study.ID))
length(unique(dados_wild$Study.ID))
length(unique(dados_captive$Study.ID))

######################################################################
# Generating the pie charts
####### Amphibia

s_amph <- table(dados_amphibia$Scope)

# ordering the categories
levels = c("just genomic data (Level 0)",
           "spatial only (Level 1)",
           "climate functional variation (Level 2a)",
           "other global change functional variation (Level 2b)",
           "adaptive potential (Level 3)")

factor_data_amph <- factor(names(s_amph), levels = levels)

# create a new object with the new order to generate the graphs
ordered_table_amph <- table(factor_data_amph)
ordered_table_amph[] <- s_amph[match(levels, names(s_amph))]

# table to data.frame
df_amph <- as.data.frame(ordered_table_amph)

# adjust column names
colnames(df_amph) <- c("category", "count")

######################################################################
####### Reptilia
s_rept <- table(dados_reptilia$Scope)

# reordering
factor_data_rept <- factor(names(s_rept), levels = levels)

# create a new object with the new order to generate the graphs
ordered_table_rept <- table(factor_data_rept)
ordered_table_rept[] <- s_rept[match(levels, names(s_rept))]

# table to data.frame
df_rept <- as.data.frame(ordered_table_rept)

# adjust column names
colnames(df_rept) <- c("category", "count")

######################################################################
####### Wild

s_wild <- table(dados_wild$Scope)

# reordering
levels = c("just genomic data (Level 0)",
           "spatial only (Level 1)",
           "climate functional variation (Level 2a)",
           "other global change functional variation (Level 2b)",
           "adaptive potential (Level 3)")

factor_data_wild <- factor(names(s_wild), levels = levels)

# create a new object with the new order to generate the graphs
ordered_table_wild <- table(factor_data_wild)
ordered_table_wild[] <- s_wild[match(levels, names(s_wild))]

# table to data.frame
df_wild <- as.data.frame(ordered_table_wild)

# adjust column names
colnames(df_wild) <- c("category", "count")

######################################################################
####### Captive
s_captive <- table(dados_captive$Scope)

# reordering
factor_data_captive <- factor(names(s_captive), levels = levels)

# create a new object with the new order to generate the graphs
ordered_table_captive <- table(factor_data_captive)
ordered_table_captive[] <- s_captive[match(levels, names(s_captive))]

# table to data.frame
df_captive <- as.data.frame(ordered_table_captive)

# adjust column names
colnames(df_captive) <- c("category", "count")


######################################################################
# pie chart
# Criar o gráfico de pizza

# Amphibia
amph <- ggplot(data = df_amph, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", alpha = 1, width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "(a) Amphibia",
    fill = "Scope"
  ) +
  # scale_fill_manual(values = c("#D55E00", "#F0E442", "#009E73", "#CC79A7", "#0072B2"))
  theme_void() +
  scale_fill_viridis_d(option = "D") +
  theme(legend.position = "none")
amph

# Reptilia
rept <- ggplot(data = df_rept, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", alpha = 1, width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "(a) Reptilia",
    fill = "Scope"
  ) +
  # scale_fill_manual(values = c("#D55E00", "#F0E442", "#009E73", "#CC79A7", "#0072B2"))
  theme_void() +  
  scale_fill_viridis_d(option = "D") +
  theme(legend.position = "none")

rept

# Wild
wild <- ggplot(data = df_wild, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", alpha = 1, width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "(b) Wild",
    fill = "Scope"
  ) +
  # scale_fill_manual(values = c("#D55E00", "#F0E442", "#009E73", "#CC79A7", "#0072B2"))
  theme_void() +
  scale_fill_viridis_d(option = "D") +
  theme(legend.position = "none")
wild

# Captive
captive <- ggplot(data = df_captive, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", alpha = 1, width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "(b) Captive",
    fill = "Scope"
  ) +
  theme_void() +
  # scale_fill_manual(values = c("#D55E00", "#F0E442", "#009E73", "#CC79A7", "#0072B2"))
  scale_fill_viridis_d(option = "D") 
# +
# theme(legend.position = "bottom")
captive

## saving
setwd("E:/Manuscritos_em_producao/Manuscrito_Kelly_Zamudio_Cold_spots/resu/final")

# all graphs in a page using grid.arrange
library(gridExtra)
combined_plot <- grid.arrange(amph, rept, wild, captive, ncol = 2)
plot(combined_plot)

ggsave("pie_scope_class_wild.png", plot = combined_plot, width = 10, height = 4, 
       dpi = 300, bg = "white")
