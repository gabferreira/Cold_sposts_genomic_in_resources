# Toward a global science of conservation genomics: coldspots in genomic resources highlight a need for equitable collaborations and capacity building 
# Code for Fig S3: Density plots for each scope
# R version 4.3.2

# packages
library(ggplot2)
library(dplyr)

# path
setwd("E:/Manuscritos_em_producao/Manuscrito_Kelly_Zamudio_Cold_spots/data")

# load data
data_raw <- read.csv("Phase_2_Filtering_updated.csv", 
                 head=T, sep= ",")
head(data_raw)

# subset yes only data and captive data
data <- subset(data_raw, Passed.Filter.2 == "Yes" & Wild.or.Captive == "Wild")

# Remover duplicatas com base na coluna 'Scope' e study
data_scope <- data %>%
  distinct(Study.ID, Scope, .keep_all = T)

## is numeric?
is.numeric(data_scope$Lat)
data_scope$Lat <- as.numeric(data_scope$Lat)
is.numeric(data_scope$Lat)

## verifying genomic data
unique(data_scope$Scope)
sum(is.na(data_scope$Scope))

# how many studies in each level
nrow(data_scope[data_scope$Scope == unique(data_scope$Scope)[4],]) # lvl 0
nrow(data_scope[data_scope$Scope == unique(data_scope$Scope)[1],]) # lvl 1
nrow(data_scope[data_scope$Scope == unique(data_scope$Scope)[2],]) # lvl 2b
nrow(data_scope[data_scope$Scope == unique(data_scope$Scope)[5],]) # lvl 2a
nrow(data_scope[data_scope$Scope == unique(data_scope$Scope)[3],]) # lvl 3

# density plot in ggplot
# factor
data_scope$Scope <- as.factor(data_scope$Scope)

# geom density for each scope

# colors
viridis::viridis(5, option = "D")

data_scope1 <- subset(data_scope, data_scope$Scope == "just genomic data (Level 0)")
lvl1 <- ggplot(data_scope1, aes(y = Lat, fill = Scope)) +
  geom_density(alpha = 1,  color = NA) +
  ylim(-50, 80)+
  xlim(0, 0.05)+
  xlab("Level 1") + ylab("Latitude")+
  # scale_fill_viridis_d(option = "D") +
  scale_fill_manual(values = c("#440154FF")) +
  # scale_fill_brewer(palette = "RdYlGn")+  # Escolher uma paleta de cores
  theme_classic() +
theme(legend.position = "none")

######################################################################
data_scope2 <- subset(data_scope, data_scope$Scope == "spatial only (Level 1)")

lvl2 <-ggplot(data_scope2, aes(y = Lat, fill = Scope)) +
  geom_density(alpha = 1,  color = NA) +
  ylim(-50, 80)+
  xlim(0, 0.05)+
  xlab("Level 2") + 
  ylab(NULL)+
  scale_fill_manual(values = c("#3B528BFF")) +
  # scale_fill_brewer(palette = "RdYlGn")+  # Escolher uma paleta de cores
  theme_classic() +
  theme(legend.position = "none")
# lvl2

#############################################################
data_scope3 <- subset(data_scope, data_scope$Scope == "other global change functional variation (Level 2b)")

lvl3 <- ggplot(data_scope3, aes(y = Lat, fill = Scope)) +
  geom_density(alpha = 1,  color = NA) +
  ylim(-50, 80)+
  xlim(0, 0.05)+
  xlab("Level 3") +
  ylab(NULL)+
  scale_fill_manual(values = c("#21908CFF")) +
  # scale_fill_manual(values = c("blue", "green", "red",
  #                   "pink", "yellow", "grey")) +
  # scale_fill_brewer(palette = "RdYlGn")+  # Escolher uma paleta de cores
  theme_classic() +
  theme(legend.position = "none")
# lvl3

#############################################################
data_scope4 <- subset(data_scope, data_scope$Scope == "climate functional variation (Level 2a)")

lvl4 <-ggplot(data_scope4, aes(y = Lat, fill = Scope)) +
  geom_density(alpha = 1,  color = NA) +
  ylim(-50, 80)+
  xlim(0, 0.05)+
  xlab("Level 4") + 
  ylab(NULL)+
  scale_fill_manual(values = c("#5DC863FF")) +
  # scale_fill_brewer(palette = "RdYlGn")+  # Escolher uma paleta de cores
  theme_classic() +
  theme(legend.position = "none")

#############################################################
data_scope5 <- subset(data_scope, data_scope$Scope == "adaptive potential (Level 3)")

lvl5 <- ggplot(data_scope5, aes(y = Lat, fill = Scope)) +
  geom_density(alpha = 1,  color = NA) +
  ylim(-50, 80)+
  # xlim(0, 0.05)+
  xlab("Level 5") + 
  ylab(NULL)+
  scale_fill_manual(values = c("#FDE725FF")) +
  # scale_fill_manual(values = c("blue", "green", "red",
  #                   "pink", "yellow", "grey")) +
  # scale_fill_brewer(palette = "RdYlGn")+  # Escolher uma paleta de cores
  theme_classic() +
  theme(legend.position = "none")
lvl5

######################################################################
# saving
setwd("E:/Manuscritos_em_producao/Manuscrito_Kelly_Zamudio_Cold_spots/resu/final")

# all plots in the same page with grid.arrange
library(gridExtra)
combined_plot <- grid.arrange(lvl1, lvl2, lvl3, lvl4, lvl5,
                              ncol = 5, nrow = 1)
plot(combined_plot)

ggsave("density_scopes.png", plot = combined_plot, width = 10, 
       height = 4, 
       dpi = 300, bg = "white")
