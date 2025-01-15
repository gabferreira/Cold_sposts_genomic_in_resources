library(ggplot2)
library(dplyr)
library(ggbreak)
library(ggrepel)
library(ggpubr)
library(tidyr)

amphibs<-read.csv("~/Desktop/Cold spots stuffs/relative_richness_amphibians_Frost.csv", header = TRUE)
reptiles<-read.csv("~/Desktop/Cold spots stuffs/relative_richness_reptilia_database.csv", header = TRUE)
country.GNGS.unctad<-read.csv("~/Desktop/Cold spots stuffs/country.GNGS.unctad.csv", header = TRUE)

#amphibians 
names(amphibs)[names(amphibs) == "relative_richness.."] <- "relative_richness"
names(amphibs)[names(amphibs) == "classes"] <- "relative_richness_percent"
names(amphibs)[names(amphibs) == "classes"] <- "relative_richness_percent"
amphibs$Country <- sub("Korea", "North Korea", amphibs$Country)
amphibs$Country <- sub("Ivory Coast", "Côte d'Ivoire", amphibs$Country)
amphibs$Country <- sub("Democratic Republic of Congo", "Democratic Republic of the Congo", amphibs$Country)
amphibs$Country <- sub("Czechia", "Czech Republic", amphibs$Country)

amphibs <- amphibs %>%
  left_join(country.GNGS.unctad, by = "Country")

head(amphibs)

# Find rows where GN.GS is NA
countries_with_na <- amphibs %>%
  filter(is.na(GN.GS)) %>%
  select(Country)

# Print the countries with NA in GN.GS
print(countries_with_na)

# Replace NA values in GN.GS with "Not UNCTAD"
amphibs <- amphibs %>%
  mutate(GN.GS = ifelse(is.na(GN.GS), "Not UNCTAD", GN.GS))

p1 <- ggplot(amphibs, aes(x = Amphibia_richness_Frost_2024, y = relative_richness)) +
  geom_point(aes(color = GN.GS), size = 1.5, position = position_jitter(width = 10, height = 1)) +  # Plot all points
  geom_text_repel(data = amphibs %>% filter(relative_richness > 25 | Amphibia_richness_Frost_2024 > 300), 
                  aes(label = Country, color = GN.GS), 
                  size = 3, 
                  max.overlaps = 50,
                  segment.size = 0.3) +  # Adjust line thickness
  scale_color_manual(values = c("#01431c", "#74c476", "grey")) +
  labs(y = "Percent species with \n genomic resources", 
       x = "Species Richness") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(size = 0.9),
        axis.line.y = element_line(size = 0.9),
        axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 24, margin = margin(t = 10)),
        axis.title.y = element_text(size = 24, margin = margin(r = 10)),
        legend.position = "none")

p2 <- ggplot(amphibs %>% filter(relative_richness < 25 & Amphibia_richness_Frost_2024 < 300),  # Filter extreme points
             aes(x = Amphibia_richness_Frost_2024, y = relative_richness)) +
  geom_point(aes(color = GN.GS), size = 1.5) +  # Add points
  geom_text_repel(aes(label = Country, color = GN.GS), 
                  size = 3, 
                  segment.size = 0.3,  # Adjust line thickness
                  max.overlaps = 50) +
  scale_color_manual(values = c("#01431c", "#74c476", "grey")) +
  labs(y = "", 
       x = "Species Richness") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(size = 0.9),
        axis.line.y = element_line(size = 0.9),
        axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 24, margin = margin(t = 10)),
        axis.title.y = element_text(size = 24, margin = margin(r = 10)))

ggarrange(p1, p2, widths = c(1.7, 2))

#reptiles

head(reptiles)

reptiles$Country <- sub("^Democratic Republic of Congo$", "Democratic Republic of the Congo", reptiles$Country)
reptiles$Country <- sub("^Congo$", "Republic of the Congo", reptiles$Country)
reptiles$Country <- sub("Czechia", "Czech Republic", reptiles$Country)
reptiles$Country <- sub("The Bahamas", "Bahamas", reptiles$Country)

reptiles <- reptiles %>%
  filter(!is.na(relative_richness_percent))

reptiles <- reptiles %>%
  left_join(country.GNGS.unctad, by = "Country")

# Find rows where GN.GS is NA
countries_with_na <- reptiles %>%
  filter(is.na(GN.GS)) %>%
  select(Country)

# Print the countries with NA in GN.GS
print(countries_with_na)

# Replace NA values in GN.GS with "Not UNCTAD"
reptiles <- reptiles %>%
  mutate(GN.GS = ifelse(is.na(GN.GS), "Not UNCTAD", GN.GS))

p3 <- ggplot(reptiles, aes(x = Reptilia, y = relative_richness)) +
  geom_point(aes(color = GN.GS), size = 1.5) +  # Plot all points
  geom_text_repel(data = reptiles %>% filter(relative_richness > 10 | Reptilia > 350), 
                  aes(label = Country, color = GN.GS), 
                  size = 3, 
                  max.overlaps = 50,
                  segment.size = 0.3) +  # Adjust line thickness
  scale_color_manual(values = c("#01431c", "#74c476", "grey")) +
  labs(y = "Percent species with \n genomic resources", 
       x = "Species Richness") +
  xlim(0,1300) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(size = 0.9),
        axis.line.y = element_line(size = 0.9),
        axis.text = element_text(size = 20),
        #axis.text.x = element_text(size = 20, hjust = 0.9),
        axis.title.x = element_text(size = 24, margin = margin(t = 10)),
        axis.title.y = element_text(size = 24, margin = margin(r = 10)),
        legend.position = "none")

p4 <- ggplot(reptiles %>% filter(relative_richness < 10 & Reptilia < 350),  # Filter extreme points
             aes(x = Reptilia, y = relative_richness)) +
  geom_point(aes(color = GN.GS), size = 1.5) +  # Add points
  geom_text_repel(aes(label = Country, color = GN.GS), 
                  size = 3, 
                  segment.size = 0.3, max.overlaps = 50) +  # Adjust line thickness
  scale_color_manual(values = c("#01431c", "#74c476", "grey")) +
  labs(y = "", 
       x = "Species Richness") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(size = 0.9),
        axis.line.y = element_line(size = 0.9),
        axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 24, margin = margin(t = 10)),
        axis.title.y = element_text(size = 24, margin = margin(r = 10)))

ggarrange(p3, p4, widths=c(1.7,2))