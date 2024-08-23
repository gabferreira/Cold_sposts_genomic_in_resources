install.packages("ggplot2")
install.packages("dplyr")
install.packages("rstatix")
install.packages("viridis")
install.packages("scales")
install.packages("tidyr")
install.packages("ggpubr")
install.packages("svglite")
library(ggplot2)
library(dplyr)
library(rstatix)
library(viridis)
library(scales)
library(tidyr)
library(ggpubr)
library(svglite)

#read in data
data_hist <- read.csv("Final_postsubmission_Phase_2_Filtering.csv", header=TRUE)

#filter by wild studies
#610 wild studies
data_hist_wild<-data_hist[data_hist$Wild.or.Captive=="Wild",]
length(unique(data_hist_wild$Study.ID))

#remove all rows with no delta data
#579 studies with at least one instance of delta data
#2323 unique genomic resources (no deduplicaiton)
#31 studies with no delta data
data_hist_wild<-data_hist_wild[is.na(data_hist_wild$delta_maximum_temperature_1960_2050)==FALSE,]
length(unique(data_hist_wild$Study.ID))
length(data_hist_wild$Study.ID)

#histogram of scopes across deltas
delta_temp_studies<-ggplot(data_hist_wild, aes(x = delta_maximum_temperature_1960_2050, fill = Scope)) +
  geom_histogram(position = "stack", alpha = 1) +
  scale_fill_viridis(discrete=TRUE) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  theme_bw() +
  theme(panel.border=element_blank(),
        axis.line.x=element_line(size=1),
        axis.line.y=element_line(size=1),
        panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size=18, margin = margin(t=10)),
        axis.title.y=element_text(size=18, margin=margin(r=10)),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12)) +
  guides(fill=guide_legend(title="Scope")) +
  labs(x = expression(Delta*T[max]*" ("*degree*"C), 1961-2060"), y = "Number of Genomic Resources")
delta_temp_studies
ggsave("Figure S5_v2.svg", plot=delta_temp_studies, width=9, height=7,dpi=300)
