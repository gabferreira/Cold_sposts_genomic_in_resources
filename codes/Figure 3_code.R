#install required packages
install.packages("svglite")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("streamgraph")
install.packages("viridis")
install.packages("plotly")
library(svglite)
library(ggplot2)
library(tidyverse)
library(streamgraph)
library(viridis)
library(hrbrthemes)
library(plotly)

#read in data
global_change_data<-read.csv("Final_postsubmission_Phase_2_Filtering.csv", header=T, sep= ",")

#rename scopes
global_change_data$Scope<-replace(global_change_data$Scope,global_change_data$Scope=="Level 1: General genomic resources", "Level 1")
global_change_data$Scope<-replace(global_change_data$Scope,global_change_data$Scope=="Level 2: Spatial genomic variation", "Level 2")
global_change_data$Scope<-replace(global_change_data$Scope,global_change_data$Scope=="Level 3: Functional Variation - global change", "Level 3")
global_change_data$Scope<-replace(global_change_data$Scope,global_change_data$Scope=="Level 4: Functional Variation - climate change", "Level 4")
global_change_data$Scope<-replace(global_change_data$Scope,global_change_data$Scope=="Level 5: Adaptive Potential - climate change", "Level 5")

#deduplicate by Study, Scope
global_change_data_dedup <- distinct(global_change_data, Study.ID, Scope, .keep_all = TRUE)


#703 unique Study, Scope combos ( a few studies double counted bc they had multiple scopes)
length(global_change_data_dedup$Study.ID)
#693 unique studies
unique(global_change_data_dedup$Study.ID)


#make a table with counts by Scope and Year
global_change_data_dedup_count <- global_change_data_dedup %>% 
  group_by(Year, Scope) %>% 
  count(nrow(`Scope`))
global_change_data_dedup_count

#make the plot!
num_study_by_year_scope<-ggplot(global_change_data_dedup_count, aes(x=Year, y= n, fill=Scope)) + 
  geom_area() + 
  xlim(2005,2025) + 
  ylim(0,120) + 
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  ylab("Number of Studies")+
  theme_bw()+
  theme(panel.border=element_blank(), axis.line.x=element_line(size=1), axis.line.y=element_line(size=1), panel.grid=element_blank(),axis.text = element_text(size=14), legend.text = element_text(size=12),legend.title=element_text(size=14), axis.title.x = element_text(size=18, margin = margin(t=10)),axis.title.y=element_text(size=18, margin=margin(r=10)))
num_study_by_year_scope

#save the plot
ggsave("Figure 3_v2.svg", plot=num_study_by_year_scope, width=9, height=7,dpi=300)


