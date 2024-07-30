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
global_change_data<-read.csv("Phase 2 Filtering - Phase 2 Filtering (4).csv", header=T, sep= ",")
#filter by passed filter 2
global_change_data<-global_change_data[global_change_data$Passed.Filter.2=="Yes",]

#rename scopes
global_change_data$Scope<-replace(global_change_data$Scope,global_change_data$Scope=="just genomic data (Level 0)", "Level 1")
global_change_data$Scope<-replace(global_change_data$Scope,global_change_data$Scope=="spatial only (Level 1)", "Level 2")
global_change_data$Scope<-replace(global_change_data$Scope,global_change_data$Scope=="climate functional variation (Level 2a)", "Level 4")
global_change_data$Scope<-replace(global_change_data$Scope,global_change_data$Scope=="other global change functional variation (Level 2b)", "Level 3")
global_change_data$Scope<-replace(global_change_data$Scope,global_change_data$Scope=="adaptive potential (Level 3)", "Level 5")

#deduplicate by Study, Scope
global_change_data_dedup <- distinct(global_change_data, Study.ID, Scope, .keep_all = TRUE)

#703 unique Study, Scope combos ( a few studies double counted bc they had multiple scopes)
length(global_change_data_dedup$Study.ID)
#693 unique studies
length(unique(global_change_data_dedup$Study.ID))


#make a table with counts by Scope and Year
global_change_data_dedup_count <- global_change_data_dedup %>% 
  group_by(Year, Scope) %>% 
  count(nrow(`Scope`))

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
ggsave("num_study_by_year_scope.svg", plot=num_study_by_year_scope, width=9, height=7,dpi=300)


