# install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("viridis")
install.packages("colorspace")
library(dplyr)
library(ggplot2)
library(viridis)
library(colorspace)

#read in data
violin_data<- read.csv("Final_postsubmission_Phase_2_Filtering.csv", head = TRUE, sep = ",")

#610 wild studies
violin_data<-violin_data[violin_data$Wild.or.Captive=="Wild",]
length(unique(violin_data$Study.ID))
#609 wild studies with sampling locality info
violin_data<-violin_data[is.na(violin_data$Latitude)==FALSE,]
length(unique(violin_data$Study.ID))

#add columns with proportion of local authors and percentage of local authors
violin_data$prop_local_authors<-violin_data$Number.of.Local.Authors/violin_data$Total.Number.of.Authors
violin_data$percentage_local_authors<-(violin_data$prop_local_authors)*100

#find studies with both GN and GS sampling and take them out
#543 studies, 66 taken out bc sampling in both GN & GS
violin_one_GN_GS_sampling<-violin_data %>% 
  group_by(Study.ID) %>%
  mutate(multiple_products = +(n_distinct(Species.Global.North.or.South) > 1)) %>%
  ungroup()
violin_one_GN_GS_sampling_final<-violin_one_GN_GS_sampling[violin_one_GN_GS_sampling$multiple_products!=1,]
violin_one_GN_GS_sampling_final_dedup<-distinct(violin_one_GN_GS_sampling_final, Study.ID, .keep_all = TRUE)
length(unique(violin_one_GN_GS_sampling_final_dedup$Study.ID))

#find studies with sampling in multiple continents and take them out
#557 studies, 52 taken out bc sampling in multiple continents
violin_one_continent_sampling<-violin_data %>% 
  group_by(Study.ID) %>%
  mutate(multiple_products = +(n_distinct(Continent) > 1)) %>%
  ungroup()
violin_one_continent_sampling_final<-violin_one_continent_sampling[violin_one_continent_sampling$multiple_products!=1,]
violin_one_continent_sampling_final_dedup<-distinct(violin_one_continent_sampling_final, Study.ID, .keep_all = TRUE)
length(unique(violin_one_continent_sampling_final_dedup$Study.ID))

#not all the same studies got removed, 19 unique studies in GN/GS dataset vs continent dataset
length(setdiff(violin_one_GN_GS_sampling_final_dedup$Study.ID, violin_one_continent_sampling_final_dedup$Study.ID))

#calculate means for each continent
violin_mean_continent <- violin_one_GN_GS_continent_combined %>%
  group_by(Continent) %>%
  summarize(mean_percent_local_authors = mean(percentage_local_authors), stderror=sd(prop_local_authors)/sqrt(n()))

#replace continent column with GN/GS for the 555 studies
violin_one_GN_GS_sampling_final_dedup$Continent<-violin_one_GN_GS_sampling_final_dedup$Species.Global.North.or.South

#combine one GN/GS and one continent datasets
violin_one_GN_GS_continent_combined<-rbind(violin_one_continent_sampling_final_dedup,violin_one_GN_GS_sampling_final_dedup)

#reorder continents for plot
violin_one_GN_GS_continent_combined$Continent <- factor(violin_one_GN_GS_continent_combined$Continent, 
                                  levels = c("North America", "Oceania", "Asia", "Europe", "South America", "Africa", "Global North", "Global South"))


violin_plot <- ggplot(violin_one_GN_GS_continent_combined, aes(x = Continent, y = percentage_local_authors, fill = Continent)) +
  geom_violin(width = 1.3, color = NA, alpha = 0.9) +
  geom_errorbar(data = violin_mean_continent, aes(x = Continent, y = mean_percent_local_authors, ymin = mean_percent_local_authors, ymax = mean_percent_local_authors, linetype = "Mean"), 
                color = "black", width = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)) +
  scale_linetype_manual(values = c("Mean" = "dashed"))+
  scale_fill_manual(values=c(palette[1:6], "#01431c", "#74c476")) +
  labs(x = "Region Sampled", y = "Percentage of Local Authors") +
  theme_bw() +
  theme(legend.position="none",panel.border=element_blank(),axis.line.x=element_line(size=0.6),axis.line.y=element_line(size=0.6),panel.grid = element_blank(),axis.title.x = element_text(size=14, margin = margin(t=10)), axis.title.y=element_text(size=14, margin=margin(r=10)),axis.text = element_text(size=12))+ylab("Percentage of Local Authors")
violin_plot

ggsave("Figure 6a_v2.svg", plot=violin_plot, width=9.5, height=4,dpi=300)

