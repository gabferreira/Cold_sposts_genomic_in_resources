#install required packages
install.packages("svglite")
install.packages("ggplot2")
library(svglite)
library(ggplot2)

#read in data
collab_data<-read.csv("Final_postsubmission_Phase_2_Filtering.csv")

#filter by Passed Filter 2, Wild, that the species had continent data (based on if it had locality data), and that it was a domestic or international collab
collab_data<-collab_data[collab_data$Wild.or.Captive=="Wild",]
collab_data<-collab_data[is.na(collab_data$Continent)==FALSE,]
collab_data<-collab_data[collab_data$Collaboration!="Single Author",]

#calculate total number of studies #606 total Domestic & International Collab studies with Continent data
length(unique(collab_data$Study.ID))

#deduplicate by Study, Class, and Continent
deduplicate_collab_data<-distinct(collab_data, Study.ID, Class, Continent, .keep_all=TRUE)
#681 unique Study, Class, Continent combos
length(deduplicate_collab_data$Study.ID)
##606 studies
length(unique(deduplicate_collab_data$Study.ID))

dedup_amphib<-deduplicate_collab_data[deduplicate_collab_data$Class=="Amphibia",]
#289 Amphib studies
length(unique(dedup_amphib$Study.ID))
dedup_rep<-deduplicate_collab_data[deduplicate_collab_data$Class=="Reptilia",]
#319 Rept studies
length(unique(dedup_rep$Study.ID))

#count by Continent, Class, and Collaboration
deduplicate_collab_data_counts<-deduplicate_collab_data %>% count(Continent, Class, Collaboration)

#break up amphibians and reptiles
collab_continent_amphibia_dedup<-deduplicate_collab_data_counts[deduplicate_collab_data_counts$Class=="Amphibia",]
collab_continent_reptilia_dedup<-deduplicate_collab_data_counts[deduplicate_collab_data_counts$Class=="Reptilia",]

#330 unique Study, Class, Continent combos
sum(collab_continent_amphibia_dedup$n)

#351 unique Study, Class, Continent combos
sum(collab_continent_reptilia_dedup$n)

#change factor order to reflect n 
collab_continent_amphibia_dedup$Continent<-factor(collab_continent_amphibia_dedup$Continent, levels = c("North America","Asia","Europe","South America","Africa","Oceania"))
collab_continent_reptilia_dedup$Continent<-factor(collab_continent_reptilia_dedup$Continent, levels = c("North America","Asia","Europe","South America","Africa","Oceania"))


collab_continent_amphibia_dedup
collab_continent_reptilia_dedup

##final amphibian continent plot (n=330)
continent_amphibia<-ggplot(collab_continent_amphibia_dedup, aes(fill=Collaboration, x=Continent, y=n)) +
  theme_bw()+
  geom_bar(position="stack", stat="identity")+ggtitle("Amphibia")+
  annotate("text", x = 6, y = 16.5, label = "17", size=7)+annotate("text", x = 6, y = 4, label = "8", size=7)+annotate("text", x = 4, y = 29, label = "6", size=7)+annotate("text", x = 4, y = 13, label = "26", size=7)+annotate("text", x = 5, y = 28, label = "4", size=7)+annotate("text", x = 5, y = 13, label = "26", size=7)+annotate("text", x = 2, y = 21.5, label = "43", size=7)+annotate("text", x = 2, y = 63, label = "40", size=7)+annotate("text", x = 1, y = 57, label = "68", size=7)+annotate("text", x = 1, y = 12, label = "24", size=7)+annotate("text", x = 3, y = 25, label = "50", size=7)+annotate("text", x = 3, y = 59, label = "18", size=7)+
  scale_fill_manual(values=c("#A8DDB5", "#4EB3D3"), name="Collaboration")+
  ylim(0,150)+
  theme(plot.title = element_text(hjust = 0.5, size=36), panel.border=element_blank(), axis.line.y=element_line(size=1), axis.line.x=element_line(size=1), panel.grid = element_blank(), legend.text = element_text(size=21), legend.title=element_text(size=23), axis.title.x = element_text(size=28, margin = margin(t=10)), axis.title.y=element_text(size=28, margin=margin(r=10)),axis.text.x = element_text(size=15.5), axis.text.y=element_text(size=19))+
  ylab("Number of Genomic Resources")
continent_amphibia

##final reptile continent plot (n=351)
continent_reptilia<-ggplot(collab_continent_reptilia_dedup, aes(fill=Collaboration, x=Continent, y=n)) +
  theme_bw()+
  geom_bar(position="stack", stat="identity")+ggtitle("Reptilia")+
  annotate("text", x = 5, y = 15.5, label = "1", size=7)+annotate("text", x = 5, y = 6, label = "12", size=7)+annotate("text", x = 4, y = 33.5, label = "9", size=7)+annotate("text", x = 4, y = 14.5, label = "29", size=7)+annotate("text", x = 3, y = 35, label = "10", size=7)+annotate("text", x = 3, y = 15, label = "30", size=7)+annotate("text", x = 2, y = 16, label = "32", size=7)+annotate("text", x = 2, y = 47.5, label = "31", size=7)+annotate("text", x = 1, y = 104.5, label = "91", size=7)+annotate("text", x = 1, y = 29.5, label = "59", size=7)+annotate("text", x = 6, y = 10.5, label = "21", size=7)+annotate("text", x = 6, y = 34, label = "26", size=7)+
  scale_fill_manual(values=c("#A6BDDB", "#3690C0"), name="Collaboration")+
  ylim(0,150)+
  theme(plot.title = element_text(hjust = 0.5, size=36), panel.border=element_blank(), axis.line.y=element_line(size=1), axis.line.x=element_line(size=1), panel.grid = element_blank(), legend.text = element_text(size=21),legend.title=element_text(size=23),axis.title.x = element_text(size=28, margin = margin(t=10)), axis.title.y=element_text(size=28, margin=margin(r=10)),axis.text.x = element_text(size=15.5), axis.text.y=element_text(size=19))+
  ylab("Number of Genomic Resources")
continent_reptilia

#save plots
ggsave("Figure S9a_v2.svg", plot=continent_amphibia, width=10.5, height=9,dpi=300)
ggsave("Figure S9b_v2.svg", plot=continent_reptilia, width=10.5, height=9,dpi=300)



