#install required packages
install.packages("svglite")
install.packages("ggplot2")
library(svglite)
library(ggplot2)


#read in data
Genomic_Scope_data<-read.csv("Final_postsubmission_Phase_2_Filtering.csv")

#filter by Passed Filter 2, Wild, and that the species had a GN/GS distinction (based on if it had locality data)
Genomic_Scope_data<-Genomic_Scope_data[Genomic_Scope_data$Wild.or.Captive=="Wild",]
Genomic_Scope_data<-Genomic_Scope_data[is.na(Genomic_Scope_data$Species.Global.North.or.South)==FALSE,]

Genomic_Scope_data$Scope<-replace(Genomic_Scope_data$Scope,Genomic_Scope_data$Scope=="Level 1: General genomic resources", "Level 1")
Genomic_Scope_data$Scope<-replace(Genomic_Scope_data$Scope,Genomic_Scope_data$Scope=="Level 2: Spatial genomic variation", "Level 2")
Genomic_Scope_data$Scope<-replace(Genomic_Scope_data$Scope,Genomic_Scope_data$Scope=="Level 3: Functional Variation - global change", "Level 3")
Genomic_Scope_data$Scope<-replace(Genomic_Scope_data$Scope,Genomic_Scope_data$Scope=="Level 4: Functional Variation - climate change", "Level 4")
Genomic_Scope_data$Scope<-replace(Genomic_Scope_data$Scope,Genomic_Scope_data$Scope=="Level 5: Adaptive Potential - climate change", "Level 5")



#refactor genomic data type to change order
Genomic_Scope_data$Genomic<-factor(Genomic_Scope_data$Genomic, levels = c("RNASeq","GBS/RAD/target capture","Whole genome"))

#Deduplicate data based on Study ID, Scope, Genomic data type, Species GN/GS, and Species)
deduplicate_place_GN_GS<-distinct(Genomic_Scope_data, Study.ID, Scope, IUCN.Latin.Binomial, Genomic, Species.Global.North.or.South,.keep_all= TRUE)

#Calculate number of genomic resources after deduplication #2018
length(deduplicate_place_GN_GS$Scope)
#Calculate number of studies after deduplication #609
length(unique(deduplicate_place_GN_GS$Study.ID))

#plot it and save plots
Scope_Area_Sampled<-ggplot(deduplicate_place_GN_GS, aes(fill=Species.Global.North.or.South, x=Scope)) +
  ylim(c(0,1600))+
  geom_bar(position="stack")+scale_fill_manual(values=c("#00441B","#74C476"), name="Area Sampled")+theme_bw()+theme(panel.border=element_blank(), axis.line.y=element_line(size=1), axis.line.x=element_line(size=1),panel.grid = element_blank(), legend.text = element_text(size=20),legend.title=element_text(size=22),axis.title.x = element_text(size=26, margin = margin(t=10)), axis.title.y=element_text(size=26, margin=margin(r=10)),axis.text = element_text(size=18))+ylab("Number of Genomic Resources")
Scope_Area_Sampled

Genomic_Area_Sampled<-ggplot(deduplicate_place_GN_GS, aes(fill=Species.Global.North.or.South, x=Genomic)) +
  ylim(c(0,1600))+
  geom_bar(position="stack")+scale_fill_manual(values=c("#00441B","#74C476"), name="Area Sampled")+theme_bw()+theme(panel.border=element_blank(), axis.line.y=element_line(size=1), axis.line.x=element_line(size=1),panel.grid = element_blank(), legend.text = element_text(size=20),legend.title=element_text(size=22),axis.title.x = element_text(size=26, margin = margin(t=10)), axis.title.y=element_text(size=26, margin=margin(r=10)),axis.text.y=element_text(size=18),axis.text.x = element_text(size=13))+ylab("Number of Genomic Resources")+xlab("Genomic Data Type")
Genomic_Area_Sampled

ggsave("Scope_Area_Sampled_v2.svg", plot=Scope_Area_Sampled, width=9, height=9,dpi=300)
ggsave("Genomic_Area_Sampled_v2.svg", plot=Genomic_Area_Sampled, width=9, height=9,dpi=300)
