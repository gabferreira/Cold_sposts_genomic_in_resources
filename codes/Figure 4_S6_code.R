install.packages("dplyr")
install.packages("svglite")
install.packages("ggplot2")
library(dplyr)
library(svglite)
library(ggplot2)

#read in data
IUCN_data<-read.csv("Phase 2 Filtering - Phase 2 Filtering (4).csv", header=T, sep= ",")
IUCN_data<-IUCN_data[IUCN_data$Passed.Filter.2=="Yes",]

#693 wild and captive
length(unique(IUCN_data$Study.ID))
IUCN_data<-IUCN_data[is.na(IUCN_data$IUCN.Status)==FALSE,]
#673 wild and captive with IUCN status
length(unique(IUCN_data$Study.ID))

#relabel Scope names
IUCN_data$Scope<-replace(IUCN_data$Scope,IUCN_data$Scope=="just genomic data (Level 0)", "Level 1")
IUCN_data$Scope<-replace(IUCN_data$Scope,IUCN_data$Scope=="spatial only (Level 1)", "Level 2")
IUCN_data$Scope<-replace(IUCN_data$Scope,IUCN_data$Scope=="climate functional variation (Level 2a)", "Level 4")
IUCN_data$Scope<-replace(IUCN_data$Scope,IUCN_data$Scope=="other global change functional variation (Level 2b)", "Level 3")
IUCN_data$Scope<-replace(IUCN_data$Scope,IUCN_data$Scope=="adaptive potential (Level 3)", "Level 5")

#refactor genomic data type to change order
IUCN_data$Genomic<-factor(IUCN_data$Genomic, levels = c("RNASeq","GBS/RAD/target capture","Whole genome"))

#refactor IUCN status to change order
IUCN_data$IUCN.Status<-factor(IUCN_data$IUCN.Status, levels = c("DD","LC","NT","VU","EN","CR","EW","EX"))

#make a new dataset excluding DD
IUCN_data_fixed<-IUCN_data[IUCN_data$IUCN.Status!="DD",]
#672 wild, captive studies with IUCN data, excluding DD
length(unique(IUCN_data_fixed$Study.ID))

#make a new dataset excluding LC
IUCN_data_fixed_noLC<-IUCN_data_fixed[IUCN_data_fixed$IUCN.Status!="LC",]

#deduplicate by Study, Species, Scope, and Genomic data type
deduplicate_place<-distinct(IUCN_data_fixed, Study.ID, Scope, IUCN.Latin.Binomial, Genomic, .keep_all= TRUE)
deduplicate_place_noLC<-distinct(IUCN_data_fixed_noLC, Study.ID, Scope, IUCN.Latin.Binomial, Genomic, .keep_all= TRUE)

#Calculate number of genomic resources after deduplication
#1933 unique Study, Species, Scope, and Genomic data type combos
length(deduplicate_place$Scope)
#672 unique studies
length(unique(deduplicate_place$Study.ID))

#436 unique Study, Species, Scope, and Genomic data type combos
length(deduplicate_place_noLC$Scope)
#235 unique studies
length(unique(deduplicate_place_noLC$Study.ID))

#calculate n for each bar of the final IUCN/Scope plots
#LC
sum(deduplicate_place$Scope=="Level 1")
sum(deduplicate_place$Scope=="Level 2")
sum(deduplicate_place$Scope=="Level 3")
sum(deduplicate_place$Scope=="Level 4")
sum(deduplicate_place$Scope=="Level 5")
#no LC
sum(deduplicate_place_noLC$Scope=="Level 1")
sum(deduplicate_place_noLC$Scope=="Level 2")
sum(deduplicate_place_noLC$Scope=="Level 3")
sum(deduplicate_place_noLC$Scope=="Level 4")
sum(deduplicate_place_noLC$Scope=="Level 5")

#final plots
#Scope
IUCN_scope_plot<-ggplot(deduplicate_place, aes(Scope, fill = IUCN.Status)) +
  geom_bar(position="fill")+geom_segment(x=0.415,y=-0.5,yend=1.0014, size=1)+
  annotate("text", x = 1, y = 1.0285, label = "(757)", size=5.8)+annotate("text", x = 2, y = 1.0285, label = "(1022)", size=5.8)+annotate("text", x = 3, y = 1.0285, label = "(74)", size=5.8)+annotate("text", x = 4, y = 1.0285, label = "(62)", size=5.8)+annotate("text", x = 5, y = 1.0285, label = "(18)", size=5.8)+
  scale_fill_manual(values = col_rocket_9[2:8], name="IUCN Status")+
  ylab("Proportion of Genomic Resources")+theme_bw()+xlab("Scope")+
  theme(panel.border=element_blank(),axis.line.x=element_line(size=0.9),panel.grid = element_blank(), legend.text = element_text(size=20),legend.title=element_text(size=20),axis.title.x = element_text(size=24, margin = margin(t=10)), axis.title.y=element_text(size=24, margin=margin(r=10)),axis.text = element_text(size=20))
IUCN_scope_plot

IUCN_no_LC_scope_plot<-ggplot(deduplicate_place_noLC, aes(Scope, fill = IUCN.Status)) +
  geom_bar(position="fill")+geom_segment(x=0.415,y=-0.5,yend=1.0014, size=1)+
  annotate("text", x = 1, y = 1.0285, label = "(151)", size=5.8)+annotate("text", x = 2, y = 1.0285, label = "(245)", size=5.8)+annotate("text", x = 3, y = 1.0285, label = "(24)", size=5.8)+annotate("text", x = 4, y = 1.0285, label = "(12)", size=5.8)+annotate("text", x = 5, y = 1.0285, label = "(4)", size=5.8)+
  scale_fill_manual(values = col_rocket_9[3:8], name="IUCN Status")+
  ylab("Proportion of Genomic Resources")+theme_bw()+xlab("Scope")+
  theme(panel.border=element_blank(),axis.line.x=element_line(size=1),panel.grid = element_blank(), legend.text = element_text(size=20),legend.title=element_text(size=20),axis.title.x = element_text(size=24, margin = margin(t=10)), axis.title.y=element_text(size=24, margin=margin(r=10)),axis.text = element_text(size=20))
IUCN_no_LC_scope_plot

ggsave("IUCN.svg", plot=IUCN_scope_plot, width=9, height=9,dpi=200)
ggsave("IUCN_noLC.svg", plot=IUCN_no_LC_scope_plot, width=9, height=9,dpi=200)

#Genomic Data
IUCN_genomic<-ggplot(deduplicate_place, aes(Genomic, fill = IUCN.Status)) +
  geom_bar(position="fill")+geom_segment(x=0.41,y=-0.5,yend=1.0014, size=1)+
  annotate("text", x = 1, y = 1.0285, label = "(397)", size=5.8)+annotate("text", x = 2, y = 1.0285, label = "(1371)", size=5.8)+annotate("text", x = 3, y = 1.0285, label = "(165)", size=5.8)+
  scale_fill_manual(values = col_rocket_9[2:8], name="IUCN Status")+
  ylab("Proportion of Genomic Resources")+
  theme_bw()+
  xlab("Genomic Data Type")+
  theme(panel.border=element_blank(),axis.line.x=element_line(size=1),panel.grid = element_blank(), legend.text = element_text(size=20),legend.title=element_text(size=20),axis.title.x = element_text(size=24, margin = margin(t=10)), axis.title.y=element_text(size=24, margin=margin(r=10)),axis.text = element_text(size=14.5))
IUCN_genomic

IUCN_noLC_genomic<-ggplot(deduplicate_place_noLC, aes(Genomic, fill = IUCN.Status)) +
  geom_bar(position="fill")+geom_segment(x=0.41,y=-0.5,yend=1.0014, size=1)+
  annotate("text", x = 1, y = 1.0285, label = "(98)", size=5.8)+annotate("text", x = 2, y = 1.0285, label = "(296)", size=5.8)+annotate("text", x = 3, y = 1.0285, label = "(42)", size=5.8)+
  scale_fill_manual(values = col_rocket_9[3:8], name="IUCN Status")+
  ylab("Proportion of Genomic Resources")+theme_bw()+
  xlab("Genomic Data Type")+
  theme(panel.border=element_blank(),axis.line.x=element_line(size=1),panel.grid = element_blank(), legend.text = element_text(size=20),legend.title=element_text(size=20),axis.title.x = element_text(size=24, margin = margin(t=10)), axis.title.y=element_text(size=24, margin=margin(r=10)),axis.text = element_text(size=14.5))
IUCN_noLC_genomic

ggsave("IUCN_genomic.svg", plot=IUCN_genomic, width=9, height=9,dpi=300)
ggsave("IUCN_noLC_genomic.svg", plot=IUCN_noLC_genomic, width=9, height=9,dpi=300)