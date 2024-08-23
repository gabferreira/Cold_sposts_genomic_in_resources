#install packages
install.packages("RColorBrewer")
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
library(RColorBrewer)

#read in data
authorship_data<-read.csv("Final_postsubmission_Phase_2_Filtering.csv", header=T, sep= ",")

#fix scope labels
authorship_data$Scope<-replace(authorship_data$Scope,authorship_data$Scope=="Level 1: General genomic resources", "Level 1")
authorship_data$Scope<-replace(authorship_data$Scope,authorship_data$Scope=="Level 2: Spatial genomic variation", "Level 2")
authorship_data$Scope<-replace(authorship_data$Scope,authorship_data$Scope=="Level 3: Functional Variation - global change", "Level 3")
authorship_data$Scope<-replace(authorship_data$Scope,authorship_data$Scope=="Level 4: Functional Variation - climate change", "Level 4")
authorship_data$Scope<-replace(authorship_data$Scope,authorship_data$Scope=="Level 5: Adaptive Potential - climate change", "Level 5")

#filter by wild and local author not NA
authorship_data_wild<-authorship_data[authorship_data$Wild.or.Captive=="Wild",]
authorship_data_wild_local_author<-authorship_data_wild[is.na(authorship_data_wild$Number.of.Local.Authors)==FALSE,]
authorship_data_wild_local_author<-authorship_data_wild_local_author[is.na(authorship_data_wild_local_author$Species.Global.North.or.South)==FALSE,]


#deduplicate study based on Study ID
deduplicate_study<-distinct(authorship_data_wild_local_author, Study.ID, .keep_all= TRUE)

#609 studies with wild & local author data
length(unique(authorship_data_wild_local_author$Study.ID))


#find studies with both GN and GS sampling and take them out
global_divide_multiple<-authorship_data_wild_local_author %>% 
  group_by(Study.ID) %>%
  mutate(multiple_products = +(n_distinct(Species.Global.North.or.South) > 1)) %>%
  ungroup()
global_divide_multiple_final<-global_divide_multiple[global_divide_multiple$multiple_products!=1,]

#543 studies with wild & local author data w/o GN & GS sampling studies
length(unique(global_divide_multiple_final$Study.ID))

#add proportion and percentage of local authors columns to dataset w/o GN & GS studies
global_divide_multiple_final$prop_local_authors<-global_divide_multiple_final$Number.of.Local.Authors/global_divide_multiple_final$Total.Number.of.Authors
global_divide_multiple_final$percentage_local_authors<-(global_divide_multiple_final$prop_local_authors)*100

#add proportion and percentage of local authors columns to dataset w/ GN & GS studies
authorship_data_wild_local_author$prop_local_authors<-authorship_data_wild_local_author$Number.of.Local.Authors/authorship_data_wild_local_author$Total.Number.of.Authors
authorship_data_wild_local_author$percentage_local_authors<-(authorship_data_wild_local_author$prop_local_authors)*100

#deduplicate for unique Study ID, Scope, GN/GS sampling combo for dataset w/0 GN & GS studies
global_divide_multiple_final_dedup<-distinct(global_divide_multiple_final,Study.ID, Scope, Species.Global.North.or.South, .keep_all= TRUE)
global_divide_multiple_final_dedup_GN<-global_divide_multiple_final[global_divide_multiple_final$Species.Global.North.or.South=="Global North",]
global_divide_multiple_final_dedup_GS<-global_divide_multiple_final[global_divide_multiple_final$Species.Global.North.or.South=="Global South",]

#deduplicate for unique Study ID, Scope, GN/GS sampling combo for dataset w/ GN & GS studies
authorship_data_dedup_both<-distinct(authorship_data_wild_local_author,Study.ID, Scope, Species.Global.North.or.South, .keep_all= TRUE)
authorship_data_dedup_GN_both<-authorship_data_dedup_both[authorship_data_dedup_both$Species.Global.North.or.South=="Global North",]
authorship_data_dedup_GS_both<-authorship_data_dedup_both[authorship_data_dedup_both$Species.Global.North.or.South=="Global South",]



#675 studies total in figure because 66 studies had sampling in the Global North & South, 54 of these are double counted. 
#609 unique studies used in this figure. 66 studies had sampling in both the GN and the GS for a total of #416 studies with sampling in the GN and #247 GS. 
#Representing #685 unique genomic resources, defined as unique study and scope combos, since some studies had multiple scopes

##Global North w/ GN & GS studies
#395 unique Study, Scope, combos (a few studies double counted bc they had multiple scopes)
length(authorship_data_dedup_GN_both$Study.ID)
#390 unique studies
length(unique(authorship_data_dedup_GN_both$Study.ID))

##Global South w/ GN & GS studies
#290 unique Study, Scope, combos (a few studies double counted bc they had multiple scopes)
length(authorship_data_dedup_GS_both$Study.ID)
#285 unique studies
length(unique(authorship_data_dedup_GS_both$Study.ID))

##Total w/ GN & GS studies
#685 unique Study, Scope, combos (a few studies double counted bc they had multiple scopes)
length(authorship_data_dedup_both$Study.ID)
#609 unique studies
length(unique(authorship_data_dedup_both$Study.ID))

#Numbers for GN plot
sum(authorship_data_dedup_GN_both$Scope=="Level 1" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GN_both$Scope=="Level 1" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global South")
sum(authorship_data_dedup_GN_both$Scope=="Level 2" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GN_both$Scope=="Level 2" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global South")
sum(authorship_data_dedup_GN_both$Scope=="Level 3" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GN_both$Scope=="Level 3" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global South")
sum(authorship_data_dedup_GN_both$Scope=="Level 4" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GN_both$Scope=="Level 4" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global South")
sum(authorship_data_dedup_GN_both$Scope=="Level 5" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GN_both$Scope=="Level 5" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global South")

#Numbers for GS plot
sum(authorship_data_dedup_GS_both$Scope=="Level 1" & authorship_data_dedup_GS_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GS_both$Scope=="Level 1" & authorship_data_dedup_GS_both$X1st.Author.Global.Divide=="Global South")
sum(authorship_data_dedup_GS_both$Scope=="Level 2" & authorship_data_dedup_GS_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GS_both$Scope=="Level 2" & authorship_data_dedup_GS_both$X1st.Author.Global.Divide=="Global South")
sum(authorship_data_dedup_GS_both$Scope=="Level 3" & authorship_data_dedup_GS_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GS_both$Scope=="Level 3" & authorship_data_dedup_GS_both$X1st.Author.Global.Divide=="Global South")
sum(authorship_data_dedup_GS_both$Scope=="Level 4" & authorship_data_dedup_GS_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GS_both$Scope=="Level 4" & authorship_data_dedup_GS_both$X1st.Author.Global.Divide=="Global South")
sum(authorship_data_dedup_GS_both$Scope=="Level 5" & authorship_data_dedup_GS_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GS_both$Scope=="Level 5" & authorship_data_dedup_GS_both$X1st.Author.Global.Divide=="Global South")



##plot it!
GN_Scope_Prop_first_author<-ggplot(authorship_data_dedup_GN_both, aes(x=Scope, fill=X1st.Author.Global.Divide))+
  ggtitle("Global North Studies")+geom_bar(position="fill")+
  geom_bar(position="fill")+geom_segment(x=0.415,y=-0.5,yend=1.0014, size=1)+
  theme_bw()+
  annotate("text", x = 1, y = 1.0285, label = "(114/5)", size=5.8)+
  annotate("text", x = 2, y = 1.0285, label = "(200/13)", size=5.8)+
  annotate("text", x = 3, y = 1.0285, label = "(31/0)", size=5.8)+
  annotate("text", x = 4, y = 1.0285, label = "(25/0)", size=5.8)+
  annotate("text", x = 5, y = 1.0285, label = "(7/0)", size=5.8)+
  scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+
  theme(plot.title = element_text(hjust = 0.5, size=26),panel.border=element_blank(),axis.line.x=element_line(size=0.9),panel.grid = element_blank(), legend.text = element_text(size=18),legend.title=element_text(size=20),axis.title.x = element_text(size=24, margin = margin(t=10)), axis.title.y=element_text(size=24, margin=margin(r=10)),axis.text = element_text(size=20))+ylab("Proportion of Studies")
GN_Scope_Prop_first_author

GS_Scope_Prop_first_author<-ggplot(authorship_data_dedup_GS_both, aes(x=Scope, fill=X1st.Author.Global.Divide))+
  ggtitle("Global South Studies")+geom_bar(position="fill")+
  geom_bar(position="fill")+geom_segment(x=0.415,y=-0.5,yend=1.0014, size=1)+
  theme_bw()+
  annotate("text", x = 1, y = 1.0285, label = "(44/49)", size=5.8)+
  annotate("text", x = 2, y = 1.0285, label = "(97/53)", size=5.8)+
  annotate("text", x = 3, y = 1.0285, label = "(3/14)", size=5.8)+
  annotate("text", x = 4, y = 1.0285, label = "(9/14)", size=5.8)+
  annotate("text", x = 5, y = 1.0285, label = "(1/6)", size=5.8)+
  scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+
  theme(plot.title = element_text(hjust = 0.5, size=26),panel.border=element_blank(),axis.line.x=element_line(size=0.9),panel.grid = element_blank(), legend.text = element_text(size=18),legend.title=element_text(size=20),axis.title.x = element_text(size=24, margin = margin(t=10)), axis.title.y=element_text(size=24, margin=margin(r=10)),axis.text = element_text(size=20))+ylab("Proportion of Studies")
GS_Scope_Prop_first_author

##without legend
GN_Scope_Prop_first_author_no_leg<-ggplot(authorship_data_dedup_GN_both, aes(x=Scope, fill=X1st.Author.Global.Divide))+
  ggtitle("Global North Studies")+geom_bar(position="fill")+
  geom_bar(position="fill")+geom_segment(x=0.415,y=-0.5,yend=1.0014, size=1)+
  theme_bw()+
  annotate("text", x = 1, y = 1.0285, label = "(114/5)", size=5.8)+
  annotate("text", x = 2, y = 1.0285, label = "(200/13)", size=5.8)+
  annotate("text", x = 3, y = 1.0285, label = "(31/0)", size=5.8)+
  annotate("text", x = 4, y = 1.0285, label = "(25/0)", size=5.8)+
  annotate("text", x = 5, y = 1.0285, label = "(7/0)", size=5.8)+
  scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5, size=26),panel.border=element_blank(),axis.line.x=element_line(size=0.9),panel.grid = element_blank(), legend.text = element_text(size=18),legend.title=element_text(size=20),axis.title.x = element_text(size=24, margin = margin(t=10)), axis.title.y=element_text(size=24, margin=margin(r=10)),axis.text = element_text(size=20))+ylab("Proportion of Studies")
GN_Scope_Prop_first_author_no_leg

GS_Scope_Prop_first_author_no_leg<-ggplot(authorship_data_dedup_GS_both, aes(x=Scope, fill=X1st.Author.Global.Divide))+
  ggtitle("Global South Studies")+geom_bar(position="fill")+
  geom_bar(position="fill")+geom_segment(x=0.415,y=-0.5,yend=1.0014, size=1)+
  theme_bw()+
  annotate("text", x = 1, y = 1.0285, label = "(44/49)", size=5.8)+
  annotate("text", x = 2, y = 1.0285, label = "(97/53)", size=5.8)+
  annotate("text", x = 3, y = 1.0285, label = "(3/14)", size=5.8)+
  annotate("text", x = 4, y = 1.0285, label = "(9/14)", size=5.8)+
  annotate("text", x = 5, y = 1.0285, label = "(1/6)", size=5.8)+
  scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, size=26),panel.border=element_blank(),axis.line.x=element_line(size=0.9),panel.grid = element_blank(), legend.text = element_text(size=18),legend.title=element_text(size=20),axis.title.x = element_text(size=24, margin = margin(t=10)), axis.title.y=element_text(size=24, margin=margin(r=10)),axis.text = element_text(size=20))+ylab("Proportion of Studies")

#save the plots
ggsave("Figure 6d_v2.svg", plot=GN_Scope_Prop_first_author_no_leg, width=8, height=8,dpi=300)
ggsave("Figure 6e_v2.svg", plot=GS_Scope_Prop_first_author_no_leg, width=8, height=8,dpi=300)


