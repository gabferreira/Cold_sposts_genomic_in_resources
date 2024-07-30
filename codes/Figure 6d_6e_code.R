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
authorship_data<-read.csv("Phase 2 Filtering - Phase 2 Filtering (4).csv", header=T, sep= ",")

#fix scope labels
authorship_data$Scope<-replace(authorship_data$Scope,authorship_data$Scope=="just genomic data (Level 0)", "Level 1")
authorship_data$Scope<-replace(authorship_data$Scope,authorship_data$Scope=="spatial only (Level 1)", "Level 2")
authorship_data$Scope<-replace(authorship_data$Scope,authorship_data$Scope=="climate functional variation (Level 2a)", "Level 4")
authorship_data$Scope<-replace(authorship_data$Scope,authorship_data$Scope=="other global change functional variation (Level 2b)", "Level 3")
authorship_data$Scope<-replace(authorship_data$Scope,authorship_data$Scope=="adaptive potential (Level 3)", "Level 5")

#deduplicate study based on Study ID
deduplicate_study<-distinct(authorship_data_wild_local_author, Study.ID, .keep_all= TRUE)


#filter by passed filter 2 and wild and local author not NA
authorship_data<-authorship_data[authorship_data$Passed.Filter.2=="Yes",]
authorship_data_wild<-authorship_data[authorship_data$Wild.or.Captive=="Wild",]
authorship_data_wild_local_author<-authorship_data_wild[is.na(authorship_data_wild$Number.of.Local.Authors..One.per.study.)==FALSE,]
authorship_data_wild_local_author<-authorship_data_wild_local_author[is.na(authorship_data_wild_local_author$Species.Global.North.or.South)==FALSE,]


#609 studies with wild & local author data
length(unique(authorship_data_wild_local_author$Study.ID))


#find studies with both GN and GS sampling and take them out
global_divide_multiple<-authorship_data_wild_local_author %>% 
  group_by(Study.ID) %>%
  mutate(multiple_products = +(n_distinct(Species.Global.North.or.South) > 1)) %>%
  ungroup()
global_divide_multiple_final<-global_divide_multiple[global_divide_multiple$multiple_products!=1,]

#555 studies with wild & local author data w/o GN & GS sampling studies
length(unique(global_divide_multiple_final$Study.ID))

#add proportion and percentage of local authors columns to dataset w/o GN & GS studies
global_divide_multiple_final$prop_local_authors<-global_divide_multiple_final$Number.of.Local.Authors..One.per.study./global_divide_multiple_final$Total.Number.of.Authors
global_divide_multiple_final$percentage_local_authors<-(global_divide_multiple_final$prop_local_authors)*100

#add proportion and percentage of local authors columns to dataset w/ GN & GS studies
authorship_data_wild_local_author$prop_local_authors<-authorship_data_wild_local_author$Number.of.Local.Authors..One.per.study./authorship_data_wild_local_author$Total.Number.of.Authors
authorship_data_wild_local_author$percentage_local_authors<-(authorship_data_wild_local_author$prop_local_authors)*100

#deduplicate for unique Study ID, Scope, GN/GS sampling combo for dataset w/0 GN & GS studies
global_divide_multiple_final_dedup<-distinct(global_divide_multiple_final,Study.ID, Scope, Species.Global.North.or.South, .keep_all= TRUE)
global_divide_multiple_final_dedup_GN<-global_divide_multiple_final[global_divide_multiple_final$Species.Global.North.or.South=="Global North",]
global_divide_multiple_final_dedup_GS<-global_divide_multiple_final[global_divide_multiple_final$Species.Global.North.or.South=="Global South",]

#deduplicate for unique Study ID, Scope, GN/GS sampling combo for dataset w/ GN & GS studies
authorship_data_dedup_both<-distinct(authorship_data_wild_local_author,Study.ID, Scope, Species.Global.North.or.South, .keep_all= TRUE)
authorship_data_dedup_GN_both<-authorship_data_dedup_both[authorship_data_dedup_both$Species.Global.North.or.South=="Global North",]
authorship_data_dedup_GS_both<-authorship_data_dedup_both[authorship_data_dedup_both$Species.Global.North.or.South=="Global South",]



#663 studies total in figure because 54 studies had sampling in the Global North & South, 54 of these are double counted. 
#609 unique studies used in this figure. 54 studies had sampling in both the GN and the GS for a total of #416 studies with sampling in the GN and #247 GS. 
#Representing #673 unique genomic resources, defined as unique study and scope combos, since some studies had multiple scopes

##Global North w/ GN & GS studies
#421 unique Study, Scope, combos (a few studies double counted bc they had multiple scopes)
length(authorship_data_dedup_GN_both$Study.ID)
#416 unique studies
length(unique(authorship_data_dedup_GN_both$Study.ID))

##Global South w/ GN & GS studies
#252 unique Study, Scope, combos (a few studies double counted bc they had multiple scopes)
length(authorship_data_dedup_GS_both$Study.ID)
#247 unique studies
length(unique(authorship_data_dedup_GS_both$Study.ID))

##Total w/ GN & GS studies
#673 unique Study, Scope, combos (a few studies double counted bc they had multiple scopes)
length(authorship_data_dedup_both$Study.ID)
#609 unique studies
length(unique(authorship_data_dedup_both$Study.ID))


sum(authorship_data_dedup_GN_both$Scope=="Level 1" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GN_both$Scope=="Level 1" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global South")
sum(authorship_data_dedup_GN_both$Scope=="Level 2" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GN_both$Scope=="Level 2" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global South")
sum(authorship_data_dedup_GN_both$Scope=="Level 3" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GN_both$Scope=="Level 4" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global North")
sum(authorship_data_dedup_GN_both$Scope=="Level 5" & authorship_data_dedup_GN_both$X1st.Author.Global.Divide=="Global North")


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
  annotate("text", x = 1, y = 1.0285, label = "(118/4)", size=5.8)+
  annotate("text", x = 2, y = 1.0285, label = "(223/13)", size=5.8)+
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
  annotate("text", x = 1, y = 1.0285, label = "(39/46)", size=5.8)+
  annotate("text", x = 2, y = 1.0285, label = "(79/42)", size=5.8)+
  annotate("text", x = 3, y = 1.0285, label = "(3/14)", size=5.8)+
  annotate("text", x = 4, y = 1.0285, label = "(8/14)", size=5.8)+
  annotate("text", x = 5, y = 1.0285, label = "(2/5)", size=5.8)+
  scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+
  theme(plot.title = element_text(hjust = 0.5, size=26),panel.border=element_blank(),axis.line.x=element_line(size=0.9),panel.grid = element_blank(), legend.text = element_text(size=18),legend.title=element_text(size=20),axis.title.x = element_text(size=24, margin = margin(t=10)), axis.title.y=element_text(size=24, margin=margin(r=10)),axis.text = element_text(size=20))+ylab("Proportion of Studies")
GS_Scope_Prop_first_author

##without legend
GN_Scope_Prop_first_author_no_leg<-ggplot(authorship_data_dedup_GN_both, aes(x=Scope, fill=X1st.Author.Global.Divide))+
  ggtitle("Global North Studies")+geom_bar(position="fill")+
  geom_bar(position="fill")+geom_segment(x=0.415,y=-0.5,yend=1.0014, size=1)+
  theme_bw()+
  annotate("text", x = 1, y = 1.0285, label = "(118/4)", size=5.8)+
  annotate("text", x = 2, y = 1.0285, label = "(223/13)", size=5.8)+
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
  annotate("text", x = 1, y = 1.0285, label = "(39/46)", size=5.8)+
  annotate("text", x = 2, y = 1.0285, label = "(79/42)", size=5.8)+
  annotate("text", x = 3, y = 1.0285, label = "(3/14)", size=5.8)+
  annotate("text", x = 4, y = 1.0285, label = "(8/14)", size=5.8)+
  annotate("text", x = 5, y = 1.0285, label = "(2/5)", size=5.8)+
  scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, size=26),panel.border=element_blank(),axis.line.x=element_line(size=0.9),panel.grid = element_blank(), legend.text = element_text(size=18),legend.title=element_text(size=20),axis.title.x = element_text(size=24, margin = margin(t=10)), axis.title.y=element_text(size=24, margin=margin(r=10)),axis.text = element_text(size=20))+ylab("Proportion of Studies")

#save the plots
ggsave("GN_Scope_Prop_first_author_5.svg", plot=GN_Scope_Prop_first_author_no_leg, width=8, height=8,dpi=300)
ggsave("GS_Scope_Prop_first_author_5.svg", plot=GS_Scope_Prop_first_author_no_leg, width=8, height=8,dpi=300)





















ggplot(authorship_data_dedup_GS_both, aes(x=Scope, fill=X1st.Author.Global.Divide))+
  ggtitle("Global South Studies")+geom_bar(position="fill")+
  theme_bw()+
  scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+
  theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Proportion of Studies")

ggplot(deduplicate_study_GN_divide_df, aes(x=Scope, fill=Last.Author.Global.Divide))+
  ggtitle("Global North Studies")+geom_bar(position="fill")+
  theme_bw()+
  scale_fill_manual(values=c("#00441B","#74C476"), name="Last Author")+
  theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Proportion of Studies")

ggplot(deduplicate_study_GS_divide_df, aes(x=Scope, fill=Last.Author.Global.Divide))+
  ggtitle("Global South Studies")+geom_bar(position="fill")+
  theme_bw()+
  scale_fill_manual(values=c("#00441B","#74C476"), name="Last Author")+
  theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Proportion of Studies")




authorship_data_GN_GS<-authorship_data_wild[is.na(authorship_data_wild$Species.Global.North.or.South)==FALSE,]
authorship_data_GN_GS$Scope<-replace(authorship_data_GN_GS$Scope,authorship_data_GN_GS$Scope=="just genomic data (Level 0)", "Level 1")
authorship_data_GN_GS$Scope<-replace(authorship_data_GN_GS$Scope,authorship_data_GN_GS$Scope=="spatial only (Level 1)", "Level 2")
authorship_data_GN_GS$Scope<-replace(authorship_data_GN_GS$Scope,authorship_data_GN_GS$Scope=="climate functional variation (Level 2a)", "Level 4")
authorship_data_GN_GS$Scope<-replace(authorship_data_GN_GS$Scope,authorship_data_GN_GS$Scope=="other global change functional variation (Level 2b)", "Level 3")
authorship_data_GN_GS$Scope<-replace(authorship_data_GN_GS$Scope,authorship_data_GN_GS$Scope=="adaptive potential (Level 3)", "Level 5")

authorship_data_GN<-authorship_data_GN_GS[authorship_data_GN_GS$Species.Global.North.or.South=="Global North",]
deduplicate_study_GN_reps<-distinct(authorship_data_GN, Study.ID, .keep_all= TRUE)


authorship_data_GS<-authorship_data_GN_GS[authorship_data_GN_GS$Species.Global.North.or.South=="Global South",]
deduplicate_study_GS_reps<-distinct(authorship_data_GN, Study.ID, .keep_all= TRUE)


deduplicate_place_GN_GS<-distinct(authorship_data_GN_GS, Study.ID, Scope, Latin.Binomial, Genomic, .keep_all= TRUE)


chisq.test(deduplicate_place_GN_GS$Scope, deduplicate_place_GN_GS$Species.Global.North.or.South)
deduplicate_place_GN_GS


length(levels(factor(authorship_data_wild_local_author$Species.Global.North.or.South)))

write.csv(authorship_data_wild_local_author, "testing.csv")




global_divide_multiple<-authorship_data_wild_local_author %>% 
  group_by(Study.ID) %>%
  mutate(multiple_products = +(n_distinct(Species.Global.North.or.South) > 1)) %>%
  ungroup()

global_divide_multiple_final<-global_divide_multiple[global_divide_multiple$multiple_products!=1,]

global_divide_multiple_final$prop_local_authors<-global_divide_multiple_final$Number.of.Local.Authors..One.per.study./global_divide_multiple_final$Total.Number.of.Authors
global_divide_multiple_final$percentage_local_authors<-(global_divide_multiple_final$prop_local_authors)*100


authorship_data_GN_divide<-global_divide_multiple_final[global_divide_multiple_final$Species.Global.North.or.South=="Global North",]
authorship_data_GS_divide<-global_divide_multiple_final[global_divide_multiple_final$Species.Global.North.or.South=="Global South",]


deduplicate_study_GN_divide<-distinct(authorship_data_GN_divide, Study.ID, .keep_all= TRUE)
deduplicate_study_GN_divide_df<-deduplicate_study_GN_divide
deduplicate_study_GS_divide<-distinct(authorship_data_GS_divide, Study.ID, .keep_all= TRUE)
deduplicate_study_GS_divide_df<-deduplicate_study_GS_divide


####GN vs GS
####Total Number GN vs GS
#first author GN
num_authors_mean_1st_author_GN_divide
num_authors_mean_1st_author_GN_divide<-deduplicate_study_GN_divide %>%
  group_by(Scope, X1st.Author.Global.Divide) %>%
  summarise(mean = mean(prop_local_authors),stderror = sd(prop_local_authors)/sqrt(n()))
num_authors_mean_1st_author_df_GN_divide<-data.frame(num_authors_mean_1st_author_GN_divide)

num_authors_mean_1st_author_GN_divide

num_authors_mean_GN_divide<-deduplicate_study_GN_divide %>%
  group_by(Scope) %>%
  summarise(mean = mean(prop_local_authors),stderror = sd(prop_local_authors)/sqrt(n()))
num_authors_mean_df_GN_divide<-data.frame(num_authors_mean_GN_divide)

deduplicate_study_GN_divide_percent<-deduplicate_study_GN_divide %>%
  group_by(Scope) %>%
  summarise(mean = mean(percentage_local_authors),stderror = sd(percentage_local_authors)/sqrt(n()))
num_authors_mean_df_GN_divide_percent<-data.frame(deduplicate_study_GN_divide_percent)

deduplicate_study_GN_divide_percent<-deduplicate_study_GN_divide %>%
  group_by(Scope) %>%
  summarise(mean = mean(percentage_local_authors),stderror = sd(percentage_local_authors)/sqrt(n()))
num_authors_mean_df_GN_divide_percent<-data.frame(deduplicate_study_GN_divide_percent)


deduplicate_study_GN_divide$X1st.Author.Global.Divide


deduplicate_study_GS_divide_percent<-deduplicate_study_GS_divide %>%
  group_by(Scope) %>%
  summarise(mean = mean(percentage_local_authors),stderror = sd(percentage_local_authors)/sqrt(n()))
num_authors_mean_df_GS_divide_percent<-data.frame(deduplicate_study_GS_divide_percent)

num_authors_mean_GS_divide<-deduplicate_study_GS_divide %>%
  group_by(Scope) %>%
  summarise(mean = mean(prop_local_authors),stderror = sd(prop_local_authors)/sqrt(n()))
num_authors_mean_df_GS_divide<-data.frame(num_authors_mean_1st_author_GS_divide)



ggplot(num_authors_mean_1st_author_df_GN_divide, aes(fill=X1st.Author.Global.Divide, x=Scope, y=mean))+
  ylim(c(0,1))+
  ggtitle("Global North Studies")+geom_bar(position=position_dodge2(preserve="single"), stat="identity")+geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 1), width = 0.2)+scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+theme_bw()+theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Global North Authors")

#first author GS
num_authors_mean_1st_author_GS_divide<-deduplicate_study_GS_divide %>%
  group_by(Scope, X1st.Author.Global.Divide) %>%
  summarise(mean = mean(prop_local_authors),stderror = sd(prop_local_authors)/sqrt(n()))
num_authors_mean_1st_author_df_GS_divide<-data.frame(num_authors_mean_1st_author_GS_divide)

ggplot(num_authors_mean_1st_author_df_GS_divide, aes(fill=X1st.Author.Global.Divide, x=Scope, y=mean))+
  ylim(c(0,1))+
  ggtitle("Global South Studies")+geom_bar(position=position_dodge2(preserve="single"), stat="identity")+geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 1), width = 0.2)+scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+theme_bw()+theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Global South Authors")

#last author GN
num_authors_mean_last_author_GN_divide
num_authors_mean_last_author_GN_divide<-deduplicate_study_GN_divide %>%
  group_by(Scope, X1st.Author.Global.Divide) %>%
  summarise(mean = mean(prop_local_authors),stderror = sd(prop_local_authors)/sqrt(n()))
num_authors_mean_last_author_df_GN_divide<-data.frame(num_authors_mean_last_author_GN_divide)

ggplot(num_authors_mean_last_author_df_GN_divide, aes(fill=X1st.Author.Global.Divide, x=Scope, y=mean))+
  ylim(c(0,1))+
  ggtitle("Global North Studies")+geom_bar(position=position_dodge2(preserve="single"), stat="identity")+geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 1), width = 0.2)+scale_fill_manual(values=c("#00441B","#74C476"), name="Last Author")+theme_bw()+theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Global North Authors")

#last author GS
num_authors_mean_last_author_GS_divide
num_authors_mean_last_author_GS_divide<-deduplicate_study_GS_divide %>%
  group_by(Scope, X1st.Author.Global.Divide) %>%
  summarise(mean = mean(prop_local_authors),stderror = sd(prop_local_authors)/sqrt(n()))
num_authors_mean_last_author_df_GS_divide<-data.frame(num_authors_mean_last_author_GS_divide)

#Local Authors by Global SOuth and NOrth and Scope (no first last distinciton)
num_authors_mean_GN_divide
ggplot(num_authors_mean_df_GN_divide_percent, aes(x=Scope, y=mean))+
  ylim(c(0,100))+
  ggtitle("Global North Studies")+geom_bar(stat = "identity", fill="#00441B")+
  geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 1), width = 0.2)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Percentage of Local Authors")

ggplot(num_authors_mean_df_GS_divide_percent, aes(x=Scope, y=mean))+
  ylim(c(0,100))+
  ggtitle("Global South Studies")+geom_bar(stat = "identity", fill="#74C476")+
  geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 1), width = 0.2)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Percentage of Local Authors")


ggplot(first_author_counts_df_GN, aes(x=Scope, y=n, fill=X1st.Author.Global.Divide))+
  ggtitle("Global North Studies")+geom_bar(position=position_dodge2(preserve="single"), stat="identity")+
  theme_bw()+
  ylim(c(0,200))+
  scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+
  theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Number of Studies")

ggplot(first_author_counts_df_GS, aes(x=Scope, y=n, fill=X1st.Author.Global.Divide))+
  ggtitle("Global South Studies")+geom_bar(position=position_dodge2(preserve="single"), stat="identity")+
  theme_bw()+
  ylim(c(0,200))+
  scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+
  theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Number of Studies")



deduplicate_study_GN_divide_df$X1st.Author.Global.Divide

deduplicate_study_GN_divide_df_1st<-deduplicate_study_GN_divide_df
boop<-group_by(deduplicate_study_GN_divide_df, Scope, X1st.Author.Global.Divide)
boop
deduplicate_study_GN_divide_df
first_author_counts_GN<-deduplicate_study_GN_divide_df %>% count(Scope, X1st.Author.Global.Divide)
first_author_counts_df_GN<-data.frame(first_author_counts_GN)
first_author_counts_GS<-deduplicate_study_GS_divide_df %>% count(Scope, X1st.Author.Global.Divide)
first_author_counts_df_GS<-data.frame(first_author_counts_GS)
first_author_counts_GN

num_authors_mean_GS_divide
ggplot(num_authors_mean_GS_divide, aes(x=Scope, y=mean))+
  ylim(c(0,1))+
  ggtitle("Global South Studies")+geom_bar(stat = "identity", fill="#74C476")+
  geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 1), width = 0.2)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Global South Authors")

num_authors_mean_GS_divide$Percentage<-num_authors_mean_GS_divide$mean

num_authors_mean_GN_divide_1st_author<-num_authors_mean_GN_divide_df %>%
  group_by(Scope, X1st.Author.Global.Divide)


num_authors_mean_1st_author_df_GN_divide




ggplot(num_authors_mean_last_author_df_GS_divide, aes(fill=X1st.Author.Global.Divide, x=Scope, y=mean))+
  ylim(c(0,1))+
  ggtitle("Global South Studies")+geom_bar(position=position_dodge2(preserve="single"), stat="identity")+geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 1), width = 0.2)+scale_fill_manual(values=c("#00441B","#74C476"), name="Last Author")+theme_bw()+theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Global South Authors")




num_authors_mean_1st_author<-deduplicate_study %>%
  group_by(Scope, X1st.Author.Global.Divide) %>%
  summarise(mean = mean(Number.of.Local.Authors..One.per.study.))
num_authors_mean_1st_author_df<-data.frame(num_authors_mean_1st_author)


ggplot(num_authors_mean_1st_author_df_GN, aes(fill=X1st.Author.Global.Divide, x=Scope, y=mean))+
  ylim(c(0,1))+
  ggtitle("Global North Studies")+geom_bar(position=position_dodge2(preserve="single"), stat="identity")+geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 1), width = 0.2)+scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+theme_bw()+theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Local Authors")


global_divide_multiple

length(unique(global_divide_multiple_final$Study.ID))
length(unique(global_divide_multiple$Study.ID))






#Total Number
#first author
num_authors_mean_1st_author<-deduplicate_study %>%
  group_by(Scope, X1st.Author.Global.Divide) %>%
  summarise(mean = mean(Number.of.Local.Authors..One.per.study.))
num_authors_mean_1st_author_df<-data.frame(num_authors_mean_1st_author)

num_authors_mean_1st_author_scope<-deduplicate_study_scope %>%
  group_by(Scope, X1st.Author.Global.Divide) %>%
  summarise(mean = mean(Number.of.Local.Authors..One.per.study.))
num_authors_mean_1st_author_scope_df<-data.frame(num_authors_mean_1st_author_scope)

#last author
num_authors_mean_last_author<-deduplicate_study %>%
  group_by(Scope, Last.Author.Global.Divide) %>%
  summarise(mean = mean(Number.of.Local.Authors..One.per.study.))
num_authors_mean_last_author_df<-data.frame(num_authors_mean_last_author)

num_authors_mean_last_author_scope<-deduplicate_study_scope %>%
  group_by(Scope, Last.Author.Global.Divide) %>%
  summarise(mean = mean(Number.of.Local.Authors..One.per.study.))
num_authors_mean_last_author_scope_df<-data.frame(num_authors_mean_last_author_scope)

authorship_data_wild_local_author

deduplicate_study_GN<-deduplicate_study[deduplicate_study$Species.Global.North.or.South=="Global North",]
deduplicate_study_GS<-deduplicate_study[deduplicate_study$Species.Global.North.or.South=="Global South",]

deduplicate_study_GN$prop_local_authors<-deduplicate_study_GN$Number.of.Local.Authors..One.per.study./deduplicate_study_GN$Total.Number.of.Authors
deduplicate_study_GS$prop_local_authors<-deduplicate_study_GS$Number.of.Local.Authors..One.per.study./deduplicate_study_GS$Total.Number.of.Authors


deduplicate_study_GN


d %>% 
  group_by(ID) %>%
  mutate(multiple_products = +(n_distinct(product_code) > 1)) %>%
  ungroup()


####GN vs GS
####Total Number GN vs GS
#first author
num_authors_mean_1st_author_GN<-deduplicate_study_GN %>%
  group_by(Scope, X1st.Author.Global.Divide) %>%
  summarise(mean = mean(prop_local_authors),stderror = sd(prop_local_authors)/sqrt(n()))
num_authors_mean_1st_author_df_GN<-data.frame(num_authors_mean_1st_author_GN)

ggplot(num_authors_mean_1st_author_df_GN, aes(fill=X1st.Author.Global.Divide, x=Scope, y=mean))+
  ylim(c(0,1))+
  ggtitle("Global North Studies")+geom_bar(position=position_dodge2(preserve="single"), stat="identity")+geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 1), width = 0.2)+scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+theme_bw()+theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.y=element_line(size=0.5),axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Local Authors")



num_authors_mean_1st_author_GS<-deduplicate_study_GS %>%
  group_by(Scope, X1st.Author.Global.Divide) %>%
  summarise(mean = mean(prop_local_authors),stderror = sd(prop_local_authors)/sqrt(n()))
num_authors_mean_1st_author_df_GS<-data.frame(num_authors_mean_1st_author_GS)

ggplot(num_authors_mean_1st_author_df_GS, aes(fill=X1st.Author.Global.Divide, x=Scope, y=mean))+ggtitle("Global South Studies")+geom_bar(position="dodge", stat="identity")+geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 1), width = 0.2)+geom_segment(x=0.4,y=-10,yend=1.001, size=1)+scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+theme_bw()+theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Local Authors")
GS_first_author


#last author
num_authors_mean_last_author_GN<-deduplicate_study_GN %>%
  group_by(Scope, Last.Author.Global.Divide) %>%
  summarise(mean = mean(prop_local_authors),stderror = sd(prop_local_authors)/sqrt(n()))
num_authors_mean_last_author_df_GN<-data.frame(num_authors_mean_last_author_GN)

GN_last_author<-ggplot(num_authors_mean_last_author_df_GN, aes(fill=Last.Author.Global.Divide, x=Scope, y=mean))+ggtitle("Global North Studies")+geom_bar(position=position_dodge2(preserve="single"), stat="identity")+geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 1), width = 0.2)+geom_segment(x=0.4,y=-10,yend=1.001, size=1)+scale_fill_manual(values=c("#00441B","#74C476"), name="Last Author")+theme_bw()+theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Local Authors")
GN_last_author


num_authors_mean_last_author_GS<-deduplicate_study_GS %>%
  group_by(Scope, Last.Author.Global.Divide) %>%
  summarise(mean = mean(prop_local_authors),stderror = sd(prop_local_authors)/sqrt(n()))
num_authors_mean_last_author_df_GS<-data.frame(num_authors_mean_last_author_GS)


GS_last_author<-ggplot(num_authors_mean_last_author_df_GS, aes(fill=Last.Author.Global.Divide, x=Scope, y=mean))+ggtitle("Global South Studies")+geom_bar(position="dodge", stat="identity")+geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 1), width = 0.2)+geom_segment(x=0.4,y=-10,yend=1.001, size=1)+scale_fill_manual(values=c("#00441B","#74C476"), name="Last Author")+theme_bw()+theme(plot.title = element_text(hjust = 0.5, size=20),panel.border=element_blank(), axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Local Authors")

ggarrange(GN_first_author, GS_first_author, GN_last_author, GS_last_author,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
plot_grid(GN_first_author, GS_first_author, GN_last_author, GN_last_author,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


deduplicate_study_GN

authorship_data<-authorship_data[authorship_data$Passed.Filter.2=="Yes",]


##Prop
#first author
num_authors_mean_1st_author_prop<-deduplicate_study %>%
  group_by(Scope, X1st.Author.Global.Divide) %>%
  summarise(mean = mean(prop_local_authors))
num_authors_mean_1st_author_prop_df<-data.frame(num_authors_mean_1st_author_prop)

num_authors_mean_1st_author_scope_prop<-deduplicate_study_scope %>%
  group_by(Scope, X1st.Author.Global.Divide) %>%
  summarise(mean = mean(prop_local_authors))
num_authors_mean_1st_author_scope_prop_df<-data.frame(num_authors_mean_1st_author_scope_prop)

#last author
num_authors_se_last_author_prop<-deduplicate_study %>%
  group_by(Scope, Last.Author.Global.Divide) %>%
  summarise(se = stderr(prop_local_authors))
num_authors_mean_last_author_prop_df<-data.frame(num_authors_mean_last_author_prop)

std.

num_authors_mean_last_author_prop<-deduplicate_study %>%
  group_by(Scope, Last.Author.Global.Divide) %>%
  summarise(mean = mean(prop_local_authors))



num_authors_mean_last_author_scope_prop<-deduplicate_study_scope %>%
  group_by(Scope, Last.Author.Global.Divide) %>%
  summarise(mean = mean(prop_local_authors))
num_authors_mean_last_author_scope_prop_df<-data.frame(num_authors_mean_last_author_scope_prop)





#PROP
##FINAL
ggplot(num_authors_mean_1st_author_df_GN, aes(fill=X1st.Author.Global.Divide, x=Scope, y=mean)) +
  geom_bar(position="dodge", stat="identity")+geom_errorbar(aes(ymin = mean-stderror, ymax = mean+stderror), position =  position_dodge(width = 0.5), width = 0.2)+geom_segment(x=0.4,y=-10,yend=1.001, size=1)+scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+theme_bw()+theme(panel.border=element_blank(), axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Local Authors")

ggplot(num_authors_mean_last_author_df_GN, aes(fill=Last.Author.Global.Divide, x=Scope, y=mean)) +
  geom_bar(position="dodge", stat="identity")+geom_segment(x=0.4,y=-10,yend=1.001, size=1)+scale_fill_manual(values=c("#00441B","#74C476"), name="Last Author")+theme_bw()+theme(panel.border=element_blank(), axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Local Authors")

ggplot(num_authors_mean_1st_author_df_GS, aes(fill=X1st.Author.Global.Divide, x=Scope, y=mean)) +
  geom_bar(position="dodge", stat="identity")+geom_segment(x=0.4,y=-10,yend=1.001, size=1)+scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+theme_bw()+theme(panel.border=element_blank(), axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Local Authors")

ggplot(num_authors_mean_last_author_df_GS, aes(fill=Last.Author.Global.Divide, x=Scope, y=mean)) +
  geom_bar(position="dodge", stat="identity")+geom_segment(x=0.4,y=-10,yend=1.001, size=1)+scale_fill_manual(values=c("#00441B","#74C476"), name="Last Author")+theme_bw()+theme(panel.border=element_blank(), axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Local Authors")







ggplot(num_authors_mean_1st_author_scope_prop_df, aes(fill=X1st.Author.Global.Divide, x=Scope, y=mean)) +
  geom_bar(position="dodge", stat="identity")+ylim(0,1)+geom_segment(x=0.4,y=-10,yend=1.001, size=1)+scale_fill_manual(values=c("#00441B","#74C476"), name="First Author")+theme_bw()+theme(panel.border=element_blank(), axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Local Authors")

ggplot(num_authors_mean_last_author_scope_prop_df, aes(fill=Last.Author.Global.Divide, x=Scope, y=mean)) +
  geom_bar(position="dodge", stat="identity")+ylim(0,1)+geom_segment(x=0.4,y=-10,yend=1.001, size=1)+scale_fill_manual(values=c("#00441B","#74C476"), name="Last Author")+theme_bw()+theme(panel.border=element_blank(), axis.line.x=element_line(size=0.5),panel.grid = element_blank(), legend.text = element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size=18, margin = margin(t=10)), axis.title.y=element_text(size=18, margin=margin(r=10)),axis.text = element_text(size=14))+ylab("Average Proportion of Local Authors")









save<-authorship_data_wild_local_author %>%
  group_by(Scope, Species.Global.North.or.South) %>%
  summarise(mean = mean(prop_local_authors))

save_2<-authorship_data_wild_local_author %>%
  group_by(Scope, Species.Global.North.or.South) %>%
  summarise(mean = mean(Number.of.Local.Authors))


authorship_data_wild_local_author<-authorship_data_wild_local_author[is.na(authorship_data_wild_local_author$Species.Global.North.or.South)==FALSE,]

deduplicate_study<-distinct(authorship_data_wild_local_author, Study.ID, .keep_all= TRUE)
unique(deduplicate_study)


authorship_data_wild_local_author<-final_datasheet_wild_local_author






author_colors<-viridis::viridis(20)
author_colors<-viridis::mako(20)
author_colors[1]