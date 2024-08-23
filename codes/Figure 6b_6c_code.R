##Anat's code
##ASE changed ggplot code

library(ggplot2)
library(dplyr)
library(rstatix)
library(viridis)
library(scales)
library(tidyr)
library(ggpubr)
library(svglite)

#read in data
data<- read.csv("Final_postsubmission_Phase_2_Filtering.csv", header=TRUE)


names(data)[names(data) == "X1st.Author.Origin.Country"] <- "First.Author.Origin.Country"
names(data)[names(data) == "X1st.Author.Global.Divide"] <- "First.Author.Global.Divide"

#local collabs and number of studies in GN GS fig

data.wild<-subset(data,data$Wild.or.Captive == "Wild")
data.filtered <- data.wild[!is.na(data.wild$Species.Global.North.or.South), ]

colnames(data.filtered)

#Find and remove studies that have sampling in both GN and GS
#Find studies that have sampling in both regions
filtered_data <- data.filtered %>%
  group_by(Study.ID) %>%
  filter(all(c("Global North", "Global South") %in% Species.Global.North.or.South))

#Count unique Study IDs
unique_study_ids <- unique(filtered_data$Study.ID)
num_unique_study_ids <- length(unique_study_ids)

# Output the number of unique study IDs
num_unique_study_ids

#Remove these studies from the dataset
#First have to make it a dataframe
unique_study_ids_df <- data.frame(Study.ID = unique_study_ids)

#Filter from dataset
data.filtered.no.GNandGS.studies <- anti_join(data.filtered, unique_study_ids_df, by = "Study.ID")

#Deduplicate by Study ID
deduplicated_data <- data.filtered.no.GNandGS.studies %>%
  distinct(Study.ID, .keep_all = TRUE)

deduplicated_data$

#calculate prop local authors
deduplicated_data$Prop.Local.Authors <- deduplicated_data$Number.of.Local.Authors / deduplicated_data$Total.Number.of.Authors

#remove first author from total author counts
#create new columns for # collabs from GN/GS - I set these equal to # authors from GN/GS at first
deduplicated_data$Total.Number.of.Authors.no.first.author<-deduplicated_data$Total.Number.of.Authors-1
deduplicated_data$Number.of.collabs.from.Global.North<-deduplicated_data$Number.of.authors.from.Global.North
deduplicated_data$Number.of.collabs.from.Global.South<-deduplicated_data$Number.of.authors.from.Global.South

#remove single author studies
deduplicated_data_no_single_author<-subset(deduplicated_data, deduplicated_data$Total.Number.of.Authors.no.first.author>0)
nrow(deduplicated_data) #543
nrow(deduplicated_data_no_single_author) #540

#sanity check: make sure this worked, and establish baseline #s
sum(deduplicated_data_no_single_author$Number.of.authors.from.Global.North) #2775
sum(deduplicated_data_no_single_author$Number.of.collabs.from.Global.North) #2775
sum(deduplicated_data_no_single_author$Number.of.authors.from.Global.South) #936
sum(deduplicated_data_no_single_author$Number.of.collabs.from.Global.South) #936

#calculate % local collabs (not including 1st author) from GN vs GS
deduplicated_data_no_single_author$Number.of.Local.Authors.No.First.Author<-deduplicated_data_no_single_author$Number.of.Local.Authors
sum(deduplicated_data_no_single_author$Number.of.Local.Authors.No.First.Author) #2575
sum(deduplicated_data_no_single_author$Number.of.Local.Authors) #2575


#first, subtract first author from the appropriate column (depending on whether they're from GN or GS)
for (i in 1:nrow(deduplicated_data_no_single_author)) {
  if (deduplicated_data_no_single_author$First.Author.Origin.Country[i] == deduplicated_data_no_single_author$Country[i]) {
    deduplicated_data_no_single_author$Number.of.Local.Authors.No.First.Author[i] <- deduplicated_data_no_single_author$Number.of.Local.Authors[i] - 1
  }
}

#sanity check: number of local authors should be slightly less now
sum(deduplicated_data_no_single_author$Number.of.Local.Authors.No.First.Author) #2361
sum(deduplicated_data_no_single_author$Number.of.Local.Authors) #2575

#now calculate Prop local collabs that doesn't include first author
#I made a column for this that matched Prop Local Authors at first
deduplicated_data_no_single_author$Prop.Local.Collabs<-deduplicated_data_no_single_author$Prop.Local.Authors

#sanity check: values should be the same
mean(deduplicated_data_no_single_author$Prop.Local.Authors) #0.7126534
mean(deduplicated_data_no_single_author$Prop.Local.Collabs) #0.7126534

deduplicated_data_no_single_author$Prop.Local.Collabs<-deduplicated_data_no_single_author$Number.of.Local.Authors.No.First.Author/deduplicated_data_no_single_author$Total.Number.of.Authors

#sanity check: values should now be different
mean(deduplicated_data_no_single_author$Prop.Local.Authors) #0.7126534
mean(deduplicated_data_no_single_author$Prop.Local.Collabs) #0.6409292

#calculate mean and SE for error bars

summary_data <- deduplicated_data_no_single_author %>%
  group_by(Species.Global.North.or.South, First.Author.Global.Divide) %>%
  summarise(mean = mean(Prop.Local.Collabs, na.rm = TRUE),
            sd = sd(Prop.Local.Collabs, na.rm = TRUE),
            n = n())

prop_local_collabs_values <- deduplicated_data_no_single_author %>%
  filter(Species.Global.North.or.South == "Global North" & First.Author.Global.Divide == "Global South") %>%
  pull(Prop.Local.Collabs)

# Print the list of Prop.Local.Collabs values
print(prop_local_collabs_values)


summary_data$se <- summary_data$sd / sqrt(summary_data$n)


p1 <- ggplot(summary_data, 
             aes(x = Species.Global.North.or.South, y = mean*100, fill = First.Author.Global.Divide)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = (mean - se)*100, ymax = (mean + se)*100), 
                position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("#01431c", "#74c476")) +
  labs(y = "Average Percentage of Local Coauthors", 
       x = "Region Sampled") +
  theme_bw() +
  theme(panel.border=element_blank(),
        panel.grid = element_blank(),
        axis.line.x=element_line(size=0.9),
        axis.line.y=element_line(size=0.9),
        axis.text = element_text(size = 20),
        axis.title.x = element_text(size=24, margin = margin(t=10)),
        axis.title.y=element_text(size=24, margin=margin(r=10)),
        legend.position = "none")


#count of studies, deduplicated by study ID, 
#studies with sampling in both GN and GS removed
count_data <- deduplicated_data_no_single_author %>%
  group_by(Species.Global.North.or.South, First.Author.Global.Divide) %>%
  summarise(count = n())

length(unique(deduplicated_data_no_single_author$Study.ID))
length(deduplicated_data_no_single_author$Study.ID)

p2 <- ggplot(count_data, aes(x = Species.Global.North.or.South, y = count, fill = First.Author.Global.Divide)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#01431c", "#74c476")) +
  labs(y = "Number of Studies", 
       x = "Region Sampled")+
  theme_bw() +
  theme(panel.border=element_blank(),
        panel.grid = element_blank(),
        axis.line.x=element_line(size=0.9),
        axis.line.y=element_line(size=0.9),
        axis.text = element_text(size = 20),
        axis.title.x = element_text(size=24, margin = margin(t=10)),
        axis.title.y=element_text(size=24, margin=margin(r=10)),
        legend.position="none")

print(p1)
print(p2)


ggsave("Figure 6b_v2.svg", plot=p1, width=8, height=8,dpi=300)
ggsave("Figure 6c_v2.svg", plot=p2, width=8, height=8,dpi=300)




