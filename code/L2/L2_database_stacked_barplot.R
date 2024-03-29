#Title: Database stacked barplot

#Project: Frugivoria

#Overview: To analyze trait composition of the database. This script creates a stacked barplot of each database component. 

#Data Input: % makeup of trait database

#Data Output: percent_comp.pdf

#Requires:  The % composition numbers that were calculated as the last step of script "L1_Frugivoria_analysis_demo.R".

#Date: #Date: March 3rd, 2023


#start pdf
pdf("percent_comp.pdf") 

#create parameters for the stacked barplot
category<- c("Mammals","Mammals","Mammals","Birds","Birds","Birds")
condition<-c("PanTHERIA","EltonTraits","New Traits in Frugivoria","PanTHERIA","EltonTraits","New Traits in Frugivoria")
  percent <- c(49.8,14.7,35.36,0,27.62,72.38) #% composition
data <- data.frame(category,condition,percent) #dataframe of composition

# Stacked barplot + percent
plot <-ggplot(data, aes(fill=condition, y=percent, x=category)) + 
  geom_bar(position="stack", stat="identity") + geom_text(aes(label = percent),size = 3, hjust = 0.6, vjust = 3, position =  "stack")  + scale_fill_manual(values=c("#999999", "#99CC00", "#488A99"), name="Data Source",breaks=c("EltonTraits","New Traits in Frugivoria", "PanTHERIA"), labels=c("EltonTraits","New Traits in Frugivoria","PanTHERIA"))+ labs(x="Taxa")  + labs(y="Percent contribution") + theme(plot.background = element_rect(fill = "white"))

plot + theme(panel.background = element_rect(fill = "white"), axis.line = element_line(colour = "dark gray", size = .5, linetype = "solid"))

#Insert path to where you want the figure saved
setwd("INSERT PATH")
dev.off()