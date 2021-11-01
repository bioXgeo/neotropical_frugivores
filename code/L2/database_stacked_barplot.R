#Title: Database stacked barplot

#Project: Montane Frugivoria

#Overviwe: To analyze trait composition of the database. This script creates a stacked barplot of each database component. 

#Data Input: % makeup of trait database

#Data Output: percent_comp.pdf

#Requires:  The % composition numbers that were calculated as the last step of script "Frugivoria_montane_analysis_demo.R".

#Date: Oct 11th, 2021


## Mammal % makeup
#pantheria %
#60.7

#elton %
#18.76

#newly added traits%
#20.53

#birds
#elton
#45.89

#newly added traits
#54.11

#start pdf
pdf("percent_comp.pdf") 

#create parameters for the stacked barplot
category<- c("Mammals","Mammals","Mammals","Birds","Birds","Birds")
condition<-c("PanTHERIA","EltonTraits","New Traits in Frugivoria","PanTHERIA","EltonTraits","New Traits in Frugivoria")
percent <- c(60.7,18.76,20.53,0,45.89,54.11) #% composition
data <- data.frame(category,condition,percent) #dataframe of composition

# Stacked barplot + percent
plot <-ggplot(data, aes(fill=condition, y=percent, x=Category)) + 
  geom_bar(position="stack", stat="identity") + geom_text(aes(label = percent),size = 3, hjust = 0.6, vjust = 3, position =  "stack")  + scale_fill_manual(values=c("#999999", "#99CC00", "#488A99"), name="Data Source",breaks=c("EltonTraits","New Traits in Frugivoria", "PanTHERIA"), labels=c("EltonTraits","New Traits in Frugivoria","PanTHERIA"))+ labs(x="Taxa")  + labs(y="Percent contribution") + theme(plot.background = element_rect(fill = "white"))

plot + theme(panel.background = element_rect(fill = "white"), axis.line = element_line(colour = "dark gray", size = .5, linetype = "solid"))

#Insert path to where you want the figure saved
setwd("INSERT PATH")
dev.off()
