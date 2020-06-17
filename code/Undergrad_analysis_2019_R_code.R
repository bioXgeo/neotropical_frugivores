# ----------------------------------------------
# Zarnetske Lab                       
# Introduction to R for undergraduates
# Nov 2019 Modified and expanded code by Beth Gerstner (parts of code by Nina Lany and Laura Twardochleb)
# This code may be modified by anyone on the project. 
# In the future we will probably split out 2 scripts: 1 for data cleaning, 1 for data analysis & plotting.
# ----------------------------------------------

#Let's use the montane Mammal Traits database as a vehicle to practice data exploration in R.

#clear the workspace
rm(list = ls())

#install some handy packages (you will only need to run this the first time)
#install.packages('ggplot2')
install.packages('tidyverse')

#load the packages (you will need to do this in each R session)
#library('ggplot2')
library(tidyverse)
library(dplyr)
library(ggplot2)

#Read in this file directly 
#You can just download the data to your local computer and read it into R with this code
# For Phoebe: 
dat <- read.csv("/Volumes/GoogleDrive/My Drive/Research/neotropical_frugivores/Database2019/Databases/analysis/undergraduate_analysis/mammal_database_11_19.csv")
# For Beth:
dat <- read.csv("/Volumes/GoogleDrive/My Drive/neotropical_frugivores/Database2019/Databases/analysis/undergraduate_analysis/mammal_database_11_19.csv")

#did it work? This lists the objects in your workspace.
ls()

######################
# BASIC QA/QC CHECKS #
######################
#look at the structure of your data:
str(dat)
#What type of object is it? Use class() to find out.

#Look at the first 6 rows of the database
head(dat)

#How many columns in the data frame? 'ncol' meaning (number of columns) is what we use here. 

#What are the column names? Figure out how to find the names of columns by searching google (this is an important skill!)
names(dat)

#How many rows are in the data frame? 


#To look at specific columns in a dataframe you can use the $ sign. For example, dat$range_size will list all the values in that column. 
#Use this knowledge to answer the questions below

#What Genera have been processed so far? Replace the '...'
unique(dat$...) 

# How many Families? Replace the...
length(unique(dat$...))

#Can you figure out how to get the family names?


#What species are present in the data?


#How many?

# You might want to know how many species belong to what diet category.
#'tapply' applies a function to each group of values
?tapply #access the help page for the function. 
tapply(dat$P_scientific_name, dat$Diet.Cat, length)


#How many species are crepuscular? 
crepuscular <- subset(dat, dat$Activity.Crepuscular==1) #reminder that we have coded crepuscular as '1' in the database
dim(crepuscular)
# 32 rows, 105 columns
nrow(crepuscular)

#How many species are nocturnal?
#Modify the code above here



#Here is another way to view the crepuscular category
tapply(dat$P_scientific_name, dat$Activity.Crepuscular, length)

#Try the code above, but figure out how to quantify the category of range_size.



######################
# PLOTTING 
######################
#Below is some code to look at histograms of certain traits. What's wrong with the first plot? 
hist(dat$Activity.Crepuscular)

hist(dat$Diet.Fruit)
hist(dat$Diet.Inv)
hist(dat$Diet.PlantO)

# If you think the labels look terrible, then you're right. Let's fix this! Ex:
hist(dat$Diet.PlantO, main="Mammal Frugivores: Plant Diet", xlab="Plant Diet Portion")

# Fix the labels for the following three lines of code. You can make this fancy if you want. Look up ?par for options.
hist(dat$Diet.Fruit)
hist(dat$Diet.Inv)
hist(dat$Diet.PlantO)

#Look at the differences in feeding types all at once. This is called subsetting a dataframe. We used [ ] for this. Subsetting is always denoted by [row,column])
feeding <- dat[,9:18]

#look at what this just did
feeding

#Generate a plot of something you're interested in
plot(dat$Diet.Cat, by=dat$scientific_name, col="purple") #you can try and get creative with the colors here

#Try and make a plot of your choosing!





# GGPLOT2 is a great package to make plots. Here we recreate the plot above by using ggplot2
# https://ggplot2.tidyverse.org/reference/
p<-ggplot(dat, aes(Diet.PlantO)) +
  geom_histogram(binwidth = 5)
p

# Note: if we wanted to do a stacked histogram for each diet type, we first need to convert the 
# wide 'dat' into a long version of 'dat' R package reshape or dplyr can do this.
# https://tidyr.tidyverse.org/

#Look at a single species
aotus_v <- dat %>% filter(P_scientific_name == 'Aotus vociferans')
aotus_v_feeding <- aotus_v[,9:18]

#Having a P_ in front of scientific_name is now getting very irritating, why don't we take it out?
colnames(dat)[which(names(dat) == "P_scientific_name")] <- "scientific_name"

#check to see if it worked
names(dat)

# I've decided that I don't want the first few columns so lets remove those (columns 1:3)... 
aotus_v_feeding<- aotus_v_feeding[,-c(1:3)] # remember that it goes rows,columns #this is why the 1:3 is after the comma, because we're saying get rid of COLUMNS 1:3

#Lets format this to go into a barplot, should be a matrix
aotus_v_feeding_mat <-as.matrix(aotus_v_feeding, mode='numeric')

#make a barplot of the species feeding traits
barplot(aotus_v_feeding_mat, main="Diet of Aotus vociferans", ylab="Percent of Diet", xlab="Diet Category", col=c("red"))

#Next we might want to know something about the threat status of species. You can hit the zoom button to see this in more detail.
#What is the dispersion of threat categories?
plot(dat$category, ylab="INSERT PROPER LABEL HERE")

#How about how many species are both small range and endangered?
dat_range_endangered <- filter(dat, range_size == 0 & category == "EN") #how can we easily count the rows here? hint: you used it above.

#How about how many species are both large home range and near threatened?





#How many species have diets of fruit greater than 20 and are vulnerable?
dat_range_v <- filter(dat, Diet.Fruit >= 20 & category == "VU")


#Make this easier to read and get the categories you are interested in 
data_range_v_small <- subset(dat_range_v, select=c(P_scientific_name,Diet.Fruit, category)) #why isn't this working? Look closely. :)

#How many species have a diet of less than 50% fruit with a status of near threatened?




#Bonus question: Why might it be important to understand how many species have small range sizes and are threatened?












