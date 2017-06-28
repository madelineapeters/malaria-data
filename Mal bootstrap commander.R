#####Load packages and data#####
#load in required packages
library(tidyr)
library(dplyr)
library(boot)

#set working directory
base_wd<-"~"
setwd(base_wd)

#read in data
mal_data <- read.csv(paste(getwd(), "Mal_data.csv", sep="/"))

#####Set-up input dataframe#####

#parameters
ID<-"T01" #patient ID
hap_num<-14 #number of haplotypes to consider (1:hap_num)

#subset data frame if necessary
mal_sub <- subset.data.frame(mal_data, PID == ID) %>%
  #remove PID column
  select(-PID) #%>% 
#remove time points not being considered (columns)
#select(-c(R1.24, R1.48, R2.24, R2.48))
#reorder columns
#mal_sub<-mal_sub[c(1,2,4,3,5)]

#####Set-up output dataframe#####

boot.df<-as.data.frame(matrix(nrow=hap_num, ncol=7))
names(boot.df)<-c("Hap", "med.R1", "med.R2", "5.T0", "95.T0", "5.T48", "95.48")

#####Set-up bootstrapping function#####

for (hap in 1:hap_num) {