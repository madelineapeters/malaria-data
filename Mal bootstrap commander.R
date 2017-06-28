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
ID<-"T04" #patient ID
hap_num<-14 #number of haplotypes to consider (1:hap_num)

#subset data frame if necessary
mal_sub <- subset.data.frame(mal_data, PID == ID) %>%
  #remove PID column
  select(-PID) #%>% 
#remove time points not being considered (columns)
#select(-c(R1.24, R1.48, R2.24, R2.48))
#reorder columns
#mal_sub<-mal_sub[c(1,2,4,3,5)]

#replace NAs with 0
mal_sub[is.na(mal_sub)] <- 0

#####Set-up output dataframe#####

boot.df<-as.data.frame(matrix(nrow=hap_num, ncol=13))
names(boot.df)<-c("Hap", "med.R1", "5.R1", "95.R1", "med.R2", "5.R2", "95.R2", "5.T0", "95.T0", "med.T0", "5.T48", "95.48", "med.T48")

#####Set-up bootstrapping function#####

#select time points to compare
w<-c(4,8) #columns with R1.48 and R2.48
x<-c(2,6) #colums with R1.0 and R2.0
y<-c(2,4) #colums with R1.0 and R1.48
z<-c(6,8) #colums with R2.0 and R2.48

for (hap in 1:hap_num) {
  source(file=paste(base_wd, "Mal bootstrap FPP.R", sep="/"))
  source(file=paste(base_wd, "Mal bootstrap med.R", sep="/"))
}