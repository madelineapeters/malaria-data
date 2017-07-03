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

#select time points to compare
w<-c(4,8) #columns with R1.48 and R2.48
x<-c(2,6) #colums with R1.0 and R2.0
y<-c(2,4) #colums with R1.0 and R1.48
z<-c(6,8) #colums with R2.0 and R2.48

#set-up final output dataframe
mal_fin<-as.data.frame(matrix(nrow=1,ncol=18))
names(mal_fin)<-c("PID", "Hap", "med.R1", "5.R1", "95.R1", "med.R2", "5.R2", "95.R2", "5.T0", "95.T0", "med.T0", "5.T48", "95.48", "med.T48", "sig1","sig2", "sig1.2", "sig2.2")

#####Start loop over patients#####
ID<-ID[1:19]
for (d in ID){

  #subset data frame if necessary
  mal_sub <- subset.data.frame(mal_data, PID == d) %>%
    #remove PID column
    select(-PID)
  
  #parameters
  hap_num<-length(mal_sub[,1]) #number of haplotypes to consider (1:hap_num)
  
  #replace NAs with 0
  mal_sub[is.na(mal_sub)] <- 0
  
  #####Set-up output dataframe#####
  
  boot.df<-as.data.frame(matrix(nrow=hap_num, ncol=17))
  names(boot.df)<-c("Hap", "med.R1", "5.R1", "95.R1", "med.R2", "5.R2", "95.R2", "5.T0", "95.T0", "med.T0", "5.T48", "95.48", "med.T48", "sig1","sig2", "sig1.2", "sig2.2")
  boot.df[1:hap_num,1]<-c(1:hap_num)
  
  #####Set-up bootstrapping function#####
  
  #loop over haplotypes
  for (hap in 1:hap_num) {
    source(file=paste(base_wd, "Mal bootstrap FPP.R", sep="/"))
    source(file=paste(base_wd, "Mal bootstrap med.R", sep="/"))
  } #end loop over haplotypes
  
  #store output in final dataframe
  PID.df<-as.data.frame(matrix(nrow=hap_num,ncol=1))
  PID.df[,1]<-d
  names(PID.df)<-"PID"
  mal_final<-bind_cols(PID.df, boot.df)
  mal_fin<-bind_rows(mal_fin, mal_final)
} #end loop over patients

