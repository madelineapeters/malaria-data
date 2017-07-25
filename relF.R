library(seqinr)
library(tidyr)
library(stringr)
library(ape)

#set working directory
setwd("C:/Users/Madeline/Desktop/Mideo lab")
file_wd<-"/FastaFiles/FastaFiles"

#load in dataframe with abundances
csp_forR_withSeqs<- read.csv(paste(getwd(), "csp_forR_withSeqs.csv", sep="/"))

csp.mod<-select(csp_forR_withSeqs, c(3,5,6,8, 10:11, 13:14))

#load in names of .aln files
file.list<-list.files(paste(getwd(), file_wd, sep="/"))

for (x in file.list){
  
  #load in specific .aln file
  dna.test<-read.dna(file = paste(getwd(), file_wd, x, sep="/"), 
                     format="clustal", skip=0, nlines=0, as.character=TRUE, as.matrix=NULL)
  dna.test<-as.data.frame(dna.test)
  
  #get sample label and create label column
  y<-((strsplit(x, split=".", fixed=TRUE)[[1]][2]))
  y<-as.numeric(y)
  lab<-as.data.frame(matrix(nrow=dim(dna.test)[1], ncol=1))
  names(lab)<-"SAMPLE"
  lab[,1]<-y
  
  #set row names as first column
  dna.test<- cbind(names = rownames(dna.test), dna.test)
  
  #convert to dataframe
  dna.test<-as.data.frame(dna.test)
  
  #remove rowname
  rownames(dna.test)<-NULL
  
  #split first column into three separate columns
  dna.test<-separate(dna.test, names, into = paste("X", 1:3, sep = "."))
  names(dna.test)[1:3]<-c("vC","S","relF")
  
  #combine base columns into one sequence column
  dna.test<-unite(dna.test, seq, 4:dim(dna.test)[2], sep="", remove=TRUE)
  
  #convert sequence column to upper case
  dna.test<-data.frame(lapply(dna.test, function(v) {
    if (is.character(v)) return(toupper(v))
    else return(v)
  }))
  
  #remove F from F column
  dna.test<-as.data.frame(sapply(dna.test,gsub,pattern="F", replacement=""))
  
  #get total reads
  sub<-subset.data.frame(csp.mod, SAMPLE==y)
  sum.count<-(sum(sub$cMAPCNT))
  
  #convert relative F abundances into absolute abundances
  dna.test$relF<-as.integer(dna.test$relF)
  dna.test$abF<-sapply(X=dna.test$relF, FUN=function(X) {
      X*sum.count
    })
  
  dna.test<-bind_cols(lab, dna.test)
  
  if (x == file.list[1]) {dna.final<-dna.test} else {dna.final<-bind_rows(dna.final, dna.test)}

}