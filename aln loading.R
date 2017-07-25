library(seqinr)
library(tidyr)
library(stringr)
library(ape)

#set working directory
setwd("C:/Users/Madeline/Desktop/Mideo lab")
file_wd<-"/FastaFiles/FastaFiles"

#load in dataframe with abundances
csp_forR_withSeqs<- read.csv(paste(getwd(), "csp_forR_withSeqs.csv", sep="/"))

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
  names(dna.test)[1:3]<-c("vC","S","F")
  
  #combine base columns into one sequence column
  dna.test<-unite(dna.test, seq, 4:dim(dna.test)[2], sep="", remove=TRUE)
  
  #convert sequence column to upper case
  dna.test<-data.frame(lapply(dna.test, function(v) {
    if (is.character(v)) return(toupper(v))
    else return(v)
  }))
  
  #remove F from F column
  dna.test<-as.data.frame(sapply(dna.test,gsub,pattern="F", replacement=""))
  
  #combine rows of the same consensus type, adding F values together
  c.list<-unique(dna.test$vC)
  c.list<-c.list[1]
  for(r in 1:length(unique(dna.test$vC))){
    c<-levels(c.list)[r]
    sub<-subset.data.frame(dna.test, vC==c)
    sub$F<-as.character(sub$F)
    dna.test$vC<-as.character(dna.test$vC)
    Fsum<-sum(as.numeric(sub$F))
    if (!is.na(Fsum)){
      sub$F<-Fsum
      sub$F<-as.integer(sub$F)
      dna.test<-subset.data.frame(dna.test, vC!=c)
      dna.test$F<-as.integer(dna.test$F)
      dna.test<-bind_rows(sub, dna.test)
    }
  }

  #join dna.test df with sample label column
  dna.test<-bind_cols(lab, dna.test)
  
  #subset .aln df by sample number
  dna.sub<-subset.data.frame(csp_forR_withSeqs, SAMPLE==y)
  names(dna.sub)[15]<-"seq"
  
  dna.join<-left_join(dna.sub, dna.test, by="seq")
  
  if (x == file.list[1]) {dna.final<-dna.join} else {dna.final<-bind_rows(dna.final, dna.join)}

} #end loop over .aln files

dna.final<-select(dna.final, -(25:dim(dna.final)[2]))
