#select time points to compare
w<-c(4,8) #columns with R1.48 and R2.48
x<-c(2,6) #colums with R1.0 and R2.0
y<-c(2,4) #colums with R1.0 and R1.48
z<-c(6,8) #colums with R2.0 and R2.48

#bootstrap to find false positive rate for time 0
bootobject.1<-replicate(10000, {

  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
    for (i in x){
      #sample replicate
      dif.start<-as.data.frame(sample(c(1:hap_num), size=10000, replace=TRUE, prob=mal_sub[,sample(x, size=1, replace=TRUE)]))
      #edit dataframe
      names(dif.start)<-c("Hap")
      dif.start<-count(dif.start,Hap)
      #join counts with Hap labels
      dif.mid<-full_join(dif.mid, dif.start, by="Hap")
      #replace NAs
      dif.mid[is.na(dif.mid)]<-0
    }
    
    #rename replicate columns
    names(dif.mid)[2:3]<-c("R1","R2")
    
    #select haplotype
    dif.mid<-slice(dif.mid, hap)
    
    #transform to proportions
    dif.mid[,2:3]<-dif.mid[,2:3]/10000
    
    #add difference column
    dif<-dif.mid$R1-dif.mid$R2
  }
)
bootobject.1<-sort(bootobject.1, decreasing=FALSE)
boot.lower<-nth(bootobject.1, 500)
boot.df[hap,8]<-boot.lower
boot.upper<-nth(bootobject.1, 9500)
boot.df[hap,9]<-boot.upper

bootobject.2<-replicate(10000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in x){
    #sample replicate
    dif.start2<-as.data.frame(sample(c(1:hap_num), size=10000, replace=TRUE, prob=mal_sub[,i]))
    #edit dataframe
    names(dif.start2)<-c("Hap")
    dif.start2<-count(dif.start2,Hap)
    #join counts with Hap labels
    dif.mid<-full_join(dif.mid, dif.start2, by="Hap")
    #replace NAs
    dif.mid[is.na(dif.mid)]<-0
  }
  
  #rename replicate columns
  names(dif.mid)[2:3]<-c("R1","R2")
  
  #select haplotype
  dif.mid<-slice(dif.mid, hap)
  
  #transform to proportions
  dif.mid[,2:3]<-dif.mid[,2:3]/10000
  
  #add difference column
  dif<-dif.mid$R1-dif.mid$R2
}
)

boot.lower.p<-length(bootobject.2[bootobject.2 < boot.lower])
boot.upper.p<-length(bootobject.2[bootobject.2 > boot.upper])
boot.df[hap,10]<-(boot.lower.p+boot.upper.p)/10000

#bootstrap to find false positive rate for time 48
bootobject.1<-replicate(10000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in w){
    #sample replicate
    dif.start<-as.data.frame(sample(c(1:hap_num), size=10000, replace=TRUE, prob=mal_sub[,sample(w, size=1, replace=TRUE)]))
    #edit dataframe
    names(dif.start)<-c("Hap")
    dif.start<-count(dif.start,Hap)
    #join counts with Hap labels
    dif.mid<-full_join(dif.mid, dif.start, by="Hap")
    #replace NAs
    dif.mid[is.na(dif.mid)]<-0
  }
  
  #rename replicate columns
  names(dif.mid)[2:3]<-c("R1","R2")
  
  #select haplotype
  dif.mid<-slice(dif.mid, hap)
  
  #transform to proportions
  dif.mid[,2:3]<-dif.mid[,2:3]/10000
  
  #add difference column
  dif<-dif.mid$R1-dif.mid$R2
}
)
bootobject.1<-sort(bootobject.1, decreasing=FALSE)
boot.lower<-nth(bootobject.1, 500)
boot.df[hap,11]<-boot.lower
boot.upper<-nth(bootobject.1, 9500)
boot.df[hap,12]<-boot.upper

bootobject.2<-replicate(10000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in w){
    #sample replicate
    dif.start2<-as.data.frame(sample(c(1:hap_num), size=10000, replace=TRUE, prob=mal_sub[,i]))
    #edit dataframe
    names(dif.start2)<-c("Hap")
    dif.start2<-count(dif.start2,Hap)
    #join counts with Hap labels
    dif.mid<-full_join(dif.mid, dif.start2, by="Hap")
    #replace NAs
    dif.mid[is.na(dif.mid)]<-0
  }
  
  #rename replicate columns
  names(dif.mid)[2:3]<-c("R1","R2")
  
  #select haplotype
  dif.mid<-slice(dif.mid, hap)
  
  #transform to proportions
  dif.mid[,2:3]<-dif.mid[,2:3]/10000
  
  #add difference column
  dif<-dif.mid$R1-dif.mid$R2
}
)

boot.lower.p<-length(bootobject.2[bootobject.2 < boot.lower])
boot.upper.p<-length(bootobject.2[bootobject.2 > boot.upper])
boot.df[hap,13]<-(boot.lower.p+boot.upper.p)/10000