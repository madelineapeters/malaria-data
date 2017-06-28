#bootstrap to CI and median for replicate 1
bootobject.1<-replicate(10000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in y){
    #sample replicate
    dif.start<-as.data.frame(sample(c(1:hap_num), size=10000, replace=TRUE, prob=mal_sub[,sample(y, size=1, replace=TRUE)]))
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
boot.df[hap,3]<-boot.lower
boot.upper<-nth(bootobject.1, 9500)
boot.df[hap,4]<-boot.upper

bootobject.2<-replicate(10000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in y){
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
boot.df[hap,2]<-median(bootobject.2)

#bootstrap to CI and median for replicate 2
bootobject.1<-replicate(10000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in z){
    #sample replicate
    dif.start<-as.data.frame(sample(c(1:hap_num), size=10000, replace=TRUE, prob=mal_sub[,sample(z, size=1, replace=TRUE)]))
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
boot.df[hap,5]<-boot.lower
boot.upper<-nth(bootobject.1, 9500)
boot.df[hap,6]<-boot.upper

bootobject.2<-replicate(10000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in z){
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
boot.df[hap,5]<-median(bootobject.2)