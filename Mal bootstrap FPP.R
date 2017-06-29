#select time points to compare

#bootstrap to find CI for time 0
bootobject.2<-replicate(1000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in x){
    #sample replicate
    dif.start2<-as.data.frame(sample(c(1:hap_num), size=1000, replace=TRUE, prob=mal_sub[1:hap_num,i]))
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
  dif.mid[,2:3]<-dif.mid[,2:3]/1000
  
  #add difference column
  dif<-dif.mid$R2-dif.mid$R2
}
)

med.T0<-median(bootobject.2)
boot.df[hap,10]<-med.T0

avg.df<-as.data.frame((mal_sub[,2]+mal_sub[,6])/2)
bootobject.1<-replicate(1000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in x){
    #sample replicate
    dif.start<-as.data.frame(sample(c(1:hap_num), size=1000, replace=TRUE, prob=avg.df[,1]))
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
  dif.mid[,2:3]<-dif.mid[,2:3]/1000
  
  #add difference column
  dif<-dif.mid$R2-dif.mid$R1
}
)
bootobject.1<-sort(bootobject.1, decreasing=FALSE)
boot.lower<-(length(bootobject.1[(bootobject.1) < (med.T0)]))
boot.df[hap,8]<-boot.lower
boot.upper<-(length(bootobject.1[(bootobject.1) > (med.T0)]))
boot.df[hap,9]<-boot.upper
boot.CI<-c(boot.lower, boot.upper)
boot.CI<-min(boot.CI)
CI.T0<-1-(boot.CI/1000)


#bootstrap to find CI for time 48
bootobject.2<-replicate(1000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in w){
    #sample replicate
    dif.start2<-as.data.frame(sample(c(1:hap_num), size=1000, replace=TRUE, prob=mal_sub[,i]))
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
  dif.mid[,2:3]<-dif.mid[,2:3]/1000
  
  #add difference column
  dif<-dif.mid$R2-dif.mid$R1
}
)

med.T48<-median(bootobject.2)
boot.df[hap,13]<-med.T48

avg.df<-as.data.frame((mal_sub[,4]+mal_sub[,8])/2)
bootobject.1<-replicate(1000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in w){
    #sample replicate
    dif.start<-as.data.frame(sample(c(1:hap_num), size=1000, replace=TRUE, prob=avg.df[,1]))
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
  dif.mid[,2:3]<-dif.mid[,2:3]/1000
  
  #add difference column
  dif<-dif.mid$R2-dif.mid$R1
}
)
bootobject.1<-sort(bootobject.1, decreasing=FALSE)
boot.lower<-(length(bootobject.1[(bootobject.1) < (med.T48)]))
boot.df[hap,11]<-boot.lower
boot.upper<-(length(bootobject.1[(bootobject.1) > (med.T48)]))
boot.df[hap,12]<-boot.upper
boot.CI<-c(boot.lower, boot.upper)
boot.CI<-min(boot.CI)
CI.T48<-1-(boot.CI/1000)

CI<-max(CI.T0, CI.T48)