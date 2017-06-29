#set CI based on time comparisons
if (CI > 0.95) {CI <- CI} else if (CI < 0.95) {CI <- 0.95}
CI.2<-0.9

#bootstrap to CI and median for replicate 1
avg.df<-as.data.frame((mal_sub[,2]+mal_sub[,4])/2)
bootobject.1<-replicate(1000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in y){
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
boot.lower<-nth(bootobject.1, (1000*(1-CI)))
boot.df[hap,3]<-boot.lower
boot.upper<-nth(bootobject.1, (1000*CI))
boot.df[hap,4]<-boot.upper

boot.lower.2<-nth(bootobject.1, (1000*(1-CI.2)))
boot.upper.2<-nth(bootobject.1, (1000*CI.2))

bootobject.2<-replicate(1000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in y){
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
  dif<-dif.mid$R1-dif.mid$R2
}
)

#find median difference
boot.df[hap,2]<-median(bootobject.2)

#determine significance based on CI values
if (is.na(boot.lower)) {
  if (is.na(boot.upper)) {boot.df[hap,14] <- "NA"} else if (median(bootobject.2) > boot.upper) {boot.df[hap,14] <- "*"} else {boot.df[hap,14] <- "ns"}
} else if (median(bootobject.2) < boot.lower) {boot.df[hap,14] <- "*"} else {
  if (is.na(boot.upper)) {boot.df[hap,14] <- "NA"} else if (median(bootobject.2) > boot.upper) {boot.df[hap,14] <- "*"} else {boot.df[hap,14] <- "ns"}
}

if (is.na(boot.lower.2)) {
  if (is.na(boot.upper.2)) {boot.df[hap,16] <- "NA"} else if (median(bootobject.2) > boot.upper.2) {boot.df[hap,16] <- "*"} else {boot.df[hap,16] <- "ns"}
} else if (median(bootobject.2) < boot.lower.2) {boot.df[hap,16] <- "*"} else {
  if (is.na(boot.upper.2)) {boot.df[hap,16] <- "NA"} else if (median(bootobject.2) > boot.upper.2) {boot.df[hap,16] <- "*"} else {boot.df[hap,16] <- "ns"}
}

#bootstrap to CI and median for replicate 2
avg.df<-as.data.frame((mal_sub[,6]+mal_sub[,8])/2)
bootobject.1<-replicate(1000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in z){
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
  dif<-dif.mid$R1-dif.mid$R2
}
)
bootobject.1<-sort(bootobject.1, decreasing=FALSE)
boot.lower<-nth(bootobject.1, (1000*(1-CI)))
boot.df[hap,6]<-boot.lower
boot.upper<-nth(bootobject.1, (1000*CI))
boot.df[hap,7]<-boot.upper

boot.lower.2<-nth(bootobject.1, (1000*(1-CI.2)))
boot.upper.2<-nth(bootobject.1, (1000*CI.2))

bootobject.2<-replicate(1000, {
  
  dif.df<-as.data.frame(matrix(nrow=hap_num, ncol=1)) 
  names(dif.df)<-c("Hap")
  dif.df[1:hap_num,]<-c(1:hap_num)
  dif.mid<-dif.df
  
  for (i in z){
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
boot.df[hap,5]<-median(bootobject.2)

if (is.na(boot.lower)) {
  if (is.na(boot.upper)) {boot.df[hap,15] <- "NA"} else if (median(bootobject.2) > boot.upper) {boot.df[hap,15] <- "*"} else {boot.df[hap,15] <- "ns"}
} else if (median(bootobject.2) < boot.lower) {boot.df[hap,15] <- "*"} else {
  if (is.na(boot.upper)) {boot.df[hap,15] <- "NA"} else if (median(bootobject.2) > boot.upper) {boot.df[hap,15] <- "*"} else {boot.df[hap,15] <- "ns"}
}

if (is.na(boot.lower.2)) {
  if (is.na(boot.upper.2)) {boot.df[hap,17] <- "NA"} else if (median(bootobject.2) > boot.upper.2) {boot.df[hap,17] <- "*"} else {boot.df[hap,17] <- "ns"}
} else if (median(bootobject.2) < boot.lower.2) {boot.df[hap,17] <- "*"} else {
  if (is.na(boot.upper.2)) {boot.df[hap,17] <- "NA"} else if (median(bootobject.2) > boot.upper.2) {boot.df[hap,17] <- "*"} else {boot.df[hap,17] <- "ns"}
}
