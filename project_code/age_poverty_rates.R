list.of.packages <- c("readr","data.table","plyr","Hmisc","WDI","varhandle", "ggplot2","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd="E:/DHSauto"
setwd(wd)
dir <-"E:/DHSauto/"

povcalcuts <- read.csv("C:/Users/Zach/Documents/Poverty data/P20incometrends20180505.csv",as.is=TRUE)
dhsmeta<- read.csv("C:/Users/Zach/Documents/P20 Analysis 2018/dhs_meta_data20180501.csv", as.is=TRUE)
dhsmeta<- subset(dhsmeta, Recode.Structure.!="DHS-I")

dhsmeta<- data.table(dhsmeta)[,.SD[which.max(EndYear),],by=c("Country.")]

dhsmeta$Country.[which(dhsmeta$Country.=="Cape Verde")]<-"Cabo Verde"
dhsmeta$Country.[which(dhsmeta$Country.=="Congo")]<-"Congo, Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Congo Democratic Republic")]<-"Congo, Democratic Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Egypt")]<-"Egypt, Arab Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Gambia")]<-"Gambia, The"
dhsmeta$Country.[which(dhsmeta$Country.=="Yemen")]<-"Yemen, Republic of"
#Afghanistan, Cambodia, Equatorial Guinea and Eritrea have had DHS surveys but don't have PovcalNet data
names(dhsmeta)[which(names(dhsmeta)=="Country.")]="CountryName"

dhsmeta$filename=paste0(dhsmeta$dhs_cc,"HR",dhsmeta$dhs_recode_code,"DT")
dhsmeta=dhsmeta[which(!is.na(dhsmeta$dhs_cc)),]
dhsmeta$RequestYear=2013
dhsmeta=dhsmeta[,c("CountryName","RequestYear","filename")]

povcalcuts <- join(dhsmeta,povcalcuts,by=c("CountryName","RequestYear"))

rdatas <- list.files(path=dir,pattern="*.RData",ignore.case=T,recursive=T,full.names=TRUE)
rdatas.split=strsplit(rdatas,"/")
rdatafolders=sapply(rdatas.split,`[`,index=4)
wealth=subset(rdatafolders,substr(rdatafolders,3,4)=="WI")

subrdatas=rdatas[which(rdatafolders %in% povcalcuts$filename)]
subrdats=rdatas[which(rdatafolders %in% wealth)]

weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}


rdatas <- list.files(path=dir,pattern="*.RData",ignore.case=T,recursive=T,full.names=TRUE)
rdatas.split=strsplit(rdatas,"/")
rdatafolders=sapply(rdatas.split,`[`,index=4)
wealth=subset(rdatafolders,substr(rdatafolders,3,4)=="WI")

subrdatas=rdatas[which(rdatafolders %in% povcalcuts$filename)]
subrdats=rdatas[which(rdatafolders %in% wealth)]

weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}

subrdatas=Filter(function(x)!any(grepl("IAHR72",x)),subrdatas)

data.list = list()
for(subrdata in subrdatas){
  povcal_filename=strsplit(subrdata, "/")[[1]][4]
  message(povcal_filename)
  load(subrdata)
  hr=data
  rm(data)
  gc()
  #Rename sample.weights var
  names(hr)[which(names(hr)=="hv005")] <- "sample.weights"
  hr$weights <- hr$sample.weights/1000000
  names(hr)[which(names(hr)=="hv271")] <- "wealth"
  names(hr)[which(names(hr)=="wlthindf")] <- "wealth"
  if(sum(is.na(hr$wealth))==nrow(hr) | is.null(hr$wealth)){
    message("No wealth score")
    no.wealth=T
    hr$wealth=NA
    
  }else{
    hr$wealth=hr$wealth/100000
    no.wealth=F
  }
  
  #Rename urban var
  names(hr)[which(names(hr)=="hv025")] <- "urban.rural"
  hr$urban <- NA
  hr$urban[which(hr$urban.rural==1)] <- 1
  hr$urban[which(hr$urban.rural==2)] <- 0
  #Rename cluster/hh var
  names(hr)[which(names(hr)=="hv001")] <- "cluster"
  names(hr)[which(names(hr)=="hv002")] <- "household"
  
  names(hr)[which(names(hr)=="hvidx")] <- "line"
  #povcalcuts
  povcalcut <- subset(povcalcuts,filename==povcal_filename)$P20Headcount/100
  extcut <- subset(povcalcuts,filename==povcal_filename)$ExtPovHC/100
  cuts <- c(povcalcut,extcut,.2)
  if(no.wealth){
    hr$p20=NA
    hr$ext=NA
    hr$np20=NA
  }else{
    povperc <- weighted.percentile(hr$wealth,hr$weights,prob=cuts)
    hr$p20 <- (hr$wealth < povperc[1])
    hr$ext <- (hr$wealth < povperc[2])
    hr$np20<- (hr$wealth < povperc[3])
  }
  
  
  
  #Person recode
  subrdat=gsub("HR","PR",subrdata)
  subrdatpr=gsub("hr","pr",subrdat)
  load(subrdatpr)
  
  pr=data
  names(attributes(pr)$label.table)= toupper(names(attributes(pr)$label.table))
  if(povcal_filename %in% c("AOHR51DT","IAHR52DT","GNHR52DT","NIHR51DT","PKHR52DT","ZMHR42DT") ){
    reg.lab = attributes(pr)$label.table[attributes(pr)$val.labels[which(names(pr)=="hv024")]][[1]]
    region.df=data.frame(regname=names(reg.lab),region=reg.lab)
  }else{
    region.df=data.frame(regname=names(attributes(pr)$label.table$HV024),region=attributes(pr)$label.table$HV024)
  }
  rm(data)
  gc()
  #weights and wealth
  wi=hr[,c("hhid","p20","ext","np20","wealth","sample.weights")]
  pr=join(pr,wi,by="hhid")
  names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
  pr$weights <- pr$sample.weights/1000000
  # names(pr)[which(names(pr)=="hv271")] <- "wealth"
  # names(pr)[which(names(pr)=="wlthindf")] <- "wealth"
  # pr$wealth <- pr$wealth/100000
  #Rename age var
  names(pr)[which(names(pr)=="hv025")] <- "urban.rural"
  pr$urban <- NA
  pr$urban[which(pr$urban.rural==1)] <- 1
  pr$urban[which(pr$urban.rural==2)] <- 0
  #Rename cluster/hh var
  names(pr)[which(names(pr)=="hv001")] <- "cluster"
  names(pr)[which(names(pr)=="hv002")] <- "household"
  
  names(pr)[which(names(pr)=="hvidx")] <- "line"
  
  
  names(pr)[which(names(pr)=="hv105")] <- "age"
  
  
  
  #Region
  if(povcal_filename!="NPHR7HDT"){
    names(pr)[which(names(pr)=="hv024")] <- "region"
  }else{
    names(pr)[which(names(pr)=="shdevreg")] <- "region" 
  }
  if(sum(is.na(pr$p20))==length(pr$p20)){
    pr$p20=0
    pr$ext=0
    message("No PovcalNet data")
  }
  age=data.table(pr)[,.(
    P20HC=weighted.mean(p20, weights, na.rm=TRUE)
    ,ExtremeHC=weighted.mean(ext, weights, na.rm=TRUE)
    ,NP20HC=weighted.mean(np20, weights, na.rm=TRUE)
    ,weights=sum(weights, na.rm=T)
  ),by=c("age")]

  
  
  age$filename=povcal_filename
  data.list[[povcal_filename]] = age

}
dat=rbindlist(data.list)
ind=read.csv("C:/Users/Zach/Documents/P20 Analysis 2018/INDagesexregion20180625.csv")
ind=data.table(ind)[,.(
  P20HC=weighted.mean(P20HC, weights, na.rm=TRUE)
  ,ExtremeHC=weighted.mean(ExtremeHC, weights, na.rm=TRUE)
  ,NP20HC=weighted.mean(NP20HC, weights, na.rm=TRUE)
  ,weights=sum(weights, na.rm=T)
),by=c("age")]
ind$filename="IAHR72DT"
dat=rbind(dat,ind)
dat=join(dat,dhsmeta,by="filename")

weights=data.table(dat)[,.(
  ntlweights=sum(weights)
),by=c("filename")]

pop=WDI(country="all",indicator="SP.POP.TOTL",extra=T,start=2013,end=2013)
pop$country[which(pop$country=="Congo, Dem. Rep.")]="Congo, Democratic Republic of" 
pop$country[which(pop$country=="Congo, Rep.")]="Congo, Republic of"
pop$country[which(pop$country=="Egypt, Arab Rep.")]="Egypt, Arab Republic of" 
pop$country[which(pop$country=="Yemen, Rep.")]="Yemen, Republic of" 
pop=pop[,c("country","SP.POP.TOTL","year")]
names(pop)=c("CountryName","total.population","RequestYear")
dat=merge(dat,pop,by=c("CountryName","RequestYear"))

dat=join(dat,weights,by=c("filename"))
dat$cellpop=(dat$weights/dat$ntlweights)*dat$total.population
dat=dat[which(dat$CountryName!=c("Afghanistan","Cambodia")),]

codeAgeCat <- function(x){
  startAge <- 0
  ageDiff <- 4
  endAge <- 4
  if(is.na(x)){
    return("missing")
  }
  while(startAge<95){
    endAge <- startAge+ageDiff
    if(x>=startAge & x<=endAge){
      return(
        paste0(startAge,"-",endAge)  
      )
    }
    startAge <- endAge + 1
  }
  if(x>=95){
    return("95+")
  }
  return("missing")
}

dat$ageCategory <- vapply(dat$age,codeAgeCat,character(1))
dat$ageCategory <- factor(dat$ageCategory,
                                 levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34"
                                            ,"35-39","40-44","45-49","50-54","55-59","60-64"
                                            ,"65-69","70-74","75-79","80-84","85-89","90-94"
                                            ,"95+","missing")                          
)


#AgeLNB
agelnb=data.table(dat)[,.(
  # P20HC=weighted.mean(P20HC, cellpop, na.rm=T)
  # ,NP20HC=weighted.mean(NP20HC, cellpop, na.rm=T)
  ExtremeHC=weighted.mean(ExtremeHC, cellpop, na.rm=T)
  # ,cellpop=sum(cellpop, na.rm=T)
),by=c("ageCategory")
]



setwd("E:/git/disaggregation/")
write.csv(agelnb,"project_data/global_age_distributions.csv",row.names=F, na="")

