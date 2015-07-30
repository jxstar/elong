library(quantmod)
library(leaps)
library(Hmisc) #describe
library(psych) #describe
library(GPArotation)
library(pastecs) #stat.desc
library(corrgram) # for corralation analysis
library(gvlma)
library(relaimpo)
library(RSQLite)

library(xlsx)
library(RMySQL)
library(ggplot2) #add for ggplot
library(reshape2)
library(dplyr)



conn <- dbConnect(MySQL(), dbname = "thdata", 
                  username="thdata_user", password="gbgj53GD2s2gy64wRT",host="121.43.197.34",port=3306)
#dbGetQuery(conn,"set names utf8")  #for macos
dbGetQuery(conn,"set names gbk") # for win
city=dbGetQuery(conn,"select * from simple_elong_city")
hotel=dbGetQuery(conn,"select * from simple_elong_hotel")
comment=dbGetQuery(conn,"select hotelid, created, ctype, replyed, date, csource from simple_elong_comment")
dbDisconnect(conn)
save(file="20150728 elong.Rdata",comment, hotel, city)

head(hotel)
str(hotel)
dim(hotel)

head(comment)
class(comment$created) = c('POSIXt','POSIXct')
comment$month=as.integer(format(as.Date(comment$created),"%Y%m"))
head(hotel)
comment=left_join(select(comment,hotelid,month,ctype,csource,replyed),select(hotel,hotelid,star,cmts,cityid,city),by=c("hotelid"="hotelid"))
head(comment)


str(comment)
range(comment$month)
table(comment$month)
names(comment)=c( "hotelid", "month",   "ctype"  , "csource", "replyed" ,"star" ,   "cmts" ,   "cityid" , "cityname" )
monthlist=201501:201507

current=201507
for (m in monthlist){
  
  mcmt=filter(comment, month==m)
  mcmt=group_by(mcmt,cityname, star)
  #table(mcmt$star)
  
  smcmt=summarise(mcmt,n())
  head(smcmt)
  names(smcmt)=c("cityname","star","numcomt")
  smcmt=melt(smcmt,id=c("cityname","star"))
  smcmt=dcast(smcmt,cityname ~ star)
  names(smcmt)=c("cityname","s0","s1","s2","s3","s4","s5")
  smcmt$tot=apply(smcmt[,2:ncol(smcmt)], 1, sum,na.rm=T)
  #str(smcmt)
  smcmt=arrange(smcmt, desc(tot))
  head(smcmt,10)
  write.xlsx(x=smcmt,file="city-star-cmts.xlsx",sheetName = as.character(m), append = T, showNA = F)

}


comment=group_by(comment,cityname, star)
smcmt=summarise(comment,n())
head(smcmt)
names(smcmt)=c("cityname","star","numcomt")
smcmt=melt(smcmt,id=c("cityname","star"))
smcmt=dcast(smcmt,cityname ~ star)
names(smcmt)=c("cityname","s0","s1","s2","s3","s4","s5")
smcmt$tot=apply(smcmt[,2:ncol(smcmt)], 1, sum,na.rm=T)
#str(smcmt)
smcmt=arrange(smcmt, desc(tot))
head(smcmt,10)
write.xlsx(x=smcmt,file="city-star-cmts.xlsx",sheetName = "aggregated", append = T, showNA = F)








comment=ungroup(comment)
comment=arrange(comment,desc(month))
comment=group_by(comment,cityname,month)
#table(mcmt$star)

smcmt=summarise(comment,n())
names(smcmt)=c("cityname","month","numcomt")
head(smcmt)
smcmt=melt(smcmt,id=c("cityname","month"))
smcmt=dcast(smcmt,cityname~month)

ungroup(smcmt)
smcmt=smcmt[,c("cityname",as.character(monthlist))]
smcmt=smcmt[order(-smcmt[,"201507"]),]
head((smcmt))

write.xlsx(x=smcmt,file="city-month-cmts.xlsx", showNA = F)




comment=group_by(comment,month,csource)
smcmt=summarise(comment,n())
names(smcmt)=c("month","csource","numcomt")

head(smcmt)
smcmt=melt(smcmt,id=c("month","csource"))
smcmt=dcast(smcmt,month~csource)
smcmt=ungroup(smcmt)
smcmt=arrange(smcmt,desc(month))
smcmt=smcmt[-which(smcmt$month==202011),]
head(smcmt)

write.xlsx(x=smcmt,file="month-csource-cmts.xlsx", showNA = F)



