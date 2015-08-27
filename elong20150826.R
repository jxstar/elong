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


if (F){
  #conn <- dbConnect(MySQL(), dbname = "thdata", 
  #                  username="thdata_stats", password="sg40kssrlER30kGSw2rgrw",host="data.thdata.com",port=3308)
  conn <- dbConnect(MySQL(), dbname = "thdata", 
                    username="root", password="123456",host="101.200.189.155",port=3306)
  #dbGetQuery(conn,"set names utf8")  #for macos
  dbGetQuery(conn,"set names gbk") # for win
  #city=dbGetQuery(conn,"select * from simple_elong_city")
  hotel=dbGetQuery(conn,"select hotelid, star, city from simple_elong_hotel")
  comment=dbGetQuery(conn,"select hotelid, city, created, csource from simple_elong_comment")
  #comment=dbGetQuery(conn,"select hotelid, city, created, csource from simple_elong_comment LIMIT 10")
  #comment=dbGetQuery(conn,"select * from simple_elong_comment LIMIT 10")
  
  csres=dbSendQuery(conn," select city, star, count(*) from
                    (select c.city, star from simple_elong_comment c left join 
                                              (select * from simple_elong_hotel group by hotelid) h  on c.hotelid=h.hotelid) as ch 
                    group by city, star")
  csgroup=dbFetch(csres,-1)
  dbClearResult(csres)
  dbDisconnect(conn)
  
  
  #hotel=select(hotel, city=city, hotelid=code, star)
  hotel=transform(hotel, hotelid=as.integer(hotelid),city=factor(city), star=factor(star))
  
  head(comment)
  class(comment$created) = c('POSIXt','POSIXct')
  comment$month=as.integer(format(as.Date(comment$created),"%Y%m"))
  comment=select(comment, -created)
  comment$csource=factor(comment$csource)
  comment$city=factor(comment$city)
  comment$hotelid=as.integer(comment$hotelid)
  head(comment)
  str(comment)
  gc()  
  names(csgroup)=c("city","star","numcomt")
  
  save(file="20150826 elong.Rdata",comment, hotel, city, csgroup)
}

load(file="20150826 elong.Rdata")




range(comment$month)
table(comment$month)
#names(comment)=c( "hotelid", "month",   "ctype"  , "csource", "replyed" ,"star" ,   "cmts" ,   "cityid" , "city" )
monthlist=201501:201508

current=201508

for (m in monthlist){
  
  mcmt=filter(comment, month==m)
  mcmt=left_join(select(mcmt,hotelid,city),distinct(select(hotel,hotelid,star),hotelid),by=c("hotelid"="hotelid"))
  mcmt=group_by(mcmt,city, star)
  #table(mcmt$star)
  
  smcmt=summarise(mcmt,n())
  head(smcmt)
  names(smcmt)=c("city","star","numcomt")
  smcmt=melt(smcmt,id=c("city","star"))
  smcmt=dcast(smcmt,city ~ star)
  #names(smcmt)=c("city","s0","s1","s2","s3","s4","s5")
  smcmt$tot=apply(smcmt[,2:ncol(smcmt)], 1, sum,na.rm=T)
  #str(smcmt)
  smcmt=arrange(smcmt, desc(tot))
  head(smcmt,10)
  write.xlsx(x=smcmt,file=paste(Sys.Date(),"elong","city-star-cmts.xlsx"),sheetName = as.character(m), append = T, showNA = F)

}


head(csgroup)
smcmt=csgroup
#names(smcmt)=c("city","star","numcomt")
smcmt=melt(smcmt,id=c("city","star"))
smcmt=dcast(smcmt,city ~ star)
smcmt$tot=apply(smcmt[,2:ncol(smcmt)], 1, sum,na.rm=T)
#str(smcmt)
smcmt=arrange(smcmt, desc(tot))
head(smcmt,10)
write.xlsx(x=smcmt,file=paste(Sys.Date(),"elong","city-star-cmts.xlsx"),sheetName = "aggregated", append = T, showNA = F)






head(comment)
comment=ungroup(comment)
comment=arrange(comment,desc(month))
comment=group_by(comment,city,month)
#table(mcmt$star)

smcmt=summarise(comment,n())
names(smcmt)=c("city","month","numcomt")
head(smcmt)
smcmt=melt(smcmt,id=c("city","month"))
smcmt=dcast(smcmt,city~month)

ungroup(smcmt)
smcmt=smcmt[,c("city",as.character(monthlist))]
smcmt=smcmt[order(-smcmt[,"201508"]),]
head((smcmt))

write.xlsx(x=smcmt,file=paste(Sys.Date(),"elong","city-month-cmts.xlsx"), showNA = F)








comment=group_by(comment,month,csource)
smcmt=summarise(comment,n())
names(smcmt)=c("month","csource","numcomt")

head(smcmt)
smcmt=melt(smcmt,id=c("month","csource"))
smcmt=dcast(smcmt,month~csource)
smcmt=ungroup(smcmt)
smcmt=arrange(smcmt,desc(month))
#smcmt=smcmt[-which(smcmt$month==202011),]
head(smcmt)
write.xlsx(x=smcmt,file=paste(Sys.Date(),"elong","month-csource-cmts.xlsx"), showNA = F)


head(hotel)
hotel=group_by(hotel,city, star)
smcmt=summarise(hotel,n())
head(smcmt)
names(smcmt)=c("city","star","numcomt")
smcmt=melt(smcmt,id=c("city","star"))
smcmt=dcast(smcmt,city ~ star)
#names(smcmt)=c("city","s0","s1","s2","s3","s4","s5")
smcmt$tot=apply(smcmt[,2:ncol(smcmt)], 1, sum,na.rm=T)
#str(smcmt)
smcmt=arrange(smcmt, desc(tot))
head(smcmt,10)
write.xlsx(x=smcmt,file=paste(Sys.Date(),"elong","city-star-nhotels.xlsx"),sheetName = "aggregated", append = T, showNA = F)

