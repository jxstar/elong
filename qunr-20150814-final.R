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
  city=dbGetQuery(conn,"select * from simple_qunar_city a")
  hotel=dbGetQuery(conn,"select * from simple_qunar_hotel")
  comment=dbGetQuery(conn,"select hotelid, created, csource, city from simple_qunar_new_comment")

  #csres=dbSendQuery(conn," select city, star, count(*) from (select c.city, star from simple_qunar_new_comment c left join simple_qunar_hotel h on c.hotelid=h.code) as ch group by city, star")
  
  csres=dbSendQuery(conn," select city, star, count(*) from
                    (select c.city, star from simple_qunar_new_comment c left join 
                                              (select * from simple_qunar_hotel group by code ) h  on c.hotelid=h.code) as ch 
                    group by city, star")
  csgroup=dbFetch(csres,-1)
  dbClearResult(csres)
  if (F)  {
    res=dbSendQuery(conn,"select hotelid, created, csource, c.city, star
                  from simple_qunar_new_comment c left join simple_qunar_hotel h on c.hotelid=h.code")
    comment=dbFetch(res,-1)
    dbClearResult(res)
  }
  dbDisconnect(conn)
  
  
  hotel=select(hotel, city=city, hotelid=code, star)
  hotel=transform(hotel, hotelid=as.integer(hotelid),city=factor(city), star=factor(star))
  head(hotel)
  str(hotel)
  dim(hotel)
  gc()
  
  class(comment$created) = c('POSIXt','POSIXct')
  comment$month=as.integer(format(as.Date(comment$created),"%Y%m"))
  comment=select(comment, -created)
  comment$csource=factor(comment$csource)
  comment$city=factor(comment$city)
  comment$hotelid=as.integer(comment$hotelid)
  
  head(comment)
  str(comment)
  gc()  

  
  save(file="20150814 qunar.Rdata",comment, hotel, city,csgroup)
}

load(file="20150814 qunar.Rdata")

range(comment$month)
table(comment$month)
monthlist=201501:201508
current=201508

#comment=left_join(comment,select(hotel,hotelid,star),by=c("hotelid"="hotelid"))
#comment=left_join(select(comment,hotelid,month,ctype,csource),select(hotel,code,star,city_code,city),by=c("hotelid"="code"))

#m=201501
for (m in monthlist){
  
  mcmt=select(filter(comment, month==m),hotelid, city)
  mcmt=left_join(select(mcmt,hotelid,city),distinct(select(hotel,hotelid,star),hotelid),by=c("hotelid"="hotelid"))
  mcmt=group_by(mcmt,city, star)
  #table(mcmt$star)
  
  smcmt=summarise(mcmt,n())
  names(smcmt)=c("city","star","numcomt")
  smcmt=melt(smcmt,id=c("city","star"))
  smcmt=dcast(smcmt,city ~ star)
  head(smcmt)
  #names(smcmt)=c("city","s0","s1","s2","s3","s4","s5")
  smcmt$tot=apply(smcmt[,2:ncol(smcmt)], 1, sum,na.rm=T)
  #str(smcmt)
  smcmt=arrange(smcmt, desc(tot))
  head(smcmt,10)
  write.xlsx(x=smcmt,file="qunr city-star-cmts.xlsx",sheetName = as.character(m), append = T, showNA = F)

}

gc()

if (F){
  conn <- dbConnect(MySQL(), dbname = "thdata", 
                    username="root", password="123456",host="101.200.189.155",port=3306)
  
  #dbGetQuery(conn,"set names utf8")  #for macos
  dbGetQuery(conn,"set names gbk") # for win
  csres=dbSendQuery(conn," select city, star, count(*) from (select c.city, star from simple_qunar_new_comment c left join simple_qunar_hotel h on c.hotelid=h.code) as ch group by city, star")
  csgroup=dbFetch(csres,-1)
  dbClearResult(csres)
  dbDisconnect(conn)
}
head(csgroup)
smcmt=csgroup
names(smcmt)=c("city","star","numcomt")
smcmt=melt(smcmt,id=c("city","star"))
smcmt=dcast(smcmt,city ~ star)
smcmt$tot=apply(smcmt[,2:ncol(smcmt)], 1, sum,na.rm=T)
#str(smcmt)
smcmt=arrange(smcmt, desc(tot))
head(smcmt,10)
write.xlsx(x=smcmt,file="qunr city-star-cmts.xlsx",sheetName = "aggregated", append = T, showNA = F)








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

write.xlsx(x=smcmt,file="qunr-city-month-cmts.xlsx", showNA = F)





comment=group_by(comment,month,csource)
smcmt=summarise(comment,n())
names(smcmt)=c("month","csource","numcomt")

head(smcmt)
smcmt=melt(smcmt,id=c("month","csource"))
smcmt=dcast(smcmt,month~csource)
smcmt=ungroup(smcmt)
smcmt=arrange(smcmt,desc(month))
head(smcmt)
write.xlsx(x=smcmt,file="qunr-month-csource-cmts.xlsx", showNA = F)


head(hotel)
names(hotel)=c("city", "hotelid", "star")
hotel=group_by(hotel,city, star)
smcmt=summarise(hotel,n())
head(smcmt)
names(smcmt)=c("city","star","numcomt")
smcmt=melt(smcmt,id=c("city","star"))
smcmt=dcast(smcmt,city ~ star)
smcmt$tot=apply(smcmt[,2:ncol(smcmt)], 1, sum,na.rm=T)
smcmt=arrange(smcmt, desc(tot))
head(smcmt,10)
write.xlsx(x=smcmt,file="qunr-city-star-nhotels.xlsx",sheetName = "aggregated", append = T, showNA = F)

