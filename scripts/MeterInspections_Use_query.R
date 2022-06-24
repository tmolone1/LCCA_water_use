library(RMySQL)
library(dplyr)
library(lubridate)
library(ggplot2)
### Query the database to return information on meter inspections and permits
# 2. Settings


# 3. Read data from db
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password, dbname=db_name,
                   host = db_host, port = db_port)
# List the tables available in this database.
tbls<-dbListTables(mydb)
print(tbls)

#subset of tables of interest
tbls<-dbListTables(mydb)[c(14,15,16,19,20,21)]
print(tbls)

permitlist_info <- dbGetQuery(mydb, paste("select id, WR_Number, Latitude, Longitude from PermitList where ControlArea = 'Laramie County Control Area'"))  
PermitBeneficialUses_HAS_PermitList <- dbGetQuery(mydb, paste("select * from PermitBeneficialUses_HAS_PermitList"))  
PermitBeneficialUses <- dbGetQuery(mydb, paste("select * from PermitBeneficialUses"))  
MeterInspections <- dbGetQuery(mydb, paste("select * from MeterInspections"))  
MeterInformation_Has_PermitList <- dbGetQuery(mydb, paste("select * from MeterInformation_Has_PermitList"))  
meterinformation <- dbGetQuery(mydb, paste("select id, Units, Fixed_Zeros, Multiplier, NumberOfDials from MeterInformation"))  
MeterInformation <- dbGetQuery(mydb, paste("select * from MeterInformation"))

dbDisconnect(mydb)
rm(list=ls(pattern="db"), tbls)
# end database query

#merge BU Code into permitlist_info table
BUs<-merge(PermitBeneficialUses_HAS_PermitList,PermitBeneficialUses,by=1)[,2:3]
permitlist_info <- merge(permitlist_info,BUs, by=1, all.x=TRUE)
nrow(permitlist_info %>% filter(is.na(BUCode)))  # returns 141 records of ~2400, not sure why these dont have a BUCode

# MeterInspections data wrangling
colnames(meterinformation)[1]<-"MeterInformation_fk"
meterinformation$NumberOfDials<-as.numeric(meterinformation$NumberOfDials)
MeterInspections$Inspection_Date<-lubridate::as_date(MeterInspections$Inspection_Date)
meterinformation$Fixed_Zeros <- meterinformation$Fixed_Zeros %>% replace(is.na(.), 0)

mutation <- MeterInspections %>% 
  group_by(MeterInformation_fk) %>% 
  arrange(Inspection_Date) %>% select(MeterInformation_fk, Permit_Ref,Inspection_Date,Meter_Reading) %>% 
  left_join(meterinformation,by='MeterInformation_fk') %>%  
  mutate(Prev_insp_date= c(NA,Inspection_Date)[1:length(Inspection_Date)], Prev_meter_read=c(NA,Meter_Reading)[1:length(Meter_Reading)])

mutation$Prev_insp_date<-lubridate::as_date(mutation$Prev_insp_date)

# case where current read > previous read, normal operation
mutation1 <- mutation %>% filter(Meter_Reading>=Prev_meter_read) %>% mutate(Volume_Used = (Meter_Reading-Prev_meter_read)*Multiplier*(10^Fixed_Zeros), Use_Period=(Inspection_Date-Prev_insp_date))

# case where current read < previous read, rollovers 
mutation2 <- mutation %>% filter(Meter_Reading<Prev_meter_read) %>% mutate(Volume_Used = ((10^NumberOfDials)-Prev_meter_read+Meter_Reading)*Multiplier*(10^Fixed_Zeros), Use_Period=(Inspection_Date-Prev_insp_date))

#rejoin
mutation <-rbind(mutation1,mutation2) %>% arrange(MeterInformation_fk)
rm(mutation1,mutation2)

# join MeterInspection Data to Permit List data based on MeterInformation_Has_PermitList
mutation <- mutation %>% inner_join(MeterInformation_Has_PermitList, by="MeterInformation_fk")
colnames(permitlist_info)[1]<-"PermitList_fk"
mutation <- mutation %>% inner_join(permitlist_info, by="PermitList_fk")

#inspect results
mutation <- mutation %>% group_by(MeterInformation_fk) %>% mutate(count_permits=length(unique(PermitList_fk)))
mutation<- mutation %>% arrange(MeterInformation_fk)
#View(mutation)

#unit conversion
conversion_factors<-read.csv("./data/conversion_factors.csv")[,2:3]
mutation<-merge(mutation, conversion_factors, by.x = "Units", by.y = 'unit')
mutation<-mutation %>% mutate(converted_use_rate_AFD=(Volume_Used/cf/as.numeric(Use_Period)))
mutation<-mutation %>% filter(Prev_insp_date>"2019-01-01",Use_Period>200 & Use_Period< 500)

summary(mutation$converted_use_rate_AFD)
summary(mutation$Use_Period)


ggplot(mutation, aes(x=Use_Period, y=converted_use_rate_AFD, shape=BUCode, color=BUCode)) +
  geom_point()


high_use<-mutation %>% filter(converted_use_rate_AFD>quantile(mutation$converted_use_rate_AFD,.95,na.rm=TRUE))
write.csv(high_use,"./outputs/high_use.csv")
write.csv(mutation,"./outputs/results.csv")

# 



# #remove extra columns and keys
# mutation <- mutation %>% select(Inspection_Date, Meter_Reading, Units, Prev_insp_date, Prev_meter_read, Volume_Used, Use_Period, WR_Number, Latitude, Longitude, BUCode)
# 
# 

# 
# 
# MeterInformation %>% filter(id %in% c("5D4EWdYYNszw","_NzaHLu-XCaa")) %>% View()
