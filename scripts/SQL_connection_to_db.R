library(RMySQL)


# 2. Settings
db_user <- 'SEO_read_only'
db_password <- 'SEOreadonly7776163'
db_name <- 'L0jORUA00kJnjKwh'
#db_table <- 'your_data_table'
db_host <- '35.225.47.45' # for local access
db_port <- 3306


# 3. Read data from db
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password, dbname=db_name,
                   host = db_host, port = db_port)

# Display structure of tables
# str(tables)


# summary(mydb)
# dbGetInfo(mydb)
# dbListResults(mydb)
# dbDisconnect(mydb)

# List the tables available in this database.
tbls<-dbListTables(mydb)
print(tbls)

#subset of tables of interest
tbls<-dbListTables(mydb)[c(14,15,16,19,20,21)]
print(tbls)

# Query the "actor" tables to get all the rows.

# returns the number of records
tbl<-"PermitList"
sql <- paste("select count(*) from", tbl)
result <- dbGetQuery(mydb, sql)  
print(result)

for (tbl in tbls) {

# returns the whole table with SendQuery + fetch
result <- dbSendQuery(mydb, paste("select * from", tbl))
data.frame <- fetch(result, n = 10) # Store the result in a R data frame object. n is used to fetch first n rows.
print(data.frame)
write.csv(data.frame,paste0("./data/",tbl,".csv"))
dbClearResult(result)
}

# returns the whole table with GetQuery
result <- dbGetQuery(mydb, paste("select * from", tbl))  
# better functionality with small #s of records, but not as safe if you don't know how big the table is

# return column names in a table
result <- dbGetQuery(mydb, paste0("select * from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME='", tbl,"'"))
print(result$COLUMN_NAME)

# returns specific columns from the selected table
result <- dbGetQuery(mydb, paste("select WR_Number from", tbl))  


dbDisconnect(mydb)
rm(list=ls())

write.csv(data.frame, "mycsv.csv")
unique(data.frame$Units)
