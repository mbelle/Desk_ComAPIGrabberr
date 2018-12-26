
##############################################
## Desk.com API data fetch
## mbellettiere 11/2018
############################################


##install packages

##install.packages(c("httr", "jsonlite", "lubridate", "ggplot2"))

##############################
# 0 - Load librairies
##############################

{
library(httr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(dplyr)
}



readuserid <- function()
{ 
  n <- readline(prompt="Enter your desk.com account email: ")
  return(as.character(n))
}

readuserpw <- function()
{ 
  n <- readline(prompt="Enter your desk.com account password: ")
  return(as.character(n))
}


getreportdata<-function()
{
  
########################
#set up configs
########################
  
baseURL<-".desk.com"
accountURL<-"nprsupport"
## start date of 1/1/2018 1514782800 change to update amount of history

pathURL<-"/api/v2/cases/search?assigned_group=DS+-+Support&since_created_at=1514782800&embed=assigned_group,assigned_user"

user<-readuserid()
pw<-readuserpw()

fullURL <- paste("https://",accountURL,baseURL,pathURL, sep="")
options(stringsAsFactors = FALSE)

df_total = data.frame()

qnext = 1
while (!is.null(qnext)) {

##Auth and get cases
r<-(GET(fullURL,authenticate(user,pw)))
ifelse(r$status_code==200,"Success!", r$status_code)


r2<-rawToChar(r$content)
##content object from JSON request
r3<-fromJSON(r2)
## get counter variables for page and total entries
qPage=r3$page
qTotalEntries=r3$total_entries
qnext=r3$`_links`$'next'$href


## extract fields and create data frame in loop to get all the data
df<-as.data.frame( cbind(r3$`_embedded`$entries$id,
         r3$`_embedded`$entries$created_at,
         r3$`_embedded`$entries$resolved_at,
         r3$`_embedded`$entries$changed_at,
         r3$`_embedded`$entries$status,
         r3$`_embedded`$entries$subject,
         paste(r3$`_embedded`$entries$custom_fields$ds_request_type,r3$`_embedded`$entries$custom_fields$ds_request_type2),
         r3$`_embedded`$entries$custom_fields$station,
         r3$`_embedded`$entries$custom_fields$ds_product,
         as.numeric( r3$`_embedded`$entries$`_links`$replies$count),
         r3$`_embedded`$entries$`_embedded`$assigned_user$name,
         r3$`_embedded`$entries$`_embedded`$assigned_group$name))

df_total <- rbind(df_total,df)

##set URL for next query
fullURL <- paste("https://",accountURL,baseURL,qnext, sep="")
}


colnames(df_total)<-c("id","created","resolved_at","Changed_at","status","subject","Type","station","product","Replies","Agent","Team")
##Remove NA from Combined Type 
df_total$Type<-gsub("NA ","",df_total$Type)
df_total$Type<-gsub(" NA","",df_total$Type)

##Export data to file in current Directory
##enhancement setwd to new directory setwd("~/Desk.com API Reports")
write.csv(df_total, file = "DeskData.csv", row.names=FALSE)

n<-(paste("saved data to DeskData.csv for",qTotalEntries," cases" ))
return(as.character(n))
}



print(getreportdata())
