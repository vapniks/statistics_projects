## Code to scrape USA state-by-state, yearly abortion and crime rate data from websites,
## and merge this with unemployment and poverty rate data.

library(rvest)
library(magrittr)
library(pbapply)
library(rowcolmacros)
library(mergeutils)
library(XLConnect)
library(doBy)
library(gdata)
library(data.table)
library(zoo)

## Get the URLs for the abortion data
statecodes <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
abortionURLs <- paste0("http://www.johnstonsarchive.net/policy/abortion/usa/ab-usa-",statecodes,".html")
## Download the webpages
abortionWebpages <- pblapply(abortionURLs,read_html)
## Extract the data into dataframes
abortionData <- lapply(abortionWebpages,
                       function(page) {
                           nodes <- html_nodes(page,"table")
                           table <- html_table(nodes,fill=TRUE)[[1]]
                           names(table) <- c("year","live_births","abortCDC_total","abortCDC_res_instate","abortCDC_res_inout",
                                             "abortAGI_total","abortAGI_res_instate","abortAGI_res_inout","fetal_deaths",
                                             "abortRatio_instate","abortRatio_inout","abortPC_instate","abortPC_inout",
                                             "abortRate_res")
                           rows <- nrow(table)
                           table <- table[-c(1,rows-1,rows),]
                           return(table)
                       })
## name the dataframes
names(abortionData) <- statecodes
## clean up the dataframes
abortionData <- lapply(abortionData,
                       function(df) {
                           df[df==" "] <- NA
                           df2 <- lapply(df,
                                         function(col) {
                                             newcol <- gsub(",|\\(|\\)","",col)
                                             as.numeric(newcol)
                                         })
                           names(df2) <- names(df)
                           return(as.data.frame(df2))
                       })
## bind all the abortion dataframes together
for(i in 1:length(abortionData)) {
    code <- statecodes[i]
    if(code=="DC") {
        abortionData[[i]]$state <- "Washington D.C."
    } else {
        abortionData[[i]]$state <- state.name[grep(statecodes[i], state.abb)]
    }
}
allAbortiondata <- do.call(rbind,abortionData)
names(allAbortiondata) <- tolower(names(allAbortiondata))
allAbortiondata <- unfactor(allAbortiondata)

## Get the URLs for the crime data
years <- c(1960:1989,1991:2013)
crimeURLs <- paste0("http://www.disastercenter.com/crime/",years,"%20Rate%20and%20Rank%20of%20Crime%20and%20Imprisonment%20by%20US%20States.html")
## download the webpages
crimeWebpages <- pblapply(crimeURLs,read_html)
## extract the data into dataframes
crimeData <- lapply(crimeWebpages,
                    function(page) {
                        nodes <- html_nodes(page,"tr:nth-child(2) tr small")
                        vals <- html_text(nodes,trim=TRUE)
                        if(length(vals)>800) {
                            cols <- 15
                        } else {
                            cols <- 12
                        }
                        df <- data.frame(matrix(vals,ncol=cols,byrow=TRUE),stringsAsFactors=FALSE)
                        names(df) <- df[1,]
                        return(df[-which(df[,1]=="State"),])
                    })
## name the dataframes
names(crimeData) <- years
## clean up the dataframes
crimeData <- lapply(crimeData,
                    function(df) {
                        df2 <- lapply(df[,-1],
                                      function(col) {
                                          newcol <- gsub(",|\\(|\\)","",col)
                                          as.numeric(newcol)
                                      })
                        df2 <- cbind(df[,1],as.data.frame(df2))
                        names(df2) <- names(df)
                        return(as.data.frame(df2,stringsAsFactors=FALSE)[,1:12])})
## bind all the crime dataframes together
for(i in 1:length(crimeData)) {
    crimeData[[i]]$year <- as.numeric(years[i])
}
allCrimedata <- do.call(rbind,crimeData)
names(allCrimedata) <- tolower(names(allCrimedata))
allCrimedata <- unfactor(allCrimedata)
allCrimedata$state <- sub("\n"," ",allCrimedata$state)

## Read poverty rate data
povertyxls <- loadWorkbook("poverty_rate.xls")
startrows <- c(6,58,162+52*c(0:(2012-1981+1)))
endrows <- startrows+50
years <- c(2014:1980)
## need to read each years data separately since there is no year column (it is written in the line between each group)
povertydata <- data.frame()
for(i in 1:length(years)) {
    srow <- startrows[i]
    erow <- endrows[i]
    year <- years[i]
    ws <- readWorksheet(povertyxls,"hstpov21",startRow=srow,startCol=1,endRow=erow,endCol=6,header=FALSE)
    names(ws) <- c("state","population","povnumber","povnumber.stderr","povpercent","povpercent.stderr")
    ws$year <- year
    povertydata <- rbind(povertydata,ws)
}
## recode state names to be compatible with other data
povertydata$state <- recodeVar(povertydata$state,src=list(c("DC","D.C.")),tgt=list("Washington D.C."))

## Read unemployment data
unemploymentxls <- loadWorkbook("unemployment_rate.xls")
unemploymentws <- readWorksheet(unemploymentxls,"States",startRow=6,startCol=2,endRow=58,endCol=37)
## need to reshape it from wide to long format
unempdata <- reshape(unemploymentws,direction="long",timevar="year",v.names="unemp",sep="",varying=2:36,times=c(1980:2014))
## rename variables, remove unecessary id var, and recode state names to be compatible with other data
unempdata$id <- NULL
names(unempdata) <- c("state","year","unemp")
unempdata$state <- recodeVar(unempdata$state,src=list(c("District of Columbia")),tgt=list("Washington D.C."))


## Merge all the data together
alldata <- multimerge(list(allAbortiondata,allCrimedata,unempdata,povertydata),by=list(c("state","year")))

## Save data in .csv file
write.csv(alldata,file="crime_abortion_unemp_poverty_data.csv",na="",row.names=FALSE)

alldata <- read.csv("crime_abortion_unemp_poverty_data.csv")

## crime rates by age
agexls <- loadWorkbook("crime_by_age_stats.xlsx")
assaultws <- readWorksheet(agexls,"Sheet1",startRow=9,startCol=2,endRow=29,endCol=11,header=FALSE)
violentws <- readWorksheet(agexls,"Sheet1",startRow=37,startCol=2,endRow=57,endCol=11,header=FALSE)
murderws <- readWorksheet(agexls,"Sheet1",startRow=64,startCol=2,endRow=84,endCol=11,header=FALSE)
rapews <- readWorksheet(agexls,"Sheet1",startRow=91,startCol=2,endRow=111,endCol=11,header=FALSE)
robberyws <- readWorksheet(agexls,"Sheet1",startRow=118,startCol=2,endRow=138,endCol=11,header=FALSE)
propertyws <- readWorksheet(agexls,"Sheet1",startRow=145,startCol=2,endRow=165,endCol=11,header=FALSE)
burglaryws <- readWorksheet(agexls,"Sheet1",startRow=172,startCol=2,endRow=192,endCol=11,header=FALSE)
larcenyws <- readWorksheet(agexls,"Sheet1",startRow=199,startCol=2,endRow=219,endCol=11,header=FALSE)
autows <- readWorksheet(agexls,"Sheet1",startRow=226,startCol=2,endRow=246,endCol=11,header=FALSE)

names(autows) <- names(larcenyws) <- names(burglaryws) <- names(propertyws) <- names(robberyws) <- names(rapews) <- names(murderws) <- names(violentws) <- names(assaultws) <- c("agegroup","yr1993","yr1994","yr1995","yr1996","yr1997","yr1998","yr1999","yr2000","yr2001")
## add variable to indicate crime type
assaultws$crime <- "assault"
violentws$crime <- "violent"
murderws$crime <- "murder"
rapews$crime <- "rape"
robberyws$crime <- "robbery"
propertyws$crime <- "property"
burglaryws$crime <- "burglary"
larcenyws$crime <- "larceny"
autows$crime <- "auto"
## for each of the previous dataframes repeat rows for age groups covering multiple ages
newrows <- c(1,rep(2,2),3:12,rep(13,5),rep(14,5),rep(15,5),rep(16,5),17:21)
newagegrps <- c("0-12",13:44,"45-49","50-54","55-59","60-64","65+")
assaultws <- assaultws[newrows,]
assaultws$agegroup <- newagegrps
violentws <- violentws[newrows,]
violentws$agegroup <- newagegrps
murderws <- murderws[newrows,]
murderws$agegroup <- newagegrps
rapews <- rapews[newrows,]
rapews$agegroup <- newagegrps
robberyws <- robberyws[newrows,]
robberyws$agegroup <- newagegrps
propertyws <- propertyws[newrows,]
propertyws$agegroup <- newagegrps
burglaryws <- burglaryws[newrows,]
burglaryws$agegroup <- newagegrps
larcenyws <- larcenyws[newrows,]
larcenyws$agegroup <- newagegrps
autows <- autows[newrows,]
autows$agegroup <- newagegrps
## put it all into one dataframe
agedata <- rbind(assaultws,violentws,murderws,rapews,robberyws,propertyws,burglaryws,larcenyws,autows)
## convert to a datatable
setDT(agedata)
## create total and proportion variables
agedata[,total:=yr1993+yr1994+yr1995+yr1996+yr1997+yr1998+yr1999+yr2000+yr2001]
agedata[,prop:=total/sum(total),by=crime]
## remove unnecessary whitespace
agedata[,agegroup:=trim(agegroup)]

## Create weighted abortion rate variables
alldata2 <- alldata
setDT(alldata2,key=c("year","state"))
years <- 2000:2015
for(s in unique(alldata2$state)) {
    for(cr in unique(agedata$crime)) {
        weightedaborts1 <- rep(0,length(years))
        weightedaborts2 <- rep(0,length(years))        
        for(l in 15:40) {
            weight <- agedata[crime==cr & agegroup==as.character(l),prop]
            lagyears <- years-l
            ##newvals <- alldata2[(year %in% lagyears) & (state==s),abortratio_inout]*weight
            abortratios1 <- alldata2[(year %in% lagyears) & (state==s),abortratio_inout]
            ##abortratios2 <- alldata2[(year %in% lagyears) & (state==s),abortagi_inout]
            abortratios1 <- na.fill(abortratios1,rep("extend",3))
            ##abortratios2 <- na.fill(abortratios2,rep("extend",3))            
            weightedaborts1 <- weightedaborts1 + abortratios1*weight
            ##weightedaborts2 <- weightedaborts2 + abortratios2*weight
        }
        varname1 <- paste0("abortw1_",cr)
        ##varname2 <- paste0("abortw2_",cr)
        alldata2[year %in% years & state==s,varname1:=weightedaborts1,with=FALSE]
        ##alldata2[year %in% years & state==s,varname2:=weightedaborts2,with=FALSE]        
        names(alldata2)[grep("^abortw1$",names(alldata2))] <- varname1
        ##names(alldata2)[grep("^abortw2$",names(alldata2))] <- varname2
    }
}

## remove "united states" rows
alldata2 <- alldata2[alldata2$state!="united states",]

## save new data in different file
write.csv(alldata2,file="crime_abortion_unemp_poverty_data2.csv",na="",row.names=FALSE)

## read police employment data
policedata <- read.csv("US_law-enforcement_employees_1995-2014.csv")
policedata$state <- tolower(policedata$state)
## fix state names to match those in alldata2
policedata[policedata$state=="district of columbia" | policedata$state=="district of  columbia","state"] <- "washington d.c."
policedata[policedata$state=="west virginia1","state"] <- "west virginia"
policedata[policedata$state=="carolina","state"] <- "south carolina"
## alldata2 <- read.csv("crime_abortion_unemp_poverty_data2.csv")
## need to make state names lowercase to make it easier to match them with police data
alldata2$state <- tolower(alldata2$state)

## check that state names match
all(unique(policedata$state) %in% unique(alldata2$state))
all(unique(alldata2$state) %in% unique(policedata$state))

## merge police data with alldata2
alldata3 <- merge(alldata2,policedata,by=c("state","year"),all.x=TRUE)

names(alldata3)[names(alldata3)=="total.employees"] <- "numpolice"

write.csv(alldata3,file="crime_abortion_unemp_poverty_data3.csv",row.names=FALSE,na="")
