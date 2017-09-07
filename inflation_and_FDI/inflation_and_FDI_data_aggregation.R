## R code for aggregating economic indicators from the World Bank and OECD with
## oil and metal price data. The data is put into a panel data object to be used
## for panel data modelling.

## Import data from World Bank Indicators
library(countrycode)
library(data.table)
library(reshape2)
library(WDI)
library(plm)
library(foreign)
library(gtools)
## Country codes to use
#oecdcodes <- c("AT","BE","CA","DK","FR","DE","GR","IS","IE","IT","JP","LU","NL","NO","PT","ES","SE","CH","TR","GB","US","FI")
oecdcodes <- c("AU","AT","BE","CA","CL","CZ","DK","EE","FI","FR","DE","GR","HU","IS","IE","IL","IT","JP","KR","LU","MX","NL","NZ","NO","PL","PT","SK","SI","ES","SE","CH","TR","GB","US")


#oecdcodes2 <- c("AT","BE","CA","DK","FR","DE","GR","IS","IE","JP","NL","NO","SE","CH","GB","US","FI")
## Descriptions of World Bank indicators
wbvars <- read.csv(file="/home/projects/Data/World_Bank_Development_Index/WDI_GDF_Series.csv")
## World Bank indicators to use
#wbindicators <- c("FP.CPI.TOTL","FP.CPI.TOTL.ZG","NY.GDP.MKTP.KD.ZG","SP.POP.GROW","BX.KLT.DINV.WD.GD.ZS","BM.KLT.DINV.GD.ZS","NE.EXP.GNFS.ZS","NE.IMP.GNFS.ZS","NE.GDI.TOTL.ZS","NE.GDI.FTOT.CD","NE.GDI.FTOT.KD","NY.GDP.DEFL.KD.ZG","FR.INR.RINR","FR.INR.DPST","FR.INR.LEND","NE.CON.GOVT.ZS","GC.DOD.TOTL.GD.ZS","BM.KLT.DINV.GD.ZS","BN.KLT.DINV.CD","BX.KLT.DINV.CD.DT","BX.KLT.DINV.CD.WD","BX.KLT.DINV.WD.GD.ZS")


## Load World Bank data
#wbdata <- WDI(country=oecdcodes, indicator=wbindicators, start=1960, end=2010, extra=FALSE)
rm(wbdatafull)
#wbdatafull <- read.csv("/home/projects/Data/World_Bank_Development_Index/WDI_GDF_reshaped_data.csv")
load("/home/projects/Data/World_Bank_Development_Index/all_WB_development_data.Rdata") # load wbdatafull
wbdatafullusedcols <- c(1,2,4,487,66,824,825,437,438,439,63,553,576,570,425,830,827,828,516,842,554:561,1206,1215,1237,1238)
rm(wbdata)
wbdata <- wbdatafull[,wbdatafullusedcols] # all data
#wbdata <- wbdatafull[wbdatafull$iso2c %in% oecdcodes,wbdatafullusedcols]

names(wbdata) <- c("country","year","iso2c","exchange_rate","population","cpi","cpi_growth","gdp_2000_usd","gdp_growth","gdp_2000_lcu","pop_growth","exports","imports","gcf","gdp_deflator_wb","real_interest_rate","deposit_rate","lending_rate","gov_consumption","gov_debt","gfcf_priv_cur_lcu","gfcf_priv_perc","gfcf_cur_usd","gfcf_cur_lcu","gfcf_2000_usd","gfcf_growth","gfcf_const_lcu","gfcf_perc","fdi_out","fdi_bop_cur_usd","fdi_in_bop_cur_usd","fdi_in")
wbdata <- wbdata[wbdata$iso2c!="",]
wbdata <- wbdata[!duplicated(wbdata[,c("year","iso2c")]),]

## find correspondence between wbvars and wbdata & wbdatafull
wbvars$wbdatafullrow <- NA
wbvars$wbdatarow <- NA
for (i in 1:(dim(wbvars)[1]))
{
  code <- wbvars[i,"Series.Code"]
  r <- as.integer(rownames(findcols(wbdatafull,paste("\\.\\.",code,".?$",sep=""))))
  wbvars$wbdatafullrow[i] <- r
  w <- which(r == wbdatafullusedcols)
  if(length(w)==1)
    {
      wbvars$wbdatarow[i] <- w
    }
}

descWBcode <- function(codes)
  {
    retval <- character(length(codes))
    i <- 1
    for(code in codes)
      {
        retval[i] <- as.character(wbvars$Long.definition[which(wbvars$Series.Code==code)])
        i <- i + 1
      }
    return(retval)
  }

descwbdatavar <- function(vars)
  {
    retval <- character(length(vars))
    i <- 1
    for(var in vars)
      {
        code <- wbvars$Series.Code[which(wbvars$wbdatarow==which(names(wbdata)==var))]
        retval[i] <- as.character(wbvars$Long.definition[which(wbvars$Series.Code==code)])
        i <- i + 1
      }
    return(retval)
  }

grepWBdefs <- function(regexp)
  {
    cols <- grep(regexp,wbvars$Long.definition,value=F)
    data.frame(col=cols,code=wbvars$Series.Code[cols],description=as.character(wbvars$Long.definition[cols]))
  }

grepWBseriesnames <- function(regexp)
  {
    cols <- grep(regexp,wbvars$Series.Name,value=F)
    data.frame(col=cols,code=wbvars$Series.Code[cols],name=wbvars$Series.Name[cols],description=as.character(wbvars$Long.definition[cols]))
  }


## load OECD economic outlook data, rename columns, and reshape it appropriately
## oecddatafull <- read.csv("/home/projects/Data/OECD/OECD_economic_outlook_variables_yearly_1960-2012.csv")
## oecddatafull <- reshape(oecddatafull,v.names="Value",timevar="Variable",idvar=c("Country","Time"),direction="wide")
## oecddatafull <- oecddatafull[,c(-2,-4)] # remove unwanted columns ("Frequency" and "Flags")
## renamecols(oecddatafull,"^Value\\.","")
## renamecols(oecddatafull,"^Time$","year")
## renamecols(oecddatafull,"^Country$","country")
## oecddatafull$iso2c <- countrycode(oecddatafull$country,"country.name","iso2c") 
rm(oecddata)
load("/home/projects/Data/OECD/OECD_almost_all_economic-outlook_vars.Rdata") # load previously saved OECD data
oecddata <- oecddatafull[oecddatafull$iso2c %in% oecdcodes,c("year","iso2c","country","Private non-residential gross fixed capital formation, value","Private non-residential gross fixed capital formation, volume","Private non-residential fixed capital formation, deflator","Gross government fixed capital formation, value","Government gross fixed capital formation, volume","Government fixed capital formation, deflator","Gross domestic product, deflator, market prices","Consumer price index","Labour productivity of the total economy")]
oecddata <- oecddata[!(oecddata$country %in% c("Euro area with Western Germany (<= 1991)","Former Federal Republic of Germany")),] # remove these rows
names(oecddata) <- c("year","iso2c","country","priv_gfcf_val","priv_gfcf_vol","priv_gfcf_deflator","gov_gfcf_val","gov_gfcf_vol","gov_gfcf_deflator","gdp_deflator_oecd","cpi_oecd","labour_productivity")

## load oil data
oilprices <- read.csv("/home/projects/Data/Metals_Minerals_Oil/BP_crude_oil_prices_yearly_1861-2010.csv")
names(oilprices) <- c("year","oil_2000_usd","oil_cur_usd")
basemetals_2000_usd<- read.csv("/home/projects/Data/Metals_Minerals_Oil/World-Bank_base_metals_constant-2000-USD.csv")
names(basemetals_2000_usd) <- c("year","basemetals_2000_usd")
basemetals_cur_usd <- read.csv("/home/projects/Data/Metals_Minerals_Oil/World-Bank_base_metals_current-USD.csv")
names(basemetals_cur_usd) <- c("year","basemetals_cur_usd")
metals_minerals_2000_usd<- read.csv("/home/projects/Data/Metals_Minerals_Oil/World-Bank_metals_and_minerals_constant-2000-USD.csv")
names(metals_minerals_2000_usd) <- c("year","metals_minerals_2000_usd")
metals_minerals_cur_usd <- read.csv("/home/projects/Data/Metals_Minerals_Oil/World-Bank_metals_and_minerals_current-USD.csv")
names(metals_minerals_cur_usd) <- c("year","metals_minerals_cur_usd")

## Merge data
rm(data)
#data <- merge(oecddata,wbdata,by=c("iso2c","year"))
#data$country.y <- NULL ## remove extra country variable
#names(data)[names(data)=="country.x"] <- "country"
#data <- merge(data,oilprices,by=c("year"))
data <- merge(wbdata,oilprices,by=c("year"))
data <- merge(data,basemetals_2000_usd,by=c("year"))
data <- merge(data,basemetals_cur_usd,by=c("year"))
data <- merge(data,metals_minerals_2000_usd,by=c("year"))
data <- merge(data,metals_minerals_cur_usd,by=c("year"))
data$basemetals_2000_usd <- as.numeric(data$basemetals_2000_usd)
data$basemetals_cur_usd <- as.numeric(data$basemetals_cur_usd)
data$metals_minerals_2000_usd <- as.numeric(data$metals_minerals_2000_usd)
data$metals_minerals_cur_usd <- as.numeric(data$metals_minerals_cur_usd)

## create panel data object, and extra variables
makegrowthvar <- defmacro(var,df,expr={
  diff(df$var)/lag(df$var)
})

rm(paneldata)
paneldata <- pdata.frame(data,index=c("iso2c","year"))
paneldata$year2 <- as.numeric(as.character(paneldata$year))
#paneldata$rel_cost <- log(paneldata$priv_gfcf_deflator/paneldata$gov_gfcf_deflator)
#paneldata$rel_cost2 <- log(paneldata$priv_gfcf_deflator/paneldata$gdp_deflator_wb)
paneldata$nominal_interest_rate <- paneldata$real_interest_rate + paneldata$cpi_growth
paneldata$openness <- paneldata$imports + paneldata$exports
#paneldata$pub_inv_pc <- paneldata$gov_gfcf_vol / paneldata$population
## paneldata$inv_corp_growth <- makegrowthvar(priv_gfcf_vol,paneldata)
## paneldata$inv_pub_growth <- makegrowthvar(gov_gfcf_vol,paneldata)
## paneldata$cpi_oecd_growth <- makegrowthvar(cpi_oecd,paneldata)
## paneldata$fdi_in_growth <- makegrowthvar(fdi_in,paneldata)
## paneldata$fdi_bop_cur_usd_growth <- makegrowthvar(fdi_bop_cur_usd,paneldata)
## paneldata$fdi_in_bop_cur_usd_growth <- makegrowthvar(fdi_in_bop_cur_usd,paneldata)
paneldata$oil_growth <- makegrowthvar(oil_2000_usd,paneldata)
paneldata$metals_minerals_growth <- makegrowthvar(metals_minerals_2000_usd,paneldata)
paneldata$basemetals_growth <- makegrowthvar(basemetals_2000_usd,paneldata)

paneldata$fdi_in_lcu <- paneldata$fdi_in_bop_cur_usd * paneldata$exchange_rate
paneldata$fdi_in_2000_lcu <- paneldata$fdi_in_lcu / paneldata$gdp_deflator_wb

paneldata$fdi_bop_2000_usd <- paneldata$fdi_bop_cur_usd / paneldata$gdp_deflator_wb
paneldata$fdi_in_bop_2000_usd <- paneldata$fdi_in_bop_cur_usd / paneldata$gdp_deflator_wb

paneldata$log_fdi <- log(paneldata$fdi_in_bop_2000_usd)
paneldata$log_cpi_growth <- log(paneldata$cpi_growth)
paneldata$log_gdp <- log(paneldata$gdp_2000_usd)
paneldata$log_interest_rate <- log(paneldata$nominal_interest_rate)
paneldata$log_openness <- log(paneldata$openness)
paneldata$log_exchange_rate <- log(paneldata$exchange_rate)
paneldata$log_gov_consumption <- log(paneldata$gov_consumption)
paneldata$log_exports <- log(paneldata$exports)
paneldata$log_imports <- log(paneldata$imports)

paneldata$Dlog_fdi <- diff(log(paneldata$fdi_in_bop_2000_usd))
paneldata$Dlog_cpi_growth <- diff(log(paneldata$cpi_growth))
paneldata$Dlog_gdp <- diff(log(paneldata$gdp_2000_usd))
paneldata$Dlog_interest_rate <- diff(log(paneldata$nominal_interest_rate))
paneldata$Dlog_openness <- diff(log(paneldata$openness))
paneldata$Dlog_exchange_rate <- diff(log(paneldata$exchange_rate))
paneldata$Dlog_gov_consumption <- diff(log(paneldata$gov_consumption))
paneldata$Dlog_exports <- diff(log(paneldata$exports))
paneldata$Dlog_imports <- diff(log(paneldata$imports))

paneldata$lag1Dlog_fdi <- lag(diff(log(paneldata$fdi_in_bop_2000_usd)),1)
paneldata$lag2Dlog_fdi <- lag(diff(log(paneldata$fdi_in_bop_2000_usd)),2)

fdispecB_DLoglag2DLog <- Dlog_fdi ~ log_cpi_growth + Dlog_gdp + log_interest_rate + Dlog_openness + Dlog_exchange_rate + log_gov_consumption + lag1Dlog_fdi + lag2Dlog_fdi

diff(log_fdi) ~ log_cpi_growth + log_gdp + log_interest_rate + log_openness + log_exchange_rate + log_gov_consumption + lag(diff(log_fdi),1:2) | lag(diff(log_fdi),2:99)

paneldata$cpisqr <- paneldata$cpi_growth^2

paneldata$fdi_in_pc <- paneldata$fdi_in / paneldata$population
paneldata$fdi_in_lcu_pc <- paneldata$fdi_in_lcu / paneldata$population
paneldata$fdi_in_2000_lcu_pc <- paneldata$fdi_in_2000_lcu / paneldata$population

paneldata$gdp_2000_pc <- paneldata$gdp_2000_usd / paneldata$population

## sort variables into alphabetical order
paneldata <- paneldata[,sort(names(paneldata),index.return=T)$ix]

## CLEAN DATA!!!!
paneldata$fdi_bop_2000_usd[7374] <- NA
## attach data to workspace to save some typing
#attach(paneldata)

save.image()
