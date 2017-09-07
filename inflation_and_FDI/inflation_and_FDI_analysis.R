library(data.table)
library(plm)
library(formula.tools)
library(mvoutlier)

## ANALYSIS

## FUNCTION FOR PERFORMING PANEL DATA ANALYSIS
paneldataanalysis <- function(spec,name,pdata,pindex="")
{
  funcs <- c(rep("plm",7),rep("pvcm",2))
  models <- c("pooling",rep("within",3),"fd",rep("random",2),"within","random")
  effects <- c(rep("individual",2),"time","twoways",rep("individual",2),"time",rep("individual",2))
  index <- ",index=pindex"
  if(any(class(pdata)=="pdata.frame"))
    {
      pindex <- names(attr(pdata,"index"))
      pdata <- as.data.frame(pdata)
    }
  ## helpful macro
  pstar <- defmacro(p,expr={
    if(p < 0.001) "***" else if(p < 0.01) "**" else if(p < 0.05) "*" else if(p < 0.1) "." else ""
  })
  ## build models and save them  
  results <- list()
  totalnuminds <- length(rhs.vars(spec)) + 1
  results$comparison <- data.frame(row.names=c("Intercept",rhs.vars(spec),"F-test p-value","R-squared","Adj R-squared"),stringsAsFactors=F)
  for(i in 1:length(funcs))
    {
      modelname <- paste(funcs[i],models[i],effects[i],sep="_")
      commandstring <- paste(funcs[i],"(spec,data=pdata",index,",model=\"",models[i],"\",effect=\"",effects[i],"\")",sep="")
      er1 <- "try-error" %in% class(try(evalstr(modelname," <- ",commandstring)))
      if(!er1)
        {
          er2 <- "try-error" %in% class(try(evalstr("sumry <- summary(",modelname,")")))
          if(!er2)
            {
              coeffvals <- as.character(round(sumry$coefficients[,1],3))
              coeffnames <- rownames(sumry$coefficients)
              coeffpvals <- sumry$coefficients[,4]
              numinds <- length(coeffnames)
              evalstr("results$comparison$",modelname,"[1:(totalnuminds+3)] <- \"-\"")
              for (j in 1:numinds)
                {
                  if(coeffnames[j] %in% c("(intercept)","(Intercept)"))
                    row <- 1
                  else
                    row <- which(rhs.vars(spec) %in% coeffnames[j])[1] + 1
                  results$comparison[row,i] <- paste(coeffvals[j],pstar(coeffpvals[j]),sep="")
                }
              results$comparison[totalnuminds+1,i] <- round(sumry$fstatistic$p.value,3)
              results$comparison[totalnuminds+2,i] <- round(sumry$r.squared[1],3)
              results$comparison[totalnuminds+3,i] <- round(sumry$r.squared[2],3)
              names(results$comparison)[i] <- modelname
              evalstr("results$",modelname," <- ",modelname)
            }
        }
      else print(paste("Couldn't estimate",modelname,"model!"))
    }
  assign(x=name,value=results,pos=1)
  numtests <- 30
  results$tests <- data.frame(name=character(numtests),H1=character(numtests),teststat=c(numtests),pval=c(numtests),sig=character(numtests),notes=character(numtests),stringsAsFactors=F)
  ## another helper macro
  savetest <- defmacro(row,test,note,expr={
    er <- "try-error" %in% class(try(x <- test))
    if(!er)
      {
        results$tests$name[row] <- if(!is.null(x$method)) x$method else NA
        results$tests$H1[row] <- if(!is.null(x$alternative)) x$alternative else NA
        results$tests$teststat[row] <- x$statistic
        if(!is.na(x$p.value))
          {
            results$tests$pval[row] <- x$p.value
            results$tests$sig[row] <- pstar(x$p.value)
          }
        results$tests$notes[row] <- note
        row <- row + 1
      }
    else print(paste("Couldn't perform test:",note))
  })
  ## perform diagnostic tests and save results
  i <- 1
  savetest(i,resettest(spec,data=pdata),"H0: model is correctly specified")
  savetest(i,bptest(plm_pooling_individual),"H0: no heteroskedasticity")  
  savetest(i,shapiro.test(plm_pooling_individual$residuals),"H0: residuals are normally distributed")
  savetest(i,purtest(plm_pooling_individual$model,test="madwu")$statistic,"H0: unit roots present")
  savetest(i,purtest(plm_pooling_individual$model,test="levinlin")$statistic,"H0: unit roots present")
  savetest(i,purtest(plm_pooling_individual$model,test="ips")$statistic,"H0: unit roots present")
  savetest(i,purtest(plm_pooling_individual$model,test="hadri")$statistic,"H0: unit roots present")    
  if(exists("plm_within_individual"))
    {
      savetest(i,pbgtest(plm_within_individual),"Requires number of time steps to be large")
      savetest(i,pdwtest(plm_within_individual),"Requires number of time steps to be large")
      if(exists("pvcm_within_individual"))
        {
          savetest(i,pooltest(plm_within_individual,pvcm_within_individual),"Chow test for poolability/group effects")
        }
      if(exists("plm_random_individual"))
        {
          savetest(i,phtest(plm_within_individual,plm_random_individual),"H0: random effects model is consistent (if rejected use fixed effects)") 
        }
    }
  if(exists("plm_pooling_individual"))
    {
      savetest(i,plmtest(plm_pooling_individual,effect="individual"),"LM test for group effects")
      savetest(i,plmtest(plm_pooling_individual,effect="time"),"LM test for time effects") 
      savetest(i,plmtest(plm_pooling_individual,effect="twoways"),"LM test for two-way effects") 
      if(exists("plm_within_individual"))
        {
          savetest(i,pFtest(plm_within_individual,plm_pooling_individual),"F-test for group effects")
        }
      if(exists("plm_within_time"))
        {
          savetest(i,pFtest(plm_within_time,plm_pooling_individual),"F-test for time effects")
        }
      if(exists("plm_within_twoways"))
        {
          savetest(i,pFtest(plm_within_twoways,plm_pooling_individual),"F-test for two-way effects")
        }
    }

  savetest(i,pbsytest(spec,data=pdata,index=pindex,test="re"),"Random effects test robust to serial correlation (but not heteroskedasticity or non-normality of residuals).")
  savetest(i,pwtest(spec,data=pdata,index=pindex),"Serial correlation/random effects test (if H0 is rejected it could be either).")
  savetest(i,pbsytest(spec,data=pdata,index=pindex,test="ar"),"Serial correlation test robust to random effects (but not heteroskedasticity or non-normality of residuals).")
  savetest(i,pwartest(spec,data=pdata,index=pindex),"Can be used for panels with few time steps")
  savetest(i,pwfdtest(spec,data=pdata,index=pindex,h0="fe"),"If significant prefer first difference model to within effects model")
  savetest(i,pwfdtest(spec,data=pdata,index=pindex,h0="fd"),"If significant prefer within effects model to first difference model (unless previous test was also significant in which case neither model is good)") 
  savetest(i,pcdtest(spec,data=as.data.frame(pdata),pindex,test="cd"),"Test for cross-sectional dependence, has low power if some cross-sectional dependence is +ve and some is -ve")
  savetest(i,pcdtest(spec,data=as.data.frame(pdata),pindex,test="lm"),"Test for cross-sectional dependence, only valid if No. of time steps is large and No. of groups is small and fixed")
  savetest(i,pcdtest(spec,data=as.data.frame(pdata),pindex,test="sclm"),"Test for cross-sectional dependence, only valid if No. of time steps is large, and No. of groups is smaller")
  assign(x=name,value=results,pos=1)
}

## Work out the best value for the threshold
findbestthresh <- function(spec,var,pdata,modeltype="within",quadratic=FALSE,minval=1,maxval=10,step=0.1)
{
  if(quadratic)
    {
      results <- matrix(nrow=(maxval-minval+1)/step,ncol=4)
      spec2 <- as.formula(paste(as.character(spec)[2],as.character(spec)[1],as.character(spec)[3],"+ dummyvar + Xdummyvar + X2dummyvar"))
    }
  else
    {
      results <- matrix(nrow=(maxval-minval+1)/step,ncol=3)
      spec2 <- as.formula(paste(as.character(spec)[2],as.character(spec)[1],as.character(spec)[3],"+ dummyvar + Xdummyvar"))
    }
  i <- 1
  for (thresh in ((minval/step):(maxval/step))*step)
    {
      pdata$dummyvar <- as.integer(pdata[,var] > thresh)
      pdata$Xdummyvar <- pdata[,var] * pdata$dummyvar
      pdata$X2dummyvar <- pdata[,var]^2 * pdata$dummyvar
      x <- plm(spec2,data=pdata,model=modeltype,subset=!badrows)
      results[i,1] <- thresh
      results[i,2] <- x$coefficients["dummyvar"]
      if(quadratic)
        results[i,3] <- x$coefficients["X2dummyvar"]
      results[i,dim(results)[2]] <- sum(x$residuals^2)
      i <- i + 1
    }
  return(results[which.min(results[,dim(results)[2]]),1])
}


## OUTLIERS
aq.plot(paneldata[complete.cases(paneldata[,get.vars(fdispec5)]),13:15])
uni.plot(paneldata[complete.cases(paneldata[,c(13,14,15)]),13:15],symb=T)


## MODEL SPECIFICATIONS
cizkowicz1 <- inv_corp_growth ~ cpi_growth + gdp_growth + inv_pub_growth + rel_cost + nominal_interest_rate

rmgrep("fdispec")
fdispecA <- fdi_in_bop_2000_usd ~ cpi_growth + gdp_2000_usd + nominal_interest_rate + openness + exchange_rate
fdispecB <- fdi_in_bop_2000_usd ~ cpi_growth + gdp_2000_usd + nominal_interest_rate + openness + exchange_rate + gov_consumption
fdispecC <- fdi_in_bop_2000_usd ~ cpi_growth + gdp_2000_usd + nominal_interest_rate + imports + exports + exchange_rate + gov_consumption
fdispecD <- fdi_in_bop_2000_usd ~ cpi_growth + gdp_2000_usd + nominal_interest_rate  + exports + exchange_rate + gov_consumption

fdispecA_LogLog <- log(fdi_in_bop_2000_usd) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + log(openness) + log(exchange_rate)
fdispecB_LogLog <- log(fdi_in_bop_2000_usd) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + log(openness) + log(exchange_rate) + log(gov_consumption)
fdispecC_LogLog <- log(fdi_in_bop_2000_usd) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + log(imports) + log(exports) + log(exchange_rate) + log(gov_consumption)
fdispecD_LogLog <- log(fdi_in_bop_2000_usd) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + log(exports) + log(exchange_rate) + log(gov_consumption)

fdispecA_DLogDLog <- diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + diff(log(openness)) + diff(log(exchange_rate))
fdispecB_DLogDLog <- diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + diff(log(openness)) + diff(log(exchange_rate)) + log(gov_consumption)
fdispecC_DLogDLog <- diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + diff(log(imports)) + diff(log(exports)) + diff(log(exchange_rate)) + log(gov_consumption)
fdispecD_DLogDLog <- diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + diff(log(exports)) + diff(log(exchange_rate)) + log(gov_consumption)

fdispecA_DLoglag1DLog <- diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + diff(log(openness)) + diff(log(exchange_rate)) + lag(diff(log(fdi_in_bop_2000_usd)),1)
fdispecB_DLoglag1DLog <- diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + diff(log(openness)) + diff(log(exchange_rate)) + log(gov_consumption) + lag(diff(log(fdi_in_bop_2000_usd)),1)
fdispecC_DLoglag1DLog <- diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + diff(log(imports)) + diff(log(exports)) + diff(log(exchange_rate)) + log(gov_consumption) + lag(diff(log(fdi_in_bop_2000_usd)),1)
fdispecD_DLoglag1DLog <- diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + diff(log(exports)) + diff(log(exchange_rate)) + log(gov_consumption) + lag(diff(log(fdi_in_bop_2000_usd)),1)

fdispecA_DLoglag2DLog <- diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + diff(log(openness)) + diff(log(exchange_rate)) + lag(diff(log(fdi_in_bop_2000_usd)),1) + lag(diff(log(fdi_in_bop_2000_usd)),2)

fdispecB_DLoglag2DLog <- diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + diff(log(gdp_2000_usd)) + log(nominal_interest_rate) + diff(log(openness)) + diff(log(exchange_rate)) + log(gov_consumption) + lag(diff(log(fdi_in_bop_2000_usd)),1) + lag(diff(log(fdi_in_bop_2000_usd)),2)

fdispecB_DLoglag2DLog <- diff(log_fdi) ~ log_cpi_growth + diff(log_gdp) + log_interest_rate + diff(log_openness) + diff(log_exchange_rate) + log_gov_consumption + lag(diff(log_fdi),1) + lag(diff(log_fdi),2)

fdispecC_DLoglag2DLog <- diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + diff(log(imports)) + diff(log(exports)) + diff(log(exchange_rate)) + log(gov_consumption) + lag(diff(log(fdi_in_bop_2000_usd)),1) + lag(diff(log(fdi_in_bop_2000_usd)),2)
fdispecD_DLoglag2DLog <- diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + diff(log(exports)) + diff(log(exchange_rate)) + log(gov_consumption) + lag(diff(log(fdi_in_bop_2000_usd)),1) + lag(diff(log(fdi_in_bop_2000_usd)),2)

finalspec2 <- diff(log_fdi) ~ log_cpi_growth + diff(log_gdp) + log_interest_rate + log_exchange_rate + log_gov_consumption + log_openness + lag(diff(log_fdi)) + lag(diff(log_fdi),2)

finalspec1 <- diff(log_fdi) ~ log_cpi_growth + diff(log_gdp) + log_interest_rate + log_exchange_rate + log_gov_consumption + log_openness + lag(diff(log_fdi))

## ROW SPECIFICATIONS
BankingCrisis <- paneldata$year %in% c(2006:2010) # rows with years of banking crises
OilCrisis <- paneldata$year %in% c(1973:1975,1979:1982,1990:1992) # rows with years of oil crisis

badrows <- BankingCrisis | OilCrisis




## ESTIMATE MODELS
cols <- c("iso2c","year","year2","country","log_fdi","fdi_in_bop_2000_usd","cpi_growth","gdp_2000_usd","nominal_interest_rate","imports","exports","openness","exchange_rate","gov_consumption")
cols <- c("iso2c","year","year2","country","log_fdi","log_cpi_growth","log_gdp","log_interest_rate","log_imports","log_exports","log_openness","log_exchange_rate","log_gov_consumption")


rows <- paneldata$nominal_interest_rate > 0 & paneldata$cpi_growth > 0 & paneldata$fdi_in_bop_2000_usd > 0 & paneldata$imports > 0 & paneldata$exports > 0 & paneldata$gov_consumption > 0  & complete.cases(paneldata[,cols])# & (paneldata$iso2c %in% oecdcodes)
rows <- evalq(complete.cases(),envir=paneldata)

controws <- PDContiguousRows(paneldata[,cols],rows,minlen=4)
for(spec in lsgrep("fdispec.*DLoglag.*DLog"))
  {
    paneldataanalysis(evalstr(spec),paste("models",spec,sep="_"),pdata=paneldata[rows,cols],pindex=c("iso2c","year"))
  }

pgmm(diff(log(fdi_in_bop_2000_usd)) ~ log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + diff(log(openness)) + diff(log(exchange_rate)) + log(gov_consumption) + lag(diff(log(fdi_in_bop_2000_usd)),1:2) | lag(diff(log(fdi_in_bop_2000_usd)),2:99),data=as.data.frame(paneldata[controws,cols]),index=c("iso2c","year"),model="twostep",transformation="d",na.action="na.omit",time.dummies=T,robust=T)

pgmm(diff(log_fdi) ~ log_cpi_growth + log_gdp + log_interest_rate + diff(log_openness) + diff(log_exchange_rate) + log_gov_consumption + lag(diff(log_fdi),1:2) | lag(diff(log_fdi),3:99),data=as.data.frame(paneldata[controws,cols]),index=c("iso2c","year"),model="twostep",transformation="d",na.action="na.omit",time.dummies=T,robust=T)

pgmm(log_fdi ~ log_cpi_growth | lag(log_fdi,3:99),data=as.data.frame(paneldata[controws,cols]),index=c("iso2c","year"),model="onestep",transformation="d",time.dummies=F,robust=F)

## THRESHOLDS
bestthresh <- findbestthresh(fdispec6,"cpi_growth",paneldata) 
bestthreshQ <- findbestthresh(fdispec5,"cpi_growth",paneldata,quadratic=T)

btQ <- findbestthresh(fdispec5,"cpi_growth",paneldata,quadratic=T)

## create dummy and interaction variables
paneldata$cpidummy <- as.integer(paneldata$cpi_growth > bestthresh)
paneldata$cpiXdummy <- paneldata$cpi_growth * paneldata$cpidummy
paneldata$cpisqrXdummy <- paneldata$cpi_growth * paneldata$cpi_growth * paneldata$cpidummy

paneldata2$cpidummy <- as.integer(paneldata2$cpi_growth > bestthresh)
paneldata2$cpiXdummy <- paneldata2$cpi_growth * paneldata2$cpidummy
paneldata2$cpisqrXdummy <- paneldata2$cpi_growth * paneldata2$cpi_growth * paneldata2$cpidummy



## preferred models are RE model or first-difference model
summary(plmrandomI1)
coeftest(plmrandomI1,vcovHC) # test coeffs using robust standard errors

summary(plmfdI1)
coeftest(plmfdI1,vcovHC) # test coeffs using robust standard errors




