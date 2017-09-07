## Graphs
library(ggplot2)
library(psych)
library(car)
library(lattice)

## Function to calculate coefficients in panel data rolling regression
##' @param spec Formula for model specification
##' @param pdata A pdata.frame object containing the data to use for the regression
##' @param var String containing name of variable whose range of values will be rolled over
##' @param rows Logical vector indicating which rows of pdata to use
##' @param winsize Number of observations to use per regression
##' @param ... Other parameters to be passed to plm function

##' @return A data.frame containing max values of var, estimated coeffs and 95% confidence intervals for the coeffs
##' for each window in the rolling regression 
calccoeffs <- function(spec,var,pdata,rows=rep(T,dim(pdata)[1]),winsize=100,...)
{
  rows <- complete.cases2(spec,var,df=pdata) & rows
  indices <- names(attr(pdata,"index"))
  pdatacc <- as.data.frame(pdata[rows,])
  maxj <- sum(rows)
  o <- order(pdatacc[,var])
  results <- data.frame(maxval=rep(NA,maxj-(winsize-1)))
  for (j in (winsize:maxj))
    {
      resrow <- j-(winsize-1)
      sample <- o[resrow:j]
      results$minval[resrow] <- pdatacc[o[resrow],var]
      results$maxval[resrow] <- pdatacc[o[j],var]
      results$medval[resrow] <- median(pdatacc[sample,var])
      x <- plm(spec,data=pdatacc[sample,],index=indices,...)
      for(coef in rhs.vars(spec))
        {
          evalstr("N <- which(rownames(vcov(x))==\"",coef,"\")")
          coefval <- evalstr("x$coefficients[\"",coef,"\"]")
          coefsd <- sqrt(vcov(x)[N,N])
          evalstr("results[resrow,\"",coef,"\"] <- coefval")
          evalstr("results[resrow,\"",coef,"L\"] <- coefval-1.96*coefsd")
          evalstr("results[resrow,\"",coef,"U\"] <- coefval+1.96*coefsd")
        }
    }
  return(results)
}

##' @param y The y-axis values for the middle value
##' @param L The y-axis value for the lower bound of the confidence interval
##' @param U The y-axis value for the upper bound of the confidence interval
##' @param rows logical or numeric vector indicating the rows of vectors x, y, L and U to use for the plot
##' @param title A string containing the title for the graph
##' @param subtitle A string containing a subtitle to be placed under the title in a slightly smaller font
##' @param xlabel A string containing a label for the x-axis
##' @param ylabel A string containing a label for the y-axis
##' @param ... Extra parameters to be passed to the plot command for plotting the middle values

##' @return No return value. Creates line plots of y, L and U against x on the same graph
##' y will be plotted as a continuous line and L & U will be plotted as dashed lines
plotConfInt <- function(x,y,L,U,rows,title="",subtitle="",...)
  {
    par(cex=1)
    plot(x[rows],y[rows],type="l",lab=c(10,10,10),main=title,...)
    lines(x[rows],L[rows],lty="dashed")
    lines(x[rows],U[rows],lty="dashed")
    grid(nx=20,ny=20,col="grey")
    abline(a=0,b=0)
    if(nchar(subtitle)>0)
      {
        par(cex=0.7)
        title(subtitle)
      }
    par(cex=1)
  }

plotConfInt2 <- function(x,y,L,U,rows,title="",subtitle="",...)
  {
    par(cex=1)
    plot(x[rows],y[rows],lab=c(10,10,10),main=title,...)
    lines(x[rows],L[rows],lty="dashed")
    lines(x[rows],U[rows],lty="dashed")
    grid(nx=20,ny=20,col="grey")
    abline(a=0,b=0)
    if(nchar(subtitle)>0)
      {
        par(cex=0.7)
        title(subtitle)
      }
    par(cex=1)
  }


plotallcoeffs <- function(res,name,max=1000000,suffix="")
  {
    coeffs <- colnames(res)[grep("[^ULl]$",colnames(res))]
    rows <- res$maxval < max
    xlab <- paste("Max value of",name,"in window")
    for (coeff in coeffs)
      {
        filename <- paste("/home/projects/inflation_and_FDI/graphs/coeffients_plot_",coeff,"_vs_",name,suffix,".eps",sep="")
        ylab <- paste("Coefficient of",coeff)
        title <- paste(ylab,"in rolling regression")
        y <- evalstr("res[,\"",coeff,"\"]")
        L <- evalstr("res[,\"",coeff,"L\"]")
        U <- evalstr("res[,\"",coeff,"U\"]")
        plotConfInt(res$maxval,y,L,U,rows,title=title,xlab=xlab,ylab=ylab)
        dev.copy2eps(file=filename)
      }
  }


plotcpigrowthcoeff <- function(spec,name)
{
  plot.new()
  mfrowold <- par("mfrow")
  par(mfrow=c(2,1))
  plotcoeffs(spec,modeltype="within",100,paneldata,"cpi_growth")
  plotcoeffs(spec,modeltype="within",10,paneldata,"cpi_growth",title="")
  dev.copy2eps(file=paste("/home/projects/inflation_and_FDI/graphs/coefficients-plot_",name,"_cpi_growth.eps",sep=""))
  par(mfrow=mfrowold)
}

plotmulticoeffs <- function(spec,name,coeffs,maxvals)
  {
    mfrowold <- par("mfrow")
    par(mfrow=c(1,1))
    plot.new()
    par(mfrow=c(3,1))
    numcoeffs <- length(coeffs)
    numgraphs <- ((numcoeffs-1) %/% 3) + 1
    for (i in 1:numgraphs)
      {
        maxj <- 3
        if(i==numgraphs) {maxj <- numcoeffs %% 3}
        for (j in 1:maxj)
          {
            plotcoeffs(spec,modeltype="within",100,paneldata,coeffs[((i-1)*numgraphs)+j])
          }
        dev.copy2eps(file=paste("/home/projects/inflation_and_FDI/graphs/coefficients-plot_",name,"_",i,".eps",sep=""))
      }
    par(mfrow=mfrowold)
  }



## PANELLED LINE GRAPHS 
plot.new()
xyplot(inv_corp_growth~year2|country,data=paneldata,type="l",scales=list(x=list(tick.number=10)))
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/line-graph_inv_corp_growth_by_country.eps")
xyplot(cpi_growth~year2|country,data=paneldata,type="l",scales=list(x=list(tick.number=10)))
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/line-graph_cpi_growth_by_country.eps")
xyplot(inv_pub_growth~year2|country,data=paneldata,type="l",scales=list(x=list(tick.number=10)))
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/line-graph_inv_pub_growth_by_country.eps")
xyplot(rel_cost~year2|country,data=paneldata,type="l",scales=list(x=list(tick.number=10)))
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/line-graph_rel_cost_by_country.eps")
xyplot(nominal_interest_rate~year2|country,data=paneldata,type="l",scales=list(x=list(tick.number=10)))
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/line-graph_nominal_interest_rate_by_country.eps")
xyplot(fdi_in_growth~year2|country,data=paneldata,type="l",scales=list(x=list(tick.number=10)))
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/line-graph_fdi_in_growth_by_country.eps")


## MULTI-LINE PLOTS
par(mfrow=c(2,2))
multilineplot(paneldata[rows,],"year2","log_fdi","country",unique(paneldata$country),xlab="year",ylab="log(FDI)",main="log(FDI), all countries",leg=F,legloc="topleft",legsize=0.5)
multilineplot(paneldata[rows,],"year2","log_gdp","country",unique(paneldata$country),xlab="year",ylab="log(GDP)",main="log(GDP), all countries",leg=F,legloc="topleft",legsize=0.5)
multilineplot(paneldata[rows,],"year2","log_cpi_growth","country",unique(paneldata$country),xlab="year",ylab="log(inflation)",main="log(inflation), all countries",leg=F,legloc="topleft",legsize=0.5)
multilineplot(paneldata[rows,],"year2","log_interest_rate","country",unique(paneldata$country),xlab="year",ylab="log(interest rate)",main="log(interest rate), all countries",leg=F,legloc="topleft",legsize=0.5)
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/multiline-plot_log-vars1.eps")
multilineplot(paneldata[rows,],"year2","log_exchange_rate","country",unique(paneldata$country),xlab="year",ylab="log(PPP exchange rate)",main="log(PPP exchange rate), all countries",leg=F,legloc="topleft",legsize=0.5)
multilineplot(paneldata[rows,],"year2","log_gov_consumption","country",unique(paneldata$country),xlab="year",ylab="log(Gov. consumption)",main="log(Gov. consumption), as % of GDP, all countries",leg=F,legloc="topleft",legsize=0.5)
multilineplot(paneldata[rows,],"year2","log_exports","country",unique(paneldata$country),xlab="year",ylab="log(exports)",main="log(exports), as % of GDP, all countries",leg=F,legloc="topleft",legsize=0.5)
multilineplot(paneldata[rows,],"year2","log_imports","country",unique(paneldata$country),xlab="year",ylab="log(imports)",main="log(imports), as % of GDP, all countries",leg=F,legloc="topleft",legsize=0.5)
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/multiline-plot_log-vars2.eps")


multilineplot(paneldata,"year2","fdi_in_bop_2000_usd","country",unique(paneldata$country),xlab="year",ylab="FDI inflows (2000 USD)",main="FDI inflows (2000 USD), all countries",leg=T,legloc="topleft",legsize=0.5)
multilineplot(paneldata,"year2","inv_corp_growth","country",unique(paneldata$country),xlab="year",ylab="Corporate investement growth rate",main="Corporate investment growth rate, all countries",leg=T,legloc="topleft",legsize=0.5)
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/multiline-graph_inv_corp_growth.eps")
multilineplot(paneldata,"year2","cpi_growth","country",unique(paneldata$country),xlab="year",ylab="Inflation rate",main="Inflation rate, all countries",leg=T,legloc="topleft",legsize=0.5)
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/multiline-graph_cpi_growth.eps")
multilineplot(paneldata,"year2","inv_pub_growth","country",unique(paneldata$country),xlab="year",ylab="Public investment growth rate",main="Public investment growth rate, all countries",leg=T,legloc="topleft",legsize=0.5)
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/multiline-graph_inv_pub_growth.eps")
multilineplot(paneldata,"year2","rel_cost","country",unique(paneldata$country),xlab="year",ylab="Relative cost of private capital",main="Relative cost of private capital, all countries",leg=T,legloc="topleft",legsize=0.5)
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/multiline-graph_rel_cost.eps")
multilineplot(paneldata,"year2","nominal_interest_rate","country",unique(paneldata$country),xlab="year",ylab="Nominal interest rate",main="Nominal interest rate, all countries",leg=T,legloc="topleft",legsize=0.5)
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/multiline-graph_nominal_interest_rate.eps")
multilineplot(paneldata,"year2","fdi_in_growth","country",unique(paneldata$country),xlab="year",ylab="Foreign direct investment",main="Foreign direct investment, all countries",leg=T,legloc="topleft",legsize=0.5)
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/multiline-graph_fdi_in_growth.eps")

## DENSITY PLOTS
densityplot(paneldata$inv_corp_growth,main="Density plot of growth of corporate investment",xlab="inv_corp_growth")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/density-plot_inv_corp_growth.eps")
densityplot(paneldata$nominal_interest_rate,main="Density plot of nominal interest rate",xlab="nominal_interest_rate")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/density-plot_nominal_interest_rate.eps")
densityplot(paneldata$rel_cost,main="Density plot of relative cost of private investment",xlab="rel_cost")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/density-plot_rel_cost.eps")
densityplot(paneldata$inv_pub_growth,main="Density plot of growth in public investment",xlab="inv_pub_growth")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/density-plot_inv_pub_growth.eps")
densityplot(paneldata$gdp_growth,main="Density plot of GDP growth",xlab="gdp_growth")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/density-plot_gdp_growth.eps")
densityplot(paneldata$cpi_growth,main="Density plot of inflation",xlab="cpi_growth")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/density-plot_cpi_growth.eps")
densityplot(paneldata$fdi_in_growth,main="Density plot of growth in foreign investment",xlab="fdi_in_growth")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/density-plot_fdi_in_growth.eps")
densityplot(paneldata$fdi_in_bop_2000_usd,main="Density plot of foreign direct investment",xlab="fdi_in_bop_2000_usd")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/density-plot_fdi_in_bop_2000_usd.eps")

## HISTOGRAMS

par(mfrow=c(4,2))
hist(paneldata$log_fdi[rows],main="log(FDI)",xlab="log(FDI)")
hist(log(paneldata$cpi_growth)[rows],main="log(inflation)",xlab="log(inflation)")
hist(log(paneldata$gdp_2000_usd)[rows],main="log(GDP)",xlab="log(GDP)")
hist(log(paneldata$nominal_interest_rate)[rows],main="log(interest rate)",xlab="log(interest rate)")
hist(log(paneldata$openness)[rows],main="log(imports+exports)\n(as % of GDP)",xlab="log(imports+exports)")
hist(log(paneldata$gov_consumption)[rows],main="log(government consumption)\n(as % of GDP)",xlab="log(gov_consumption)")
hist(log(paneldata$exchange_rate)[rows],main="log(exchange rate)\n(at purchase power parity)",xlab="log(exchange rate)")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/histograms_all_log_variables.eps")

par(mfrow=c(4,2))
hist(paneldata$fdi_in_bop_2000_usd[rows],main="FDI inflows",xlab="FDI")
hist(paneldata$cpi_growth[rows],main="Inflation",xlab="inflation")
hist(paneldata$gdp_2000_usd[rows],main="GDP",xlab="GDP")
hist(paneldata$nominal_interest_rate[rows],main="Nominal interest rate",xlab="interest rate")
hist(paneldata$openness[rows],main="Imports+Exports\n(as % of GDP)",xlab="imports+exports")
hist(paneldata$gov_consumption[rows],main="Government consumption\n(as % of GDP)",xlab="gov_consumption")
hist(paneldata$exchange_rate[rows],main="Exchange rate\n(at purchase power parity)",xlab="exchange rate")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/histograms_all_variables.eps")


## SCATTERPLOTS
savescatter <- defmacro(x,y,rows,expr={
  scatterplot(x~y,data=paneldata[rows,],smooth=F,boxplot=F,main=paste(tostring(x),"vs",tostring(y)))
  dev.copy2eps(file=paste("/home/projects/inflation_and_FDI/graphs/scatterplot_",tostring(x),"_VS_",tostring(y),".eps",sep=""))
})

rows <- which(paneldata$fdi_in_bop_2000_usd > 0 & paneldata$cpi_growth > 0)
savescatter(log(fdi_in_bop_2000_usd),log(cpi_growth),rows)
savescatter(log(fdi_in_bop_2000_usd),log(gdp_2000_usd),rows)
savescatter(log(fdi_in_bop_2000_usd),log(nominal_interest_rate),rows)
savescatter(log(fdi_in_bop_2000_usd),log(openness),rows)
savescatter(log(fdi_in_bop_2000_usd),log(exchange_rate),rows)
savescatter(log(fdi_in_bop_2000_usd),log(gov_consumption),rows)



cols <- c("iso2c","year","year2","country","log_fdi","log_cpi_growth","log_gdp","log_interest_rate","log_imports","log_exports","log_openness","log_exchange_rate","log_gov_consumption")
rows <- complete.cases(paneldata[,cols])

par(mfrow=c(3,2))
plot(log(paneldata$cpi_growth)[rows],paneldata$log_fdi[rows],main="log(inflation) vs log(FDI)",xlab="log(FDI)",ylab="log(inflation)")
plot(log(paneldata$gdp_2000_usd)[rows],paneldata$log_fdi[rows],main="log(GDP) vs log(FDI)",xlab="log(FDI)",ylab="log(GDP)")
plot(log(paneldata$nominal_interest_rate)[rows],paneldata$log_fdi[rows],main="log(interest rate) vs log(FDI)",xlab="log(FDI)",ylab="")
plot(log(paneldata$openness)[rows],paneldata$log_fdi[rows],main="log(imports+exports) vs log(FDI)",xlab="log(FDI)",ylab="log(imports+exports)")
plot(log(paneldata$exchange_rate)[rows],paneldata$log_fdi[rows],main="log(PPP exchange rate) vs log(FDI)",xlab="log(FDI)",ylab="log(exchange rate)")
plot(log(paneldata$gov_consumption)[rows],paneldata$log_fdi[rows],main="log(Gov. consumption) vs log(FDI)",xlab="log(FDI)",ylab="log(Gov. consumption)")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/scatterplot_all_vs_log_fdi.eps")

pairs(~log(fdi_in_bop_2000_usd) + log(cpi_growth) + log(gdp_2000_usd) + log(nominal_interest_rate) + log(openness) + log(exchange_rate) + log(gov_consumption),data=paneldata[rows,],main="Scatter plots of all variables",pch=".")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/scatterplot_all-logged-variables.eps")

## BOXPLOTS
boxplot(paneldata$fdi_in_bop_2000_usd)

## COEFFICIENT PLOTS
plot.new()
par(mfrow=c(1,1))
plotcoeffs(fdispec6,modeltype="within",10,paneldata,"cpi_growth",rows=!badrows,title="Rolling regression coefficients plot")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/coefficients-plot_fdispec6_cpi_growth.eps")
plotcoeffs(fdispec6,modeltype="within",10,paneldata,"cpi_growth",title="Rolling regression coefficients plot")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/coefficients-plot_fdispec6_cpi_growth_all-rows.eps")

plotmulticoeffs(cizkowicz1,"test",c("gdp_growth","cpi_growth","inv_pub_growth","rel_cost","nominal_interest_rate"),100)

## cizkowicz1, all vars
plot.new()
par(mfrow=c(2,1))
plotcoeffs(cizkowicz1,modeltype="within",100,paneldata,"cpi_growth")
plotcoeffs(cizkowicz1,modeltype="within",100,paneldata,"gdp_growth",title="")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/coefficients-plot_cizkowicz1_cpi_growth_and_gdp_growth.eps")
plot.new()
par(mfrow=c(3,1))
plotcoeffs(cizkowicz1,modeltype="within",100,paneldata,"inv_pub_growth",title="")
plotcoeffs(cizkowicz1,modeltype="within",100,paneldata,"nominal_interest_rate",title="")
plotcoeffs(cizkowicz1,modeltype="within",100,paneldata,"rel_cost",title="")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/coefficients-plot_cizkowicz1_inv_pub_growth_nominal_interest_rate_rel_cost.eps")
## fdispec1, all vars
plot.new()
par(mfrow=c(2,1))
plotcoeffs(fdispec1,modeltype="within",100,paneldata,"cpi_growth")
plotcoeffs(fdispec1,modeltype="within",100,paneldata,"gdp_growth",title="")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/coefficients-plot_fdispec1_cpi_growth_and_gdp_growth.eps")
plot.new()
par(mfrow=c(3,1))
plotcoeffs(fdispec1,modeltype="within",100,paneldata,"inv_pub_growth",title="")
plotcoeffs(fdispec1,modeltype="within",100,paneldata,"nominal_interest_rate",title="")
plotcoeffs(fdispec1,modeltype="within",100,paneldata,"rel_cost",title="")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/coefficients-plot_fdispec1_inv_pub_growth_nominal_interest_rate_rel_cost.eps")
## fdispec4, all vars
plot.new()
par(mfrow=c(2,1))
plotcoeffs(fdispec4,modeltype="within",100,paneldata,"cpi_growth")
plotcoeffs(fdispec4,modeltype="within",100,paneldata,"gdp_growth",title="")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/coefficients-plot_fdispec4_cpi_growth_and_gdp_growth.eps")
plot.new()
par(mfrow=c(3,1))
plotcoeffs(fdispec4,modeltype="within",100,paneldata,"inv_pub_growth",title="")
plotcoeffs(fdispec4,modeltype="within",100,paneldata,"nominal_interest_rate",title="")
plotcoeffs(fdispec4,modeltype="within",100,paneldata,"rel_cost",title="")
dev.copy2eps(file="/home/projects/inflation_and_FDI/graphs/coefficients-plot_fdispec4_inv_pub_growth_nominal_interest_rate_rel_cost.eps")

