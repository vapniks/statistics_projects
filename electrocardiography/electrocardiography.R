## Load some libraries
library(magrittr)
library(data.table)
library(mergeutils)
## Load the data
data <- read.csv("FinalCompendium_22022016.csv")
extradata <- read.csv("StudyPatientsDataHP_22022016.csv")
## Convert to datatables for easier manipulation
data <- as.data.table(data)
extradata <- as.data.table(extradata)
setkey(data,Patient.ID)
setkey(extradata,ID)
## change factors to strings to that they can be compared
data$LV <- as.character(data$LV)
data$LVopt <- as.character(data$LVopt)
data$AVD <- as.character(data$AVD)
data$AVDopt <- as.character(data$AVDopt)
## indicate which are the rows with the optimal LV and AVD values wrt MPI
grp2rows <- ((data$AVD==data$AVDopt | is.na(data$AVDopt)) & data$LV==data$LVopt & !is.na(data$LVaT10.90))
## Find which ones have the smallest LVaT10.90 values within those set to optimal LV & AVD values (wrt MPI)
data[grp2rows,bestlvat2:=min(LVaT10.90,na.rm=TRUE),by="Patient.ID"]
data[grp2rows,bestrow2:=(LVaT10.90==bestlvat2),by="Patient.ID"]
data$bestrow2[is.na(data$bestrow2)] <- FALSE
## Find which ones have the smallest LVaT10.90 values within other rows
data[!grp2rows,bestlvat1:=min(LVaT10.90,na.rm=TRUE),by="Patient.ID"]
data[!grp2rows,bestrow1:=(LVaT10.90==bestlvat1),by="Patient.ID"]
data$bestrow1[is.na(data$bestrow1)] <- FALSE
## create datatables for different groups
grp1data <- data[bestrow1==TRUE,list(Patient.ID,LV,LVopt,AVD,AVDopt,LVaT10.90,ExtConfig)]
grp2data <- data[bestrow2==TRUE,list(Patient.ID,LV,LVopt,AVD,AVDopt,LVaT10.90,ExtConfig)]
## save data
write.csv(grp1data,file="grp1data.csv",row.names=FALSE)
write.csv(grp2data,file="grp2data.csv",row.names=FALSE)
write.csv(data,file="maindata.csv",row.names=FALSE)
write.csv(extradata,file="extradata.csv",row.names=FALSE)
## check
print(data[,list(Patient.ID,LVaT10.90,bestrow)],nrows=Inf)
## GRAPHS
options()
## overall density plot of LVaT10.90
plot(density(data$LVaT10.90,na.rm=TRUE)) # clearly the distribution is skewed
dev.copy2pdf(file="LVaT_density-plot.pdf")
## density plots of LVaT10 by patient
densityplot(~LVaT10.90|Patient.ID,data=data,cex=0.1)
dev.copy2pdf(file="LVaT_density-plot_by_patient.pdf")
## density plots of LVaT10 by config
densityplot(~LVaT10.90|ExtConfig,data=data,cex=0.1)
dev.copy2pdf(file="LVaT_density-plot_by_configuration.pdf")

## Merge bestrows of data with extradata
x <- data[bestrow==TRUE,list(Patient.ID,bestlvat,ExtConfig,LV,AVD)]
mergedata1 <- x[extradata]

## Check if LVaT10.90 values are normally distributed
shapiro.test(data$LVaT10.90) # the distribution is skewed
## LVaT10.90 values are not normally distributed so ANOVA test is not appropriate
## Need to do a non-parametric test instead: Kruskal-Wallis
kruskal.test(data$LVaT10.90,data$Patient.ID)


