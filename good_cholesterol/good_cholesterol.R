## Statistical analysis of double blind randomised controlled study of the effect of
## reconstituted HDL on carotid plaque stabilisation.

## first get the data from the excel file:
library(XLConnect)
xlwb <- loadWorkbook("/home/ben/Documents/Teaching/Hosaam_Nasr_(medical_study)/Gill_s_data_for_stat_1.xls")
sheetnames <- c("MMP-9","IL-6","MCP-1","sCD40L","TF","HDL","LDL","CHOLEST","TG","ApoAI","CRP","FIBRINOGEN","D-DIMER")
allsheets <- readWorksheet(xlwb,sheetnames,header=TRUE)
names(allsheets) <- sub("-","_",names(allsheets))
names(allsheets)
## [1] "MMP_9"      "IL_6"       "MCP_1"      "sCD40L"     "TF"        
## [6] "HDL"        "LDL"        "CHOLEST"    "TG"         "ApoAI"     
## [11] "CRP"        "FIBRINOGEN" "D_DIMER"   
names(allsheets[[1]])
## [1] "PT.ID."             "a.active.b.placebo" "PRE.INF"           
## [4] "X24.HRS.POST.INF"   "X48.HRS.POST.INF"

## Convert to numeric
for(sheet in names(allsheets))
{
  for(col in 3:5)
  allsheets[[sheet]][,col] <- as.numeric(allsheets[[sheet]][,col])
}

library(car) ## needed for Levenes test of homogeneity of variances
## formulas for comparing placebo with active for 24hr and 48hr effects
form1 <- X24.HRS.POST.INF - PRE.INF ~ a.active.b.placebo
form2 <- X48.HRS.POST.INF - PRE.INF ~ a.active.b.placebo
form3 <- X48.HRS.POST.INF - X24.HRS.POST.INF ~ a.active.b.placebo
results <- data.frame(row.names=names(allsheets))
## perform normality, homogeneity of variance, t and wilcox tests
for(sheet in names(allsheets))
{
  data <- allsheets[[sheet]]
  arows <- data[,"a.active.b.placebo"]=="a"
  brows <- data[,"a.active.b.placebo"]=="b"
  adata24 <- data[arows,"X24.HRS.POST.INF"] - data[arows,"PRE.INF"]
  bdata24 <- data[brows,"X24.HRS.POST.INF"] - data[brows,"PRE.INF"]
  adata48 <- data[arows,"X48.HRS.POST.INF"] - data[arows,"PRE.INF"]
  bdata48 <- data[brows,"X48.HRS.POST.INF"] - data[brows,"PRE.INF"]
  adatadiff <- data[brows,"X48.HRS.POST.INF"] - data[brows,"X24.HRS.POST.INF"]
  bdatadiff <- data[brows,"X48.HRS.POST.INF"] - data[brows,"X24.HRS.POST.INF"]
  results[sheet,"normtest-a-24"] <- shapiro.test(adata24)$p.value
  results[sheet,"normtest-b-24"] <- shapiro.test(bdata24)$p.value
  results[sheet,"normtest-a-48"] <- shapiro.test(adata48)$p.value
  results[sheet,"normtest-b-48"] <- shapiro.test(bdata48)$p.value
  results[sheet,"normtest-a-diff"] <- shapiro.test(adatadiff)$p.value
  results[sheet,"normtest-b-diff"] <- shapiro.test(bdatadiff)$p.value
  data24 <- data[,"X24.HRS.POST.INF"] - data[,"PRE.INF"]
  data48 <- data[,"X48.HRS.POST.INF"] - data[,"PRE.INF"]
  datadiff <- data[,"X48.HRS.POST.INF"] - data[,"X24.HRS.POST.INF"]
  results[sheet,"vartest-24"] <- leveneTest(data24,data[,"a.active.b.placebo"])[3][1,]
  results[sheet,"vartest-48"] <- leveneTest(data48,data[,"a.active.b.placebo"])[3][1,]
  results[sheet,"vartest-diff"] <- leveneTest(datadiff,data[,"a.active.b.placebo"])[3][1,]  
  results[sheet,"ttest-24"] <- t.test(form1,data)$p.value
  results[sheet,"ttest-48"] <- t.test(form2,data)$p.value
  results[sheet,"ttest-diff"] <- t.test(form3,data)$p.value
  results[sheet,"wtest-24"] <- wilcox.test(form1,data)$p.value
  results[sheet,"wtest-48"] <- wilcox.test(form2,data)$p.value
  results[sheet,"wtest-diff"] <- wilcox.test(form3,data)$p.value
}

## Since normality is violated but homogeneity of variances isn't, use the Wilcox test results.
wtestpvals <- c(results[,"wtest-24"],results[,"wtest-48"],results[,"wtest-diff"])
## keep record of what the corresponding tests were
tests <- c(paste(rownames(results),"24hr",sep="_"),
           paste(rownames(results),"48hr",sep="_"),
           paste(rownames(results),"48-24hr",sep="_"))
## Need to adjust p-values to control for family wise error (FWER) or false discovery rate (FDR).
## Since test are not independent, and observations violate normality assumption,
## we use Holm-Bonferroni (controls for FWER) and Benjamini-Yekutieli methods (controls for FDR)
wtestpvalsBY <- p.adjust(wtestpvals,method="BY")
wtestpvalsholm <- p.adjust(wtestpvals,method="holm")

## put these p-values into a single dataframe
pvalues <- data.frame(wtestpvals,wtestpvalsholm,wtestpvalsBY)
rownames(pvalues) <- tests
names(pvalues) <- c("Wilcoxon-test_p-value","Holm-adjusted_p-value","BY-adjusted_p-value")

## Which tests were rejected?
alpha <- 0.05
rejectedBY <- tests[wtestpvalsBY < alpha]
## [1] "MMP_9_24hr" "TF_24hr"    "HDL_24hr"   "ApoAI_24hr" "IL_6_48hr"
## [6] "TF_48hr"    "HDL_48hr"   "ApoAI_48hr"
rejectedholm <- tests[wtestpvalsholm < alpha]
## [1] "MMP_9_24hr" "TF_24hr"    "HDL_24hr"   "ApoAI_24hr" "TF_48hr"
## [6] "HDL_48hr"   "ApoAI_48hr"

## Comparison of the two methods shows that they agree on all but one of the tests:
## Holm-Bonferroni rejects the 15th test ("IL_6_48hr"), but Benjamini-Yekutieli doesn't.

## Write the results back to the excel workbook
createSheet(xlwb,name="analysis")
writeWorksheet(xlwb,results,"analysis",header=TRUE,rownames="protein")
appendWorksheet(xlwb,pvalues,"analysis",header=TRUE,rownames="Test")
saveWorkbook(xlwb)


