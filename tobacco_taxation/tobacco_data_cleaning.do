capture log close
local directory "/home/project"
local datadirectory "/home/project/Data"
log using "`directory'/stata_output.log", replace

clear all
cd "`directory'"
set mem 500m
set more off
set linesize 200

local datafiles "hse99gp3 hse00ai hse01ai hse02ai hse03ai hse04gpa hse05ai hse06ai hse07ai hse08ai hse09ai hse10ai"
local allfiles "hse98ai `datafiles'"
local hsevars "totinc cigdyal hserial econact"

/* loop through all the files (starting with 1998), sum the daily cigarette consumptions off all members of each household */
/* and save total household income category and employment status */  
local i = 1998
foreach file in `allfiles'{
  display "Processing `file'"
  use `datadirectory'/`file', clear
  compress
  keep `hsevars'
  keep if cigdyal >= 0 & econact > 0
  collapse (sum) cigdyal (first) totinc (first) econact, by(hserial)
  gen year = `i'
  save `file'_new.dta, replace
  clear all
  local ++i
}

/* Join the data together into a single file */
use hse98ai_new.dta, clear
keep `hsevars' year
foreach file in `datafiles'{
  display "Appending `file'"
  append using `file'_new.dta, keep(`hsevars' year)
}

/* create value labels for income category */
label define incomecat 1 "<£520" 2 "£520<£1,600" 3 "£1,600<£2,600" 4 "£2,600<£3,600" 5 "£3,600<£5,200" 6 "£5,200<£7,800" 7 "£7,800<£10,400" 8 "£10,400<£13,000" 9 "£13,000<£15,600" 10 "£15,600<£18,200" 11 "£18,200<£20,800" 12 "£20,800<£23,400" 13 "£23,400<£26,000" 14 "£26,000<£28,600" 15 "£28,600<£31,200" 16 "£31,200<£33,800" 17 "£33,800<£36,400" 18 "£36,400<£41,600" 19 "£41,600<£46,800" 20 "£46,800<£52,000" 21 "£52,000<£60,000" 22 "£60,000<£70,000" 23 "£70,000<£80,000" 24 "£80,000<£90,000" 25 "£90,000<£100,000" 26 "£100,000<£110,000" 27 "£110,000<£120,000" 28 "£120,000<£130,000" 29 "£130,000<£140,000" 30 "£140,000<£150,000" 31 "£150,000+"
label values totinc incomecat
/* create value labels for employment status */
label define empstatus 1 "Employed" 2 "Unemployed" 3 "Retired" 4 "Other"
label values econact empstatus

/* Label the variables */
label variable cigdyal "Average number of cigarettes smoked per day"
label variable totinc "Income category (total household income)"
label variable hserial "Household serial number"
label variable econact "Employment status"

/* add data for relative tobacco price index */
gen reltobpriceidx = 0
replace reltobpriceidx = 190.4 if year == 1998
replace reltobpriceidx =   209 if year == 1999
replace reltobpriceidx = 220.6 if year == 2000
replace reltobpriceidx = 228.5 if year == 2001
replace reltobpriceidx = 231.6 if year == 2002
replace reltobpriceidx =   233 if year == 2003
replace reltobpriceidx = 234.4 if year == 2004
replace reltobpriceidx = 237.8 if year == 2005
replace reltobpriceidx = 240.8 if year == 2006
replace reltobpriceidx = 243.1 if year == 2007
replace reltobpriceidx = 244.2 if year == 2008
replace reltobpriceidx = 255.2 if year == 2009
replace reltobpriceidx = 262.6 if year == 2010
label variable reltobpriceidx "Tobacco price index relative to retail price index"
/* add data for tobacco retail recorded price */
gen tobaccorrp = 0
replace tobaccorrp = 3.36 if year == 1998
replace tobaccorrp = 3.64 if year == 1999
replace tobaccorrp = 3.88 if year == 2000
replace tobaccorrp = 4.22 if year == 2001
replace tobaccorrp = 4.39 if year == 2002
replace tobaccorrp = 4.51 if year == 2003
replace tobaccorrp = 4.65 if year == 2004
replace tobaccorrp = 4.82 if year == 2005
replace tobaccorrp = 5.05 if year == 2006
replace tobaccorrp = 5.33 if year == 2007
replace tobaccorrp = 5.44 if year == 2008
replace tobaccorrp = 5.67 if year == 2009
replace tobaccorrp = 6.13 if year == 2010
/* divide by 20 to get price per cigarette */
replace tobaccorrp = tobaccorrp/20
label variable tobaccorrp "Cost per cigarette in most popular price category (source TMA)."
/* add data for affordability (original) */
gen affordorig = 0
replace affordorig = 86.8 if year == 1998
replace affordorig = 81.4 if year == 1999
replace affordorig = 80.4 if year == 2000 
replace affordorig =   81 if year == 2001 
replace affordorig = 81.6 if year == 2002 
replace affordorig = 83.5 if year == 2003 
replace affordorig = 83.9 if year == 2004 
replace affordorig = 84.3 if year == 2005 
replace affordorig = 84.6 if year == 2006 
replace affordorig = 84.1 if year == 2007 
replace affordorig = 84.7 if year == 2008 
replace affordorig =   82 if year == 2009 
replace affordorig =   79 if year == 2010
label variable affordorig "Affordability index for tobacco (original)"
/* add data for affordability (revised) */
gen affordrevis = 0
replace affordrevis = 80.3 if year == 1998
replace affordrevis = 74.9 if year == 1999
replace affordrevis = 73.6 if year == 2000
replace affordrevis = 73.7 if year == 2001
replace affordrevis = 73.8 if year == 2002 
replace affordrevis = 75.1 if year == 2003 
replace affordrevis = 74.9 if year == 2004 
replace affordrevis = 74.6 if year == 2005 
replace affordrevis = 74.2 if year == 2006 
replace affordrevis = 73.1 if year == 2007 
replace affordrevis =   73 if year == 2008 
replace affordrevis = 70.1 if year == 2009 
replace affordrevis =   67 if year == 2010 
label variable affordrevis "Affordability index for tobacco (revised)"
/* add data for tobacco tax */
gen taxpercig = 0
replace taxpercig = 2.65 if year == 1998 
replace taxpercig = 2.88 if year == 1999 
replace taxpercig = 3.08 if year == 2000 
replace taxpercig = 3.37 if year == 2001 
replace taxpercig = 3.46 if year == 2002 
replace taxpercig = 3.55 if year == 2003 
replace taxpercig = 3.65 if year == 2004 
replace taxpercig = 3.77 if year == 2005 
replace taxpercig = 3.91 if year == 2006 
replace taxpercig = 4.07 if year == 2007 
replace taxpercig = 4.18 if year == 2008 
replace taxpercig = 4.34 if year == 2009 
replace taxpercig = 4.67 if year == 2010 
replace taxpercig = 5.08 if year == 2011 
replace taxpercig = 5.45 if year == 2012 
replace taxpercig = 5.91 if year == 2013
replace taxpercig = taxpercig/20
label variable taxpercig "Tax per cigarette in most popular price category (source TMA)"

gen taxpc = taxpercig / tobaccorrp
label variable taxpc "Tobacco tax as % of price"

/* calculate yearly household spending on tobacco */
gen tobaccospend = tobaccorrp * cigdyal * 365
label variable tobaccospend "Yearly household spend on tobacco."
/* create dummy variable for cigarette advertising ban */
gen adban = 0
replace adban = 1 if year > 2002
label variable adban "Whether or not the cigarette advertising ban is in place."
/* remove data with missing or invalid income category */
keep if totinc >= 1 & totinc <= 31

/* calculate average income for each income category */
gen incomeest = 0
replace incomeest = 400                  if totinc == 1 
replace incomeest = (1600+520)/2         if totinc == 2 
replace incomeest = (2600+1600)/2        if totinc == 3 
replace incomeest = (3600+2600)/2        if totinc == 4 
replace incomeest = (5200+3600)/2        if totinc == 5 
replace incomeest = (7800+5200)/2        if totinc == 6 
replace incomeest = (10400+7800)/2       if totinc == 7 
replace incomeest = (13000+10400)/2      if totinc == 8 
replace incomeest = (15600+13000)/2      if totinc == 9 
replace incomeest = (18200+15600)/2      if totinc == 10
replace incomeest = (20800+18200)/2      if totinc == 11
replace incomeest = (23400+20800)/2      if totinc == 12
replace incomeest = (26000+23400)/2      if totinc == 13
replace incomeest = (28600+26000)/2      if totinc == 14
replace incomeest = (31200+28600)/2      if totinc == 15
replace incomeest = (33800+31200)/2      if totinc == 16
replace incomeest = (36400+33800)/2      if totinc == 17
replace incomeest = (41600+36400)/2      if totinc == 18
replace incomeest = (46800+41600)/2      if totinc == 19
replace incomeest = (52000+46800)/2      if totinc == 20
replace incomeest = (60000+52000)/2      if totinc == 21
replace incomeest = (70000+60000)/2      if totinc == 22
replace incomeest = (80000+70000)/2      if totinc == 23
replace incomeest = (90000+80000)/2      if totinc == 24
replace incomeest = (100000+90000)/2     if totinc == 25
replace incomeest = (110000+100000)/2    if totinc == 26
replace incomeest = (120000+110000)/2    if totinc == 27
replace incomeest = (130000+120000)/2    if totinc == 28
replace incomeest = (140000+130000)/2    if totinc == 29
replace incomeest = (150000+140000)/2    if totinc == 30
replace incomeest = 200000               if totinc == 31
label variable incomeest "Estimated income (median of income category)"
/* Variable to indicate smokers */
gen smokers = 0
replace smokers = 1 if cigdyal > 0
label variable smokers "Whether or not there are any smokers in the household"
/* Create new variables for cigdyal and tobaccospend that are always >0 for log models */
gen cigdyal2 = cigdyal
replace cigdyal2 = 0.01 if smokers == 0
gen tobaccospend2 = tobaccospend
replace tobaccospend2 = 0.01 if smokers == 0
label variable cigdyal2 "Average number of cigarettes smoked per day (0 -> 0.01)"
label variable tobaccospend2 "Yearly household spend on tobacco (0 -> 0.01)"

/* calculate proportion of yearly household income spent on tobacco */
gen spendtobac = tobaccospend2 / incomeest
label variable spendtobac "Proportion of yearly household income spent on tobacco"

/* calculate the cost of a cigarette as a proportion of daily household income */
gen cigcost = tobaccorrp / (incomeest/365)
label variable cigcost "Cost of a cigarette as a proportion of daily household income"

/* create variables for log models */
gen lncigdyal = ln(cigdyal2)
label variable lncigdyal "log(cigdyal2)"
gen lnspendtobac = ln(spendtobac)
label variable lnspendtobac "log(spendtobac)"
gen lnincomeest = ln(incomeest)
label variable lnincomeest "log(incomeest)"
gen lntobaccospend = ln(tobaccospend2)
label variable lntobaccospend "log(tobaccospend2)"
gen lntobaccorrp = ln(tobaccorrp)
label variable lntobaccorrp "log(tobaccorrp)"
gen lnreltobpriceidx = ln(reltobpriceidx)
label variable lnreltobpriceidx "log(reltobpriceidx)"
gen lncigcost = ln(cigcost)
label variable lncigcost "log(cigcost)"
gen lntaxpercig = ln(taxpercig)
label variable lntaxpercig "log(taxpercig)"

save "alldata.dta", replace
