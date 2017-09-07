
capture log close
local directory "/home/project"
log using "`directory'/stata_output.log", replace

clear all
cd "`directory'"
set mem 500m
set more off
set linesize 200

use alldata.dta, clear

/* hserial            Household serial number */
/* cigdyal            Average number of cigarettes smoked per day */
/* totinc             Income category (total household income) */
/* econact            Employment status */
/* year               Year */
/* reltobpriceidx     Tobacco price index relative to retail price index */
/* tobaccorrp         Cost of 20 cigarettes in most popular price category (source TMA). */
/* tobaccospend       Yearly household spend on tobacco. */
/* adban              Whether or not the cigarette advertising ban is in place. */
/* incomeest          Estimated income (median of income category) */
/* smokers            Whether or not there are any smokers in the household */
/* cigdyal2           Average number of cigarettes smoked per day (0 -> 0.01) */
/* tobaccospend2      Yearly household spend on tobacco (0 -> 0.01) */
/* tobaccotax         Tax per cigarette in most popular price category (source TMA)  */
/* spendtobac         Proportion of yearly household income spent on tobacco */
/* cigcost            Cost of a cigarette as a proportion of daily household income */
/* lncigdyal          log(cigdyal2) */
/* lnspendtobac       log(spendtobac) */
/* lnincomeest        log(incomeest) */
/* lntobaccospend     log(tobaccospend2) */
/* lntobaccorrp       log(tobaccorrp) */
/* lnreltobpriceidx   log(reltobpriceidx) */
/* lncigcost          log(cigcost) */

/* Load dodiagnostics and dodiagnostics_smokers programs */
run ../tobacco_programs.do

/* Only keep households with more than 3 years of data */
tempvar q
bysort hserial: gen `q' = _N
keep if `q'> 3

/* Summary statistics */
sum cigdyal2 totinc spendtobac tobaccospend2
  
/* Histograms of main variables   */
histogram totinc, xtitle("Income category") title("Income distribution") frequency
graph export income_hist.eps, replace
histogram cigdyal2, title("Cigarette consumption") xtitle("Average daily consumption") frequency
graph export cig_consumption_hist.eps, replace

histogram cigdyal2 if smoker == 1, title("Cigarette consumption (smokers only)") xtitle("Average daily consumption") frequency
graph export cig_consumption_smokers_hist.eps, replace

catplot econact, title("Employment status")
graph export employment_status_hist.eps, replace
histogram spendtobac, title("% of yearly income spent on cigarettes") frequency
graph export spendtobac_hist.eps, replace
histogram spendtobac if smoker == 1, title("% of yearly income spent on cigarettes (smokers only)") frequency
graph export spendtobac_smokers_hist.eps, replace

histogram spendtobac if smoker == 1 & spendtobac <= 1 , title("% of yearly income spent on cigarettes (smokers only)") frequency
graph export spendtobac_smokers_hist2.eps, replace

histogram tobaccospend2, title("Yearly income spent on cigarettes") frequency
graph export tobaccospend_hist.eps, replace
histogram tobaccospend2 if smoker == 1, title("Yearly income spent on cigarettes (smokers only)") frequency
graph export tobaccospend_smoker_hist.eps, replace

/* correlation between proportion of income spent on tobacco and estimated income */
pwcorr spendtobac incomeest, sig /* highly significant negative correlation (not surprising) */
graph twoway scatter spendtobac incomeest 
graph export spendtobac_incomeest_scatter.eps, replace
/* correlation between yearly amount spent on tobacco and estimated income */
pwcorr tobaccospend2 incomeest, sig /* highly significant negative correlation - poorer people spend more on tobacco */
graph twoway scatter tobaccospend2 incomeest
graph export tobaccospend_incomeest_scatter.eps, replace

/* Compare income distributions between light and heavy smokers */
local heavy_smoker_thresh 20
histogram totinc if cigdyal2 < `heavy_smoker_thresh', title("Income distribution of light smokers") xtitle("Income category") frequency
graph export income_light_smokers.eps, replace
histogram totinc if cigdyal2 >= `heavy_smoker_thresh', title("Income distribution of heavy smokers") xtitle("Income category") frequency
graph export income_heavy_smokers.eps, replace
/* Compare employment status between light and heavy smokers */
catplot econact if cigdyal2 < `heavy_smoker_thresh', title("Employment status of light smokers") 
graph export employment_status_light_smokers.eps, replace
catplot econact if cigdyal2 >= `heavy_smoker_thresh', title("Employment status of heavy smokers") 
graph export employment_status_heavy_smokers.eps, replace

/* Find best functional form */
reg cigdyal2 tobaccorrp incomeest
dodiagnostics linlin_basic
reg lncigdyal tobaccorrp incomeest
dodiagnostics loglin_basic
reg lncigdyal lntobaccorrp lnincomeest
dodiagnostics loglog_basic
/* loglin and loglog have smallest AIC. Choose loglog for ease of interpretation */

/* Panel data modelling */
/* Set up the panel and time variables */
xtset hserial year
/* Need to increase matrix size to handle large number of categories for hserial */
set matsize 3500

/* First test for fixed effects */
xtreg lncigdyal lncigcost lntobaccorrp i.econact adban year, fe
est sto fe_model1
/* no significant fixed effects found */

/* Now test for the existence of random effects */
xtreg lncigdyal lncigcost lntobaccorrp i.econact adban year, re
est sto re_model1
xttest0  

/* No significant fixed or random effects found, so just use pooled model. */
xtset, clear
/* Sort the observations by household then year, and make it a time-series so that we can perform */
/* the autocorrelation test (bgtest) */
sort hserial year
gen ID = _n
tsset ID

estimates clear

/* Now try a few different models */
reg lncigdyal lntobaccorrp lnincomeest, vce(robust)
est sto loglog_basic_rb
reg lncigdyal lntobaccorrp lnincomeest year, vce(robust)
est sto loglog_yr_rb
reg lncigdyal lntobaccorrp lnincomeest i.econact year, vce(robust)
est sto loglog_econ_yr_rb
reg lncigdyal lntobaccorrp lnincomeest adban year, vce(robust)
est sto loglog_adban_yr_rb
reg lncigdyal lntobaccorrp lnincomeest i.econact adban year, vce(robust)
est sto loglog_all_rb
reg lncigdyal lntobaccorrp lnincomeest i.econact adban, vce(robust)
est sto loglog_econ_adban_rb
reg lncigdyal lntobaccorrp lnincomeest i.econact, vce(robust)
est sto loglog_econ_rb
reg lncigdyal lntobaccorrp lnincomeest adban, vce(robust)
est sto loglog_adban_rb
/* smokers only */
reg lncigdyal lntobaccorrp lnincomeest if smoker == 1, vce(robust)
est sto loglog_basic_rb_sm
reg lncigdyal lntobaccorrp lnincomeest year if smoker == 1, vce(robust)
est sto loglog_yr_rb_sm
reg lncigdyal lntobaccorrp lnincomeest i.econact year if smoker == 1, vce(robust)
est sto loglog_econ_yr_rb_sm
reg lncigdyal lntobaccorrp lnincomeest adban year if smoker == 1, vce(robust)
est sto loglog_adban_yr_rb_sm
reg lncigdyal lntobaccorrp lnincomeest i.econact adban year if smoker == 1, vce(robust)
est sto loglog_all_rb_sm
reg lncigdyal lntobaccorrp lnincomeest i.econact adban if smoker == 1, vce(robust)
est sto loglog_econ_adban_rb_sm
reg lncigdyal lntobaccorrp lnincomeest i.econact if smoker == 1, vce(robust)
est sto loglog_econ_rb_sm
reg lncigdyal lntobaccorrp lnincomeest adban if smoker == 1, vce(robust)
est sto loglog_adban_rb_sm

local regs4table "loglog_basic_rb loglog_yr_rb loglog_adban_rb loglog_adban_yr_rb loglog_adban_yr_rb_sm"
/* Create regressions table to compare models */
esttab `regs4table', nogap stats(r2 r2_a F) se replace

/* Save as text file */
esttab  `regs4table' using regressions_table.txt, onecell nogap stats(r2 r2_a F) se compress tab replace
/* Save to rich text format file (can be pasted into Word) */
esttab  `regs4table' using regressions_table.rtf, onecell nogap stats(r2 r2_a F) se compress tab replace

/* adban becomes significant when we examine smokers only */

/* l in 11550 4305   */
/* 4315   */
/* 3350   */
/* 11265   */
/* 8055   */
/* 683   */
/* 6543   */
/* 6355   */
/* 1468    */
/* -24.6954 */
/* -14.17279 */
/* -13.90557 */
/* -13.48999 */
/* -12.61699 */
/* 9.271697   */
/* 9.936737   */
/* 11.91025   */
/* 12.09676   */
/* 12.4031   */


/* Save Cooks distances to check for influential data */
/* predict cdistances1, cooksd */
/* Leverages plot */
/* lvr2plot */
/* graph export sresids1_leverages.eps, replace */


/* stop logging */
capture log close
