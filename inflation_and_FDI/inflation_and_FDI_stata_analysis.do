/* Dynamic panel data analysis */

capture log close
log using "stata_output.log", replace

clear all
set mem 50m
set more off
set linesize 200

use paneldata.dta, clear

/* set indices for panel and creat time dummy's */
tsset iso2c year2 , yearly
xi i.year2

/* create new variables for threshold and quadratic regression */
drop threshdummy log_cpiXdummy logcpisqr
gen threshdummy = (cpi_growth > 10 & cpi_growth < 45) if !missing(cpi_growth)
gen log_cpiXdummy = log_cpi_growth * threshdummy
gen logcpisqr = log_cpi_growth * log_cpi_growth
gen logcpisqrXdummy = log_cpi_growth * log_cpi_growth * threshdummy

/* Unit root tests */
xtunitroot fisher log_cpi_growth, dfuller lags(1) 
xtunitroot fisher log_fdi, dfuller lags(1)
gen Dlog_fdi = D.log_fdi
xtunitroot fisher Dlog_fdi, dfuller lags(1)
xtunitroot fisher log_gdp, dfuller lags(1)
gen Dlog_gdp = D.log_gdp
xtunitroot fisher Dlog_gdp, dfuller lags(1)
xtunitroot fisher log_interest_rate, dfuller lags(1)
xtunitroot fisher log_exchange_rate, dfuller lags(1)
xtunitroot fisher log_gov_consumption, dfuller lags(1)
xtunitroot fisher log_imports, dfuller lags(1)
xtunitroot fisher log_exports, dfuller lags(1)
xtunitroot fisher log_openness, dfuller lags(1) 

/* macros for regressions */
local xtabond2opts small twostep robust orthogonal artests(5) noconstant 
local fixedvars1 D.log_fdi log_cpi_growth D.log_gdp log_interest_rate log_exchange_rate log_gov_consumption _Iyear2* LD.log_fdi
local fixedvars2 D.log_fdi log_cpi_growth D.log_gdp log_interest_rate log_exchange_rate log_gov_consumption _Iyear2* LD.log_fdi L2D.log_fdi
local varsnocpi2 D.log_fdi D.log_gdp log_interest_rate log_exchange_rate log_gov_consumption _Iyear2* LD.log_fdi L2D.log_fdi L.log_exports
local gmmivs1 gmmstyle(LD.log_fdi log_cpi_growth LD.log_gdp, collapse eq(level))
local gmmivs2 gmmstyle(L2D.log_fdi log_cpi_growth LD.log_gdp, collapse eq(level))
local gmmivsnocpi1 gmmstyle(LD.log_fdi LD.log_gdp, collapse eq(level))
local gmmivsnocpi2 gmmstyle(L2D.log_fdi LD.log_gdp, collapse eq(level))
local cpiXdummyiv gmmstyle(log_cpiXdummy, collapse eq(level))
local threshiv ivstyle(threshdummy, eq(level))
local normalivs ivstyle(log_interest_rate log_exchange_rate log_gov_consumption, eq(level))
local openiv ivstyle(L.log_openness, eq(level))
local expiv ivstyle(L.log_exports, eq(level))
local impiv ivstyle(L.log_imports, eq(level))
local expimpiv ivstyle(L.log_exports L.log_imports, eq(level))

/* store regression outputs */
eststo clear
/* with 1 lag of fdi  */
eststo estlag1open: xtabond2 `fixedvars1' L.log_openness, `gmmivs1' `normalivs' `openiv' `xtabond2opts'
eststo estlag1exp: xtabond2 `fixedvars1' L.log_exports, `gmmivs1' `normalivs' `expiv' `xtabond2opts'
eststo estlag1imp: xtabond2 `fixedvars1' L.log_imports, `gmmivs1' `normalivs' `impiv' `xtabond2opts'
eststo estlag1expimp: xtabond2 `fixedvars1' L.log_exports L.log_imports, `gmmivs1' `normalivs' `expimpiv' `xtabond2opts'
/* with 2 lags of fdi  */
eststo estlag2open: xtabond2 `fixedvars2' L.log_openness, `gmmivs2' `normalivs' `openiv' `xtabond2opts'
eststo estlag2exp: xtabond2 `fixedvars2' L.log_exports, `gmmivs2' `normalivs' `expiv' `xtabond2opts'
eststo estlag2imp: xtabond2 `fixedvars2' L.log_imports, `gmmivs2' `normalivs' `impiv' `xtabond2opts'
eststo estlag2expimp: xtabond2 `fixedvars2' L.log_exports L.log_imports, `gmmivs2' `normalivs' `expimpiv' `xtabond2opts'
/* threshold and quadratic regressions */
eststo estcpiXonly: xtabond2 `varsnocpi2' log_cpiXdummy, `gmmivsnocpi2' `cpiXdummyiv' `normalivs' `threshiv' `expiv' `xtabond2opts'
eststo estthreshonly: xtabond2 `varsnocpi2' threshdummy, `gmmivsnocpi2' `normalivs' `threshiv' `expiv' `xtabond2opts'
eststo estthresh: xtabond2 `fixedvars2' threshdummy L.log_exports, `gmmivs2' `normalivs' `threshiv' `expiv' `xtabond2opts'
eststo estcpiX: xtabond2 `fixedvars2' log_cpiXdummy L.log_exports, `gmmivs2' `cpiXdummyiv' `normalivs' `threshiv' `expiv' `xtabond2opts'
eststo estthreshall: xtabond2 `fixedvars2' threshdummy log_cpiXdummy L.log_exports, `gmmivs2' `cpiXdummyiv' `normalivs' `threshiv' `expiv' `xtabond2opts'
eststo estsqr: xtabond2 `fixedvars2' logcpisqr L.log_exports, gmmstyle(L2D.log_fdi log_cpi_growth LD.log_gdp logcpisqr, collapse eq(level)) `normalivs' `threshiv' `expiv' `xtabond2opts'
eststo estsqrXdummy: xtabond2 `varsnocpi2' log_cpiXdummy logcpisqrXdummy, gmmstyle(L2D.log_fdi L.log_cpi_growth logcpisqr LD.log_gdp, collapse eq(level)) `normalivs' `threshiv' `expiv' `xtabond2opts'

/* save latex tables */
local esttabopts tex replace drop(_Iyear2*) scalars(F F_p hansen hansenp sargan sarganp ar1 ar1p ar2 ar2p) coeflabels(D.log_fdi "$\Delta\log(FDI)$" log_cpi_growth "log(inflation)" D.log_gdp "$\Delta\log(GDP)$" log_interest_rate "log(interest rate)" log_exchange_rate "log(PPP)" log_gov_consumption "log(gov consumption)" LD.log_fdi "$\Delta\log(FDI)[-1]$" L2D.log_fdi "$\Delta\log(FDI)[-2]$" L.log_exports "log(exports)[-1]" L.log_imports "log(imports)[-1]" L.log_openness "log(exports+imports)[-1]" threshdummy "dummy" log_cpiXdummy "$\log(inflation)\times dummy$" logcpisqr "$\log(inflation)^2$" logcpisqrXdummy "$\log(inflation)^2\times dummy$") b(3) t(3)
drop _est_estlag*
drop __000008
esttab estlag1* using 1lag_GMM_table.tex, `esttabopts'
drop __000008
esttab estlag2* using 2lags_GMM_table.tex, `esttabopts'
drop _est_estlag*
drop __000008
esttab estlag2exp estcpiXonly estcpiX estsqr estsqrXdummy using theshold_GMM_table.tex, `esttabopts'

