cap log close
local maindirectory "."
local datadirectory "./data"

clear all
cd "`maindirectory'"
set mem 1000m
set matsize 5000
set more off
set linesize 200

log using "`maindirectory'/stata.log", replace


/* Read csv data, add date variable and save as stata file */
/* insheet using "`datadirectory'/combined_data_4.csv", clear */
/* gen date = quarterly(time,"YQ") */
/* save `datadirectory'/combined_data.dta, replace */

use "`datadirectory'/combined_data.dta", clear	

local countries "fr ger it ch tu uk us"

/* sir   : * short-term interest rates                 */
/* bcby  : bank & corporate yield                      */
/* cpi   : * Consumer Price Index (cpi)                */
/* ip    : industrial production                       */
/* gdp   : * GDP constant prices                       */
/* spi   : share price index                           */
/* dy    : stock index dividend yield                  */
/* lir   : * long-term interest rates                  */
/* spg   : * share price index growth/return           */
/* rygd  : * reverse yield gap (dividend)              */
/* rygr  : * reverse yield gap (returns)               */
/* ysd   : * Yield spread differential                 */
/* cqs   : * credit quality spread                     */
/* fdbyd : * foreign domestic bond yield differential  */

local indicators "sir lir ysd fdbyd cqs cpi ip gdp spi dy rygd spg rygr bcby"
local corrindicators "cpi gdp sir lir rygd rygr cqs"

tsset date, quarterly

/* Create forward and backward lag variables for cross-correlations */
foreach country in `countries' {
  foreach ind in `corrindicators' {
    local var `country'`ind'
    cap confirm variable `var'
    if !_rc {
      foreach lag in 2 4 6 8 {
        gen `var'l`lag' = L`lag'.`var'
        gen `var'f`lag' = F`lag'.`var'
      }
    }
  }
}

/* Check */
/* l frcpi frcpil2 frcpil4 frcpil6 frcpil8 in 1/20 */
/* l frcpi frcpif2 frcpif4 frcpif6 frcpif8 in 1/20 */
/* l gercpi gercpil2 gercpil4 gercpil6 gercpil8 in 1/20 */
/* l gercpi gercpif2 gercpif4 gercpif6 gercpif8 in 1/20 */


/* Cross-correlations: */
foreach country in `countries' {
  foreach indicator in cpi gdp ys {
    local prefix `country'`indicator'
    pwcorr `prefix' L2.`prefix' L4.`prefix' L6.`prefix' L8.`prefix' F2.`prefix' F4.`prefix' F6.`prefix' F8.`prefix'
  }
}

/* Whole sample: 1950q1 to 2014q1 */
local startdate 1950q1
local enddate 2014q1

/* Capture all commands to reduce noise in output */

foreach country in `countries' {
    di as text "Country: `country'"
    foreach ind1 in gdp cpi {
        foreach ind2 in rygr sir lir spg ysd cqs fdbyd {
            local var1 `country'`ind1'
            local var2 `country'`ind2'
            di "Variables: `var1' `var2'"
            /* First fill in gaps in data by interpolating values */
            cap drop var1a
            cap drop var2a
            cap ipolate `var1' date, generate(var1a)
            cap ipolate `var2' date, generate(var2a)
            /* find AIC values for different lag lengths  */
            cap varsoc var1a var2a, maxlag(8)
            if _rc == 0 {
                /* save the results matrix */
                matrix stats=r(stats)
                /* find optimal lag length (the one with lowest AIC value) */
                local minaic = 1000000
                local bestlags = 1
                forvalues i = 1/9 {
                    if stats[`i',7] < `minaic' {
                        local minaic = stats[`i',7]
                        local bestlags = `i' - 1
                    }
                }
                /* Work out forecasting period (use last 12 quarters of available data) */
                cap var var1a var2a, lags(1/`bestlags')        
                local maxtime = `=e(tmax)'
                local mintime = `maxtime' - 12
                local firstbreak = `mintime' + 1
                local lastbreak = `mintime' + 10
                /* Granger causality test */
                cap reg var1a L(1/`bestlags').var1a L(1/`bestlags').var2a
                cap testparm L(1/`bestlags').var2a
                di "F-statistic for Granger causality test = `r(F)'"
                di "p-value for Granger causality test = `r(p)'"
                /* Chow breakpoint test / QLR test for coefficient stability */
                scalar maxf = 0
                forvalues t = `firstbreak'/`lastbreak' {
                    cap drop dummy 
                    cap drop interact1 interact2
                    cap gen dummy = (date>`t')
                    cap gen interact1 = dummy*var1a
                    cap gen interact2 = dummy*var2a
                    cap reg var1a L(1/`bestlags').var1a L(1/`bestlags').var2a dummy L(1/`bestlags').interact1 L(1/`bestlags').interact2
                    cap testparm dummy L(1/`bestlags').interact1 L(1/`bestlags').interact2
                    if maxf < r(F) {
                        scalar maxf = r(F)
                    }
                }
                di "QLR statistic = `=scalar(maxf)'"
                /* create some variables to store the forecasts in */
                cap drop yhat1var
                cap drop yhat4var
                cap drop yhat8var
                cap drop yhat1arima
                cap drop yhat4arima
                cap drop yhat8arima
                cap gen yhat1var = .
                cap gen yhat4var = .
                cap gen yhat8var = .
                cap gen yhat1arima = .
                cap gen yhat4arima = .
                cap gen yhat8arima = .
                /* Forecasts */
                forvalues t = `mintime'/`maxtime' {
                    /* VAR model */
                    cap var var1a var2a if date > tq(`startdate') & date <= `t', lags(1/`bestlags')
                    /* one/four/eight quarter ahead prediction */
                    cap drop fc_var1a
                    cap drop fc_var2a
                    cap fcast compute fc_, step(9) dynamic(`t')
                    cap replace yhat1var = fc_var1a if date == `t' + 1
                    cap replace yhat4var = fc_var1a if date == `t' + 4
                    cap replace yhat8var = fc_var1a if date == `t' + 8
                    /* ARIMA model (for comparison) */
                    cap arima var1a if date > tq(`startdate') & date <= `t', ar(1/`bestlags')
                    /* one/four/eight quarter ahead prediction */
                    cap drop yhat1temp
                    cap drop yhat4temp
                    cap drop yhat8temp
                    cap predict yhat1temp if date == `t' + 1, y
                    cap predict yhat4temp if date == `t' + 4, y dynamic(`t')
                    cap predict yhat8temp if date == `t' + 8, y dynamic(`t')
                    cap replace yhat1arima = yhat1temp if date == `t' + 1
                    cap replace yhat4arima = yhat4temp if date == `t' + 4
                    cap replace yhat8arima = yhat8temp if date == `t' + 8
                }
                /* Calculate RMSEs */
                cap drop yhat1varresid
                cap drop yhat1arimaresid
                cap drop yhat4varresid
                cap drop yhat4arimaresid
                cap drop yhat8varresid
                cap drop yhat8arimaresid
                /* 1 step ahead */
                cap drop nonmissing
                cap drop numnonmissing
                cap drop sumsqresid
                cap drop varrmse
                cap drop arrmse
                cap gen yhat1varresid = (yhat1var - var1a)*(yhat1var - var1a)
                cap gen nonmissing = 1 if !missing(yhat1varresid)
                cap egen numnonmissing = sum(nonmissing)
                cap egen sumsqresid = sum(yhat1varresid)
                cap gen varrmse = (sumsqresid/numnonmissing)^.5
                di "RMSE for 1 step ahead with var model = " varrmse
                cap gen yhat1arimaresid = (yhat1arima - var1a)*(yhat1arima - var1a)
                cap drop nonmissing numnonmissing sumsqresid 
                cap gen nonmissing = 1 if !missing(yhat1arimaresid)
                cap egen numnonmissing = sum(nonmissing)
                cap egen sumsqresid = sum(yhat1arimaresid)
                cap gen arrmse = (sumsqresid/numnonmissing)^.5
                di "RMSE for 1 step ahead with arima model = " arrmse
                di "Ratio of var RMSE to arima RMSE (1-step ahead) = " varrmse/arrmse
                /* 4 step ahead */
                cap drop nonmissing numnonmissing sumsqresid varrmse arrmse
                cap gen yhat4varresid = (yhat4var - var1a)*(yhat4var - var1a)
                cap gen nonmissing = 1 if !missing(yhat4varresid)
                cap egen numnonmissing = sum(nonmissing)
                cap egen sumsqresid = sum(yhat4varresid)
                cap gen varrmse = (sumsqresid/numnonmissing)^.5
                di "RMSE for 4 step ahead with var model = " varrmse
                cap gen yhat4arimaresid = (yhat4arima - var1a)*(yhat4arima - var1a)
                cap drop nonmissing numnonmissing sumsqresid
                cap gen nonmissing = 1 if !missing(yhat4arimaresid)
                cap egen numnonmissing = sum(nonmissing)
                cap egen sumsqresid = sum(yhat4arimaresid)
                cap gen arrmse = (sumsqresid/numnonmissing)^.5
                di "RMSE for 4 step ahead with arima model = " arrmse
                di "Ratio of var RMSE to arima RMSE (4-step ahead) = " varrmse/arrmse
                /* 8 step ahead */
                drop nonmissing numnonmissing sumsqresid varrmse arrmse
                cap gen yhat8varresid = (yhat8var - var1a)*(yhat8var - var1a)
                cap gen nonmissing = 1 if !missing(yhat8varresid)
                cap egen numnonmissing = sum(nonmissing)
                cap egen sumsqresid = sum(yhat8varresid)
                cap gen varrmse = (sumsqresid/numnonmissing)^.5
                di "RMSE for 8 step ahead with var model = " varrmse
                cap gen yhat8arimaresid = (yhat8arima - var1a)*(yhat8arima - var1a)
                cap drop nonmissing numnonmissing sumsqresid 
                cap gen nonmissing = 1 if !missing(yhat8arimaresid)
                cap egen numnonmissing = sum(nonmissing)
                cap egen sumsqresid = sum(yhat8arimaresid)
                cap gen arrmse = (sumsqresid/numnonmissing)^.5
                di "RMSE for 8 step ahead with arima model = " arrmse
                di "Ratio of var RMSE to arima RMSE (8-step ahead) = " varrmse/arrmse
            }
        }
    }

}

/* stop logging */
cap log close

