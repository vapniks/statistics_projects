/* stata analysis of US abortion and crime data */

capture log close
local maindirectory "/home/projects/abortion_and_crime/"
local datadirectory "`maindirectory'"
    
clear all
cd "`maindirectory'"
set mem 1000m
set matsize 800
set more off
set linesize 200

/* start log and load data  */
log using "`maindirectory'stata_analysis.log", replace
use "`datadirectory'abortion_and_crime.dta", clear

/* create categorical variable for state */
encode state, generate(state2)

drop population4 

/* create new variables */
capture drop lgviolent lgproperty lgmurder lgrape lgrobbery lgassault lgburglary lglarceny lgauto lgunemp lgpovpercent laglgpolicerate stdabort pop_mean
gen lgviolent = log(violent)
gen lgproperty = log(property)
gen lgmurder = log(murder)
gen lgrape = log(rape)
gen lgrobbery = log(robbery)
gen lgassault = log(assault)
gen lgburglary = log(burglary)
gen lglarceny = log(larceny)
gen lgauto = log(auto)
gen policerate = numpolice / (population2/1000)
gen lgpolicerate = log(policerate)
gen lgunemp = log(unemp)
gen lgpovpercent = log(povpercent)
capture drop lgabort_assault lgabort_violent lgabort_murder lgabort_rape lgabort_robbery lgabort_property lgabort_burglary lgabort_larceny lgabort_auto
gen lgabort_assault = log(abortw1_assault)
gen lgabort_violent = log(abortw1_violent)
gen lgabort_murder = log(abortw1_murder)
gen lgabort_rape = log(abortw1_rape)
gen lgabort_robbery = log(abortw1_robbery)
gen lgabort_property = log(abortw1_property)
gen lgabort_burglary = log(abortw1_burglary)
gen lgabort_larceny = log(abortw1_larceny)
gen lgabort_auto = log(abortw1_auto)

center abortratio_inout, standardize generate(stdabort)
/* create variable to weight states by average population size */
egen pop_mean = mean(population2), by(state2)

/* descriptive statistics and graphs*/

histogram unemp, percent normal xtitle(Unemployment Rate) xlabel(#10) name(gunemp)
histogram povpercent, percent normal xtitle(Poverty Rate) xlabel(#10) name(gpovpercent)
histogram abortratio_inout, percent normal xtitle(Abortion Rate per 1000 live births) xlabel(#10) name(gabortion)
histogram policerate, percent normal xtitle(Police employees per 1000 residents) xlabel(#10) name(gpolicerate)

graph combine gunemp gpovpercent gabortion gpolicerate, rows(2) cols(2) name(gindepvars)
graph export "histograms_indepvars.png", replace
graph drop _all
 
histogram violent, percent normal xtitle(Violent crimes per 100000 people) xlabel(#10) name(gviolent)
histogram property, percent normal xtitle(Property crimes per 100000 people) xlabel(#10) name(gproperty)
histogram murder, percent normal xtitle(Murder crimes per 100000 people) xlabel(#10) name(gmurder)
histogram rape, percent normal xtitle(Rape crimes per 100000 people) xlabel(#10) name(grape)
histogram robbery, percent normal xtitle(Robbery crimes per 100000 people) xlabel(#10) name(grobbery)
histogram assault, percent normal xtitle(Assault crimes per 100000 people) xlabel(#10) name(gassault)
histogram burglary, percent normal xtitle(Burglary crimes per 100000 people) xlabel(#10) name(gburglary)
histogram larceny, percent normal xtitle(Larceny crimes per 100000 people) xlabel(#10) name(glarceny)
histogram auto, percent normal xtitle(Motor vehicle crimes per 100000 people) xlabel(#10) name(gauto)
   
graph combine gviolent gproperty gmurder grape grobbery gassault gburglary glarceny gauto, rows(3) cols(3) name(gcrimehis)
graph export "histogram_crime.png", replace name(gcrimehis)

/* summarize variables */
sum unemp povpercent abortratio_inout
sum violent property rape murder robbery burglary assault auto larceny

/* by state */
tabstat unemp povpercent abortratio_inout, by(state2)
tabstat violent property rape murder robbery burglary assault auto larceny, by(state2)


/* set panel and time var for panel data analysis */
xtset state2 year, yearly 
gen laglgpolicerate = L.lgpolicerate
graph drop _all

/* Abortion time series plots for four states */
tsline abortratio_inout if state=="new york" && year > 1960, title( Abortion rate in New york state) xtitle(Year) ytitle(Abortions per 1000 live births) name(gnewyorkabort)

tsline abortratio_inout if state=="california" && year > 1960, title( Abortion rate in California state) xtitle(Year) ytitle(Abortions per 1000 live births) name(gcaliforniaabort)
 
tsline abortratio_inout if state=="florida" && year > 1960, title( Abortion rate in Florida state) xtitle(Year) ytitle(Abortions per 1000 live births) name(gfloridaabort)

tsline abortratio_inout if state=="texas" && year > 1960, title( Abortion rate in Texas state) xtitle(Year) ytitle(Abortions per 1000 live births) name(gtexasabort) 

graph combine gnewyorkabort gcaliforniaabort gfloridaabort gtexasabort, rows(2) cols(2) name(gtsabort)
graph export "timeseries_plots_abort.eps", replace name(gtsabort)

/* Time series plots for different crime rates in four states */
tsline robbery if state=="new york" && year > 1960, title( Robbery Crime Rate in New york state) xtitle(Year) ytitle(Robbery per 100000) name(gnewyorkrobbery)

tsline robbery if state=="california" && year > 1960, title( Robbery Crime Rate in California state) xtitle(Year) ytitle(Robbery per 100000) name(gcaliforniarobbery)

tsline robbery if state=="florida" && year > 1960, title( Robbery Crime Rate in Florida state) xtitle(Year) ytitle(Robbery per 100000) name(gfloridarobbery)

tsline robbery if state=="texas" && year > 1960, title( Robbery Crime Rate in Texas state) xtitle(Year) ytitle(Robbery per 100000) name(gtexasrobbery) 

graph combine gnewyorkrobbery gcaliforniarobbery gfloridarobbery gtexasrobbery, rows(2) cols(2) name(gtsrobbery)

graph export "timeseries_plots_robbery.eps", replace name(gtsrobbery)

tsline violent if state=="new york" && year > 1960, title( violent Crime Rate in New york state) xtitle(Year) ytitle(violent per 100000) name(gnewyorkviolent)

tsline violent if state=="california" && year > 1960, title( violent Crime Rate in California state) xtitle(Year) ytitle(violent per 100000) name(gcaliforniaviolent)

tsline violent if state=="florida" && year > 1960, title( violent Crime Rate in Florida state) xtitle(Year) ytitle(violent per 100000) name(gfloridaviolent)

tsline violent if state=="texas" && year > 1960, title( violent Crime Rate in Texas state) xtitle(Year) ytitle(violent per 100000) name(gtexasviolent) 

graph combine gnewyorkviolent gcaliforniaviolent gfloridaviolent gtexasviolent, rows(2) cols(2) name(gtsviolent)

graph export "timeseries_plots_violent.eps", replace name(gtsviolent)

tsline murder if state=="new york" && year > 1960, title( murder Crime Rate in New york state) xtitle(Year) ytitle(murder per 100000) name(gnewyorkmurder)

tsline murder if state=="california" && year > 1960, title( murder Crime Rate in California state) xtitle(Year) ytitle(murder per 100000) name(gcaliforniamurder)

tsline murder if state=="florida" && year > 1960, title( murder Crime Rate in Florida state) xtitle(Year) ytitle(murder per 100000) name(gfloridamurder)

tsline murder if state=="texas" && year > 1960, title( murder Crime Rate in Texas state) xtitle(Year) ytitle(murder per 100000) name(gtexasmurder) 

graph combine gnewyorkmurder gcaliforniamurder gfloridamurder gtexasmurder, rows(2) cols(2) name(gtsmurder)

graph export "timeseries_plots_murder.eps", replace name(gtsmurder)

tsline rape if state=="new york" && year > 1960, title( rape Crime Rate in New york state) xtitle(Year) ytitle(rape per 100000) name(gnewyorkrape)

tsline rape if state=="california" && year > 1960, title( rape Crime Rate in California state) xtitle(Year) ytitle(rape per 100000) name(gcaliforniarape)

tsline rape if state=="florida" && year > 1960, title( rape Crime Rate in Florida state) xtitle(Year) ytitle(rape per 100000) name(gfloridarape)

tsline rape if state=="texas" && year > 1960, title( rape Crime Rate in Texas state) xtitle(Year) ytitle(rape per 100000) name(gtexasrape) 

graph combine gnewyorkrape gcaliforniarape gfloridarape gtexasrape, rows(2) cols(2) name(gtsrape)

graph export "timeseries_plots_rape.eps", replace name(gtsrape)

tsline larceny if state=="new york" && year > 1960, title( larceny Crime Rate in New york state) xtitle(Year) ytitle(larceny per 100000) name(gnewyorklarceny)

tsline larceny if state=="california" && year > 1960, title( larceny Crime Rate in California state) xtitle(Year) ytitle(larceny per 100000) name(gcalifornialarceny)

tsline larceny if state=="florida" && year > 1960, title( larceny Crime Rate in Florida state) xtitle(Year) ytitle(larceny per 100000) name(gfloridalarceny)

tsline larceny if state=="texas" && year > 1960, title( larceny Crime Rate in Texas state) xtitle(Year) ytitle(larceny per 100000) name(gtexaslarceny) 

graph combine gnewyorklarceny gcalifornialarceny gfloridalarceny gtexaslarceny, rows(2) cols(2) name(gtslarceny)

graph export "timeseries_plots_larceny.eps", replace name(gtslarceny)

tsline burglary if state=="new york" && year > 1960, title( burglary Crime Rate in New york state) xtitle(Year) ytitle(burglary per 100000) name(gnewyorkburglary)

tsline burglary if state=="california" && year > 1960, title( burglary Crime Rate in California state) xtitle(Year) ytitle(burglary per 100000) name(gcaliforniaburglary)

tsline burglary if state=="florida" && year > 1960, title( burglary Crime Rate in Florida state) xtitle(Year) ytitle(burglary per 100000) name(gfloridaburglary)

tsline burglary if state=="texas" && year > 1960, title( burglary Crime Rate in Texas state) xtitle(Year) ytitle(burglary per 100000) name(gtexasburglary) 

graph combine gnewyorkburglary gcaliforniaburglary gfloridaburglary gtexasburglary, rows(2) cols(2) name(gtsburglary)

graph export "timeseries_plots_burglary.eps", replace name(gtsburglary)

tsline auto if state=="new york" && year > 1960, title( auto Crime Rate in New york state) xtitle(Year) ytitle(auto per 100000) name(gnewyorkauto)

tsline auto if state=="california" && year > 1960, title( auto Crime Rate in California state) xtitle(Year) ytitle(auto per 100000) name(gcaliforniaauto)

tsline auto if state=="florida" && year > 1960, title( auto Crime Rate in Florida state) xtitle(Year) ytitle(auto per 100000) name(gfloridaauto)

tsline auto if state=="texas" && year > 1960, title( auto Crime Rate in Texas state) xtitle(Year) ytitle(auto per 100000) name(gtexasauto) 

graph combine gnewyorkauto gcaliforniaauto gfloridaauto gtexasauto, rows(2) cols(2) name(gtsauto)

graph export "timeseries_plots_auto.eps", replace name(gtsauto)

tsline property if state=="new york" && year > 1960, title( property Crime Rate in New york state) xtitle(Year) ytitle(property per 100000) name(gnewyorkproperty)

tsline property if state=="california" && year > 1960, title( property Crime Rate in California state) xtitle(Year) ytitle(property per 100000) name(gcaliforniaproperty)

tsline property if state=="florida" && year > 1960, title( property Crime Rate in Florida state) xtitle(Year) ytitle(property per 100000) name(gfloridaproperty)

tsline property if state=="texas" && year > 1960, title( property Crime Rate in Texas state) xtitle(Year) ytitle(property per 100000) name(gtexasproperty) 

graph combine gnewyorkproperty gcaliforniaproperty gfloridaproperty gtexasproperty, rows(2) cols(2) name(gtsproperty)

graph export "timeseries_plots_property.eps", replace name(gtsproperty)

tsline assault if state=="new york" && year > 1960, title( assault Crime Rate in New york state) xtitle(Year) ytitle(assault per 100000) name(gnewyorkassault)

tsline assault if state=="california" && year > 1960, title( assault Crime Rate in California state) xtitle(Year) ytitle(assault per 100000) name(gcaliforniaassault)

tsline assault if state=="florida" && year > 1960, title( assault Crime Rate in Florida state) xtitle(Year) ytitle(assault per 100000) name(gfloridaassault)

tsline assault if state=="texas" && year > 1960, title( assault Crime Rate in Texas state) xtitle(Year) ytitle(assault per 100000) name(gtexasassault) 

graph combine gnewyorkassault gcaliforniaassault gfloridaassault gtexasassault, rows(2) cols(2) name(gtsassault)

graph export "timeseries_plots_assault.eps", replace name(gtsassault)

/* All crime rate time series plots combined */
graph combine gtsrobbery gtsviolent gtsmurder gtsrape gtslarceny gtsauto gtsburglary gtsassault gtsproperty,rows(3) cols(3) name(gtsallcrime)

graph export "timeseries_plots_allcrime.eps", replace name(gtsallcrime)

/* Time series plots for control variables in California */
tsline unemp if state=="california" && year > 1960, title(Unemployment rate in California) xtitle(Year) ytitle(Unemployment rate) name(gcaliforniaunemp)

tsline povpercent if state=="california" && year > 1960, title(Poverty rate in California) xtitle(Year) ytitle(Poverty rate) name(gcaliforniapovpercent)

tsline policerate if state=="california" && year > 1960, title(Police employment rate in California) xtitle(Year) ytitle(Police employment) name(gcaliforniapolicerate)

graph combine gcaliforniaunemp gcaliforniapovpercent gcaliforniapolicerate, rows(2) cols(2) name(gtscalifornia)
graph export "timeseries_plots_california.eps", replace name(gtscalifornia)

/* temporarily collapse data by year to get time series plots of crime rates */
preserve
collapse (mean) violent property murder rape robbery assault burglary larceny auto, by(year)
tsset year, yearly

graph drop _all
local crimetsopts "xtitle(Year) ytitle(Average crimes rate)"
tsline violent, title(Violent crime) `crimetsopts' name(gtsviolent)
tsline property, title(Propery crime) `crimetsopts' name(gtsproperty)
tsline murder, title(Murder) `crimetsopts' name(gtsmurder)
tsline rape, title(Rape) `crimetsopts' name(gtsrape)
tsline robbery, title(Robbery) `crimetsopts' name(gtsrobbery)
tsline assault, title(Assault) `crimetsopts' name(gtsassault)
tsline burglary, title(Burglary) `crimetsopts' name(gtsburglary)
tsline larceny, title(Larceny) `crimetsopts' name(gtslarceny)
tsline auto, title(Motor vehicle crime) `crimetsopts' name(gtsauto)

graph combine gtsviolent gtsproperty gtsmurder gtsrape gtsrobbery gtsassault gtsburglary gtslarceny gtsauto, rows(3) cols(3) name(gtscrime)
graph export "timeseries_plots_crime.eps", replace name(gtscrime)
restore

/* PANEL DATA MODELLING */

/* Unit root tests */
xtunitroot fisher unemp, lags(2) pperron
xtunitroot fisher abortratio_inout, lags(2) pperron
xtunitroot fisher povpercent, lags(2) pperron
xtunitroot fisher policerate, lags(2) pperron

/* estimate fixed effects model */
xtreg lgrobbery abortw1_robbery unemp povpercent laglgpolicerate i.year, fe
estimates store femodel
capture drop fit1 fit2 resid1 resid2
predict fit1, xbu
predict resid1, e

/* test for heteroscedasticity */
xttest3
/* test for serial correlation */
xi: xtserial lgrobbery abortw1_robbery unemp povpercent lgpolicerate i.year

graph drop _all
scatter resid1 fit1, name(gresids1) title(Residuals against fitted values)
scatter resid1 abortw1_robbery, name(gresids2) title(Residuals against weighted abortion rate)
graph export "resids_vs_fitted.eps", name(gresids1) replace
graph export "resids_vs_abortw.eps", name(gresids2) replace

/* estimate random effects model */
xtreg lgrobbery abortw1_robbery unemp povpercent laglgpolicerate i.year, re
estimates store remodel

/* perform Hausman test to see if we can use random effects */
hausman femodel remodel

/* Some more models for comparison */
xtreg lgrobbery abortw1_robbery unemp povpercent laglgpolicerate i.year [aweight=pop_mean], fe vce(robust)
estimates store femodel1

xtreg lgrobbery abortw1_robbery unemp povpercent laglgpolicerate [aweight=pop_mean], fe vce(robust)
estimates store femodel2

xtreg lgrobbery abortw1_robbery unemp povpercent [aweight=pop_mean], fe vce(robust)
estimates store femodel3

xtreg lgrobbery abortw1_robbery [aweight=pop_mean], fe vce(robust)
estimates store femodel4

local xttableopts nogap se star(* 0.1 ** 0.05 *** 0.01) drop(*year*) replace scalars("F F" "p p-value" "r2_w R^2 within" "r2_b R^2 between" "r2_o R^2 overall" "r2_a adjusted R^2")

esttab femodel1 femodel2 femodel3 femodel4 using robbery_panel_models.rtf, `xttableopts' 

/* final models */
xtreg lgrobbery abortw1_robbery unemp povpercent laglgpolicerate i.year [aweight=pop_mean], fe vce(robust)
estimate store robberymodel

xtreg lgmurder abortw1_murder unemp povpercent laglgpolicerate i.year [aweight=pop_mean], fe vce(robust)
estimate store murdermodel

xtreg lgrape abortw1_rape unemp povpercent laglgpolicerate i.year [aweight=pop_mean], fe vce(robust)
estimate store rapemodel

xtreg lgassault abortw1_assault unemp povpercent laglgpolicerate i.year [aweight=pop_mean], fe vce(robust)
estimate store assaultmodel

xtreg lgproperty abortw1_property unemp povpercent laglgpolicerate i.year [aweight=pop_mean], fe vce(robust)
estimate store propertymodel

xtreg lglarceny abortw1_larceny unemp povpercent laglgpolicerate i.year [aweight=pop_mean], fe vce(robust)
estimate store larcenymodel

xtreg lgviolent abortw1_violent unemp povpercent laglgpolicerate i.year [aweight=pop_mean], fe vce(robust)
estimate store violentmodel

xtreg lgauto abortw1_auto unemp povpercent laglgpolicerate i.year [aweight=pop_mean], fe vce(robust)
estimate store automodel

xtreg lgburglary abortw1_burglary unemp povpercent laglgpolicerate i.year [aweight=pop_mean], fe vce(robust)
estimate store burglarymodel

esttab robberymodel murdermodel rapemodel assaultmodel propertymodel using all_panel_models1.rtf, `xttableopts'
esttab larcenymodel violentmodel automodel burglarymodel using all_panel_models2.rtf, `xttableopts'

reg lgrobbery abortw1_robbery unemp povpercent laglgpolicerate i.year [aweight=pop_mean], vce(robust)
estimate store robberymodel2

reg lgmurder abortw1_murder unemp povpercent laglgpolicerate i.year [aweight=pop_mean], vce(robust)
estimate store murdermodel2

reg lgrape abortw1_rape unemp povpercent laglgpolicerate i.year [aweight=pop_mean], vce(robust)
estimate store rapemodel2

reg lgassault abortw1_assault unemp povpercent laglgpolicerate i.year [aweight=pop_mean], vce(robust)
estimate store assaultmodel2

reg lgproperty abortw1_property unemp povpercent laglgpolicerate i.year [aweight=pop_mean], vce(robust)
estimate store propertymodel2

reg lglarceny abortw1_larceny unemp povpercent laglgpolicerate i.year [aweight=pop_mean], vce(robust)
estimate store larcenymodel2

reg lgviolent abortw1_violent unemp povpercent laglgpolicerate i.year [aweight=pop_mean], vce(robust)
estimate store violentmodel2

reg lgauto abortw1_auto unemp povpercent laglgpolicerate i.year [aweight=pop_mean], vce(robust)
estimate store automodel2

reg lgburglary abortw1_burglary unemp povpercent laglgpolicerate i.year [aweight=pop_mean], vce(robust)
estimate store burglarymodel2

local tableopts nogap se star(* 0.1 ** 0.05 *** 0.01) drop(*year*) replace scalars("F F" "p p-value" "r2 R^2" "r2_a Adjusted R^2")

esttab robberymodel2 murdermodel2 rapemodel2 assaultmodel2 propertymodel2 using non-panel_models1.rtf, `tableopts'
esttab larcenymodel2 violentmodel2 automodel2 burglarymodel2 using non-panel_models2.rtf, `tableopts'

