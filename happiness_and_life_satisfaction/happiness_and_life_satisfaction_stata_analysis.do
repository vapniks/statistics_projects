/* PRELIMINARY STUFF (set directories, memory, etc.) */
    
capture log close
local maindirectory "."
local datadirectory "`maindirectory'/data"
    
clear all
cd "`maindirectory'"
set mem 1000m
set matsize 800
set more off
set linesize 200

log using "`maindirectory'/stata_analysis.log", replace
use "`datadirectory'/dataset_04.dta", clear
/* DATA MUNGING - create new variables, macros etc. */
/** Set reference categories */
replace employment_status="" if employment_status=="NA"
replace gender="" if gender=="NA"
replace education="" if education=="NA"
replace marital_status="" if marital_status=="NA"
char employment_status[omit] "Full time"
char gender[omit] "Male"
char education[omit] "No formal education"
char marital_status[omit] "Married"
char ccode[omit] "JO" /* set country reference category to Jordan */
encode countrycode, generate(ccode)
/** Scale variables appropriately */
gen gdppc2 = gdppc / 1000
gen gdppcdiff2 = gdppcdiff / 1000
gen incomesq = income_category*income_category
/** New variables */
gen age2 = age*age
gen fulltime = (employment_status=="Full time")
gen unemployed = (employment_status=="Unemployed")
/* DATA CHECKS */
/** check for multicollinearity with other macroeconomic variables */
pwcorr gini gdppc2 unemployment inflation, sig /* gini is negatively correlated with all other macroeconomic vars */
collin unemployment inflation gdppc2 gini /* high level of multicollinearity */
/** check for panel unit roots in macroeconomic data */
preserve
use "wdidata.dta", clear
xtset countrycode year
sum
xtunitroot fisher gdppc, pperron lags(1) /* unit root! */
xtunitroot fisher unemployment, pperron lags(1) /* unit root! */
xtunitroot fisher inflation, pperron lags(1) /* no unit roots */
restore
/* DEFINE MODELS */
/** Set control variables */
local controlvars "age age2 i.education i.employment_status i.gender i.incomecat3 i.marital_status num_children"
local controlvars2 "age age2 i.education i.gender i.incomecat3 i.marital_status num_children"
local allvars "happiness_level life_satisfaction gdppc2 gdppcdiff2 unemployment inflation `controlvars'"
/** happiness models (1-4A) */
local model1vars "happiness_level gdppc2 unemployment inflation `controlvars'"
local model2vars "happiness_level gdppc2 gdppcdiff2 `controlvars'"
local model3vars "happiness_level gdppc2 unemployment inflation"
local model4vars "happiness_level gdppc2 gdppcdiff2"
local model4Avars "happiness_level `controlvars'"
local modelnames "model1 model2 model3 model4 model4A" /* update list of models */
/** life satisfaction models (5-8A) */
local model5vars "life_satisfaction gdppc2 unemployment inflation `controlvars'"
local model6vars "life_satisfaction gdppc2 gdppcdiff2 `controlvars'"
local model7vars "life_satisfaction gdppc2 unemployment inflation"
local model8vars "life_satisfaction gdppc2 gdppcdiff2"
local model8Avars "life_satisfaction `controlvars'"
local modelnames "`modelnames' model5 model6 model7 model8 model8A" /* update list of models */
/** life satisfaction models restricted to employed/unemployed (9-14) */
local model9vars "life_satisfaction gdppc2 unemployment inflation `controlvars2' if fulltime"
local model10vars "life_satisfaction gdppc2 unemployment inflation `controlvars2' if unemployed"
local model11vars "life_satisfaction gdppc2 c.gdppc2#unemployed unemployment c.unemployment#unemployed inflation c.inflation#unemployed `controlvars2'"
local model12vars "life_satisfaction gdppcdiff2 unemployment inflation `controlvars2' if fulltime"
local model13vars "life_satisfaction gdppcdiff2 unemployment inflation `controlvars2' if unemployed"
local model14vars "life_satisfaction gdppcdiff2 c.gdppcdiff2#unemployed unemployment c.unemployment#unemployed inflation c.inflation#unemployed `controlvars2'"
local modelnames "`modelnames' model9 model10 model11 model12 model13 model14" /* update list of models */
/** happiness models with income inequality (with and without controls to check model stability) (gini1-gini3) */
local modelgini1vars "happiness_level `controlvars' unemployment inflation gdppc2 equality finance_sa gini"
local modelgini2vars "happiness_level `controlvars' equality finance_sa gini"
local modelgini3vars "happiness_level unemployment inflation gdppc2 equality finance_sa gini"
local modelnames "`modelnames' modelgini1 modelgini2 modelgini3" /* update list of models */
/** life satisfaction models with income inequality (with and without controls to check model stability) (gini4-gini6) */
local modelgini4vars "life_satisfaction `controlvars' unemployment inflation gdppc2 equality finance_sa gini"
local modelgini5vars "life_satisfaction `controlvars' equality finance_sa gini"
local modelgini6vars "life_satisfaction unemployment inflation gdppc2 equality finance_sa gini"
local modelnames "`modelnames' modelgini4 modelgini5 modelgini6" /* update list of models */
/** happiness models with income inequality and health (gini7-gini9) */
local modelgini7vars "happiness_level `controlvars' life_expectancy unemployment inflation gdppc2 gini"
local modelgini8vars "happiness_level `controlvars' life_expectancy gini"
local modelgini9vars "happiness_level unemployment inflation life_expectancy gdppc2 gini"
local modelnames "`modelnames' modelgini7 modelgini8 modelgini9" /* update list of models */
/** life satisfaction models with income inequality and health (gini10-gini12) */
local modelgini10vars "life_satisfaction `controlvars' life_expectancy unemployment inflation gdppc2 gini"
local modelgini11vars "life_satisfaction `controlvars' life_expectancy gini"
local modelgini12vars "life_satisfaction unemployment inflation gdppc2 finance_sa gini"
local modelnames "`modelnames' modelgini10 modelgini11 modelgini12" /* update list of models */
/** corruption models (corrupt1-corrupt2) **/
local modelcorrupt1vars "life_satisfaction gini life_expectancy corruption"
local modelcorrupt1Avars "life_satisfaction gini life_expectancy corruption `controlvars'"
local modelcorrupt2vars "life_satisfaction gdppc2 unemployment inflation gini life_expectancy corruption"
local modelcorrupt2Avars "life_satisfaction gdppc2 unemployment inflation gini life_expectancy corruption `controlvars'"
local modelcorrupt3vars "happiness_level gini life_expectancy corruption"
local modelcorrupt3Avars "happiness_level gini life_expectancy corruption `controlvars'"
local modelcorrupt4vars "happiness_level gdppc2 unemployment inflation gini life_expectancy corruption"
local modelcorrupt4Avars "happiness_level gdppc2 unemployment inflation gini life_expectancy corruption `controlvars'"
local modelnames "`modelnames' modelcorrupt1 modelcorrupt2 modelcorrupt3 modelcorrupt4 modelcorrupt1A modelcorrupt2A modelcorrupt3A modelcorrupt4A"
/* ESTIMATE AND STORE ORDERED PROBIT MODELS */
local conditionwords "if fulltime unemployed"    
foreach name in `modelnames' {
    /* save models without country effects */
    di "`name'"
    eststo `name': quietly xi: oprobit ``name'vars'
    /* save models with country effect */
    di "`name'_panel"
    local condition : list `name'vars & conditionwords
    local withoutcondition : list `name'vars - conditionwords
    eststo `name'_panel : quietly xi: oprobit `withoutcondition' i.countrycode `condition'
    if !regexm("``name'vars'","#") {
        /* save marginal effects */
        di "`name'_me"
        quietly : meoprobit
        mat `name'_me = RES
        mat2txt, matrix(`name'_me) saving(`name'_me.txt) replace
    }
}
/* OUTPUT TABLES */
/** macros for table options */
local commonopts "label nogap onecell se pr2 star(* 0.1 ** 0.05 *** 0.01) replace"
local orderopt "order(gdppc2 gdppcdiff2 unemployment inflation age age2)"
local dropopt "drop(*marital* *education* *employmen_* *children* *gender* *age* *incomecat*)"
/** happiness and life satisfaction without any macroeconomic variables (with country effects) */
esttab model4A_panel model8A_panel using models_without_macroeconomic_vars.rtf, mtitles("Happiness level" "Life satisfaction") `commonopts'
/** happiness and life satisfaction, without income inequality but with control vars */
esttab model1 model2 model5 model6 using main_models.rtf, mtitles("Happiness level with unemployment & inflation" "Happiness level with diff(GDP)" "Life satisfaction with unemployment & inflation" "Life satisfaction with diff(GDP)") `commonopts' `orderopt'
/** now repeat but including country effects */
esttab model1_panel model2_panel model5_panel model6_panel using main_models_panel.rtf, mtitles("Happiness level with unemployment & inflation" "Happiness level with diff(GDP)" "Life satisfaction with unemployment & inflation" "Life satisfaction with diff(GDP)") `commonopts' `orderopt'    
/** happiness and life satisfaction, without income inequality but with control vars */
esttab model3_panel model4_panel model7_panel model8_panel using main_models_no_personal_characteristics_panel.rtf, mtitles("Happiness level (no personal characteristics)" "Happiness level (no personal characteristics)" "Life satisfaction (no personal characteristics)" "Life satisfaction (no personal characteristics)") `commonopts' `orderopt'
/** happiness level with income inequality */
esttab modelgini1_panel modelgini2_panel modelgini3_panel using income_inequality_happiness_panel.rtf, mtitles("happiness_level (with controls)" "happiness_level (with controls)" "happiness_level (without controls)") `commonopts' `dropopt'
/** life satisfaction with income inequality */
esttab modelgini4_panel modelgini5_panel modelgini6_panel using income_inequality_life_satisfaction_panel.rtf, mtitles("life_satisfaction (with controls)" "life_satisfaction (with controls)" "life_satisfaction (without controls)") `commonopts' `dropopt'
/** happiness level with income inequality and health var */
esttab modelgini7_panel modelgini8_panel modelgini9_panel using income_inequality_happiness_health_education_panel.rtf, mtitles("happiness_level (with controls)" "happiness_level (with controls)" "happiness_level (without controls)") `commonopts' `dropopt'
/** life satisfaction with income inequality and health var */
esttab modelgini10_panel modelgini11_panel modelgini12_panel using income_inequality_life_satisfaction_heath_education_panel.rtf, mtitles("life_satisfaction (with controls)" "life_satisfaction (with controls)" "life_satisfaction (without controls)") `commonopts' `dropopt'
/** restricted to employed/unemployed */
esttab model9_panel model10_panel model12_panel model13_panel using employed_vs_unemployed_panel.rtf, mtitles("life satisfaction (employed)" "life satisfaction (unemployed)" "life satisfaction (employed)" "life satisfaction (unemployed)") `commonopts' `orderopt'
/** interaction with unemployed dummy var */
esttab model11_panel model14_panel using employed_unemployed_gap_panel.rtf, `commonopts' 
/** corruption models */
esttab modelcorrupt1 modelcorrupt1A modelcorrupt2 modelcorrupt2A using corruption_life-satisfaction_models.rtf, `commonopts'
esttab modelcorrupt3 modelcorrupt3A modelcorrupt4 modelcorrupt4A using corruption_happiness_models.rtf, `commonopts'
esttab modelcorrupt1_panel modelcorrupt1A_panel modelcorrupt2_panel modelcorrupt2A_panel using corruption_life-satisfaction_models_panel.rtf, `commonopts'
esttab modelcorrupt3_panel modelcorrupt3A_panel modelcorrupt4_panel modelcorrupt4A_panel using corruption_happiness_models_panel.rtf, `commonopts'
/*** comparison of panel and non-panel models ***/
esttab modelcorrupt2A modelcorrupt2A_panel, `commonopts' 
esttab modelcorrupt4A modelcorrupt4A_panel, `commonopts'
/* STOP LOGGING */
capture log close

