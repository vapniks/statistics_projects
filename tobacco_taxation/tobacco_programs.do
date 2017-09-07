

/* Program for performing diagnostic tests after estimating model */
/* It takes one argument which is the name to use for storing residuals & graphs */  
capture program drop dodiagnostics
program dodiagnostics, nclass
/* First store the model */
estimates store `1'_model
/* save model r-squared and F-value */
scalar r2_value = e(r2)
scalar f_value = e(F)
capture confirm variable `1'_resids
if !_rc {
  drop `1'_resids
}
/* store standardised residuals */
predict `1'_resids, rstandard
/* Ramsey RESET test */
capture {  
    estat ovtest
}
if !_rc {
    scalar reset_f_value = r(F)
    scalar reset_p_value = r(p)
    scalar reset_df1 = r(df)
    scalar reset_df2 = r(df_r)
}
/* VIF values (don't store these) */
estat vif
/* White test for heteroskedasticity */
capture {  
    whitetst
}
if !_rc {
    scalar white_p_value = r(p)
    scalar white_chisq_value = r(white)
    scalar white_df = r(df)
}
/* Breusch-Pagan heteroskedasticity test */
capture {
    estat hettest
}
if !_rc {
    scalar breusch_pagan_p_value = r(p)
    scalar breusch_pagan_chisq_value = r(chi2)
    scalar breusch_pagan_df = r(df)
}
/* Breusch-Godfrey autocorrelation test */
capture {
    estat bgodfrey
}
if !_rc {
    scalar breusch_godfrey_p_value = el(r(p),1,1)
    scalar breusch_godfrey_df = el(r(df),1,1)
    scalar breusch_godfrey_chisq_value = el(r(chi2),1,1)
}
/* Durbin-Watson test */
capture {
    estat dwatson
}
if !_rc {
    scalar dwatson_dw = r(dw)
    scalar dwatson_k = r(k)
    scalar dwatson_n = r(n)
}
/* Akaike information criteria */
capture {
    estat ic
}
if !_rc {
    scalar aic_value = el(r(S),1,5)
    scalar bic_value = el(r(S),1,6)
}
/* Shapiro-Wilks test */
capture {
    swilk `1'_resids
}
if !_rc {
    scalar swilk_p_value = r(p)
    scalar swilk_z = r(z)
}
/* Histogram of residuals */
capture {  
    histogram `1'_resids, normal
}
if !_rc {
    graph export `1'_resids_hist.eps, replace
}
/* Q-Q plot for residuals */
capture {
    qnorm `1'_resids
}
if !_rc {
    graph export `1'_resids_QQ.eps, replace
}
log using `1'.log, replace
di "*! Diagnostics for `1' model"
di "*! "
di "*! R-squared for model: " r2_value
di "*! F-value for model: " f_value
di "*! AIC for model: " aic_value
di "*! BIC for model: " bic_value
di "*! Ramsey RESET test: F(" reset_df1 "," reset_df2 ") = " reset_f_value ", p = " reset_p_value
di "*! White test: Chisq(" white_df ") = " white_chisq_value ", p = " white_p_value
di "*! Breusch-Pagan test: Chisq(" breusch_pagan_df ") = " breusch_pagan_chisq_value ", p = " breusch_pagan_p_value
di "*! Breusch-Godfrey test: Chisq(" breusch_godfrey_df ") = " breusch_godfrey_chisq_value ", p = " breusch_godfrey_p_value
di "*! Durbin-Watson test: Durbin-Watson statistic(" dwatson_k "," dwatson_n ") = " dwatson_dw
di "*! Shapiro-Wilks test: Z-value = " swilk_z ", p = " swilk_p_value
di " "
di " Histogram of residuals stored in `1'_resids_hist.eps"
di " QQ plot for residuals stored in `1'_resids_QQ.eps"
di " Diagnostic output stored in `1'.log, to view file enter: type `1'.log, starbang"
log close
scalar drop _all
end


/* Program for performing diagnostic tests after estimating model with only smokers */
/* It takes one argument which is the name to use for storing residuals & graphs */
/* The model should be estimated just before running this command */
capture program drop dodiagnostics_smokers
program dodiagnostics_smokers, nclass
/* First store the model */
estimates store `1'_model
/* save model r-squared and F-value */
scalar r2_value = e(r2)
scalar f_value = e(F)
capture confirm variable `1'_smoker_resids
if !_rc{
  drop `1'_smoker_resids
}
/* store standardised residuals */
predict `1'_smoker_resids if smoker == 1, rstandard
/* Ramsey RESET test */
estat ovtest
scalar reset_f_value = r(F)
scalar reset_p_value = r(p)
scalar reset_df1 = r(df)
scalar reset_df2 = r(df_r)
/* VIF values (don't store these) */
estat vif
/* White test for heteroskedasticity */
whitetst
scalar white_p_value = r(p)
scalar white_chisq_value = r(white)
scalar white_df = r(df)
/* Breusch-Pagan heteroskedasticity test */
estat hettest
scalar breusch_pagan_p_value = r(p)
scalar breusch_pagan_chisq_value = r(chi2)
scalar breusch_pagan_df = r(df)
/* Breusch-Godfrey autocorrelation test */
estat bgodfrey
scalar breusch_godfrey_p_value = el(r(p),1,1)
scalar breusch_godfrey_df = el(r(df),1,1)
scalar breusch_godfrey_chisq_value = el(r(chi2),1,1)
/* Durbin-Watson test */
estat dwatson
scalar dwatson_dw = r(dw)
scalar dwatson_k = r(k)
scalar dwatson_n = r(n)
/* Akaike information criteria */
estat ic
scalar aic_value = el(r(S),1,5)
scalar bic_value = el(r(S),1,6)
/* Shapiro-Wilks test */
swilk `1'_smoker_resids
scalar swilk_p_value = r(p)
scalar swilk_z = r(z)
/* Histogram of residuals */
histogram `1'_smoker_resids if smoker == 1, normal
graph export `1'_smoker_resids_hist.eps, replace
/* Q-Q plot for residuals */
qnorm `1'_smoker_resids if smoker == 1
graph export `1'_smoker_resids_QQ.eps, replace
log using `1'.log, replace
di "*!  Diagnostics for `1' model"
di "*!  "
di "*! R-squared for model: " r2_value
di "*! F-value for model: " f_value
di "*! AIC for model: " aic_value
di "*! BIC for model: " bic_value
di "*! Ramsey RESET test: F(" reset_df1 "," reset_df2 ") = " reset_f_value ", p = " reset_p_value
di "*! White test: Chisq(" white_df ") = " white_chisq_value ", p = " white_p_value
di "*! Breusch-Pagan test: Chisq(" breusch_pagan_df ") = " breusch_pagan_chisq_value ", p = " breusch_pagan_p_value
di "*! Breusch-Godfrey test: Chisq(" breusch_godfrey_df ") = " breusch_godfrey_chisq_value ", p = " breusch_godfrey_p_value
di "*! Durbin-Watson test: Durbin-Watson statistic(" dwatson_k "," dwatson_n ") = " dwatson_dw
di "*! Shapiro-Wilks test: Z-value = " swilk_z ", p = " swilk_p_value
di " "
di "Histogram of residuals stored in `1'_smoker_resids_hist.eps"
di "QQ plot for residuals stored in `1'_smoker_resids_QQ.eps"
scalar drop _all
log close
end

