
' Code for performing LM test for heteroskedasticity on the residuals of a binary choice model (logistic regression)
' H0 : No heteroskedasticity
' H1 : heteroskedasticity in the form var(ui) = exp(2*Q87*\gamma)
' (see page 265 of Eviews 7 Users Guide II)

' Set the sample
smpl 1 5488
' Estimate the model with options: d=1 (logistic regression), and h (Huber/White robust covariances) 
' (use d=p for probit model, and d=x for gompit model)

equation reg1.binary(d=l,h) Q5 C Q38 Q86 Q87 Q88 Q89 Q90 Q91 Q92 Q93 Q94 Q214B

' Save the standardized residuals from the model in stdresid
reg1.makeresids(s) stdres
' Calculate the fitted probabilities and index (logit) and save them in p_hat and xb respectively
reg1.forecast p_hat
reg1.forecast(i) xb
' Calculate factor in auxilliary regression
series fac=@dnorm(-xb)/@sqrt(p_hat*(1-p_hat))

' Estimate auxilliary regression
equation reg2.ls stdres fac fac*Q38 fac*Q86 fac*Q87 fac*Q88 fac*Q89 fac*Q90 fac*Q91 fac*Q92 fac*Q93 fac*Q94 fac*Q214B Q87*(-xb)*fac
' Calculate fitted values of auxilliary regression
reg2.forecast fitted
' LM test statistic is the sum squares of these fitted values
scalar lm_test=@sumsq(fitted)
' Calculate the p-value for the test (based on Chi-squared distribution with 1df)
scalar p_value = 1-@cchisq(lm_test,1)



