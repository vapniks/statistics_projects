# statistics_projects
A few example statistics projects using stata, eviews and R. 
Only the code files are included, not the reports.

  * **tobacco_taxation** : regression analysis of panel data from the "Health Survey for England" 1998-2007 to explore the 
	relationship between income and smoking, and the effects of taxation.
  * **abortion_and_crime** : analysis of the effects on crime rates of changes in US abortion law. This replicates a study by 
	Donahue & Levitt and uses their weighted abortion rate index, but with more recent data. The data was obtained from multiple 
	sources and merged together using R. 
  * **inflation_and_FDI** : analysis of non-linear and threshold effects of inflation on foreign direct investment. 
	This is a cross-country panel data study using robust dynamic panel models with instrumental variables (Arellano-Bover), 
	rolling regressions, thresholded and quadratic models, along with the appropriate diagnostic testing and graphs.
  * **inflation_and_financial_spreads** : forecasting inflation and GDP using financial spread variables. 
	Perform Chow breakpoint / QLR testing for coefficient stability, and assess forecast accuracy of several VAR models in loop.
  * **happiness_and_life_satisfaction** : ordered probit models to assess the impact of macroeconomic factors on self-reported
	happiness and life-satisfaction.
  * **reserve_ratios** : stability analysis of the German banking sector in the 19th century by fitting SVAR (Structured Impulse 
    Response Functions) models to reserve ratio time series, and examining the IRFs (Impulse Response Functions). 
	Several different structure matrices are tried for the IRFs (Impulse Response Functions), based on different criteria.
  * **student_survey** : factor analysis of university student survey data. Extracted factors are then used in a Gompit binary 
    response model (Gompit is used since the two classes are very unbalanced).

