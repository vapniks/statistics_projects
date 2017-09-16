capture log close
local maindirectory "/home/projects/reserve_ratios"
local datadirectory "/home/projects/reserve_ratios"
clear all
cd "`maindirectory'"
set mem 1000m
set matsize 800
set more off
set linesize 200
log using "`maindirectory'/stata_analysis.log", replace
use "`datadirectory'/bankdata3.dta", clear

sum

varsoc rbresratio sbresratio bbresratio wbresratio babresratio fbresratio bsresratio, maxlag(10)

quietly var rbresratio sbresratio bbresratio wbresratio babresratio fbresratio bsresratio
vargranger

/* useful macros */
local modelvars rbresratio sbresratio bbresratio wbresratio babresratio fbresratio bsresratio
local modelopts "lags(1) dfk small"
/* lower triangular A matrices for recursive causal chain in short-run dynamics (first var is most independent and causal) */
/* A1 is ordered by sum distances (largest sum most causal): RB SB BB WB BAB FB BS */
/*              RB(1)           SB(2)        BB(3)        WB(4)        BAB(5)        FB(6)        BS(7)    */
matrix A1=[1,0,0,0,0,0,0\.,1,0,0,0,0,0\.,.,1,0,0,0,0\.,.,.,1,0,0,0\.,.,.,.,1,0,0\.,.,.,.,.,1,0\.,.,.,.,.,.,1]
/* A2 is ordered by distance from RB (closest is most causal): RB SB FB BS BAB BB WB */
/*              RB(1)           SB(2)        BB(6)        WB(7)        BAB(5)        FB(3)        BS(4)    */
matrix A2 = [1,0,0,0,0,0,0\.,1,0,0,0,0,0\.,.,.,.,.,1,0\.,.,.,.,.,.,1\.,.,.,.,1,0,0\.,.,1,0,0,0,0\.,.,.,1,0,0,0]
/* A3 is ordered by distance from RB (closest is most causal): RB SB FB BS BAB BB WB */
/*              RB(1)           SB(2)        BB(6)        WB(7)        BAB(5)        FB(3)        BS(4)    */
matrix A3 = [1,0,0,0,0,0,0\.,1,0,0,0,0,0\.,.,.,.,.,1,0\.,.,.,.,.,.,1\.,.,.,.,1,0,0\.,.,1,0,0,0,0\.,.,.,1,0,0,0]
/* A4 is ordered using chi^2 values from Granger causality tests : FB SB BAB WB RB BB BS */
/*              RB(5)           SB(2)        BB(6)        WB(4)        BAB(3)        FB(1)        BS(7)    */
matrix A4 = [.,.,.,.,1,0,0\.,1,0,0,0,0,0\.,.,.,.,.,1,0\.,.,.,1,0,0,0\.,.,1,0,0,0,0\1,0,0,0,0,0,0\.,.,.,.,.,.,1]
/* A5 is ordered using p-values from Granger causality tests : FB BAB BB WB SB RB BS
/*              RB(6)           SB(5)        BB(3)        WB(4)        BAB(2)        FB(1)        BS(7)    */
matrix A5 = [.,.,.,.,.,1,0\.,.,.,.,1,0,0\.,.,1,0,0,0,0\.,.,.,1,0,0,0\.,1,0,0,0,0,0\1,0,0,0,0,0,0\.,.,.,.,.,.,1]
/* diagonal B matrix for scaling the impulses */
matrix B=[.,0,0,0,0,0,0\0,.,0,0,0,0,0\0,0,.,0,0,0,0\0,0,0,.,0,0,0\0,0,0,0,.,0,0\0,0,0,0,0,.,0\0,0,0,0,0,0,.]
/* lower-triangular C1 matrix for recursive causal chain in long-run dynamics (first var is most independent and causal) */
/*              RB(1)           SB(2)        BB(3)        WB(4)        BAB(5)        FB(6)        BS(7)    */
matrix C1=[.,0,0,0,0,0,0\.,.,0,0,0,0,0\.,.,.,0,0,0,0\.,.,.,.,0,0,0\.,.,.,.,.,0,0\.,.,.,.,.,.,0\.,.,.,.,.,.,.]
/* label rows and columns of matrices */
foreach mat in A1 A2 A3 A4 A5 B C1 {
   matrix rownames `mat' = `modelvars'
   matrix colnames `mat' = `modelvars'
}

eststo var1: quietly var `modelvars', `modelopts'
eststo svar_short1: quietly svar `modelvars', `modelopts' aeq(A1) beq(B) 
eststo svar_short2: quietly svar `modelvars', `modelopts' aeq(A2) beq(B) 
eststo svar_short3: quietly svar `modelvars', `modelopts' aeq(A3) beq(B) 
eststo svar_short4: quietly svar `modelvars', `modelopts' aeq(A4) beq(B) 
eststo svar_short5: quietly svar `modelvars', `modelopts' aeq(A5) beq(B) 
eststo svar_long1: quietly svar `modelvars', `modelopts' lreq(C1)

irf create var1_irfs, set(irfs) step(10) estimates(var1)
irf create svar_s1_irfs, set(irfs) step(10) estimates(svar_short1)
irf create svar_s2_irfs, set(irfs) step(10) estimates(svar_short2)
irf create svar_s3_irfs, set(irfs) step(10) estimates(svar_short3)
irf create svar_s4_irfs, set(irfs) step(10) estimates(svar_short4)
irf create svar_s5_irfs, set(irfs) step(10) estimates(svar_short5)
irf create svar_l1_irfs, set(irfs) step(10) estimates(svar_long1)
irf set irfs

/* IRF graph for normal VAR */
irf graph irf, irf(var1_irfs)
graph save var1_irfs, replace
graph export var1_irfs.eps, replace mag(150)
/* IRF graphs for SVAR's */
foreach name in s1 s2 s3 s4 s5 l1 {
   display "creating `name' graph"
   irf graph sirf, irf(svar_`name'_irfs)
   graph save svar_`name'_irfs, replace
   display "exporting `name' graph"
   graph export svar_`name'_irfs.eps, replace mag(150)
}
