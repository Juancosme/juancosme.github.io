*******************************************************
* cps2024_credentials — run_04_make_tables.do
* Stata 17
*******************************************************
version 17
set more off
clear all
capture log close _all

cd "C:\Users\juanc\Documents\cps2024_credentials"
capture mkdir results
capture mkdir temp
log using "results\run_04_make_tables.smcl", replace

*------------------------------*
* Load data
*------------------------------*
use "temp\analysis_ready.dta", clear

* College (BA+) flag from educ
capture drop college
decode educ, gen(__educ_str)
gen strL __educ_l = lower(__educ_str)
gen byte college = regexm(__educ_l, "bachelor|master|professional|doctor")
label var college "College (Bachelor's or higher)"
drop __educ_*

* Assignment sample: 18–65
keep if inrange(age,18,65)

*------------------------------*
* Model (1): OLS
*------------------------------*
estimates clear
reg lnwage license c.age c.agesq black female college [pw=asecwt], vce(robust)
scalar beta_short = _b[license]
scalar R2_short   = e(r2)
estimates store M1

*------------------------------*
* Model (2): Occ FE + State FE
*------------------------------*
areg lnwage license c.age c.agesq black female college i.statefip ///
    [pw=asecwt], absorb(occ2010) vce(robust)
scalar beta_long = _b[license]
scalar R2_long   = e(r2)
estimates store M2

*------------------------------*
* Altonji ratio & Oster delta
*------------------------------*
scalar altonji_ratio = .
if (beta_long != 0) scalar altonji_ratio = (beta_short - beta_long)/beta_long
display as result "Altonji implied ratio = " %6.3f altonji_ratio

program drop _all
program define _oster, rclass
    syntax , bshort(real) blong(real) r2short(real) r2long(real) rmax(real)
    tempname d
    scalar `d' = .
    if (`blong'!=`bshort') & (`r2long'!=`r2short') {
        scalar `d' = ((`blong')/(`bshort' - `blong')) * ( (`rmax' - `r2long')/(`r2long' - `r2short') )
    }
    return scalar delta = `d'
end

tempname d1 d2 d3
scalar Rmax1 = 1
scalar Rmax2 = 1.5*R2_long
scalar Rmax3 = 1.3*R2_long

quietly _oster , bshort(`=beta_short') blong(`=beta_long') r2short(`=R2_short') r2long(`=R2_long') rmax(`=Rmax1')
scalar `d1' = r(delta)
quietly _oster , bshort(`=beta_short') blong(`=beta_long') r2short(`=R2_short') r2long(`=R2_long') rmax(`=Rmax2')
scalar `d2' = r(delta)
quietly _oster , bshort(`=beta_short') blong(`=beta_long') r2short(`=R2_short') r2long(`=R2_long') rmax(`=Rmax3')
scalar `d3' = r(delta)

display as result "Oster δ (Rmax=1.0)     = " %9.3f `d1'
display as result "Oster δ (Rmax=1.5*R~ ) = " %9.3f `d2' "   (R~= " %6.3f R2_long ")"
display as result "Oster δ (Rmax=1.3*R~ ) = " %9.3f `d3' "   (R~= " %6.3f R2_long ")"

*------------------------------*
* Heterogeneity: Age>35, license×black
*------------------------------*
preserve
keep if age>35
areg lnwage i.license##i.black c.age c.agesq female college i.statefip ///
    [pw=asecwt], absorb(occ2010) vce(robust)
estimates store P2
margins black, at(license=(0 1))
margins, dydx(license) over(black)
restore

*------------------------------*
* Descriptive: licensing by race (aweights in tab)
*------------------------------*
tab license black [aw=asecwt], row

*------------------------------*
* DOCX Table 1: M1 vs M2
*------------------------------*
putdocx clear
putdocx begin
putdocx paragraph, style(Title)
putdocx text ("Effect of Licensing on Log Wages")
putdocx paragraph
putdocx text ("Weighted by ASECWT. Model (2) includes Occupation and State fixed effects. Sample: employed 18–65; certificates dropped.")

local keepvars  license age agesq black female college
local showlabs "License Age Age^2 Black Female College"

* +2 rows to hold N and R-squared
putdocx table t1 = (9,3), border(all, single) layout(autofitcontents)
putdocx table t1(1,1) = ("Variable")
putdocx table t1(1,2) = ("Model (1) — OLS")
putdocx table t1(1,3) = ("Model (2) — Occ+State FE")
putdocx table t1(1,.), bold

local r = 2
local k = 1
foreach v of local keepvars {
    local lab : word `k' of `showlabs'
    putdocx table t1(`r',1) = ("`lab'")

    * Model (1)
    estimates restore M1
    local pos = colnumb(e(b), "`v'")
    if (`pos' > 0) {
        local b1 = string(_b[`v'],"%9.3f")
        local s1 = string(_se[`v'],"%9.3f")
        local t1v = abs(_b[`v']/_se[`v'])
        local p1  = 2*ttail(e(df_r), `t1v')
        local star1 = cond(`p1'<.01,"***", cond(`p1'<.05,"**", cond(`p1'<.10,"*","")))
        putdocx table t1(`r',2) = ("`b1' (`s1')`star1'")
    }
    else putdocx table t1(`r',2) = ("-")

    * Model (2)
    estimates restore M2
    local pos = colnumb(e(b), "`v'")
    if (`pos' > 0) {
        local b2 = string(_b[`v'],"%9.3f")
        local s2 = string(_se[`v'],"%9.3f")
        local t2v = abs(_b[`v']/_se[`v'])
        local p2  = 2*ttail(e(df_r), `t2v')
        local star2 = cond(`p2'<.01,"***", cond(`p2'<.05,"**", cond(`p2'<.10,"*","")))
        putdocx table t1(`r',3) = ("`b2' (`s2')`star2'")
    }
    else putdocx table t1(`r',3) = ("-")

    local ++r
    local ++k
}

* N and R-squared rows
estimates restore M1
local N1 = e(N)
local R1 = string(e(r2), "%6.3f")
estimates restore M2
local N2 = e(N)
local R2 = string(e(r2), "%6.3f")

putdocx table t1(8,1) = ("N")
putdocx table t1(8,2) = ("`N1'")
putdocx table t1(8,3) = ("`N2'")
putdocx table t1(9,1) = ("R-squared")
putdocx table t1(9,2) = ("`R1'")
putdocx table t1(9,3) = ("`R2'")

putdocx paragraph
putdocx text ("Notes: Coefficients with robust SEs in parentheses; weighted by ASECWT. Model (2) includes occupation FE (absorbed) and state dummies. Certificates dropped.")
putdocx paragraph, spacing(before, 0) spacing(after, 0)
putdocx text ("* p<0.10, ** p<0.05, *** p<0.01")

* Sensitivity summary
putdocx paragraph
local ar  : display %5.3f altonji_ratio
local d1s : display %5.3f `d1'
local d2s : display %5.3f `d2'
local d3s : display %5.3f `d3'
putdocx text ("Sensitivity (Problem 3) — Altonji ratio = `ar'; Oster delta: `d1s' (Rmax=1.0), `d2s' (Rmax=1.5×R~), `d3s' (Rmax=1.3×R~).")

putdocx save "results\table_p1.docx", replace

*------------------------------*
* DOCX Table 2: Interaction (Age>35)
*------------------------------*
putdocx clear
putdocx begin
putdocx paragraph, style(Title)
putdocx text ("Licensing × Black (Age > 35): Log Wage")
putdocx paragraph
putdocx text ("Weighted by ASECWT. Occupation and State fixed effects absorbed. Sample: employed >35; certificates dropped.")

local rowlabs   "License Black License#Black Age Age^2 Female College"
local coefnames 1.license 1.black 1.license#1.black age agesq female college

putdocx table t2 = (10,2), border(all, single) layout(autofitcontents)
putdocx table t2(1,1) = ("Variable")
putdocx table t2(1,2) = ("Model (FE)")
putdocx table t2(1,.), bold

local r = 2
forvalues i=1/7 {
    local lab  : word `i' of `rowlabs'
    local coef : word `i' of `coefnames'
    putdocx table t2(`r',1) = ("`lab'")

    estimates restore P2
    local coef_use "`coef'"
    local pos = colnumb(e(b), "`coef_use'")
    if (`pos'==0 & "`coef_use'"=="1.license#1.black") {
        local coef_use "c.license#1.black"
        local pos = colnumb(e(b), "`coef_use'")
    }

    if (`pos' > 0) {
        local b  = string(_b[`coef_use'],"%9.3f")
        local s  = string(_se[`coef_use'],"%9.3f")
        local t3 = abs(_b[`coef_use']/_se[`coef_use'])
        local p3 = 2*ttail(e(df_r), `t3')
        local star = cond(`p3'<.01,"***", cond(`p3'<.05,"**", cond(`p3'<.10,"*","")))
        putdocx table t2(`r',2) = ("`b' (`s')`star'")
    }
    else putdocx table t2(`r',2) = ("-")

    local ++r
}

estimates restore P2
local Np2 = string(e(N), "%9.0f")
local Rp2 = string(e(r2), "%6.3f")

putdocx table t2(9,1)  = ("N")
putdocx table t2(9,2)  = ("`Np2'")
putdocx table t2(10,1) = ("R-squared")
putdocx table t2(10,2) = ("`Rp2'")

putdocx paragraph
putdocx text ("Notes: Sample restricted to Age>35. Occupation FE (absorbed) and state dummies included. Coefficients with robust SEs in parentheses; weighted by ASECWT. Certificates dropped.")
putdocx paragraph
putdocx text ("Interpretation: 'license' is the effect for non-Black; 'black' is the baseline race gap when unlicensed; 'license#black' is the differential licensing effect for Black workers (total effect for Black licensed = license + license#black).")
putdocx paragraph, spacing(before, 0) spacing(after, 0)
putdocx text ("* p<0.10, ** p<0.05, *** p<0.01")

putdocx save "results\table_p2.docx", replace

*------------------------------*
* LaTeX exports for Overleaf
*------------------------------*
cap which esttab
if _rc ssc install estout, replace
which esttab
estimates dir

* Absolute output paths
local out1 "C:\Users\juanc\Documents\cps2024_credentials\results\table_p1.tex"
local out2 "C:\Users\juanc\Documents\cps2024_credentials\results\table_p2.tex"

* Table 1 (M1 vs M2) — fixed addnotes() syntax
estimates restore M1
estimates restore M2
esttab M1 M2 using "`out1'", replace booktabs label se nocons ///
    b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("OLS" "Occ+State FE") ///
    keep(license age agesq black female college) ///
    varlabels(license "License" age "Age" agesq "Age$^2$" black "Black" female "Female" college "College (BA+)") ///
    s(N r2, labels("N" "R-squared")) ///
    title("Effect of Licensing on Log Wages") ///
    addnotes("Weighted by ASECWT. Model (2) includes occupation fixed effects (absorbed) and state dummies. Sample: employed 18--65; certificates dropped." ///
             "* p<0.10, ** p<0.05, *** p<0.01")

* Table 2 (P2) — fixed addnotes() syntax
estimates restore P2
esttab P2 using "`out2'", replace booktabs label se nocons ///
    b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    keep(1.license 1.black 1.license#1.black age agesq female college) ///
    varlabels(1.license "License" 1.black "Black" 1.license#1.black "License$\times$Black" ///
              age "Age" agesq "Age$^2$" female "Female" college "College (BA+)") ///
    s(N r2, labels("N" "R-squared")) ///
    title("Licensing $\times$ Black (Age $>$ 35): Log Wage") ///
    addnotes("Weighted by ASECWT. Occupation fixed effects (absorbed) and state dummies included. Sample: employed $>$35; certificates dropped." ///
             "Interpretation: 'License' is the effect for non-Black; 'Black' is the baseline race gap when unlicensed; 'License$\times$Black' is the differential licensing effect for Black workers (total for Black licensed = License + License$\times$Black)." ///
             "* p<0.10, ** p<0.05, *** p<0.01")

* Quick checks
confirm file "`out1'"
confirm file "`out2'"
dir "C:\Users\juanc\Documents\cps2024_credentials\results"

log close
display as result "Saved: results\\table_p1.docx"
display as result "Saved: results\\table_p2.docx"
display as result "Saved: results\\table_p1.tex"
display as result "Saved: results\\table_p2.tex"

