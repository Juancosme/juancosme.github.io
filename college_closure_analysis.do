********************************************************************************
* COLLEGE CLOSURE ANALYSIS WITH SHIFT-SHARE EXPOSURE MEASURE
* Author: [Your Name]
* Date: October 24, 2025
* Purpose: Analyze local labor market effects of college closures using DiD
*          with shift-share exposure to education sector
********************************************************************************

clear all
set more off
set maxvar 10000

* Set your working directory
cd "C:\Users\YourName\Documents\College_Closure_Project\"

* Create folders for output
cap mkdir "output"
cap mkdir "output\tables"
cap mkdir "output\figures"

********************************************************************************
* PART 1: LOAD AND CLEAN IPUMS ACS DATA
********************************************************************************

use "ipums_acs_2009_2019.dta", clear

* Keep only working-age population (16-64)
keep if age >= 16 & age <= 64

* Create 5-digit county FIPS code (combining state and county)
gen countyfip = statefip*1000 + countyfip
label variable countyfip "5-digit County FIPS code"

* Keep only observations with valid county codes
drop if countyfip == . | countyfip == 0

********************************************************************************
* PART 2: CREATE KEY VARIABLES
********************************************************************************

* Employment status variables
gen employed = (empstat == 1)
label variable employed "Employed"

gen unemployed = (empstat == 2)
label variable unemployed "Unemployed"

gen in_laborforce = (labforce == 2)
label variable in_laborforce "In labor force"

* Education sector workers (IND codes 7860-7890 = Educational services)
gen educ_sector = (ind >= 7860 & ind <= 7890 & employed == 1)
label variable educ_sector "Works in education sector"

* Wage variable (set N/A codes to missing)
replace incwage = . if incwage >= 999998
gen log_wage = ln(incwage) if incwage > 0
label variable log_wage "Log wage income"

* Demographic variables for controls
gen female = (sex == 2)
gen white = (race == 1)
gen black = (race == 2)
gen asian = (race == 4 | race == 5 | race == 6)
gen native = (race == 3)
gen hispanic = (hispan > 0 & hispan < 9)

* Education levels
gen hs_or_less = (educ <= 6)
gen some_college = (educ >= 7 & educ <= 9)
gen bachelors_plus = (educ >= 10)

********************************************************************************
* PART 3: CALCULATE BASELINE EXPOSURE MEASURE (2009 ACS = 2005-2009)
********************************************************************************

preserve

* Keep only 2009 5-year sample (baseline)
keep if year == 2009

* Calculate total employment and education employment by county
collapse (sum) total_emp=employed ///
              educ_emp=educ_sector ///
         [pw=perwt], by(countyfip)

* Calculate exposure measure
gen exposure = educ_emp / total_emp
label variable exposure "Education employment share (2009 baseline)"

* Replace missing exposure with 0 (counties with no education employment)
replace exposure = 0 if exposure == .

* Save baseline exposure
save "baseline_exposure.dta", replace

restore

********************************************************************************
* PART 4: CALCULATE PANEL OUTCOME VARIABLES (2010-2019)
********************************************************************************

* Keep only 2010-2019 data for outcomes
keep if year >= 2010 & year <= 2019

* Collapse to county-year level with all outcomes and controls
collapse (mean) employment_rate=employed ///
               unemployment_rate=unemployed ///
               lfp_rate=in_laborforce ///
               median_wage=incwage ///
               mean_log_wage=log_wage ///
               pct_female=female ///
               pct_white=white ///
               pct_black=black ///
               pct_asian=asian ///
               pct_native=native ///
               pct_hispanic=hispanic ///
               pct_hs_or_less=hs_or_less ///
               pct_some_college=some_college ///
               pct_bachelors_plus=bachelors_plus ///
               median_age=age ///
         (count) population=perwt ///
         [pw=perwt], by(countyfip year)

* Convert rates to percentages
replace employment_rate = employment_rate * 100
replace unemployment_rate = unemployment_rate * 100
replace lfp_rate = lfp_rate * 100
replace pct_female = pct_female * 100
replace pct_white = pct_white * 100
replace pct_black = pct_black * 100
replace pct_asian = pct_asian * 100
replace pct_hispanic = pct_hispanic * 100
replace pct_hs_or_less = pct_hs_or_less * 100
replace pct_some_college = pct_some_college * 100
replace pct_bachelors_plus = pct_bachelors_plus * 100

* Label variables
label variable employment_rate "Employment rate (%)"
label variable unemployment_rate "Unemployment rate (%)"
label variable lfp_rate "Labor force participation rate (%)"
label variable median_wage "Median wage income ($)"
label variable mean_log_wage "Mean log wage"
label variable population "County population"

* Save panel data
save "panel_outcomes.dta", replace

********************************************************************************
* PART 5: MERGE WITH CLOSURE DATA
********************************************************************************

* Load closure data
use "closure.dta", clear

* Keep only closures from 2010-2019 (drop always-treated)
keep if earliestyear >= 2010 & earliestyear <= 2019

* Keep only counties with non-missing enrollment data
* (This filters out counties with missing exposure data)
keep if efytotlt != . & eaptot != .

* Save clean closure data
save "closure_clean.dta", replace

* Create full panel (all treated counties × all years 2010-2019)
expand 10
bysort countyfip: gen year = 2009 + _n
keep countyfip year earliestyear treat alwaystreat

* Create treatment variables
gen post = (year >= earliestyear)
label variable post "Post-closure period"

* Save closure panel
save "closure_panel.dta", replace

* Merge with outcome data
merge 1:1 countyfip year using "panel_outcomes.dta"
keep if _merge == 3  // Keep only matched observations
drop _merge

* Merge with baseline exposure
merge m:1 countyfip using "baseline_exposure.dta"
keep if _merge == 3  // Keep only counties with exposure data
drop _merge

* Create interaction term
gen exposure_x_post = exposure * post
label variable exposure_x_post "Exposure × Post-closure"

* Save final analysis dataset
save "analysis_data.dta", replace

********************************************************************************
* PART 6: SUMMARY STATISTICS
********************************************************************************

use "analysis_data.dta", clear

* Table 1: Summary statistics by treatment status
eststo clear

* Pre-period statistics (year < earliestyear)
eststo pre: estpost summarize employment_rate unemployment_rate ///
    lfp_rate median_wage population exposure ///
    if post == 0, detail

* Post-period statistics (year >= earliestyear)  
eststo post: estpost summarize employment_rate unemployment_rate ///
    lfp_rate median_wage population exposure ///
    if post == 1, detail

* Export to LaTeX
esttab pre post using "output/tables/table1_summary_stats.tex", ///
    cells("mean(fmt(2)) sd(fmt(2)) p25(fmt(2)) p50(fmt(2)) p75(fmt(2))") ///
    label nomtitle nonumber replace booktabs ///
    title("Summary Statistics by Treatment Period")

* Export to Excel  
esttab pre post using "output/tables/table1_summary_stats.xlsx", ///
    cells("mean sd min max") label replace

********************************************************************************
* PART 7: MAIN DIFFERENCE-IN-DIFFERENCES REGRESSIONS
********************************************************************************

use "analysis_data.dta", clear

* Encode county for fixed effects
encode countyfip, gen(county_id)

eststo clear

********************************************************************************
* Panel A: Employment Rate Outcomes
********************************************************************************

* Column 1: Basic DiD (no exposure interaction)
eststo emp1: reghdfe employment_rate post ///
    pct_female pct_black pct_asian pct_hispanic ///
    median_age pct_bachelors_plus, ///
    absorb(county_id year) vce(cluster county_id)

* Column 2: DiD with exposure interaction (MAIN SPECIFICATION)
eststo emp2: reghdfe employment_rate post exposure_x_post ///
    pct_female pct_black pct_asian pct_hispanic ///
    median_age pct_bachelors_plus, ///
    absorb(county_id year) vce(cluster county_id)

* Column 3: Unemployment rate - Basic DiD
eststo unemp1: reghdfe unemployment_rate post ///
    pct_female pct_black pct_asian pct_hispanic ///
    median_age pct_bachelors_plus, ///
    absorb(county_id year) vce(cluster county_id)

* Column 4: Unemployment rate - with exposure
eststo unemp2: reghdfe unemployment_rate post exposure_x_post ///
    pct_female pct_black pct_asian pct_hispanic ///
    median_age pct_bachelors_plus, ///
    absorb(county_id year) vce(cluster county_id)

* Export Table 2: Main Results
esttab emp1 emp2 unemp1 unemp2 using "output/tables/table2_main_results.tex", ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label replace booktabs ///
    mtitles("Emp Rate" "Emp Rate" "Unemp Rate" "Unemp Rate") ///
    title("Main Results: Effect of College Closures on Labor Market Outcomes") ///
    addnotes("Standard errors clustered at county level in parentheses." ///
             "All regressions include county and year fixed effects." ///
             "Controls: % female, % black, % asian, % hispanic, median age, % bachelors+")

esttab emp1 emp2 unemp1 unemp2 using "output/tables/table2_main_results.xlsx", ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) label replace

********************************************************************************
* Panel B: Wage and Population Outcomes
********************************************************************************

eststo clear

* Column 1: Median wages - Basic DiD
eststo wage1: reghdfe median_wage post ///
    pct_female pct_black pct_asian pct_hispanic ///
    median_age pct_bachelors_plus, ///
    absorb(county_id year) vce(cluster county_id)

* Column 2: Median wages - with exposure
eststo wage2: reghdfe median_wage post exposure_x_post ///
    pct_female pct_black pct_asian pct_hispanic ///
    median_age pct_bachelors_plus, ///
    absorb(county_id year) vce(cluster county_id)

* Column 3: Log wages - Basic DiD
eststo logwage1: reghdfe mean_log_wage post ///
    pct_female pct_black pct_asian pct_hispanic ///
    median_age pct_bachelors_plus, ///
    absorb(county_id year) vce(cluster county_id)

* Column 4: Log wages - with exposure
eststo logwage2: reghdfe mean_log_wage post exposure_x_post ///
    pct_female pct_black pct_asian pct_hispanic ///
    median_age pct_bachelors_plus, ///
    absorb(county_id year) vce(cluster county_id)

* Export Table 3: Wage Results
esttab wage1 wage2 logwage1 logwage2 using "output/tables/table3_wage_results.tex", ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label replace booktabs ///
    mtitles("Wage" "Wage" "Log Wage" "Log Wage") ///
    title("Effect of College Closures on Wage Outcomes") ///
    addnotes("Standard errors clustered at county level in parentheses." ///
             "All regressions include county and year fixed effects.")

esttab wage1 wage2 logwage1 logwage2 using "output/tables/table3_wage_results.xlsx", ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) label replace

********************************************************************************
* PART 8: HETEROGENEITY ANALYSIS BY EXPOSURE QUARTILES
********************************************************************************

use "analysis_data.dta", clear

* Create exposure quartiles
xtile exposure_quartile = exposure if year == 2010, nq(4)
bysort countyfip: egen exposure_q = max(exposure_quartile)

* Generate quartile indicators
forvalues q = 1/4 {
    gen exp_q`q' = (exposure_q == `q')
    gen exp_q`q'_post = exp_q`q' * post
}

eststo clear

* Regression with quartile interactions
eststo hetero: reghdfe employment_rate post ///
    exp_q2_post exp_q3_post exp_q4_post ///
    pct_female pct_black pct_asian pct_hispanic ///
    median_age pct_bachelors_plus, ///
    absorb(county_id year) vce(cluster county_id)

esttab hetero using "output/tables/table4_heterogeneity.tex", ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label replace booktabs ///
    title("Heterogeneous Effects by Education Sector Exposure") ///
    addnotes("Quartile 1 (lowest exposure) is the omitted category." ///
             "Standard errors clustered at county level in parentheses.")

********************************************************************************
* PART 9: FIGURES
********************************************************************************

use "analysis_data.dta", clear

* Figure 1: Distribution of closures over time
preserve
collapse (count) n_closures=countyfip, by(earliestyear)
twoway bar n_closures earliestyear, ///
    xlabel(2010(1)2019) ///
    ytitle("Number of Counties with Closures") ///
    xtitle("Year of Closure") ///
    title("College Closures by Year (2010-2019)") ///
    graphregion(color(white)) bgcolor(white)
graph export "output/figures/figure1_closures_by_year.png", replace width(3000)
graph export "output/figures/figure1_closures_by_year.pdf", replace
restore

* Figure 2: Event study plot (employment rate)
preserve

* Create event time (years relative to closure)
gen event_time = year - earliestyear

* Keep only -5 to +5 years around closure
keep if event_time >= -5 & event_time <= 5

* Run event study regression
reghdfe employment_rate ib(-1).event_time ///
    pct_female pct_black pct_asian pct_hispanic ///
    median_age pct_bachelors_plus, ///
    absorb(county_id year) vce(cluster county_id)

* Extract coefficients
matrix coef = r(table)
svmat coef

* Create event study plot
coefplot, vertical ///
    drop(_cons) ///
    yline(0, lcolor(red) lpattern(dash)) ///
    xline(-0.5, lcolor(gray) lpattern(dash)) ///
    xlabel(-5(1)5) ///
    xtitle("Years Relative to Closure") ///
    ytitle("Effect on Employment Rate (pp)") ///
    title("Event Study: Employment Rate") ///
    graphregion(color(white)) bgcolor(white)
graph export "output/figures/figure2_event_study.png", replace width(3000)
graph export "output/figures/figure2_event_study.pdf", replace

restore

* Figure 3: Exposure distribution
histogram exposure, ///
    bin(30) ///
    fraction ///
    xtitle("Education Employment Share (2009)") ///
    ytitle("Fraction of Counties") ///
    title("Distribution of Education Sector Exposure") ///
    graphregion(color(white)) bgcolor(white)
graph export "output/figures/figure3_exposure_dist.png", replace width(3000)
graph export "output/figures/figure3_exposure_dist.pdf", replace

* Figure 4: Treatment effect by exposure level
preserve

* Create exposure deciles
xtile exposure_decile = exposure if year == 2010, nq(10)
bysort countyfip: egen exp_dec = max(exposure_decile)

* Calculate mean exposure by decile
bysort exp_dec: egen mean_exposure = mean(exposure)

* Run regression for each decile
forvalues d = 1/10 {
    reghdfe employment_rate post if exp_dec == `d', ///
        absorb(county_id year) vce(cluster county_id)
    gen coef_`d' = _b[post] if exp_dec == `d'
    gen se_`d' = _se[post] if exp_dec == `d'
}

* Collapse to decile level
collapse (mean) mean_exposure coef_* se_*, by(exp_dec)

* Reshape for plotting
gen effect = .
gen ci_low = .
gen ci_high = .
forvalues d = 1/10 {
    replace effect = coef_`d' if exp_dec == `d'
    replace ci_low = coef_`d' - 1.96*se_`d' if exp_dec == `d'
    replace ci_high = coef_`d' + 1.96*se_`d' if exp_dec == `d'
}

* Plot
twoway (rarea ci_low ci_high mean_exposure, color(gs14)) ///
       (line effect mean_exposure, lcolor(navy) lwidth(thick)), ///
    yline(0, lcolor(red) lpattern(dash)) ///
    xtitle("Education Employment Share") ///
    ytitle("Effect on Employment Rate (pp)") ///
    title("Treatment Effect Heterogeneity by Exposure") ///
    legend(order(2 "Point Estimate" 1 "95% CI")) ///
    graphregion(color(white)) bgcolor(white)
graph export "output/figures/figure4_effect_by_exposure.png", replace width(3000)
graph export "output/figures/figure4_effect_by_exposure.pdf", replace

restore

********************************************************************************
* PART 10: ROBUSTNESS CHECKS
********************************************************************************

use "analysis_data.dta", clear

eststo clear

* Robustness 1: Different control sets
eststo robust1: reghdfe employment_rate post exposure_x_post, ///
    absorb(county_id year) vce(cluster county_id)

eststo robust2: reghdfe employment_rate post exposure_x_post ///
    pct_female median_age, ///
    absorb(county_id year) vce(cluster county_id)

eststo robust3: reghdfe employment_rate post exposure_x_post ///
    pct_female pct_black pct_asian pct_hispanic ///
    median_age pct_bachelors_plus pct_some_college, ///
    absorb(county_id year) vce(cluster county_id)

* Export robustness table
esttab robust1 robust2 robust3 using "output/tables/table5_robustness.tex", ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label replace booktabs ///
    mtitles("No Controls" "Basic Controls" "Full Controls") ///
    title("Robustness: Different Control Specifications")

********************************************************************************
* FINAL SUMMARY OUTPUT
********************************************************************************

di ""
di "=========================================================================="
di "ANALYSIS COMPLETE!"
di "=========================================================================="
di ""
di "Output files saved to:"
di "  - Tables: output/tables/"
di "  - Figures: output/figures/"
di ""
di "Key files:"
di "  - Table 1: Summary statistics (table1_summary_stats.tex/xlsx)"
di "  - Table 2: Main DiD results (table2_main_results.tex/xlsx)"
di "  - Table 3: Wage outcomes (table3_wage_results.tex/xlsx)"
di "  - Table 4: Heterogeneity analysis (table4_heterogeneity.tex)"
di "  - Table 5: Robustness checks (table5_robustness.tex)"
di ""
di "  - Figure 1: Closures by year (figure1_closures_by_year.png/pdf)"
di "  - Figure 2: Event study plot (figure2_event_study.png/pdf)"
di "  - Figure 3: Exposure distribution (figure3_exposure_dist.png/pdf)"
di "  - Figure 4: Effect by exposure (figure4_effect_by_exposure.png/pdf)"
di ""
di "=========================================================================="

log close
