********************************************************************************
* RECONNECT PROJECT - DIFFERENCE-IN-DIFFERENCES ANALYSIS
********************************************************************************
* Author: Juan C.
* Date: November 15, 2025
* Project: Impact of USDA ReConnect Broadband Funding on County Employment & Income
*
* This do-file implements a two-way fixed effects DiD design to estimate
* the causal effect of ReConnect broadband funding on county-level outcomes
*
* Treatment: ReConnect broadband funding (2019-2022)
* Outcomes: Total employment (primary), Median household income (secondary)
* Panel: 959 counties across 10 states, 2019-2023 (3 time periods)
********************************************************************************

clear all
set more off
set scheme s2color

* Set working directory (CHANGE THIS TO YOUR PATH)
cd "C:\Users\juanc\OneDrive\Desktop\Reconnect_Project\data\raw"

* Install required packages (run once)
* ssc install reghdfe
* ssc install estout
* ssc install coefplot

********************************************************************************
* SECTION 1: DATA LOADING AND PREPARATION
********************************************************************************

* Load the master dataset (using insheet - no Java needed)
insheet using "master_reconnect_data.csv", clear comma names

* Convert string variables to numeric if needed
destring, replace

* Check data structure
describe
summarize

* county_fips is already numeric, so just rename it for clarity
gen county_id = county_fips

* Declare panel structure
xtset county_id year
xtdescribe

* Label variables for tables
label variable total_employment "Total Employment"
label variable median_household_income "Median Household Income"
label variable treated "ReConnect Treatment"
label variable post "Post-Treatment Period"
label variable treated_post "Treatment × Post"
label variable rural "Rural County (RUCC ≥ 4)"
label variable log_employment "Log(Employment)"
label variable log_income "Log(Income)"

* Create state fixed effects
encode state_name, gen(state_id)

********************************************************************************
* SECTION 2: DESCRIPTIVE STATISTICS
********************************************************************************

* Table 1: Summary Statistics by Treatment Status
estpost tabstat total_employment median_household_income rural ///
    if year == 2019, by(treated) statistics(mean sd) columns(statistics)
esttab using "table1_summary_stats.csv", replace ///
    cells("mean(fmt(0)) sd(fmt(0))") label noobs

* Summary stats for full sample
summarize total_employment median_household_income rural treated ///
    if year == 2019, detail

* Check treatment balance
ttest total_employment if year == 2019, by(treated)
ttest median_household_income if year == 2019, by(treated)
ttest rural if year == 2019, by(treated)

********************************************************************************
* SECTION 3: PARALLEL TRENDS VISUALIZATION
********************************************************************************

* Create year indicators
tab year, gen(year_)

* Calculate mean employment by treatment status and year
preserve
collapse (mean) total_employment median_household_income, by(treated year)

* Plot parallel trends for employment
twoway (connected total_employment year if treated==0, lcolor(blue) mcolor(blue)) ///
       (connected total_employment year if treated==1, lcolor(red) mcolor(red)), ///
       xlabel(2019 2022 2023) xtitle("Year") ytitle("Average Employment") ///
       legend(label(1 "Control") label(2 "Treated")) ///
       title("Parallel Trends: Employment") ///
       xline(2019.5, lpattern(dash) lcolor(gray))
graph export "fig_parallel_trends_employment.png", replace

* Plot parallel trends for income (subsample with data)
twoway (connected median_household_income year if treated==0, lcolor(blue) mcolor(blue)) ///
       (connected median_household_income year if treated==1, lcolor(red) mcolor(red)), ///
       xlabel(2019 2022 2023) xtitle("Year") ytitle("Average Median Income") ///
       legend(label(1 "Control") label(2 "Treated")) ///
       title("Parallel Trends: Median Household Income") ///
       xline(2019.5, lpattern(dash) lcolor(gray))
graph export "fig_parallel_trends_income.png", replace

restore

********************************************************************************
* SECTION 4: MAIN DIFFERENCE-IN-DIFFERENCES REGRESSIONS
********************************************************************************

* =============================================================================
* TABLE 2: MAIN RESULTS - EMPLOYMENT
* =============================================================================

* Column 1: Basic DiD (no controls)
reghdfe total_employment treated_post, ///
    absorb(county_id year) vce(cluster county_id)
estimates store emp_col1

* Column 2: Add rural control
reghdfe total_employment treated_post rural, ///
    absorb(county_id year) vce(cluster county_id)
estimates store emp_col2

* Column 3: Add state-year fixed effects
reghdfe total_employment treated_post rural, ///
    absorb(county_id state_id#year) vce(cluster county_id)
estimates store emp_col3

* Column 4: Log specification
reghdfe log_employment treated_post rural, ///
    absorb(county_id year) vce(cluster county_id)
estimates store emp_col4

* Export Table 2
esttab emp_col1 emp_col2 emp_col3 emp_col4 using "table2_employment_main.csv", ///
    replace b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label stats(N r2_a, fmt(0 3) labels("Observations" "Adjusted R²")) ///
    title("Table 2: Effect of ReConnect on County Employment") ///
    mtitles("Basic" "w/ Rural" "State-Year FE" "Log Spec")

* =============================================================================
* TABLE 3: MAIN RESULTS - INCOME (Subsample)
* =============================================================================

* Column 1: Basic DiD
reghdfe median_household_income treated_post if !missing(median_household_income), ///
    absorb(county_id year) vce(cluster county_id)
estimates store inc_col1

* Column 2: Add rural control
reghdfe median_household_income treated_post rural if !missing(median_household_income), ///
    absorb(county_id year) vce(cluster county_id)
estimates store inc_col2

* Column 3: Add state-year fixed effects
reghdfe median_household_income treated_post rural if !missing(median_household_income), ///
    absorb(county_id state_id#year) vce(cluster county_id)
estimates store inc_col3

* Column 4: Log specification
reghdfe log_income treated_post rural if !missing(log_income), ///
    absorb(county_id year) vce(cluster county_id)
estimates store inc_col4

* Export Table 3
esttab inc_col1 inc_col2 inc_col3 inc_col4 using "table3_income_main.csv", ///
    replace b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label stats(N r2_a, fmt(0 3) labels("Observations" "Adjusted R²")) ///
    title("Table 3: Effect of ReConnect on Median Household Income") ///
    mtitles("Basic" "w/ Rural" "State-Year FE" "Log Spec")

********************************************************************************
* SECTION 5: EVENT STUDY (Parallel Trends Test)
********************************************************************************

* Create year-treatment interactions
gen treated_2022 = treated * (year == 2022)
gen treated_2023 = treated * (year == 2023)

label variable treated_2022 "Treated × 2022"
label variable treated_2023 "Treated × 2023"

* Event study regression for employment
reghdfe total_employment treated_2022 treated_2023, ///
    absorb(county_id year) vce(cluster county_id)
estimates store event_emp

* Event study regression for income
reghdfe median_household_income treated_2022 treated_2023 ///
    if !missing(median_household_income), ///
    absorb(county_id year) vce(cluster county_id)
estimates store event_inc

* Export event study results
esttab event_emp event_inc using "table4_event_study.csv", ///
    replace b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label stats(N r2_a, fmt(0 3) labels("Observations" "Adjusted R²")) ///
    title("Table 4: Event Study - Testing Parallel Trends") ///
    mtitles("Employment" "Income")

* Coefficient plot for event study (Employment)
coefplot event_emp, keep(treated_2022 treated_2023) ///
    vertical yline(0) ///
    title("Event Study: Employment Effects by Year") ///
    ytitle("Coefficient") xtitle("Year Relative to Treatment") ///
    coeflabels(treated_2022 = "2022" treated_2023 = "2023")
graph export "fig_event_study_employment.png", replace

********************************************************************************
* SECTION 6: HETEROGENEOUS EFFECTS
********************************************************************************

* =============================================================================
* TABLE 5: Heterogeneity by Rural/Urban Status
* =============================================================================

* Employment effects by rural status
gen treated_post_rural = treated_post * rural
gen treated_post_urban = treated_post * (1 - rural)

label variable treated_post_rural "Treatment × Post × Rural"
label variable treated_post_urban "Treatment × Post × Urban"

reghdfe total_employment treated_post_rural treated_post_urban, ///
    absorb(county_id year) vce(cluster county_id)
estimates store het_rural_emp

* Test equality of coefficients
test treated_post_rural = treated_post_urban

* Income effects by rural status
reghdfe median_household_income treated_post_rural treated_post_urban ///
    if !missing(median_household_income), ///
    absorb(county_id year) vce(cluster county_id)
estimates store het_rural_inc

test treated_post_rural = treated_post_urban

* Export heterogeneity results
esttab het_rural_emp het_rural_inc using "table5_heterogeneity.csv", ///
    replace b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label stats(N r2_a, fmt(0 3) labels("Observations" "Adjusted R²")) ///
    title("Table 5: Heterogeneous Effects by Rural Status") ///
    mtitles("Employment" "Income")

********************************************************************************
* SECTION 7: ROBUSTNESS CHECKS
********************************************************************************

* =============================================================================
* TABLE 6: Robustness Checks
* =============================================================================

* Robustness 1: Drop extreme outliers (top/bottom 5% by employment in 2019)
preserve
egen emp_2019 = mean(total_employment) if year == 2019, by(county_id)
egen temp = mean(emp_2019), by(county_id)
replace emp_2019 = temp
drop temp
_pctile emp_2019 if year == 2019, p(5 95)
drop if emp_2019 < r(r1) | emp_2019 > r(r2)
reghdfe total_employment treated_post rural, ///
    absorb(county_id year) vce(cluster county_id)
estimates store robust_outliers
restore

* Robustness 2: Different clustering (state level)
reghdfe total_employment treated_post rural, ///
    absorb(county_id year) vce(cluster state_id)
estimates store robust_state_cluster

* Robustness 3: Drop if missing income (common sample)
reghdfe total_employment treated_post rural if !missing(median_household_income), ///
    absorb(county_id year) vce(cluster county_id)
estimates store robust_common_sample

* Robustness 4: Alternative outcome - establishments
reghdfe num_establishments treated_post rural, ///
    absorb(county_id year) vce(cluster county_id)
estimates store robust_estabs

* Export robustness checks
esttab robust_outliers robust_state_cluster robust_common_sample robust_estabs ///
    using "table6_robustness.csv", ///
    replace b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label stats(N r2_a, fmt(0 3) labels("Observations" "Adjusted R²")) ///
    title("Table 6: Robustness Checks") ///
    mtitles("Drop Outliers" "State Cluster" "Common Sample" "Establishments")

********************************************************************************
* SECTION 8: ADDITIONAL SPECIFICATIONS
********************************************************************************

* Check if results hold with additional controls
* (Add any sector-specific employment if available in data)

* If you have sector employment data:
capture confirm variable employment_retail
if !_rc {
    * Control for baseline industry composition
    reghdfe total_employment treated_post rural ///
        employment_retail employment_manufacturing, ///
        absorb(county_id year) vce(cluster county_id)
    estimates store spec_sector_controls
}

********************************************************************************
* SECTION 9: SUMMARY STATISTICS FOR PAPER
********************************************************************************

* Create summary statistics table for treated vs control in 2019 (baseline)
estpost ttest total_employment median_household_income rural ///
    num_establishments if year == 2019, by(treated)
esttab using "table_baseline_balance.csv", replace ///
    cells("mu_1(fmt(0)) mu_2(fmt(0)) b(fmt(0) star)") ///
    label star(* 0.10 ** 0.05 *** 0.01) ///
    title("Baseline Balance: Treated vs Control Counties (2019)")

********************************************************************************
* SECTION 10: CALCULATE PERCENTAGE EFFECTS
********************************************************************************

* Calculate percentage effects from log specifications
* For employment
quietly reghdfe log_employment treated_post rural, ///
    absorb(county_id year) vce(cluster county_id)
    
di "Employment Effect: " 100*(exp(_b[treated_post]) - 1) "%"

* For income
quietly reghdfe log_income treated_post rural if !missing(log_income), ///
    absorb(county_id year) vce(cluster county_id)
    
di "Income Effect: " 100*(exp(_b[treated_post]) - 1) "%"

********************************************************************************
* SECTION 11: EXPORT KEY RESULTS FOR PAPER
********************************************************************************

* Main result: Employment
quietly reghdfe total_employment treated_post rural, ///
    absorb(county_id year) vce(cluster county_id)

di "============================================================"
di "MAIN RESULT - EMPLOYMENT"
di "============================================================"
di "Coefficient: " _b[treated_post]
di "Standard Error: " _se[treated_post]
di "P-value: " 2*ttail(e(df_r), abs(_b[treated_post]/_se[treated_post]))
di "95% CI: [" _b[treated_post] - 1.96*_se[treated_post] ///
    ", " _b[treated_post] + 1.96*_se[treated_post] "]"
di "============================================================"

* Main result: Income
quietly reghdfe median_household_income treated_post rural ///
    if !missing(median_household_income), ///
    absorb(county_id year) vce(cluster county_id)

di "============================================================"
di "MAIN RESULT - INCOME"
di "============================================================"
di "Coefficient: " _b[treated_post]
di "Standard Error: " _se[treated_post]
di "P-value: " 2*ttail(e(df_r), abs(_b[treated_post]/_se[treated_post]))
di "95% CI: [" _b[treated_post] - 1.96*_se[treated_post] ///
    ", " _b[treated_post] + 1.96*_se[treated_post] "]"
di "============================================================"

********************************************************************************
* END OF ANALYSIS
********************************************************************************

log close
