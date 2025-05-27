************************* STATA Packages 
**** STATA Packages 
ssc install ftools
ssc install reghdfe

**************** Local Projections with Difference in Difference (Main analysis and sectoral analysis)

use "$github_path/Final_data.dta", clear

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main ANALYSIS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

******************* Define the pre/post windows and stabilization period 
local post_window 	4                   // Post disaster window 
local pre_window 	2                 // Pre disaster window to control for pretrends 
local L = 4                          // set time-window for defining clean controls
                                        // (note:doing so, you are assuming that effects stabilize 
                                        // after L periods; see Section 3.2 in DGJT, 2023)
****** declare panel dataset
egen ID_var = group(Territory_ID)    // unit identification variable 
xtset ID_var year  // declare panel database
* select period
keep if year > 1994
drop if year > 2022
** log transformations 

gen output= 100*ln(GDP) // gdp is in millions euro	
gen working_age = 100*ln(working_pop)		
gen lCapitalstock = 100*ln(Capitalstock)						
gen investment = 100*ln(GFCF)
gen productivity = 100*ln(prod_hw)
gen lproductivity_pp = 100*ln(productivity_pp)
gen ltotal_hours = 100*ln(total_hours)	
gen lHW = 100*ln(HW)	
gen ltotal_employment= 100*ln(total_employment)
gen lpop = 100*ln(population_nuts3)

*** dependent variable set 0
#delimit ;						
vl create dependent_variables = (output productivity investment working_age);
#delimit cr			

*** dependent variable set 1
#delimit ;						
vl create dependent_variables_set1 = (output);
#delimit cr			


*** dependent variable set 2	

#delimit ;						
vl create dependent_variables_set2 = (output productivity investment);
#delimit cr		
		
	

gen t =_n - `pre_window' - 1                // t = 0 means the time of event
replace t=. if t> `post_window'
	
sort ID_var year
*******************************************************
***    LP-DiD USING ALL ADMISSIBLE CLEAN CONTROLS ***
	
// Create variables that are useful for running LP-DiD regressions

foreach dv of varlist $dependent_variables {
	forval h = 0/`post_window' {
		qui gen D`h'`dv' = F`h'.`dv' - L.`dv'
		}

	forval h = 2/`pre_window' {
		gen Dm`h'`dv' = L`h'.`dv' - L.`dv'
	}

}

************************ Predicted hazards for heatwaves, floods and droughts
sort Territory_ID year

** For baseline events 
gen predicted_hazard_droughts = 0                  // = 1 (treatment)
gen predicted_hazard_floods = 0
gen predicted_hazard_heatwave = 0
replace predicted_hazard_droughts = 1 if drought_event== 1    // Droughts
replace predicted_hazard_floods =1 if flood_event_max3days==1 // Floods 
replace predicted_hazard_heatwave =1 if hotsummer_2C ==1     // Summer heatwave

gen treat = 0 //  common treatment variable showing hazard of any event in a particular year
replace treat = 1 if predicted_hazard_droughts ==1 | predicted_hazard_floods == 1 | predicted_hazard_heatwave== 1



xtset ID_var year

local string "abs(L.treat)!=1"

forv k=2/`L' {
	local string = "`string' & abs(L`k'.treat)!=1"
}
disp "`string'"

gen CCS_0 = 0
replace CCS_0 = 1 if `string'

forval j = 1/`post_window' {
	local i = `j'-1 
	gen CCS_`j' = 0
	replace CCS_`j' = 1 if CCS_`i'==1 & abs(F`j'.treat)!=1
	}	

gen CCS_m1 = CCS_0

forval j = 2/`pre_window' {
	local i = `j'-1 
	gen CCS_m`j' = 0
	replace CCS_m`j' = 1 if CCS_m`i'==1 & L.CCS_m`i'==1
	}

	

**************       LPDID HEATWAVES (FIGURE 1)

preserve 
// 
*LP-DiD using a time window for clean controls (previously treated units re-enter the control sample L periods after treatment) and without ruling out composition effects

foreach dv of varlist $dependent_variables {
    eststo clear
    capture drop b u d u68 d68 Zero
    gen b_lpdid_5a_`dv' = .
    gen tstat_lpdid_5a_`dv' = .
    gen Zero = 0 if t != .
    gen b = 0
    gen u = 0  // New variable for upper 90% confidence bound
    gen d = 0   // New variable for upper 90% confidence bound
    gen u68 = 0  // New variable for upper 68% confidence bound
    gen d68 = 0  // New variable for lower 68% confidence bound
			forval h = 0/`post_window' {
				qui reghdfe D`h'`dv' 	    							 ///
                    treat l.`dv' l2.`dv' l3.`dv' l4.`dv' i.hotwinter_2C i.hotspring_2C i.hotautumn_2C ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_heatwave==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)), 	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Baseline_climate == 1 (cold regions), Baseline_climate == 2 (temperate), Baseline_climate = 3 (hot)
					qui replace b_lpdid_5a_`dv'= _b[treat] if t==`h'
					qui replace tstat_lpdid_5a_`dv' = _b[treat]/_se[treat] if t==`h'
       
        replace b = _b[treat] if t == `h'
        replace u = _b[treat] + 1.645 * _se[treat] if t == `h'  // 90% upper bound
        replace d = _b[treat] - 1.645 * _se[treat] if t == `h'  // 90% lower bound
        
        replace u68 = _b[treat] + 1.0 * _se[treat] if t == `h'  // 68% upper bound
        replace d68 = _b[treat] - 1.0 * _se[treat] if t == `h'  // 68% lower bound
					qui eststo D`h'lpdid_5a
				
					if `h'>1 & `h'<=`pre_window' {
						qui reghdfe Dm`h'`dv'                              ///
                treat l.`dv' l2.`dv' l3.`dv' l4.`dv' i.hotwinter_2C i.hotspring_2C i.hotautumn_2C  ///   /* treatment indicator */
                if ((treat == 1 & predicted_hazard_heatwave == 1 | treat == 0) & (CCS_`post_window' == 1 & CCS_m`pre_window' == 1)),  ///   /* clean treatment and control condition */
                absorb(year Territory_ID) vce(cluster Territory_ID) level(90) /* time indicators */
** For subgroup analysis select: Baseline_climate == 1 (cold regions), Baseline_climate == 2 (temperate), Baseline_climate = 3 (hot)
						qui replace b_lpdid_5a_`dv' = _b[treat] if t==-`h'
						qui replace tstat_lpdid_5a_`dv'= _b[treat]/_se[treat] if t==-`h'
						
            replace b = _b[treat] if t == -`h'
            replace u = _b[treat] + 1.645 * _se[treat] if t == -`h'  // 90% upper bound
            replace d = _b[treat] - 1.645 * _se[treat] if t == -`h'  // 90% lower bound
            
            replace u68 = _b[treat] + 1.0 * _se[treat] if t == -`h'  // 68% upper bound
            replace d68 = _b[treat] - 1.0 * _se[treat] if t == -`h'  // 68% lower bound
            
						qui eststo Dm`h'lpdid_5a 
					}
				}
				
			nois esttab, se nocons keep(treat)
			disp(b)
			disp(b_lpdid_5a_`dv')

*** Graph
graph twoway ///
    (rarea d68 u68 t, fcolor(red%15) lcolor(red%0)) ///  // 68% confidence intervals
    || (rarea d u t, fcolor(red%10) lcolor(red%0)) ///  // 90% confidence intervals
    || (line b t, lcolor(navy) yline(0, lcolor(black)) ///  // Treatment effect line and zero line
	title("Figure 1 `dv'", size(medium)) ///
    xlabel(-2(1)4, nogrid labsize(medlarge)) ylabel(, nogrid) ///
    graphregion(color(white)) plotregion(color(white)) xline(0, lcolor(red)) ///
	xtitle("years since event", size(medium)) ///
    legend(order(3 "Heatwaves" 1 "68% confidence interval" 2 "90% confidence interval") ///
    size(medium) pos(6) rows(2)  col(1)))
	
	graph export "Figure_1_`dv'.png", replace

		}		
	
restore 

**************       LPDID HEATWAVES (FIGURE 2)


preserve 
// 
*Cold regions 

foreach dv of varlist $dependent_variables_set2 {
    eststo clear
    capture drop b u d u68 d68 Zero
    gen b_lpdid_5a_`dv' = .
    gen tstat_lpdid_5a_`dv' = .
    gen Zero = 0 if t != .
    gen b = 0
    gen u = 0  // New variable for upper 90% confidence bound
    gen d = 0   // New variable for upper 90% confidence bound
    gen u68 = 0  // New variable for upper 68% confidence bound
    gen d68 = 0  // New variable for lower 68% confidence bound
			forval h = 0/`post_window' {
				qui reghdfe D`h'`dv' 	    							 ///
                    treat l.`dv' l2.`dv' l3.`dv' l4.`dv' i.hotwinter_2C i.hotspring_2C i.hotautumn_2C ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_heatwave==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)) & Baseline_climate ==1, 	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Baseline_climate == 1 (cold regions), Baseline_climate == 2 (temperate), Baseline_climate = 3 (hot)
					qui replace b_lpdid_5a_`dv'= _b[treat] if t==`h'
					qui replace tstat_lpdid_5a_`dv' = _b[treat]/_se[treat] if t==`h'
       
        replace b = _b[treat] if t == `h'
        replace u = _b[treat] + 1.645 * _se[treat] if t == `h'  // 90% upper bound
        replace d = _b[treat] - 1.645 * _se[treat] if t == `h'  // 90% lower bound
        
        replace u68 = _b[treat] + 1.0 * _se[treat] if t == `h'  // 68% upper bound
        replace d68 = _b[treat] - 1.0 * _se[treat] if t == `h'  // 68% lower bound
					qui eststo D`h'lpdid_5a
				
					if `h'>1 & `h'<=`pre_window' {
						qui reghdfe Dm`h'`dv'                              ///
                treat l.`dv' l2.`dv' l3.`dv' l4.`dv' i.hotwinter_2C i.hotspring_2C i.hotautumn_2C  ///   /* treatment indicator */
                if ((treat == 1 & predicted_hazard_heatwave == 1 | treat == 0) & (CCS_`post_window' == 1 & CCS_m`pre_window' == 1)) & Baseline_climate ==1,  ///   /* clean treatment and control condition */
                absorb(year Territory_ID) vce(cluster Territory_ID) level(90) /* time indicators */
** For subgroup analysis select: Baseline_climate == 1 (cold regions), Baseline_climate == 2 (temperate), Baseline_climate = 3 (hot)
						qui replace b_lpdid_5a_`dv' = _b[treat] if t==-`h'
						qui replace tstat_lpdid_5a_`dv'= _b[treat]/_se[treat] if t==-`h'
						
            replace b = _b[treat] if t == -`h'
            replace u = _b[treat] + 1.645 * _se[treat] if t == -`h'  // 90% upper bound
            replace d = _b[treat] - 1.645 * _se[treat] if t == -`h'  // 90% lower bound
            
            replace u68 = _b[treat] + 1.0 * _se[treat] if t == -`h'  // 68% upper bound
            replace d68 = _b[treat] - 1.0 * _se[treat] if t == -`h'  // 68% lower bound
            
						qui eststo Dm`h'lpdid_5a 
					}
				}
				
			nois esttab, se nocons keep(treat)
			disp(b)
			disp(b_lpdid_5a_`dv')

*** Graph
graph twoway ///
    (rarea d68 u68 t, fcolor(red%15) lcolor(red%0)) ///  // 68% confidence intervals
    || (rarea d u t, fcolor(red%10) lcolor(red%0)) ///  // 90% confidence intervals
    || (line b t, lcolor(navy) yline(0, lcolor(black)) ///  // Treatment effect line and zero line
	title("Figure 2 `dv' cold regions", size(medium)) ///
    xlabel(-2(1)4, nogrid labsize(medlarge)) ylabel(, nogrid) ///
    graphregion(color(white)) plotregion(color(white)) xline(0, lcolor(red)) ///
	xtitle("years since event", size(medium)) ///
    legend(order(3 "Heatwaves" 1 "68% confidence interval" 2 "90% confidence interval") ///
    size(medium) pos(6) rows(2)  col(1)))
	
	graph export "Figure_2_`dv'_cold.png", replace

		}		

restore 


preserve 
// 
*Temperate regions 

foreach dv of varlist $dependent_variables_set2 {
    eststo clear
    capture drop b u d u68 d68 Zero
    gen b_lpdid_5a_`dv' = .
    gen tstat_lpdid_5a_`dv' = .
    gen Zero = 0 if t != .
    gen b = 0
    gen u = 0  // New variable for upper 90% confidence bound
    gen d = 0   // New variable for upper 90% confidence bound
    gen u68 = 0  // New variable for upper 68% confidence bound
    gen d68 = 0  // New variable for lower 68% confidence bound
			forval h = 0/`post_window' {
				qui reghdfe D`h'`dv' 	    							 ///
                    treat l.`dv' l2.`dv' l3.`dv' l4.`dv' i.hotwinter_2C i.hotspring_2C i.hotautumn_2C ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_heatwave==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)) & Baseline_climate ==2, 	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Baseline_climate == 1 (cold regions), Baseline_climate == 2 (temperate), Baseline_climate = 3 (hot)
					qui replace b_lpdid_5a_`dv'= _b[treat] if t==`h'
					qui replace tstat_lpdid_5a_`dv' = _b[treat]/_se[treat] if t==`h'
       
        replace b = _b[treat] if t == `h'
        replace u = _b[treat] + 1.645 * _se[treat] if t == `h'  // 90% upper bound
        replace d = _b[treat] - 1.645 * _se[treat] if t == `h'  // 90% lower bound
        
        replace u68 = _b[treat] + 1.0 * _se[treat] if t == `h'  // 68% upper bound
        replace d68 = _b[treat] - 1.0 * _se[treat] if t == `h'  // 68% lower bound
					qui eststo D`h'lpdid_5a
				
					if `h'>1 & `h'<=`pre_window' {
						qui reghdfe Dm`h'`dv'                              ///
                treat l.`dv' l2.`dv' l3.`dv' l4.`dv' i.hotwinter_2C i.hotspring_2C i.hotautumn_2C  ///   /* treatment indicator */
                if ((treat == 1 & predicted_hazard_heatwave == 1 | treat == 0) & (CCS_`post_window' == 1 & CCS_m`pre_window' == 1)) & Baseline_climate ==2,  ///   /* clean treatment and control condition */
                absorb(year Territory_ID) vce(cluster Territory_ID) level(90) /* time indicators */
** For subgroup analysis select: Baseline_climate == 1 (cold regions), Baseline_climate == 2 (temperate), Baseline_climate = 3 (hot)
						qui replace b_lpdid_5a_`dv' = _b[treat] if t==-`h'
						qui replace tstat_lpdid_5a_`dv'= _b[treat]/_se[treat] if t==-`h'
						
            replace b = _b[treat] if t == -`h'
            replace u = _b[treat] + 1.645 * _se[treat] if t == -`h'  // 90% upper bound
            replace d = _b[treat] - 1.645 * _se[treat] if t == -`h'  // 90% lower bound
            
            replace u68 = _b[treat] + 1.0 * _se[treat] if t == -`h'  // 68% upper bound
            replace d68 = _b[treat] - 1.0 * _se[treat] if t == -`h'  // 68% lower bound
            
						qui eststo Dm`h'lpdid_5a 
					}
				}
				
			nois esttab, se nocons keep(treat)
			disp(b)
			disp(b_lpdid_5a_`dv')

*** Graph
graph twoway ///
    (rarea d68 u68 t, fcolor(red%15) lcolor(red%0)) ///  // 68% confidence intervals
    || (rarea d u t, fcolor(red%10) lcolor(red%0)) ///  // 90% confidence intervals
    || (line b t, lcolor(navy) yline(0, lcolor(black)) ///  // Treatment effect line and zero line
	title("Figure 2 `dv' temperate regions", size(medium)) ///
    xlabel(-2(1)4, nogrid labsize(medlarge)) ylabel(, nogrid) ///
    graphregion(color(white)) plotregion(color(white)) xline(0, lcolor(red)) ///
	xtitle("years since event", size(medium)) ///
    legend(order(3 "Heatwaves" 1 "68% confidence interval" 2 "90% confidence interval") ///
    size(medium) pos(6) rows(2)  col(1)))
	
	graph export "Figure_2_`dv'_temperate.png", replace

}

restore 


preserve 
// 
*Hot regions 

foreach dv of varlist $dependent_variables_set2 {
    eststo clear
    capture drop b u d u68 d68 Zero
    gen b_lpdid_5a_`dv' = .
    gen tstat_lpdid_5a_`dv' = .
    gen Zero = 0 if t != .
    gen b = 0
    gen u = 0  // New variable for upper 90% confidence bound
    gen d = 0   // New variable for upper 90% confidence bound
    gen u68 = 0  // New variable for upper 68% confidence bound
    gen d68 = 0  // New variable for lower 68% confidence bound
			forval h = 0/`post_window' {
				qui reghdfe D`h'`dv' 	    							 ///
                    treat l.`dv' l2.`dv' l3.`dv' l4.`dv' i.hotwinter_2C i.hotspring_2C i.hotautumn_2C ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_heatwave==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)) & Baseline_climate ==3, 	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Baseline_climate == 1 (cold regions), Baseline_climate == 2 (temperate), Baseline_climate = 3 (hot)
					qui replace b_lpdid_5a_`dv'= _b[treat] if t==`h'
					qui replace tstat_lpdid_5a_`dv' = _b[treat]/_se[treat] if t==`h'
       
        replace b = _b[treat] if t == `h'
        replace u = _b[treat] + 1.645 * _se[treat] if t == `h'  // 90% upper bound
        replace d = _b[treat] - 1.645 * _se[treat] if t == `h'  // 90% lower bound
        
        replace u68 = _b[treat] + 1.0 * _se[treat] if t == `h'  // 68% upper bound
        replace d68 = _b[treat] - 1.0 * _se[treat] if t == `h'  // 68% lower bound
					qui eststo D`h'lpdid_5a
				
					if `h'>1 & `h'<=`pre_window' {
						qui reghdfe Dm`h'`dv'                              ///
                treat l.`dv' l2.`dv' l3.`dv' l4.`dv' i.hotwinter_2C i.hotspring_2C i.hotautumn_2C  ///   /* treatment indicator */
                if ((treat == 1 & predicted_hazard_heatwave == 1 | treat == 0) & (CCS_`post_window' == 1 & CCS_m`pre_window' == 1)) & Baseline_climate ==3,  ///   /* clean treatment and control condition */
                absorb(year Territory_ID) vce(cluster Territory_ID) level(90) /* time indicators */
** For subgroup analysis select: Baseline_climate == 1 (cold regions), Baseline_climate == 2 (temperate), Baseline_climate = 3 (hot)
						qui replace b_lpdid_5a_`dv' = _b[treat] if t==-`h'
						qui replace tstat_lpdid_5a_`dv'= _b[treat]/_se[treat] if t==-`h'
						
            replace b = _b[treat] if t == -`h'
            replace u = _b[treat] + 1.645 * _se[treat] if t == -`h'  // 90% upper bound
            replace d = _b[treat] - 1.645 * _se[treat] if t == -`h'  // 90% lower bound
            
            replace u68 = _b[treat] + 1.0 * _se[treat] if t == -`h'  // 68% upper bound
            replace d68 = _b[treat] - 1.0 * _se[treat] if t == -`h'  // 68% lower bound
            
						qui eststo Dm`h'lpdid_5a 
					}
				}
				
			nois esttab, se nocons keep(treat)
			disp(b)
			disp(b_lpdid_5a_`dv')

*** Graph
graph twoway ///
    (rarea d68 u68 t, fcolor(red%15) lcolor(red%0)) ///  // 68% confidence intervals
    || (rarea d u t, fcolor(red%10) lcolor(red%0)) ///  // 90% confidence intervals
    || (line b t, lcolor(navy) yline(0, lcolor(black)) ///  // Treatment effect line and zero line
	title("Figure 2 `dv' hot regions", size(medium)) ///
    xlabel(-2(1)4, nogrid labsize(medlarge)) ylabel(, nogrid) ///
    graphregion(color(white)) plotregion(color(white)) xline(0, lcolor(red)) ///
	xtitle("years since event", size(medium)) ///
    legend(order(3 "Heatwaves" 1 "68% confidence interval" 2 "90% confidence interval") ///
    size(medium) pos(6) rows(2)  col(1)))
	
	graph export "Figure_2_`dv'_hot.png", replace

		}		
	
restore


**************       LPDID HEATWAVES (FIGURE 3)

preserve 
// 
*Threshold 1.75 degree

drop treat
drop predicted_hazard_heatwave

** recreate predicted hazards on the new threshold
gen predicted_hazard_heatwave = 0
replace predicted_hazard_heatwave =1 if hotsummer_175C ==1     // Summer heatwave

gen treat = 0 //  common treatment variable showing hazard of any event in a particular year
replace treat = 1 if predicted_hazard_droughts ==1 | predicted_hazard_floods == 1 | predicted_hazard_heatwave== 1


foreach dv of varlist $dependent_variables_set1 {
    eststo clear
    capture drop b u d u68 d68 Zero
    gen b_lpdid_5a_`dv' = .
    gen tstat_lpdid_5a_`dv' = .
    gen Zero = 0 if t != .
    gen b = 0
    gen u = 0  // New variable for upper 90% confidence bound
    gen d = 0   // New variable for upper 90% confidence bound
    gen u68 = 0  // New variable for upper 68% confidence bound
    gen d68 = 0  // New variable for lower 68% confidence bound
			forval h = 0/`post_window' {
				qui reghdfe D`h'`dv' 	    							 ///
                    treat l.`dv' l2.`dv' l3.`dv' l4.`dv' i.hotwinter_175C i.hotspring_175C i.hotautumn_175C ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_heatwave==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)), 	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
					qui replace b_lpdid_5a_`dv'= _b[treat] if t==`h'
					qui replace tstat_lpdid_5a_`dv' = _b[treat]/_se[treat] if t==`h'
       
        replace b = _b[treat] if t == `h'
        replace u = _b[treat] + 1.645 * _se[treat] if t == `h'  // 90% upper bound
        replace d = _b[treat] - 1.645 * _se[treat] if t == `h'  // 90% lower bound
        
        replace u68 = _b[treat] + 1.0 * _se[treat] if t == `h'  // 68% upper bound
        replace d68 = _b[treat] - 1.0 * _se[treat] if t == `h'  // 68% lower bound
					qui eststo D`h'lpdid_5a
				
					if `h'>1 & `h'<=`pre_window' {
						qui reghdfe Dm`h'`dv'                              ///
                treat l.`dv' l2.`dv' l3.`dv' l4.`dv' i.hotwinter_175C i.hotspring_175C i.hotautumn_175C  ///   /* treatment indicator */
                if ((treat == 1 &  predicted_hazard_heatwave== 1 | treat == 0) & (CCS_`post_window' == 1 & CCS_m`pre_window' == 1)) ,  ///   /* clean treatment and control condition */
                absorb(year Territory_ID) vce(cluster Territory_ID) level(90) /* time indicators */
						qui replace b_lpdid_5a_`dv' = _b[treat] if t==-`h'
						qui replace tstat_lpdid_5a_`dv'= _b[treat]/_se[treat] if t==-`h'
						
            replace b = _b[treat] if t == -`h'
            replace u = _b[treat] + 1.645 * _se[treat] if t == -`h'  // 90% upper bound
            replace d = _b[treat] - 1.645 * _se[treat] if t == -`h'  // 90% lower bound
            
            replace u68 = _b[treat] + 1.0 * _se[treat] if t == -`h'  // 68% upper bound
            replace d68 = _b[treat] - 1.0 * _se[treat] if t == -`h'  // 68% lower bound
            
						qui eststo Dm`h'lpdid_5a 
					}
				}
				
			nois esttab, se nocons keep(treat)
			disp(b)
			disp(b_lpdid_5a_`dv')

*** Graph
graph twoway ///
    (rarea d68 u68 t, fcolor(red%15) lcolor(red%0)) ///  // 68% confidence intervals
    || (rarea d u t, fcolor(red%10) lcolor(red%0)) ///  // 90% confidence intervals
    || (line b t, lcolor(navy) yline(0, lcolor(black)) ///  // Treatment effect line and zero line
	title("Figure 3 Threshold 1.75 degree Celcius", size(medium)) ///
    xlabel(-2(1)4, nogrid labsize(medlarge)) ylabel(, nogrid) ///
    graphregion(color(white)) plotregion(color(white)) xline(0, lcolor(red)) ///
	xtitle("years since event", size(medium)) ///
    legend(order(3 "Heatwaves" 1 "68% confidence interval" 2 "90% confidence interval") ///
    size(medium) pos(6) rows(2)  col(1)))
	graph export "Figure_3a.png", replace

		}		
	
restore


******************

preserve 
// 
*Threshold 2.25 degree

drop treat
drop predicted_hazard_heatwave

** recreate predicted hazards on the new threshold
gen predicted_hazard_heatwave = 0
replace predicted_hazard_heatwave =1 if hotsummer_225C ==1     // Summer heatwave

gen treat = 0 //  common treatment variable showing hazard of any event in a particular year
replace treat = 1 if predicted_hazard_droughts ==1 | predicted_hazard_floods == 1 | predicted_hazard_heatwave== 1

foreach dv of varlist $dependent_variables_set1 {
    eststo clear
    capture drop b u d u68 d68 Zero
    gen b_lpdid_5a_`dv' = .
    gen tstat_lpdid_5a_`dv' = .
    gen Zero = 0 if t != .
    gen b = 0
    gen u = 0  // New variable for upper 90% confidence bound
    gen d = 0   // New variable for upper 90% confidence bound
    gen u68 = 0  // New variable for upper 68% confidence bound
    gen d68 = 0  // New variable for lower 68% confidence bound
			forval h = 0/`post_window' {
				qui reghdfe D`h'`dv' 	    							 ///
                    treat l.`dv' l2.`dv' l3.`dv' l4.`dv' i.hotwinter_225C i.hotspring_225C i.hotautumn_225C ///   /* treatment indicator */
				if 	((treat==1 &  predicted_hazard_heatwave==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)), 	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Baseline_climate == 1 (cold regions), Baseline_climate == 2 (temperate), Baseline_climate = 3 (hot)
					qui replace b_lpdid_5a_`dv'= _b[treat] if t==`h'
					qui replace tstat_lpdid_5a_`dv' = _b[treat]/_se[treat] if t==`h'
       
        replace b = _b[treat] if t == `h'
        replace u = _b[treat] + 1.645 * _se[treat] if t == `h'  // 90% upper bound
        replace d = _b[treat] - 1.645 * _se[treat] if t == `h'  // 90% lower bound
        
        replace u68 = _b[treat] + 1.0 * _se[treat] if t == `h'  // 68% upper bound
        replace d68 = _b[treat] - 1.0 * _se[treat] if t == `h'  // 68% lower bound
					qui eststo D`h'lpdid_5a
				
					if `h'>1 & `h'<=`pre_window' {
						qui reghdfe Dm`h'`dv'                              ///
                treat l.`dv' l2.`dv' l3.`dv' l4.`dv' i.hotwinter_225C i.hotspring_225C i.hotautumn_225C  ///   /* treatment indicator */
                if ((treat == 1 & predicted_hazard_heatwave == 1 | treat == 0) & (CCS_`post_window' == 1 & CCS_m`pre_window' == 1)),  ///   /* clean treatment and control condition */
                absorb(year Territory_ID) vce(cluster Territory_ID) level(90) /* time indicators */
** For subgroup analysis select: Baseline_climate == 1 (cold regions), Baseline_climate == 2 (temperate), Baseline_climate = 3 (hot)
						qui replace b_lpdid_5a_`dv' = _b[treat] if t==-`h'
						qui replace tstat_lpdid_5a_`dv'= _b[treat]/_se[treat] if t==-`h'
						
            replace b = _b[treat] if t == -`h'
            replace u = _b[treat] + 1.645 * _se[treat] if t == -`h'  // 90% upper bound
            replace d = _b[treat] - 1.645 * _se[treat] if t == -`h'  // 90% lower bound
            
            replace u68 = _b[treat] + 1.0 * _se[treat] if t == -`h'  // 68% upper bound
            replace d68 = _b[treat] - 1.0 * _se[treat] if t == -`h'  // 68% lower bound
            
						qui eststo Dm`h'lpdid_5a 
					}
				}
				
			nois esttab, se nocons keep(treat)
			disp(b)
			disp(b_lpdid_5a_`dv')

*** Graph
graph twoway ///
    (rarea d68 u68 t, fcolor(red%15) lcolor(red%0)) ///  // 68% confidence intervals
    || (rarea d u t, fcolor(red%10) lcolor(red%0)) ///  // 90% confidence intervals
    || (line b t, lcolor(navy) yline(0, lcolor(black)) ///  // Treatment effect line and zero line
    xlabel(-2(1)4, nogrid labsize(medlarge)) ylabel(, nogrid) ///
	title("Figure 3 Threshold 2.25 degree Celcius", size(medium)) ///
    graphregion(color(white)) plotregion(color(white)) xline(0, lcolor(red)) ///
	xtitle("years since event", size(medium)) ///
    legend(order(3 "Heatwaves" 1 "68% confidence interval" 2 "90% confidence interval") ///
    size(medium) pos(6) rows(2)  col(1)))
	
	graph export "Figure_3b.png", replace

		}		
	
restore



**************       LPDID DROUGHTS (FIGURE 4)

preserve 
foreach dv of varlist $dependent_variables {
    eststo clear
    capture drop b u d u68 d68 Zero
    gen b_lpdid_5a_`dv' = .
    gen tstat_lpdid_5a_`dv' = .
    gen Zero = 0 if t != .
    gen b = 0
    gen u = 0  // New variable for upper 90% confidence bound
    gen d = 0   // New variable for upper 90% confidence bound
    gen u68 = 0  // New variable for upper 68% confidence bound
    gen d68 = 0  // New variable for lower 68% confidence bound
			forval h = 0/`post_window' {
				qui reghdfe D`h'`dv' 	    							 ///
						treat  l.`dv'  l2.`dv'  l3.`dv'  l4.`dv'    ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_droughts==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)),	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Baseline_climate == 1 (cold regions), Baseline_climate == 2 (temperate), Baseline_climate = 3 (hot)
					qui replace b_lpdid_5a_`dv'= _b[treat] if t==`h'
					qui replace tstat_lpdid_5a_`dv' = _b[treat]/_se[treat] if t==`h'
       
        replace b = _b[treat] if t == `h'
        replace u = _b[treat] + 1.645 * _se[treat] if t == `h'  // 90% upper bound
        replace d = _b[treat] - 1.645 * _se[treat] if t == `h'  // 90% lower bound
        
        replace u68 = _b[treat] + 1.0 * _se[treat] if t == `h'  // 68% upper bound
        replace d68 = _b[treat] - 1.0 * _se[treat] if t == `h'  // 68% lower bound
					qui eststo D`h'lpdid_5a
				
					if `h'>1 & `h'<=`pre_window' {
						qui reghdfe Dm`h'`dv'  	  																		///
						treat  l.`dv'  l2.`dv'  l3.`dv'  l4.`dv'     ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_droughts==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)),	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Baseline_climate == 1 (cold regions), Baseline_climate == 2 (temperate), Baseline_climate = 3 (hot)

						qui replace b_lpdid_5a_`dv' = _b[treat] if t==-`h'
						qui replace tstat_lpdid_5a_`dv'= _b[treat]/_se[treat] if t==-`h'
						
            replace b = _b[treat] if t == -`h'
            replace u = _b[treat] + 1.645 * _se[treat] if t == -`h'  // 90% upper bound
            replace d = _b[treat] - 1.645 * _se[treat] if t == -`h'  // 90% lower bound
            
            replace u68 = _b[treat] + 1.0 * _se[treat] if t == -`h'  // 68% upper bound
            replace d68 = _b[treat] - 1.0 * _se[treat] if t == -`h'  // 68% lower bound
            
						qui eststo Dm`h'lpdid_5a
					}
				}
				
			nois esttab, se nocons keep(treat)
			disp(b)
			disp(b_lpdid_5a_`dv')

graph twoway ///
    (rarea d68 u68 t, fcolor(orange%15) lcolor(navy%0)) ///  // 68% confidence intervals
    || (rarea d u t, fcolor(orange%10) lcolor(navy%0)) ///  // 90% confidence intervals
    || (line b t, lcolor(navy) yline(0, lcolor(black)) ///  // Treatment effect line and zero line
	title("Figure 4 `dv'", size(medium)) ///
    xlabel(-2(1)4, nogrid labsize(medlarge)) ylabel(, nogrid) ///
    graphregion(color(white)) plotregion(color(white)) xline(0, lcolor(red)) ///
	xtitle("years since event", size(medium)) ///
    legend(order(3 "Drought" 1 "68% confidence interval" 2 "90% confidence interval") ///
    size(medium) pos(6) rows(2)  col(1)))				
	disp "`dv'"

   graph export "Figure_4_`dv'.png", replace
		}	
		
restore 



**************       LPDID FLOODS (FIGURE 5)

preserve 
foreach dv of varlist $dependent_variables {
    eststo clear
    capture drop b u d u68 d68 Zero
    gen b_lpdid_5a_`dv' = .
    gen tstat_lpdid_5a_`dv' = .
    gen Zero = 0 if t != .
    gen b = 0
    gen u = 0  // New variable for upper 90% confidence bound
    gen d = 0   // New variable for upper 90% confidence bound
    gen u68 = 0  // New variable for upper 68% confidence bound
    gen d68 = 0  // New variable for lower 68% confidence bound
			forval h = 0/`post_window' {
				qui reghdfe D`h'`dv' 	    							 ///
						treat  l.`dv'  l2.`dv'  l3.`dv'  l4.`dv'     ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_floods==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)) ,	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Income_group == 1 (for low income),  Income_group == 2 (for middle income) and Income_group ==3 (for high income)
					qui replace b_lpdid_5a_`dv'= _b[treat] if t==`h'
					qui replace tstat_lpdid_5a_`dv' = _b[treat]/_se[treat] if t==`h'
       
        replace b = _b[treat] if t == `h'
        replace u = _b[treat] + 1.645 * _se[treat] if t == `h'  // 90% upper bound
        replace d = _b[treat] - 1.645 * _se[treat] if t == `h'  // 90% lower bound
        
        replace u68 = _b[treat] + 1.0 * _se[treat] if t == `h'  // 68% upper bound
        replace d68 = _b[treat] - 1.0 * _se[treat] if t == `h'  // 68% lower bound
					qui eststo D`h'lpdid_5a
				
					if `h'>1 & `h'<=`pre_window' {
						qui reghdfe Dm`h'`dv'  	  																		///
						treat  l.`dv'  l2.`dv'  l3.`dv'  l4.`dv'     ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_floods==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)),	 ///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Income_group == 1 (for low income),  Income_group == 2 (for middle income) and Income_group ==3 (for high income)
						qui replace b_lpdid_5a_`dv' = _b[treat] if t==-`h'
						qui replace tstat_lpdid_5a_`dv'= _b[treat]/_se[treat] if t==-`h'
						
            replace b = _b[treat] if t == -`h'
            replace u = _b[treat] + 1.645 * _se[treat] if t == -`h'  // 90% upper bound
            replace d = _b[treat] - 1.645 * _se[treat] if t == -`h'  // 90% lower bound
            
            replace u68 = _b[treat] + 1.0 * _se[treat] if t == -`h'  // 68% upper bound
            replace d68 = _b[treat] - 1.0 * _se[treat] if t == -`h'  // 68% lower bound
            
						qui eststo Dm`h'lpdid_5a
					} 
				}
				
			nois esttab, se nocons keep(treat)
			disp(b)
			disp(b_lpdid_5a_`dv')


graph twoway ///
    (rarea d68 u68 t, fcolor(navy%30) lcolor(navy%0)) ///  // 68% confidence intervals
    || (rarea d u t, fcolor(navy%10) lcolor(navy%0)) ///  // 90% confidence intervals
    || (line b t, lcolor(navy) yline(0, lcolor(black)) ///  // Treatment effect line and zero line
	title("Figure 5 `dv'", size(medium)) ///
    xlabel(-2(1)4, nogrid labsize(medlarge)) ylabel(, nogrid) ///
    graphregion(color(white)) plotregion(color(white)) xline(0, lcolor(red)) ///
	xtitle("years since event", size(medium)) ///
    legend(order(3 "Floods" 1 "68% confidence interval" 2 "90% confidence interval") ///
    size(medium) pos(6) rows(2)  col(1)))
				disp "`dv'"
	     graph export "Figure_5_`dv'.png", replace
		}				
		
restore 

**************       LPDID FLOODS (FIGURE 6)

*** low income 
preserve 
foreach dv of varlist $dependent_variables_set2 {
    eststo clear
    capture drop b u d u68 d68 Zero
    gen b_lpdid_5a_`dv' = .
    gen tstat_lpdid_5a_`dv' = .
    gen Zero = 0 if t != .
    gen b = 0
    gen u = 0  // New variable for upper 90% confidence bound
    gen d = 0   // New variable for upper 90% confidence bound
    gen u68 = 0  // New variable for upper 68% confidence bound
    gen d68 = 0  // New variable for lower 68% confidence bound
			forval h = 0/`post_window' {
				qui reghdfe D`h'`dv' 	    							 ///
						treat  l.`dv'  l2.`dv'  l3.`dv'  l4.`dv'     ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_floods==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1))  & Income_group == 1,	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Income_group == 1 (for low income),  Income_group == 2 (for middle income) and Income_group ==3 (for high income)
					qui replace b_lpdid_5a_`dv'= _b[treat] if t==`h'
					qui replace tstat_lpdid_5a_`dv' = _b[treat]/_se[treat] if t==`h'
       
        replace b = _b[treat] if t == `h'
        replace u = _b[treat] + 1.645 * _se[treat] if t == `h'  // 90% upper bound
        replace d = _b[treat] - 1.645 * _se[treat] if t == `h'  // 90% lower bound
        
        replace u68 = _b[treat] + 1.0 * _se[treat] if t == `h'  // 68% upper bound
        replace d68 = _b[treat] - 1.0 * _se[treat] if t == `h'  // 68% lower bound
					qui eststo D`h'lpdid_5a
				
					if `h'>1 & `h'<=`pre_window' {
						qui reghdfe Dm`h'`dv'  	  																		///
						treat  l.`dv'  l2.`dv'  l3.`dv'  l4.`dv'     ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_floods==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)) & Income_group == 1,	 ///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Income_group == 1 (for low income),  Income_group == 2 (for middle income) and Income_group ==3 (for high income)
						qui replace b_lpdid_5a_`dv' = _b[treat] if t==-`h'
						qui replace tstat_lpdid_5a_`dv'= _b[treat]/_se[treat] if t==-`h'
						
            replace b = _b[treat] if t == -`h'
            replace u = _b[treat] + 1.645 * _se[treat] if t == -`h'  // 90% upper bound
            replace d = _b[treat] - 1.645 * _se[treat] if t == -`h'  // 90% lower bound
            
            replace u68 = _b[treat] + 1.0 * _se[treat] if t == -`h'  // 68% upper bound
            replace d68 = _b[treat] - 1.0 * _se[treat] if t == -`h'  // 68% lower bound
            
						qui eststo Dm`h'lpdid_5a
					} 
				}
				
			nois esttab, se nocons keep(treat)
			disp(b)
			disp(b_lpdid_5a_`dv')


graph twoway ///
    (rarea d68 u68 t, fcolor(navy%30) lcolor(navy%0)) ///  // 68% confidence intervals
    || (rarea d u t, fcolor(navy%10) lcolor(navy%0)) ///  // 90% confidence intervals
    || (line b t, lcolor(navy) yline(0, lcolor(black)) ///  // Treatment effect line and zero line
	title("Figure 6 `dv' low income", size(medium)) ///
    xlabel(-2(1)4, nogrid labsize(medlarge)) ylabel(, nogrid) ///
    graphregion(color(white)) plotregion(color(white)) xline(0, lcolor(red)) ///
	xtitle("years since event", size(medium)) ///
    legend(order(3 "Floods" 1 "68% confidence interval" 2 "90% confidence interval") ///
    size(medium) pos(6) rows(2)  col(1)))
				disp "`dv'"
	     graph export "Figure_6_`dv'_lowincome.png", replace
		}				
		
restore 

**** middle income 

preserve 
foreach dv of varlist $dependent_variables_set2 {
    eststo clear
    capture drop b u d u68 d68 Zero
    gen b_lpdid_5a_`dv' = .
    gen tstat_lpdid_5a_`dv' = .
    gen Zero = 0 if t != .
    gen b = 0
    gen u = 0  // New variable for upper 90% confidence bound
    gen d = 0   // New variable for upper 90% confidence bound
    gen u68 = 0  // New variable for upper 68% confidence bound
    gen d68 = 0  // New variable for lower 68% confidence bound
			forval h = 0/`post_window' {
				qui reghdfe D`h'`dv' 	    							 ///
						treat  l.`dv'  l2.`dv'  l3.`dv'  l4.`dv'     ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_floods==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1))  & Income_group == 2,	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Income_group == 1 (for low income),  Income_group == 2 (for middle income) and Income_group ==3 (for high income)
					qui replace b_lpdid_5a_`dv'= _b[treat] if t==`h'
					qui replace tstat_lpdid_5a_`dv' = _b[treat]/_se[treat] if t==`h'
       
        replace b = _b[treat] if t == `h'
        replace u = _b[treat] + 1.645 * _se[treat] if t == `h'  // 90% upper bound
        replace d = _b[treat] - 1.645 * _se[treat] if t == `h'  // 90% lower bound
        
        replace u68 = _b[treat] + 1.0 * _se[treat] if t == `h'  // 68% upper bound
        replace d68 = _b[treat] - 1.0 * _se[treat] if t == `h'  // 68% lower bound
					qui eststo D`h'lpdid_5a
				
					if `h'>1 & `h'<=`pre_window' {
						qui reghdfe Dm`h'`dv'  	  																		///
						treat  l.`dv'  l2.`dv'  l3.`dv'  l4.`dv'     ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_floods==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)) & Income_group == 2,	 ///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Income_group == 1 (for low income),  Income_group == 2 (for middle income) and Income_group ==3 (for high income)
						qui replace b_lpdid_5a_`dv' = _b[treat] if t==-`h'
						qui replace tstat_lpdid_5a_`dv'= _b[treat]/_se[treat] if t==-`h'
						
            replace b = _b[treat] if t == -`h'
            replace u = _b[treat] + 1.645 * _se[treat] if t == -`h'  // 90% upper bound
            replace d = _b[treat] - 1.645 * _se[treat] if t == -`h'  // 90% lower bound
            
            replace u68 = _b[treat] + 1.0 * _se[treat] if t == -`h'  // 68% upper bound
            replace d68 = _b[treat] - 1.0 * _se[treat] if t == -`h'  // 68% lower bound
            
						qui eststo Dm`h'lpdid_5a
					} 
				}
				
			nois esttab, se nocons keep(treat)
			disp(b)
			disp(b_lpdid_5a_`dv')


graph twoway ///
    (rarea d68 u68 t, fcolor(navy%30) lcolor(navy%0)) ///  // 68% confidence intervals
    || (rarea d u t, fcolor(navy%10) lcolor(navy%0)) ///  // 90% confidence intervals
    || (line b t, lcolor(navy) yline(0, lcolor(black)) ///  // Treatment effect line and zero line
	title("Figure 6 `dv' middle income", size(medium)) ///
    xlabel(-2(1)4, nogrid labsize(medlarge)) ylabel(, nogrid) ///
    graphregion(color(white)) plotregion(color(white)) xline(0, lcolor(red)) ///
	xtitle("years since event", size(medium)) ///
    legend(order(3 "Floods" 1 "68% confidence interval" 2 "90% confidence interval") ///
    size(medium) pos(6) rows(2)  col(1)))
				disp "`dv'"
	     graph export "Figure_6_`dv'_middleincome.png", replace
		}				
		
restore 

**** high income 

preserve 
foreach dv of varlist $dependent_variables_set2 {
    eststo clear
    capture drop b u d u68 d68 Zero
    gen b_lpdid_5a_`dv' = .
    gen tstat_lpdid_5a_`dv' = .
    gen Zero = 0 if t != .
    gen b = 0
    gen u = 0  // New variable for upper 90% confidence bound
    gen d = 0   // New variable for upper 90% confidence bound
    gen u68 = 0  // New variable for upper 68% confidence bound
    gen d68 = 0  // New variable for lower 68% confidence bound
			forval h = 0/`post_window' {
				qui reghdfe D`h'`dv' 	    							 ///
						treat  l.`dv'  l2.`dv'  l3.`dv'  l4.`dv'     ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_floods==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1))  & Income_group == 3,	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Income_group == 1 (for low income),  Income_group == 2 (for middle income) and Income_group ==3 (for high income)
					qui replace b_lpdid_5a_`dv'= _b[treat] if t==`h'
					qui replace tstat_lpdid_5a_`dv' = _b[treat]/_se[treat] if t==`h'
       
        replace b = _b[treat] if t == `h'
        replace u = _b[treat] + 1.645 * _se[treat] if t == `h'  // 90% upper bound
        replace d = _b[treat] - 1.645 * _se[treat] if t == `h'  // 90% lower bound
        
        replace u68 = _b[treat] + 1.0 * _se[treat] if t == `h'  // 68% upper bound
        replace d68 = _b[treat] - 1.0 * _se[treat] if t == `h'  // 68% lower bound
					qui eststo D`h'lpdid_5a
				
					if `h'>1 & `h'<=`pre_window' {
						qui reghdfe Dm`h'`dv'  	  																		///
						treat  l.`dv'  l2.`dv'  l3.`dv'  l4.`dv'     ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_floods==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)) & Income_group == 3,	 ///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
** For subgroup analysis select: Income_group == 1 (for low income),  Income_group == 2 (for middle income) and Income_group ==3 (for high income)
						qui replace b_lpdid_5a_`dv' = _b[treat] if t==-`h'
						qui replace tstat_lpdid_5a_`dv'= _b[treat]/_se[treat] if t==-`h'
						
            replace b = _b[treat] if t == -`h'
            replace u = _b[treat] + 1.645 * _se[treat] if t == -`h'  // 90% upper bound
            replace d = _b[treat] - 1.645 * _se[treat] if t == -`h'  // 90% lower bound
            
            replace u68 = _b[treat] + 1.0 * _se[treat] if t == -`h'  // 68% upper bound
            replace d68 = _b[treat] - 1.0 * _se[treat] if t == -`h'  // 68% lower bound
            
						qui eststo Dm`h'lpdid_5a
					} 
				}
				
			nois esttab, se nocons keep(treat)
			disp(b)
			disp(b_lpdid_5a_`dv')


graph twoway ///
    (rarea d68 u68 t, fcolor(navy%30) lcolor(navy%0)) ///  // 68% confidence intervals
    || (rarea d u t, fcolor(navy%10) lcolor(navy%0)) ///  // 90% confidence intervals
    || (line b t, lcolor(navy) yline(0, lcolor(black)) ///  // Treatment effect line and zero line
	title("Figure 6 `dv' high income", size(medium)) ///
    xlabel(-2(1)4, nogrid labsize(medlarge)) ylabel(, nogrid) ///
    graphregion(color(white)) plotregion(color(white)) xline(0, lcolor(red)) ///
	xtitle("years since event", size(medium)) ///
    legend(order(3 "Floods" 1 "68% confidence interval" 2 "90% confidence interval") ///
    size(medium) pos(6) rows(2)  col(1)))
				disp "`dv'"
	     graph export "Figure_6_`dv'_highincome.png", replace
		}				
		
restore 


/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% SECTORAL ANALYSIS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/



merge 1:1 Territory_ID year using "$github_path/GVA_NACE_sector"
keep if _merge == 3
drop  _merge

foreach var in Agriculture Manufacturing Construction Services{
gen ln_`var'=100*ln(`var')
}


#delimit ;		

	vl create dependent_variables_sector = (ln_Agriculture ln_Manufacturing ln_Construction ln_Services);

#delimit cr						/* Changes the delimiter back to carriage return */

	
sort ID_var year

foreach dv of varlist $dependent_variables_sector {
	forval h = 0/`post_window' {
		qui gen D`h'`dv' = F`h'.`dv' - L.`dv'
		}

	forval h = 2/`pre_window' {
		gen Dm`h'`dv' = L`h'.`dv' - L.`dv'
	}


}


**************       LPDID FLOODS SECTOR ANALYSIS (FIGURE 7)

preserve 
foreach dv of varlist $dependent_variables_sector {
    eststo clear
    capture drop b u d u68 d68 Zero
    gen b_lpdid_5a_`dv' = .
    gen tstat_lpdid_5a_`dv' = .
    gen Zero = 0 if t != .
    gen b = 0
    gen u = 0  // New variable for upper 90% confidence bound
    gen d = 0   // New variable for upper 90% confidence bound
    gen u68 = 0  // New variable for upper 68% confidence bound
    gen d68 = 0  // New variable for lower 68% confidence bound
			forval h = 0/`post_window' {
				qui reghdfe D`h'`dv' 	    							 ///
						treat  l.`dv'  l2.`dv'  l3.`dv'  l4.`dv'     ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_floods==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)) ,	///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
					qui replace b_lpdid_5a_`dv'= _b[treat] if t==`h'
					qui replace tstat_lpdid_5a_`dv' = _b[treat]/_se[treat] if t==`h'
       
        replace b = _b[treat] if t == `h'
        replace u = _b[treat] + 1.645 * _se[treat] if t == `h'  // 90% upper bound
        replace d = _b[treat] - 1.645 * _se[treat] if t == `h'  // 90% lower bound
        
        replace u68 = _b[treat] + 1.0 * _se[treat] if t == `h'  // 68% upper bound
        replace d68 = _b[treat] - 1.0 * _se[treat] if t == `h'  // 68% lower bound
					qui eststo D`h'lpdid_5a
				
					if `h'>1 & `h'<=`pre_window' {
						qui reghdfe Dm`h'`dv'  	  																		///
						treat  l.`dv'  l2.`dv'  l3.`dv'  l4.`dv'     ///   /* treatment indicator */
				if 	((treat==1 & predicted_hazard_floods==1  | treat==0)  & (CCS_`post_window'==1 &  CCS_m`pre_window'==1)),	 ///   /* clean treatment and control condition */
						absorb(year Territory_ID) vce(cluster Territory_ID) level(90)		   		   /* time indicators */
						qui replace b_lpdid_5a_`dv' = _b[treat] if t==-`h'
						qui replace tstat_lpdid_5a_`dv'= _b[treat]/_se[treat] if t==-`h'
						
            replace b = _b[treat] if t == -`h'
            replace u = _b[treat] + 1.645 * _se[treat] if t == -`h'  // 90% upper bound
            replace d = _b[treat] - 1.645 * _se[treat] if t == -`h'  // 90% lower bound
            
            replace u68 = _b[treat] + 1.0 * _se[treat] if t == -`h'  // 68% upper bound
            replace d68 = _b[treat] - 1.0 * _se[treat] if t == -`h'  // 68% lower bound
            
						qui eststo Dm`h'lpdid_5a
					} 
				}
				
			nois esttab, se nocons keep(treat)
			disp(b)
			disp(b_lpdid_5a_`dv')


graph twoway ///
    (rarea d68 u68 t, fcolor(navy%30) lcolor(navy%0)) ///  // 68% confidence intervals
    || (rarea d u t, fcolor(navy%10) lcolor(navy%0)) ///  // 90% confidence intervals
    || (line b t, lcolor(navy) yline(0, lcolor(black)) ///  // Treatment effect line and zero line
	title("Figure 7 `dv'", size(medium)) ///
    xlabel(-2(1)4, nogrid labsize(medlarge)) ylabel(, nogrid) ///
    graphregion(color(white)) plotregion(color(white)) xline(0, lcolor(red)) ///
	xtitle("years since event", size(medium)) ///
    legend(order(3 "Floods" 1 "68% confidence interval" 2 "90% confidence interval") ///
    size(medium) pos(6) rows(2)  col(1)))
				disp "`dv'"
	graph export "Figure_7_`dv'.png", replace
		}				
		
restore 
