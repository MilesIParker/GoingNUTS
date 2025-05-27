

********************** Prepare the Data
**** Required STATA Packages 
ssc install tsspell
ssc install gammafit

********************** Import the Data from raw_files folder and merge using combine.ado file 
global github_path "https://raw.githubusercontent.com/MilesIParker/GoingNUTS/main"

*ADO file 
combine, saveas("raw_data.dta") // this will store the file in STATA memory and local directory 

********************** Define the global variables 

global year year 
global date date 
global date date
global country country_long
global Country_ID Country_ID
global Territory_ID Territory_ID

********************** Rename variables 
rename cntr_code ${Country_ID}
rename nuts_id ${Territory_ID} // for merging 

* Keep EU27 sample 
*  "AT" "BE" "BG" "HR" "CY" "CZ" "DK" "EE" "FI" "FR" "DE" "EL" "HU" "IE" "IT" "LV" "LT" "LU" "MT"  "NL"  "PL" "PT" "RO" "SK" "SI"  "ES" "SE" 
*** drop non-EU27 Countries 
drop if ${Country_ID} == "CH" 
drop if ${Country_ID} ==  "IS" 
drop if ${Country_ID} ==  "ME"
drop if ${Country_ID} == "NO" 
drop if ${Country_ID} == "TR" 
drop if ${Country_ID} == "UK" 
drop if ${Country_ID} == "RS"
drop if ${Country_ID} == "AL"
drop if ${Country_ID} == "MK"
drop if ${Country_ID} == "LI"


* Formating the Date variable 
gen date = date(time, "YMD")
format date %td
gen year = year(date)     // year
gen quarter= quarter(date) // calender quarter 
gen month = month(date) // month 

gen calenderyear = yq(year,quarter) // Time variable for our analysis 
format calenderyear %tq
sort ${Territory_ID} year quarter month // sorting based on calander year variables
gen int t_var = ym(year, month) // integer time variable for panel analysis 
format t_var %tm

* Drop missing data 
drop if pr_mean == . & t_mean == . // drop missing values 
* list Territory_ID if pr_mean ==. // check which territories drop out 


* Redefine quarters based on Seasons
gen meteo_quarter = . // Seasonal quarters 
replace meteo_quarter  = 1 if month == 12 | month == 1 | month == 2  // winter 
replace meteo_quarter = 2 if month == 3 | month == 4 | month == 5   // spring 
replace meteo_quarter  = 3 if month == 6 | month == 7 | month == 8   // summers 
replace meteo_quarter  = 4 if month == 9 | month == 10 | month == 11 // fall 

* Adjust the year for December data for meteorological year 
gen meteo_only_year = year 
replace meteo_only_year= year +1 if month == 12
gen meteoyear = yq(meteo_only_year,meteo_quarter)
format meteoyear %tq


***************** Defining the heatwaves 
bysort ${Country_ID} ${Territory_ID} meteo_only_year: egen annual_avg_temp_dev = mean(t_diff_1991_2020) // average annual temperature deviation from historical mean

* Generate long run mean (1991_2020) for meteorological year in each quarter 
egen aux = mean(t_mean) if meteo_only_year > 1990 & meteo_only_year < 2021 & meteo_only_year !=.  , by (${Territory_ID} meteo_quarter) 
egen meteor_avgtemp_quarter_hist = min(aux) , by (${Territory_ID} meteo_quarter) 
drop aux 

* Generate long run mean (1991_2020) for calendar year in each quarter 
egen aux = mean(t_mean) if year > 1990 & year < 2021 & year !=.  , by (${Territory_ID} quarter) 
egen calendar_avgtemp_quarter_hist = min(aux) , by (${Territory_ID} quarter) 
sort ${Territory_ID} meteo_only_year meteo_quarter
drop aux 

* Generate long run mean (1991_2020) for meteorological year in each month 
egen aux = mean(t_mean) if meteo_only_year > 1990 & meteo_only_year < 2021 & meteo_only_year !=.  , by (${Territory_ID} month) 
egen meteor_avgtemp_month_hist = min(aux) , by (${Territory_ID} month) 
drop aux 

* Generate quarterly absolute temperature averages both for meteorological and calendar year 
egen temp_meteo_quarter = mean(t_mean), by (${Territory_ID} meteo_quarter meteo_only_year) // seasonal average of abs temperature 
egen temp_quarter = mean (t_mean), by (${Territory_ID} quarter year) // quarterly average of abs temperature

* Generate quarterly deviations from long run mean 
gen temp_meteo_quarter_dev = temp_meteo_quarter -  meteor_avgtemp_quarter_hist   // Quarterly deviations from historical mean based on seasons 
gen temp_calendar_quarter_dev = temp_quarter - calendar_avgtemp_quarter_hist     // Quarterly deviations from historical mean based on calander year

****************** Create weather dummies

gen winter = .
replace winter = 1 if meteo_quarter ==1
gen spring = .
replace spring = 1 if meteo_quarter ==2
gen summer = .
replace summer = 1 if meteo_quarter ==3
gen autumn = .
replace autumn = 1 if meteo_quarter ==4


**** variables for heat shocks 


*Baseline 

gen  hotsummer_2C =. 
replace hotsummer_2C = 1 if  temp_meteo_quarter_dev >2 & summer==1 &  temp_meteo_quarter_dev!=.
replace hotsummer_2C = 1 if temp_meteo_quarter_dev == 2 & summer==1 & temp_meteo_quarter_dev!=.
replace hotsummer_2C = 0 if  temp_meteo_quarter_dev<2 & summer==1 &  temp_meteo_quarter_dev!=.

gen  hotautumn_2C =. 
replace hotautumn_2C = 1 if  temp_meteo_quarter_dev >2 & autumn==1 &  temp_meteo_quarter_dev!=.
replace hotautumn_2C = 1 if temp_meteo_quarter_dev == 2 & autumn==1 & temp_meteo_quarter_dev!=.
replace hotautumn_2C = 0 if  temp_meteo_quarter_dev<2 & autumn==1 &  temp_meteo_quarter_dev!=.

gen  hotwinter_2C =. 
replace hotwinter_2C = 1 if  temp_meteo_quarter_dev >2 & winter==1 &  temp_meteo_quarter_dev!=.
replace hotwinter_2C = 1 if temp_meteo_quarter_dev == 2 & winter==1 & temp_meteo_quarter_dev!=.
replace hotwinter_2C = 0 if  temp_meteo_quarter_dev<2 & winter==1 &  temp_meteo_quarter_dev!=.

gen  hotspring_2C =. 
replace hotspring_2C = 1 if  temp_meteo_quarter_dev >2 & spring==1 &  temp_meteo_quarter_dev!=.
replace hotspring_2C = 1 if temp_meteo_quarter_dev == 2 & spring==1 & temp_meteo_quarter_dev!=.
replace hotspring_2C = 0 if  temp_meteo_quarter_dev<2 & spring==1 &  temp_meteo_quarter_dev!=.

*Robustness check 1.75

gen  hotsummer_175C =. 
replace hotsummer_175C = 1 if  temp_meteo_quarter_dev >1.75 & summer==1 &  temp_meteo_quarter_dev!=.
replace hotsummer_175C = 1 if temp_meteo_quarter_dev == 1.75 & summer==1 & temp_meteo_quarter_dev!=.
replace hotsummer_175C = 0 if  temp_meteo_quarter_dev<1.75 & summer==1 &  temp_meteo_quarter_dev!=.



gen  hotautumn_175C =. 
replace hotautumn_175C = 1 if  temp_meteo_quarter_dev >1.75 & autumn==1 &  temp_meteo_quarter_dev!=.
replace hotautumn_175C = 1 if temp_meteo_quarter_dev == 1.75 & autumn==1 & temp_meteo_quarter_dev!=.
replace hotautumn_175C = 0 if  temp_meteo_quarter_dev<1.75 & autumn==1 &  temp_meteo_quarter_dev!=.

gen  hotwinter_175C =. 
replace hotwinter_175C = 1 if  temp_meteo_quarter_dev >1.75 & winter==1 &  temp_meteo_quarter_dev!=.
replace hotwinter_175C = 1 if temp_meteo_quarter_dev == 1.75 & winter==1 & temp_meteo_quarter_dev!=.
replace hotwinter_175C = 0 if  temp_meteo_quarter_dev<1.75 & winter==1 &  temp_meteo_quarter_dev!=.

gen  hotspring_175C =. 
replace hotspring_175C = 1 if  temp_meteo_quarter_dev >1.75 & spring==1 &  temp_meteo_quarter_dev!=.
replace hotspring_175C = 1 if temp_meteo_quarter_dev == 1.75 & spring==1 & temp_meteo_quarter_dev!=.
replace hotspring_175C = 0 if  temp_meteo_quarter_dev<1.75 & spring==1 &  temp_meteo_quarter_dev!=.


*Robustness check 2.25

gen  hotsummer_225C =. 
replace hotsummer_225C = 1 if  temp_meteo_quarter_dev >2.25 & summer==1 &  temp_meteo_quarter_dev!=.
replace hotsummer_225C = 1 if temp_meteo_quarter_dev == 2.25 & summer==1 & temp_meteo_quarter_dev!=.
replace hotsummer_225C = 0 if  temp_meteo_quarter_dev<2.25 & summer==1 &  temp_meteo_quarter_dev!=.
replace hotsummer_225C = 0 if  temp_meteo_quarter_dev<-2.25 & summer==1 &  temp_meteo_quarter_dev!=.


gen  hotautumn_225C =. 
replace hotautumn_225C = 1 if  temp_meteo_quarter_dev >2.25 & autumn==1 &  temp_meteo_quarter_dev!=.
replace hotautumn_225C = 1 if temp_meteo_quarter_dev == 2.25 & autumn==1 & temp_meteo_quarter_dev!=.
replace hotautumn_225C = 0 if  temp_meteo_quarter_dev<2.25 & autumn==1 &  temp_meteo_quarter_dev!=.
replace hotautumn_225C = 0 if  temp_meteo_quarter_dev<-2.25 & autumn==1 &  temp_meteo_quarter_dev!=.

gen  hotwinter_225C =. 
replace hotwinter_225C = 1 if  temp_meteo_quarter_dev >2.25 & winter==1 &  temp_meteo_quarter_dev!=.
replace hotwinter_225C = 1 if temp_meteo_quarter_dev == 2.25 & winter==1 & temp_meteo_quarter_dev!=.
replace hotwinter_225C = 0 if  temp_meteo_quarter_dev<2.25 & winter==1 &  temp_meteo_quarter_dev!=.
replace hotwinter_225C = 0 if  temp_meteo_quarter_dev<-2.25 & winter==1 &  temp_meteo_quarter_dev!=.


gen  hotspring_225C =. 
replace hotspring_225C = 1 if  temp_meteo_quarter_dev >2.25 & spring==1 &  temp_meteo_quarter_dev!=.
replace hotspring_225C = 1 if temp_meteo_quarter_dev == 2.25 & spring==1 & temp_meteo_quarter_dev!=.
replace hotspring_225C = 0 if  temp_meteo_quarter_dev<2.25 & spring==1 &  temp_meteo_quarter_dev!=.
replace hotspring_225C = 0 if  temp_meteo_quarter_dev<-2.25 & spring==1 &  temp_meteo_quarter_dev!=.




****************** Defining the floods 

******* Declare panel data
egen ID_var = group(Territory_ID)  // temporrarily for time set 
xtset ID_var t_var

******************* Estimate SPI index to predict hazard of floods 

 *** Baseline metric (flood metric using three days maximum precipitation)
drop if pr_total == 0 // data cleaning
gammafit pr_max_3day, vce(cluster Territory_ID)
gen Probability_3 = .
replace Probability_3 = gammap(e(alpha), pr_max_3day/e(beta))
gen SPI_3 = invnorm(Probability_3) // SPI Index using max three days precipitation using three days accumulation period 

gen hazard_max3days = .
replace hazard_max3days = 7 if  SPI_3 >= 2 // extreme wet
replace hazard_max3days = 6 if    inrange(SPI_3,1.5,1.999)  // very wet
replace hazard_max3days = 5 if     inrange(SPI_3,1.0,1.499)  // moderate wet
replace hazard_max3days = 4 if inrange(SPI_3,-0.999,0.999) // near normal
replace hazard_max3days = 3 if inrange(SPI_3,-1.499,-1.0) // moderate dryness 
replace hazard_max3days = 2 if inrange(SPI_3,-1.999,-1.5)  // severe dryness 
replace hazard_max3days = 1 if SPI_3 <=-2 // extreme dryness 
replace hazard_max3days = . if SPI_3 == .
g hazard_max3days_label= word("extremely_dry very_dry moderately_dry normal_precipitation moderately_wet very_wet extremely_wet", hazard_max3days)

gen flood_max3days = .
replace flood_max3days  = 1 if hazard_max3days == 7 
replace flood_max3days  = 0 if flood_max3days ==.
tsset ID_var t_var
tsspell, pcond(flood_max3days) 
egen max  = max(_seq), by(ID_var year)  
gen flood_event_max3days = .
replace flood_event_max3days = 1 if max > 0
replace flood_event_max3days = 0 if flood_event_max3days == . 

sort ${Territory_ID} t_var
drop _seq _spell _end max

*** Robustness check (flood metric using total average precipitation in a given month)

gammafit pr_total, vce(cluster Territory_ID)
gen Probability_1 = .
replace Probability_1 = gammap(e(alpha), pr_total/e(beta))
gen SPI_1 = invnorm(Probability_1) // SPI Index using average precipitaion using monthly accumulation period

******************* define SPI_1 index using monthly accumulation

gen hazard_monthlyaccum = .
replace hazard_monthlyaccum = 7 if  SPI_1 >= 2 // extreme wet 
replace hazard_monthlyaccum = 6 if    inrange(SPI_1,1.5,1.999)  // very wet
replace hazard_monthlyaccum = 5 if     inrange(SPI_1,1.0,1.499)  // moderate wet
replace hazard_monthlyaccum = 4 if inrange(SPI_1,-0.999,0.999) // near normal
replace hazard_monthlyaccum = 3 if inrange(SPI_1,-1.499,-1.0) // moderate dryness 
replace hazard_monthlyaccum = 2 if inrange(SPI_1,-1.999,-1.5)  // severe dryness 
replace hazard_monthlyaccum = 1 if SPI_1 <=-2 // extreme dryness 
replace hazard_monthlyaccum = . if SPI_1 == .
g flood_monthlyaccum_label = word("extremely_dry very_dry moderately_dry normal_precipitation moderately_wet very_wet extremely_wet", hazard_monthlyaccum)


gen flood_monthlyaccum = .
replace flood_monthlyaccum  = 1 if hazard_monthlyaccum == 7 
replace flood_monthlyaccum  = 0 if flood_monthlyaccum ==.
tsset ID_var t_var
tsspell, pcond(flood_monthlyaccum) 
egen max  = max(_seq), by(ID_var year)  
gen flood_event_monthlyaccum = .
replace flood_event_monthlyaccum = 1 if max > 0
replace flood_event_monthlyaccum = 0 if flood_event_monthlyaccum == . 

drop _seq _spell _end max


********************Define droughts

******************* Precipitation Anomalies to define droughts

*SPI Index with quarterly accumulation period 

egen pr_accumulated = mean(pr_total), by (${Territory_ID} quarter year) // quarterly average of precipitation 

* Collapse data to quarterly level and estimate the SPI index based on 3 months accumulation period

keep if month == 2 | month == 5 | month == 8 | month == 11 // dataset is now quarterly data 


gammafit pr_accumulated, vce(cluster Territory_ID)

gen Probability_drought = .
replace Probability_drought = gammap(e(alpha), pr_accumulated/e(beta))
gen SPI_drought = invnorm(Probability_drought) // SPI Index using average precipitaion using monthly accumulation period

** define hazard
******************* define SPI_drought index using accumulated precipitaion for 3 months
gen hazard_drought = .
replace hazard_drought = 7 if  SPI_drought >= 2 // extreme wet 
replace hazard_drought = 6 if    inrange(SPI_drought,1.5,1.999)  // very wet
replace hazard_drought = 5 if     inrange(SPI_drought,1.0,1.499)  // moderate wet
replace hazard_drought = 4 if inrange(SPI_drought,-0.999,0.999) // near normal
replace hazard_drought = 3 if inrange(SPI_drought,-1.499,-1.0) // moderate dryness 
replace hazard_drought = 2 if inrange(SPI_drought,-1.999,-1.5)  // severe dryness 
replace hazard_drought = 1 if SPI_drought <=-2 // extreme dryness 
replace hazard_drought = . if SPI_drought == .
g hazarddrought = word("extremely_dry very_dry moderately_dry normal_precipitation moderately_wet very_wet extremely_wet", hazard_drought)

gen indicator_rainfall = .
replace indicator_rainfall  = 1 if hazard_drought == 1 | hazard_drought == 2
replace indicator_rainfall  = 0 if hazard_drought > 2 // extreme dry conditions based on total monthly precipitation

tsset ID_var t_var
* ssc install tsspell
tsspell, pcond(indicator_rainfall)    // generate sequence, number of spells and end of the spell
egen max  = max(indicator_rainfall), by(ID_var year)    // maximum number if sequence of ones in a year 
gen drought_event = .
replace drought_event = 1 if max ==1
replace drought_event = 0 if drought_event == . & max == 0

drop max _end _spell _seq
sort ${Territory_ID} t_var

**************** convert the quarterly data in annual series 

bysort ${Territory_ID} ${year}: egen aux_hotsummer_2C = max(hotsummer_2C)
bysort ${Territory_ID} ${year}: egen aux_hotwinter_2C= max(hotwinter_2C)
bysort ${Territory_ID} ${year}: egen aux_hotautumn_2C = max(hotautumn_2C)
bysort ${Territory_ID} ${year}: egen aux_hotspring_2C= max(hotspring_2C)

bysort ${Territory_ID} ${year}: egen aux_hotsummer_175C = max(hotsummer_175C)
bysort ${Territory_ID} ${year}: egen aux_hotwinter_175C= max(hotwinter_175C)
bysort ${Territory_ID} ${year}: egen aux_hotautumn_175C = max(hotautumn_175C)
bysort ${Territory_ID} ${year}: egen aux_hotspring_175C= max(hotspring_175C)

bysort ${Territory_ID} ${year}: egen aux_hotsummer_225C = max(hotsummer_225C)
bysort ${Territory_ID} ${year}: egen aux_hotwinter_225C= max(hotwinter_225C)
bysort ${Territory_ID} ${year}: egen aux_hotautumn_225C = max(hotautumn_225C)
bysort ${Territory_ID} ${year}: egen aux_hotspring_225C= max(hotspring_225C)

drop hotsummer_2C hotautumn_2C hotwinter_2C hotspring_2C hotsummer_175C hotautumn_175C hotwinter_175C hotspring_175C hotsummer_225C hotautumn_225C hotwinter_225C hotspring_225C 

rename aux_hotsummer_2C hotsummer_2C
rename aux_hotwinter_2C hotwinter_2C
rename aux_hotautumn_2C  hotautumn_2C
rename aux_hotspring_2C hotspring_2C

rename aux_hotsummer_175C hotsummer_175C
rename aux_hotspring_175C hotspring_175C
rename aux_hotwinter_175C hotwinter_175C
rename aux_hotautumn_175C hotautumn_175C

rename aux_hotsummer_225C hotsummer_225C
rename aux_hotspring_225C  hotspring_225C 
rename aux_hotwinter_225C  hotwinter_225C 
rename aux_hotautumn_225C hotautumn_225C 

**** Define variable to capture baseline climate of regions  

*** Hot/medium/cold regions terciles (1 "Cold" 2 "Medium" 3 "Hot")
xtile Baseline_climate=meteor_avgtemp_quarter_hist if summer ==1,n(3)
by Territory_ID, sort: fillmissing Baseline_climate, with(max) 

keep if quarter == 3 // select any quarter to convert it into annual data

keep Territory_ID Country_ID name_latn year drought_event flood_event_monthlyaccum flood_event_max3days hotwinter_2C hotsummer_2C hotautumn_2C hotspring_2C hotsummer_175C hotwinter_175C hotautumn_175C hotspring_175C hotsummer_225C hotwinter_225C hotautumn_225C hotspring_225C Baseline_climate 

**** Label variables 

label variable year "Year"
label variable Baseline_climate "1Cold 2medium 3hot regions"
label variable flood_event_max3days "Flood event: Baseline"
label variable flood_event_monthlyaccum "Flood event: robust check"
label variable drought_event "Drought event: Baseline"

label variable hotsummer_2C "Heatwave summer 2Celcius: Baseline"
label variable hotsummer_175C "Heatwave summer 1.75Celcius: Robustness check"
label variable hotsummer_225C "Heatwave summer 2.25Celcius: Robustness check"

label variable hotspring_2C "Heatwave spring 2Celcius"
label variable hotspring_175C "Heatwave spring 1.75Celcius"
label variable hotspring_225C "Heatwave spring 2.25Celcius"

label variable hotwinter_2C "Heatwave winter 2Celcius"
label variable hotwinter_175C "Heatwave  winter 1.75Celcius"
label variable hotwinter_225C "Heatwave  winter 2.25Celcius"

label variable hotautumn_2C "Heatwave autumn 2Celcius"
label variable hotautumn_175C "Heatwave autumn 1.75Celcius"
label variable hotautumn_225C "Heatwave  autumn 2.25Celcius"


****** merge with macro dataset

merge 1:1 Territory_ID year using "$github_path/Macrodata_NUTS3.dta" // merge with macro data
*keep if _merge ==3
drop _merge

*** Income regions terciles (1 "Low income" 2 "Middle income" 3 "High income")
xtile Income_group=gdp_per_capita, n(3)
label variable Income_group "1low 2middle 3high income regions"

*** Select time period
keep if year > 1994
drop if year > 2022
save  "Final_data.dta", replace // Final data for analysis 
