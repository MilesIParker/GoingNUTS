# GoingNUTS

This repository includes replication codes for the project "Going NUTS: the regional impact of extreme climate events over the medium term"

This replication package provides:

Input files (Data preperation): 

             Raw weather data for each country and territorial regions saved as "Data_*.csv" (Folder raw_files)
             
             "Macrodata_NUTS3"  for macro data
             
             "GVA_NACE_sector"  Gross value added for sectoral analysis


Output files (for data analysis)

              "Final_data.dta" 



Step 1 (Optional): 
              Run the do-file "prepare_data.do".  This file runs the ado-file "combine.do" and combines the weather data from all countries and creates an output file "raw_data.dta".
              The program then cleans the raw data and creates the event variables and subgroups for further analysis.

Step 2 (Analysis): run "main_analysis.do" runs the main analysis (Figures 1, Figure 2, Figure 3, Figure 4, Figure 5, Figure 6)
       and also conducts sectoral analysis for all three types of events. 

# Set the path

global github_path "https://raw.githubusercontent.com/MilesIParker/GoingNUTS/main"

# Required STATA packages 

ssc install tsspell 

ssc install ftools 

ssc install reghdfe

ssc install gammafit
