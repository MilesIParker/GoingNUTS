# GoingNUTS

This repository includes replication codes for the article:

Sehrish Usman, Guzmán González-Torres Fernández, Miles Parker, "Going NUTS: The regional impact of extreme climate events over the medium term",
European Economic Review, 2025, 105081, ISSN 0014-2921, https://doi.org/10.1016/j.euroecorev.2025.105081, https://www.sciencedirect.com/science/article/pii/S001429212500131X


Input files (Data preparation): 

             Raw weather data for each country and territorial region saved as "Data_*.csv" (Folder raw_files)
             
             "Macrodata_NUTS3"  for macro data
             
             "GVA_NACE_sector"  Gross value added for sectoral analysis


Output files (for data analysis)

              "Final_data.dta" 



Step 1 (Optional): 
              Run the do-file "prepare_data.do".  This file runs the ado-file "combine.do" and combines the weather data from all countries and creates an output file "raw_data.dta".
              The program then cleans the raw data and creates the event variables and subgroups for further analysis.

Step 2 (Analysis): run "main_analysis.do" for the main analysis (Figures 1, Figure 2, Figure 3, Figure 4, Figure 5, Figure 6)
       and also sectoral analysis for all three types of events. Results will be saved in your local drive. 

# Set the path

global github_path "https://raw.githubusercontent.com/MilesIParker/GoingNUTS/main"

# Required STATA packages 

ssc install tsspell 

ssc install ftools 

ssc install reghdfe

ssc install gammafit
