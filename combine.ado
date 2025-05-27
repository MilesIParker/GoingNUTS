program define combine
    version 15.1
    syntax [, Saveas(string)]

    * List of all remote CSV files 
    local files Data_AL.csv Data_AT.csv Data_BE.csv Data_BG.csv Data_CH.csv Data_CY.csv Data_CZ.csv Data_DE_part1.csv Data_DE_part2.csv Data_DE_part3.csv Data_DK.csv ///
                Data_EE.csv Data_EL.csv Data_ES.csv Data_FI.csv Data_FR.csv Data_HR.csv Data_HU.csv Data_IE.csv Data_IS.csv Data_IT.csv ///
                Data_LI.csv Data_LT.csv Data_LU.csv Data_LV.csv Data_ME.csv Data_MK.csv Data_MT.csv Data_NL.csv Data_NO.csv Data_PL.csv ///
                Data_PT.csv Data_RO.csv Data_RS.csv Data_SE.csv Data_SI.csv Data_SK.csv Data_TR.csv Data_UK_part1.csv Data_UK_part2.csv

    tempfile combined
    local first_done = 0

    foreach file of local files {
        local url = "https://raw.githubusercontent.com/MilesIParker/GoingNUTS/main/raw_files/`file'"

        display "🔄 Importing `file' from `url' ..."

        quietly import delimited "`url'", clear

        if `first_done' == 0 {
            save `combined', replace
            local first_done = 1
        }
        else {
            append using `combined'
            save `combined', replace
        }
    }

    use `combined', clear

    if ("`saveas'" != "") {
        save "`saveas'", replace
        display "✅ Combined data saved as `saveas'"
    }
    else {
        display "✅ Combined data loaded in memory. Use 'save' to write to disk."
    }
end
