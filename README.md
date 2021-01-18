# PCHS R Shiny App

This is an R Shiny App used by Primary Care Health Services to create reports on cancer screening rates over time. 
It uses quarterly reports outputted from Eye-to-Eye, and may be generalized to be usable for other health care facilities. 


# Purpose

To easily convert quarterly reports into time-series visuals. This will allow PCHS to observe changes in cancer screening rates over time, identify sites that should be targeted for policies to improve cancer screening, and evaluate whether such policies are successful and should be continued or propogated to new sites. 

# Usage

Users can go to the [online server](http://mayalapp.shinyapps.io/PCHS-cancer-screening-report-generator) to use the app. Use the dropdown menu to choose a cancer screening type, upload quarterly report files, and click "Create plots." 

The code for the R Shiny app is located in the `app` directory. 

## Notes about quarterly report files

Example files are provided in the `quarterlyReports` directory. Important formatting notes: 
- Files should be xlsx files named using the format `"MM-DD-YY xxxxxxxx.xlsx"` where `MM-DD-YY` is the date of the current quarter. 
- If data from multiple quarters is included, the data from the previous quarter should be listed first, followed by data from the current quarter (note: file names were used for dating the quarterly data to account for typos in the dates of historical quarterly reports) 
- The words "Item" and "Location" should not be used anywhere except to note the rows where the type of data and the site names are indicated, respectively
- Follow the format of the example files as closely as possible to ensure the app does not fail. 


# Future work

1. Check if "3 years" should be changed to "1 year" 
1. Ensure that labels on the pdf plots output do not fail when adding more than 4 quarters of data
1. Add instructions so users with little computer experience can easily use the R Shiny app
1. Test using facet-wrap and facet-grid for plots 
1. Make more generalizable for other organizations 
   1. Make file of data "essentials" 
   1. Make screening report types more generalizable (names, descriptions, etc.) 
1. If file won't load - copy and paste into new excel file and resave 
1. Update readme
