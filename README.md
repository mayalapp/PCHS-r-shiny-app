# Time-series visualization app

This repo containts two R Shiny Apps. 

1. The Primary Care Health Services (PCHS) app creates time-series visuals of patient data (e.g. cancer screening rates) using quarterly reports outputted from I-to-I. 
1. The "generalized" app has been adapted for a more general audience, and also uses time-series data to produce visuals. 


# Purpose

To easily convert quarterly report data into time-series visuals. This will allow PCHS (and other organizations) to observe changes in patient care over time, identify sites that should be targeted for policies to improve care, and evaluate whether such policies are successful and should be continued or propogated to new sites. 

# Usage

Users can go to the [PCHS web app](http://mayalapp.shinyapps.io/PCHS-cancer-screening-report-generator) or the [time-series visualizer web app](https://mayalapp.shinyapps.io/time-series-visualizer/). 

The code for the R Shiny apps are located in the `PCHS-app` directory and the `generalized-app` directory. 

## Notes about quarterly report files

Example files are provided in the `quarterlyReports` directory. Important formatting notes: 
- Files should be xlsx files named using the format `"MM-DD-YY xxxxxxxx.xlsx"` where `MM-DD-YY` is the date of the current quarter. 
- If data from multiple quarters is included, the data from the previous quarter should be listed first, followed by data from the current quarter (note: file names were used for dating the quarterly data to account for typos in the dates of historical quarterly reports) 
- The words "Item" and "Location" should not be used anywhere except to note the rows where the type of data and the site names are indicated, respectively
- Follow the format of the `report_format.xlsx` file as closely as possible to ensure the app does not fail. 

If a datafile gets currupted (will not load on the app), try copying the information to a new excel file and saving it as a new file. 
