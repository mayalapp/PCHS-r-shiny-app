# Time-series visualization app

This repo containts two R Shiny Apps. 

1. The Primary Care Health Services (PCHS) app creates time-series visuals of Clinical Quality Measures (CQMs) using quarterly reports outputted from I-to-I. 
1. The "generalized" app has been adapted for a more general audience, and also uses time-series data to produce visuals. 


# Purpose

To easily convert time-series report data into time-series visuals. This will allow PCHS (and other organizations) to observe changes in patient care over time, identify sites that should be targeted for policies to improve care, and evaluate whether such policies are successful and should be continued or propogated to new sites. 

# Usage

Users can go to the [PCHS web app](http://mayalapp.shinyapps.io/PCHS-cancer-screening-report-generator) or the [time-series visualizer web app](https://mayalapp.shinyapps.io/pchs_cqm_report_app). 

The code for the R Shiny apps are located in the `PCHS-app` directory and the `generalized-app` directory. 

## Notes about time-series report files

To use the app, upload time-series report files using the template given in `quarterlyReports/MM-DD-YY report_template.xlsx`. Example quarterly report files are provided in the `quarterlyReports` directory. 

Notes on extending the app to other usages: 
- Reports following the template given in `MM-DD-YY report_template.xlsx` do not need to be **quarterly** reports - they can use any timeframe (e.g. monthly reports, annual reports, etc.)
- Column names do not need to be locations/sites - they can be any category for the data (e.g. providers, patient demographics, etc.)
     - **Note:** even if the data contains other category names, make sure the reports label them as "Locations" (as in the template file) since this word is used to help the app find the categories

Important formatting notes: 
- Files should be xlsx files named using the format `"MM-DD-YY xxxxxxxx.xlsx"` where `MM-DD-YY` is the date of the current report. 
- Follow the format of the `MM-DD-YY report_template.xlsx` file as closely as possible to ensure the app does not fail. 
- Make sure the data is in the **first sheet** in the excel file. 
- The words "Item" and "Location" should not be used anywhere except to note the rows where the type of data and the site names are indicated, respectively
- Multiple quarters in a single file
    - Some example files have data from multiple quarters in a single file. We recommend avoiding this when possible. 
    - If data from multiple quarters is included, the data from the previous quarter should be listed first, followed by data from the current quarter (note: file names were used for dating the quarterly data to account for typos in the dates of historical quarterly reports) 
    - Data must be from **quarterly** reports when using this format (other timeframes will not work properly)


If a datafile gets currupted (will not load on the app), try copying the information to a new excel file and saving it as a new file. 


## Notes about header files 

Header files are used to create report/plot titles, include notes about patients/CQMs/etc, and change the group name for anonymized plots (e.g. "Site 1", "Site 2", etc.). A template for the header file is given in  `quarterlyReports/header_file_template.xlsx`. The header file name must contain the word "header" in it. 

Make sure the header information is in the **first sheet** in the excel file. 


