# finds row indices where a word is located in a df
# inputs: df - raw df of data from we are interested in searching
#        wrd - word we are interested in finding
# outputs: vector of indices in quart_report which rows of df contain wrd, sorted by which index comes first
find_word_row <- function(df, wrd){
    temp = which(grepl(wrd, df, ignore.case = TRUE)) # find which columns contain the word wrd
    item_index = c() # initialize row index
    
    # for every column with the word "item", find the row index where it exists
    for(i in 1:length(temp)){
        # append the row index to our list - only keep unique indicies
        item_index = c(item_index, grep(wrd, t(df[i]), ignore.case = TRUE))%>%unique()
    }
    
    # return sorted values
    return(sort((item_index)))
}

# gets Report type, patient notes, or group from header file 
# inputs: 
# header_df - raw header file dataframe 
# x - the name of what is to be extracted (either "Report type", "Notes" or "Group") - this should appear in the first column of the header file 
# output: extracted string of interest - empty string if "x" does not appear in the header file
extract_from_header = function(header_df, x){
    string_out = ""
    
    r = find_word_row(header_df, x)
    
    if(length(r == 0)){ # check to see if "x" appears anywhere in the header file 
        
        if(x == "Notes"){ # if extracting notes, may be multiple lines
            i = r
            while(i == r || (is.na(header_df[i,1]) && !is.na(header_df[i,2]))){
                string_out = paste(string_out,header_df[i,2], " ", sep = "\n")
                i = i + 1
            }
        }
        else{
            string_out = header_df[r,2]
        }

    }
    return(string_out)
}



# gets date for the report from the name of the file 
# IMPORTANT: bases date off of file name of the report NOT the dates actually in the file
# inputs: file_name - file name of the report. should be of the form "MM-DD-YY PCHS xxxxx Rate.xlsx"
# output: date for this quarterly report
extract_date = function(file_name){
    temp = strsplit(file_name, split = " ")
    date_temp_index = which(grepl("-", temp[[1]]))
    quart_report_date = temp[[1]][date_temp_index]%>%as.Date(format = "%m-%d-%y")
    return(quart_report_date)
}

# extracts the rates and patient numbers from the quarterly report
# inputs:
#     quart_report - the dataframe with the information read directly from the quarterly report xlsx file
#     report_date - the date of the quarterly report file - extracted from file name via extract_date
# output: a clean dataframe with location, date, rate, and total number of patients for each quarter.
#         Also includes rate for all of PCHS
extract_data <- function(quart_report, report_date){
    
    clean_data = data.frame()
    item_index = find_word_row(quart_report, "Item")  # find rows with the word "Item" in raw date- to delineat data tables from the report file
    
    # for each of the 4 data tables in a report, extract the data
    for(i in 1:length(item_index)){
        
        # create the ith data table
        if(! is.na(item_index[i+1])){ # if we're not on our last table,
            # create the table to include the "Location" (just above item_index row) through to the next item_index
            table_i_data = data.frame(quart_report[(item_index[i]-1):(item_index[i+1]-2),])
        }else{
            # if we're on the last table, create from "Location" until end of quart_report dataframe
            table_i_data = data.frame(quart_report[(item_index[i]-1):dim(quart_report)[1],])
            
        }
        
        # find rows with "Location" word in current data table
        loc_row_index = find_word_row(table_i_data, "Location")
        # finds columns with "Value" word in current data table
        value_col_index = which(grepl("Value", table_i_data, ignore.case = TRUE))
        
        # finds first row with data - two options because when there is multiple locations, need to use sapply instead of apply
        #       converts all of the columns with "Value" to numerics,
        #       checks which are NAs (because they didn't have numbers and therefore aren't data),
        #       then the first row that does not have an NA is the first row with data
        if(length(value_col_index)>1){
            first_data_row_index = apply(table_i_data[,value_col_index], MARGIN = 2, FUN = as.numeric)%>% is.na()%>%ifelse(FALSE, TRUE)%>%which()%>%min()
        }else{
            first_data_row_index = sapply(table_i_data[,value_col_index], FUN = as.numeric)%>% is.na()%>%ifelse(FALSE, TRUE)%>%which()%>%min()
        }
        
        
        # extracts data - two options - data table with multiple locations or All data
        # creates vectors with total number of patients, patients that meet the criteria (criteria_patients), and rate
        if(length(loc_row_index > 0 )){   # if we are on a data table with multiple locations
            
            # find columns with location names (anywhere that isn't blank in the "location" row except the first column with the word "Location")
            loc_col_index = which(!is.na(table_i_data[loc_row_index,]))[-1]
            
            location = table_i_data[loc_row_index, loc_col_index]%>%t()     # make a df with the location names
            colnames(location) = 'location'                                 # rename df column so it is called "location"
            
            # create vector with total number of patients for each location
            all_patients = table_i_data[first_data_row_index, loc_col_index] %>% t() %>% as.numeric()
            
            # create vector with number of patients that meet criteria at each location
            criteria_patients = table_i_data[first_data_row_index+1, loc_col_index ] %>% t() %>% as.numeric()
            
            # create vector with rate at which patients meet criteria at each location
            rate = criteria_patients / all_patients * 100
            
        } else{ # for data tables summarizing  rate for all locations together
            location = "All"
            
            # create vector with total number of patients for "All"
            # (only select first value column in case multiple quarters of data are given)
            all_patients = table_i_data[first_data_row_index, value_col_index[1]] %>% t() %>% as.numeric()
            
            # create vector with number of criteria_patients at "All" 
            # (only select first value column in case multiple quarters of data are given)
            criteria_patients = table_i_data[first_data_row_index+1, value_col_index[1] ] %>% t() %>% as.numeric()
            
            # create vector with  rate at each location
            rate = criteria_patients / all_patients * 100
        }
        
        # put all  vectors into df for this data table
        clean_data_i = data.frame(location = location, rate = rate, all_patients= all_patients, criteria_patients = criteria_patients)
        label.months = 3
        # add date to df
        if(i <= 2 && length(item_index) > 2){ # first two data tables are for the data from the previous quarter, when 2 quarters are given
            clean_data_i = clean_data_i%>%mutate(date = report_date - months(label.months))
        }else{ # dfs 3-4 are for tables  in this quarterly report
            clean_data_i = clean_data_i%>%mutate(date = report_date)
        }
        
        # append df for table_i to full clean_data
        clean_data = rbind(clean_data, clean_data_i)
        
    }
    
    # do not include "CHC" patients (not connected to a location) and get rid of redundant data
    clean_data = clean_data%>%filter(location != "CHC")%>%unique()
    
    return(clean_data)
}


