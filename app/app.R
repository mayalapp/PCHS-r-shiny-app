#https://shiny.rstudio.com/tutorial/


#setwd("~/Documents/dad_data_project/")
library(shiny)
library(tidyverse)
library(readxl)
library(gridExtra)
library(patchwork)
library(shinyFiles)
library(directlabels)
library(colorspace)
#library(lubridate)

# things that could go wrong:
# don't include "Item" anywhere in the patient description

# -------------------------------------------------------------------------------------------#

# creates r shiny user interface
ui = fluidPage(
  # *Input() functions - e.g. sliderInput(), numericInput(), etc.
  # example: always have inputId and label parameters

  verticalLayout(
  wellPanel(
    # dropdown with different screening options. starts on blank. affects notes on which patients are used, report title, and graph titles
      selectInput(inputId = "screening.type", label = "Choose screening type",
                        choices = c("","Colorectal Cancer Screening", "Mammogram Screening", "Cervical Cancer Screening")),

      # select files. may select all files
      fileInput(inputId = "files",
                label = "Choose quarterly report xlsx files. File names should begin with the date of the quarterly report: \"MM-DD-YY xxxxxxxx.xlsx\". ",
                multiple = TRUE, accept = c(".csv", ".xlsx")),

      # button to generate plots
      actionButton(inputId = "run", "Create plots")
            ),


  # *Output() funtions  - e.g. dataTableOutput(), plotOutput, etc.
  # example: always have outputId


  titlePanel(textOutput(outputId = "report.title")),
  verbatimTextOutput(outputId = "notes"),  # notes on which patients are included in screening rates
  h3(textOutput(outputId = "text.PCHS")),
  textOutput(outputId = "warn.no.files"), # currently not used?
  plotOutput(outputId = "plot.allLocationsSummary", height = 500), # outputs one plot with a line for each location

  #wellPanel(
    headerPanel(""),  # add space
    h3(textOutput(outputId = "text.locations")),  # separate plots of each individual location
  #),

  plotOutput(outputId = "plot.individualLocations") # outputs two plots for each location - total patients and screening rate
  )
)


# -------------------------------------------------------------------------------------------#


server = function(input, output){


# gets date for the report
# IMPORTANT: bases date off of file name of the report NOT the dates actually in the file
# inputs: file_name - file name of the report. should be of the form "MM-DD-YY PCHS xxxxx Screening Rate.xlsx"
# output: date for this quarterly report
extract_date = function(file_name){
    temp = strsplit(file_name, split = " ")
    date_temp_index = which(grepl("-", temp[[1]]))
    quart_report_date = temp[[1]][date_temp_index]%>%as.Date(format = "%m-%d-%y")
    return(quart_report_date)
}


# finds row indices where a word is located in a df
# IMPORTANT: don't include the word "item" in the patient notes - could mess this function up
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


# extracts the screening rates and patient numbers from the quarterly report
# inputs:
#     quart_report - the dataframe with the information read directly from the quarterly report xlsx file
#     report_date - the date of the quarterly report file - extracted from file name via extract_date
# output: a clean dataframe with location, date, screening rate, and total number of patients for each quarter.
#         Also includes screening rate for all of PCHS
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
    # creates vectors with total number of patients, screened patients, and screening rate
    if(length(loc_row_index > 0 )){   # if we are on a data table with multiple locations

      # find columns with location names (anywhere that isn't blank in the "location" row except the first column with the word "Location")
      loc_col_index = which(!is.na(table_i_data[loc_row_index,]))[-1]

      location = table_i_data[loc_row_index, loc_col_index]%>%t()     # make a df with the location names
      colnames(location) = 'location'                                 # rename df column so it is called "location"

      # create vector with total number of patients for each location
      all_patients = table_i_data[first_data_row_index, loc_col_index] %>% t() %>% as.numeric()

      # create vector with number of screen patients at each location
      screened_patients = table_i_data[first_data_row_index+1, loc_col_index ] %>% t() %>% as.numeric()

      # create vector with screening rate at each location
      screening_rate = screened_patients / all_patients * 100

    } else{ # for data tables summarizing screening rate for all locations together
      location = "All"

      # create vector with total number of patients for each location
      all_patients = table_i_data[first_data_row_index, value_col_index] %>% t() %>% as.numeric()

      # create vector with number of screen patients at each location
      screened_patients = table_i_data[first_data_row_index+1, value_col_index ] %>% t() %>% as.numeric()

      # create vector with screening rate at each location
      screening_rate = screened_patients / all_patients * 100
    }

    # put all  vectors into df for this data table
    clean_data_i = data.frame(location = location, screening_rate = screening_rate, all_patients= all_patients, screened_patients = screened_patients)

    # add date to df
    if(i <= 2 && length(item_index) > 2){ # first two data tables are for the data from the previous quarter, when 2 quarters are given
      clean_data_i = clean_data_i%>%mutate(date = report_date - months(3))
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

# creates screening line plot for a specific location
# inputs:
#      df - dataframe with full cleaned data (including all locations)
#      loc - string of location name
#      ymin - y axis minimum value
#      ymax - y axis max value
create_screening_plot = function(df, loc, ymin, ymax){
  df%>%filter(location == loc)%>%
    ggplot(aes(x = date, y = screening_rate))+
    geom_line(size = 1.5)+
    theme_bw()+
    guides(size = FALSE)+
    labs(x = "Date", y = "Number of Patients")+
    ylim(ymin, ymax)+
    labs(x = ax.date, y = ax.screening)+
    ggtitle(paste(loc, input$screening.type, "Rate"))+
    plot_options+
    scale_x_date(date_labels = "%b %Y")
}

# creates screening line plot for a specific location
# inputs:
#      df - dataframe with full cleaned data (including all locations)
#      loc - string of location name
#      ymax - y axis max value
create_patient_barplot = function(df, loc, ymax){
  df%>%filter(location == loc)%>%
    ggplot(aes(x = date, y = all_patients))+
    geom_bar(stat = "identity")+
    theme_bw()+
    labs(x = ax.date, y = ax.patients)+
    ggtitle(paste(loc, title.patients()))+
    plot_options+
    ylim(0, ymax)+
    scale_x_date(date_labels = "%b %Y")
}


###########

# creates df of data from all selected quarterly report files
 data = reactive({
   clean_data = data.frame()

   # get data from each file
   for(i in 1:length(input$files$name)){
     quart_report = read_xlsx(input$files$datapath[[i]], col_names = FALSE)    # read in data of file_i
     date = extract_date(input$files$name[[i]])                                # extract date from file_i
     clean_data = rbind(clean_data, extract_data(quart_report, date))          # append data from this report
   }

   # data should only contain unique values (get rid of duplicates from files containing info from previous quarter)
   clean_data = unique(clean_data)

   # relevel so "All" is first level
   clean_data$location <- relevel(clean_data$location, "All")

   clean_data
  }
  )

 # setting up axes for plots - note: titles of plots based on input$screening.type
 ax.date = "Date"
 ax.screening = "Screening Rate (%)"
 ax.patients = "Number of Patients"
 ax.location = "Site"

# output report title  - based on input$screening.type
 output$report.title = renderText({ paste(input$screening.type, "Report")})


 # output notes on patients included in "All patients" and screening rates
 output$notes = renderText({

   if(input$screening.type == "Colorectal Cancer Screening"){
     paste("Notes on patient data: ","","All patients: ","Active patients between 50 and 75 years of age that DO NOT have colorectal cancer that had a
medical visit during the 3 years prior to the end of the reporting period. ", "",
                 "Screened patients: ", "Patients that received Colonscopy in the last 10 years; Fecal Immunochemical Test (FIT)
in the last 1 year; Fecal Occult Testing (FOBT) in the 1  year; or FIT DNA in the last 3 years.", sep = "\n")

   }else if(input$screening.type == "Cervical Cancer Screening"){
     paste("Notes on patient data: ","","All patients: ", "Active Female patients between 21 and 64 years that had a visit during the 3 years prior
to the end of the reporting period and that DID  NOT received a hysterectomy. ", "",
                 "Screened patients:", "Active Female patients between 21 and 29 years that had a cevical cancer screening within
the last 3 years or active female patients between 30 and 64 years old that had a cervical cancer
screening within the last 5 years.", sep = "\n")
   }else if(input$screening.type == "Mammogram Screening"){
     paste("Notes on patient data: ","","All patients: ", "Active Female patients between the 50 and 74 years of age that DID NOT have a mastectomy and
that  had a medical visit during the 3 years prior to the end of the reporting period. ","",
                 "Screened patients: ", "Patients that received a mammogram during the 2 years prior to the end of the reporting period.", sep = "\n")

   }
 })


# create variable for title of All patients plots
 title.patients = reactive({
   if(input$screening.type == "Colorectal Cancer Screening"){
     ("Patients 50-75 Years Old")
   }else if(input$screening.type == "Cervical Cancer Screening"){
     ("Female Patients 21-64 Years Old")
   }else if(input$screening.type == "Mammogram Screening"){
     ("Female Patients 50-74 Years Old")
   }
 })


# set plot options for all plots to abide by
 plot_options = theme(axis.text=element_text(size=14),
                      axis.title=element_text(size=16,face="bold"),
                      plot.title = element_text(size = 20, face = "bold"),
                      legend.text = element_text(size=14),
                      legend.title = element_text(size=16, face = "bold"))

combined_plot_width = 1250
plot_colors = darken(c("#000000", "#80CDC1", "#B8E186", "#9fb88c", "#92C5DE", "#DFC27D", "#FDB863",  "#EA9999", "#7686c4", "#D5A6BD", "#A2C4C9", "#D5A6BD", "#F4A582"))

observeEvent(input$run, {   # create run button to plot graphs


   # output single line plot for screening rates of all locations together
   output$plot.allLocationsSummary = renderPlot({

    date_summary = data()%>%summarize(min_date = min(date), max_date = max(date))   # determine first and last dates plotted

    p1 = list()
    p1[[1]] = data()%>%filter(location == "All")%>%
      ggplot(aes(x = date, y = all_patients))+
      geom_bar(stat = "identity")+
      theme_bw()+
      labs(x = ax.date, y = ax.patients)+
      ggtitle(paste("PCHS", title.patients()))+
      plot_options+
      scale_x_date(date_labels = "%b %Y")

    # create plot
    p1[[2]] = data()%>%
      ggplot(aes(x = date, y = screening_rate, color = location))+
      geom_line(data = data()%>%filter(location == "All"), color = "grey", size = 10, alpha = 0.5)+   # plot shadow around "All" (behind other lines)
      geom_point(aes())+ # plot points for all sites
      geom_line(aes(), size = 1.5)+ # plot lines for all sites
      geom_line(data = data()%>%filter(location == "All"), color = "black", size = 1.5)+   # plot "All" on top of other lines
      #xlim(date_summary$min_date, date_summary$max_date + months(8))+   # change x axis lims?
      #annotate("text", x = annotation$date + months(1), y = annotation$screening_rate, label = "  ", size = 10)+   # annotation for avg. rate
      theme_bw()+
      guides(size = FALSE, color = FALSE)+   # don't include legend for size of dots
      labs(x = ax.date, y = ax.screening, color = ax.location)+
      ggtitle(paste("PCHS",input$screening.type, "Rates"))+
      plot_options+
      scale_x_date(#date_breaks = "3 months",
                   date_labels = "%b %Y",
                   #labels=date_format("%b-%Y"),
                   #limits = c(date_summary$min_date,date_summary$max_date + weeks(6)))+ #extend xlim so labels aren't cut off
                  limits = c(date_summary$min_date,date_summary$max_date + months(2)))+ #extend xlim so labels aren't cut off
     geom_dl(aes(label = location), method = list(dl.trans(x = x + .3), "last.qp", cex = 1.2, fontface = "bold")) +
      #scale_color_brewer(palette = "Set3")
      scale_color_manual(values = plot_colors)
      #geom_dl(aes(label = location), method = list(dl.combine("last.points")), cex = 0.8)

    #%>%
    #  direct.label("last.qp")

    #Levels: All Alma Illery Braddock CHC East End Hazelwood Hill House McKeesport Steel Valley West End Wilkinsburg

    # # margin of white space between plots
    # margin = theme(plot.margin = unit(rep(1, times = 2), "cm"))
    # grid.arrange(grobs = lapply(p1, "+", margin), nrow = 1, widths = c(1.5,2))  # output plot
    grid.arrange(grobs = p1, nrow = 1, widths = c(1.5,2))  # output plot

    }, height = 500, width = combined_plot_width)


   # output two plots for each location - one showing total number of patients, other with screening rate
  output$plot.individualLocations = renderPlot({
    # find max number of patients
    max_patients = data()%>%filter(location != "All")%>%group_by(location)%>%summarize(max_patients_loc = max(all_patients))%>%filter(max_patients_loc == max(max_patients_loc))
    max_patients = max_patients$max_patients_loc

    # find max range of screening rates for single location
    temp_data = data()%>%filter(location!="All")%>%group_by(location)%>%
      summarize(rate_range = max(screening_rate)- min(screening_rate),   # find ranges of screening rates for each location
                middle_rate = min(screening_rate) + 0.5 * rate_range)    # find middle between max and min screening rate for each locaiton
    max_range = temp_data%>%filter(rate_range == max(rate_range)) # calculate max range
    max_range = max_range$rate_range                              # isolate max range as a number
    y_ranges = temp_data%>%mutate(ymin = middle_rate - 0.5 * max_range, ymax = middle_rate + 0.5 * max_range)  # create new ranges for y axes


    nLocations = length(unique(data()$location))    # get number of locations in dataset
    p3 = list()   # initialize list of all patients bar graph
    p4 = list()# initialize list of screening line plot

    # for each location, create both plots
    for( i in 1:(nLocations-1)){

      location_i = data()$location[i]                   # get name of ith location

      # create all patient bar graph
      p1 = create_patient_barplot(data(), location_i, max_patients)#+scale_fill_manual(values = c(plot_colors[i+1], "black"))
      #create screening rate line plots
      p2 = create_screening_plot(data(), location_i, y_ranges$ymin[i], y_ranges$ymax[i])#+scale_color_manual(values = c(plot_colors[i+1], "black"))

      p3[[i]] = p1
      p4[[i]] = p2
    }

    # arrange plots into one output

    plts = rbind(p3, p4)

    # margin of white space between plots
    margin = theme(plot.margin = unit(rep(1, times = nLocations), "cm"))

    grid.arrange(grobs = lapply(plts, "+", margin), widths = c(1.5, 2), heights = 4*rep(1, times = nLocations))


  }, height = 4100, width = combined_plot_width)



 }) # isolate end

 # output title for individual locations
  output$text.locations = renderText("Graphs for individual sites")
  output$text.PCHS = renderText("Graphs for all PCHS sites")
}


# run app
shinyApp(ui = ui, server = server)

#sharing apps shinyapps.io

