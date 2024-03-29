#https://shiny.rstudio.com/tutorial/
# health reports app

# load required libraries
library(shiny)
library(tidyverse)
library(readxl)
library(gridExtra)
library(patchwork)
library(shinyFiles)
library(directlabels)
library(colorspace)
library(lubridate)


# functions to extract data from xlsx files - do not require user inputted values
source("functions.R")


# -------------------------------------------------------------------------------------------#

#--------------------
# creates r shiny user interface
#--------------------

ui = fluidPage(

  verticalLayout(
    titlePanel(textOutput(outputId = "app.title")),
    
    # get user inputs
  wellPanel(
    
   
      # select files needed for report
      fileInput(inputId = "files",
                label = "Choose quarterly report xlsx files. File names should begin with the date of the quarterly report: \"MM-DD-YY xxxxxxxx.xlsx\". 
                Also include a header file, if desired. Header file should be named \"header xxxxxxx.xlsx\". ",
                multiple = TRUE, accept = c(".xlsx")),

      # option to anonymize locations/providers/etc 
      checkboxInput(inputId = "anonymize", label = "Anonymize plot outputs", value = FALSE), 
      
      # option to print each site/provider/etc on different page for PDFs
      checkboxInput(inputId = "page.per.site", label = "PDF report: one group of plots per page", value = FALSE), 
      
      # button to generate plots
      actionButton(inputId = "run", "Create plots"),
      # button to download pdf report
      downloadButton("download.report", "Download Report PDF")
      

            ),


  # output report
  titlePanel(textOutput(outputId = "report.title")),
  verbatimTextOutput(outputId = "notes"),  # notes on which patients meet criteria to be included in rate
  h3(textOutput(outputId = "text.allSites")),
  # textOutput(outputId = "warn.no.files"), # currently not used
  
  # for debugging
  dataTableOutput(outputId = "debug"), 
  
  # outputs one plot with a line for each location
  plotOutput(outputId = "plot.allLocationsSummary", height = 500), 

  headerPanel(""),  # add space
  h3(textOutput(outputId = "text.locations")),  # header for plots of each individual site

  # outputs two plots for each location - total patients and rate
  plotOutput(outputId = "plot.individualLocations") 
  )
)


# -------------------------------------------------------------------------------------------#

#--------------------
# CREATES THE OBJECTS THAT ARE OUTPUTTED/INPUTTED TO THE APP
#--------------------

server = function(input, output){
  
  output$app.title = renderText({ 
    paste("Time-series Visualizer App")
  })
  
  
  #--------------------
  # SETTING UP PLOTS
  #--------------------

    # setting up axes for plots 
    ax.date = "Date"
    # ax.rate = "Rate (%)" set this after getting report type
    ax.patients = "Number of Patients"
    ax.location = "Site"

    # create variable for title of All patients plots
    title.patients = reactive({
      "Number of Patients for Each Quarter"
    })


    # set plot options for all plots to abide by
    plot_options = theme(axis.text=element_text(size=14),
                         axis.title=element_text(size=16,face="bold"),
                         plot.title = element_text(size = 20, face = "bold"),
                         legend.text = element_text(size=14),
                         legend.title = element_text(size=16, face = "bold"))

    combined_plot_width = 1250
    #plot_colors = darken(c("#000000", "#80CDC1", "#B8E186", "#9fb88c", "#92C5DE", "#DFC27D", "#FDB863",  "#EA9999", "#7686c4", "#D5A6BD", "#A2C4C9", "#D5A6BD", "#F4A582"))

  # creates rate line plot for a specific location
  # inputs:
  #      df - dataframe with full cleaned data (including all locations)
  #      loc - string of location name
  #      ymin - y axis minimum value
  #      ymax - y axis max value
  create_rate_plot = function(df, loc, ymin, ymax,  mycolor = "grey"){
    loc_data = df %>%filter(location == loc)
    
    loc_data %>% 
      ggplot(aes(x = date, y = rate))+
      geom_line(size = 1.5, color = mycolor)+
      theme_bw()+
      guides(size = FALSE)+
      labs(x = "Date", y = "Number of Patients")+
      ylim(ymin, ymax)+
      labs(x = ax.date, y = ax.rate())+
      ggtitle(paste(loc, report_type(), "Rate"))+
      plot_options+
      scale_x_date(date_labels = "%b %Y", breaks = loc_data$date)+
      theme(axis.text.x = element_text(angle = 90))
  }

  # creates bar plot for a specific location
  # inputs:
  #      df - dataframe with full cleaned data (including all locations)
  #      loc - string of location name
  #      ymax - y axis max value
  create_patient_barplot = function(df, loc, ymax, mycolor = "grey"){
    loc_data = df%>%filter(location == loc)
    
    loc_data%>%
      ggplot(aes(x = date, y = all_patients))+
      geom_bar(stat = "identity", fill = mycolor)+
      theme_bw()+
      labs(x = ax.date, y = ax.patients)+
      ggtitle(paste(loc, title.patients()))+
      plot_options+
      ylim(0, ymax)+
      scale_x_date(date_labels = "%b %Y", breaks = loc_data$date)+
      theme(axis.text.x = element_text(angle = 90))
  }


  #######################################

  # EXTRACTING AND CLEANING DATA
  # ------------------------------
# creates df of data from all selected quarterly report files
 data = reactive({
   clean_data = data.frame()

   # get data from each file
   for(i in 1:length(input$files$name)){
     if(!grepl("header", input$files$name[[i]], ignore.case = TRUE)) {
       quart_report = read_xlsx(input$files$datapath[[i]], col_names = FALSE)    # read in data of file_i
       date = extract_date(input$files$name[[i]])                                # extract date from file_i
       clean_data = rbind(clean_data, extract_data(quart_report, date))          # append data from this report
     }
     
   }

   # data should only contain unique values (get rid of duplicates from files containing info from previous quarter)
   clean_data = unique(clean_data)
   clean_data = clean_data%>%mutate(location = as.factor(location))
print(clean_data)
   # relevel so "All" is first level
   clean_data$location <- relevel(clean_data$location, "All")

   
   # to anonymize the data 
   if(input$anonymize){
     clean_data = clean_data%>%mutate(location = ifelse(location == "All", "All", paste(grp(), as.numeric(location) - 1, sep = " ")))
   }
   
   
   clean_data
  }
  )
  
  nLocations = reactive(length(unique(data()$location))) 
  
  
  # set plot colors 
  plot_colors = reactive({
    # Colors to use if there are <= 33 locations (not including All)
    plot_colors = darken(c("#000000", "#80CDC1", "#B8E186", "#9fb88c", "#92C5DE", "#DFC27D", 
                           "#FDB863",  "#EA9999", "#7686c4", "#D5A6BD", "#F4A582" , '#e6194B', 
                           '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#42d4f4', 
                           '#f032e6', '#bfef45', '#fabed4', '#469990', '#dcbeff', '#9A6324', 
                           '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', 
                           '#a9a9a9', 'darkgreen', 'darkmagenta'))
    if(nLocations() > length(plot_colors)){ # if there are more locations than colors 
      plot_colors = c("#000000", rep("grey", times = nLocations())) # make all locations grey (except "All", which is still black) 
    }
    
    paste(plot_colors)
  })
  
  # create dataframe for header file 
  header_df = reactive({
    header_df = data.frame() # initialize df 
    
      for(i in 1:length(input$files$name)){
        if(grepl("header", input$files$name[[i]], ignore.case = TRUE)){ #if this is the header file 
          header_df = read_xlsx(input$files$datapath[[i]], col_names = FALSE)    # read in data of file_i
          break 
        }
      }
    
    header_df
  })
  
  # get report type - either from input or header file 
  report_type = reactive({
      report_type = extract_from_header(header_df(), "Report type")
  })
  
  # get notes - either from input or header file 
  patient_notes = reactive({
      report_type = extract_from_header(header_df(), "Notes")
  })
  
  
  # get group (site/provider/etc) - from header file 
  grp = reactive({
      grp = extract_from_header(header_df(), "Group")
  })
  
  
  ax.rate = reactive(paste(report_type(), "Rate (%)"))
  
  

  #######################################

  # OUTPUTS
  #------------------------------

 
observeEvent(input$run, {   # create run button to plot graphs

  
  # output report title 
  output$report.title = renderText({ 
    paste(report_type(), "Report")
  })
  
  
  # output notes on patients included in "All patients" and patient criteria to be included in rates 
  output$notes = renderText({
    paste(patient_notes())
  })
  
  #######################################

  # ALL LOCATIONS PLOTS
  #------------------------------
  output$text.allSites = renderText("Graphs for all sites")

   # output single line plot for rates of all locations together
   output$plot.allLocationsSummary = renderPlot({
     

    date_summary = data()%>%summarize(min_date = min(date), max_date = max(date))   # determine first and last dates plotted

    p1 = list()
    p1[[1]] = data()%>%filter(location == "All")%>%
      ggplot(aes(x = date, y = all_patients))+
      geom_bar(stat = "identity")+
      theme_bw()+
      labs(x = ax.date, y = ax.patients)+
      ggtitle(paste(title.patients()))+
      plot_options+
      scale_x_date(date_labels = "%b %Y", breaks = data()$date)+
      theme(axis.text.x = element_text(angle = 90))

    # create plot
    p1[[2]] = data()%>%
      ggplot(aes(x = date, y = rate, color = location))+
      geom_line(data = data()%>%filter(location == "All"), color = "grey", size = 10, alpha = 0.5)+   # plot shadow around "All" (behind other lines)
      geom_point(aes())+ # plot points for all sites
      geom_line(aes(), size = 1.5)+ # plot lines for all sites
      geom_line(data = data()%>%filter(location == "All"), color = "black", size = 1.5)+   # plot "All" on top of other lines
      #xlim(date_summary$min_date, date_summary$max_date + months(8))+   # change x axis lims?
      #annotate("text", x = annotation$date + months(1), y = annotation$rate, label = "  ", size = 10)+   # annotation for avg. rate
      theme_bw()+
      guides(size = FALSE, color = FALSE)+   # don't include legend for size of dots
      labs(x = ax.date, y = ax.rate(), color = ax.location)+
      ggtitle(paste(report_type(), "Rates"))+
      plot_options+
      scale_x_date(date_labels = "%b %Y", breaks = data()$date)+
      theme(axis.text.x = element_text(angle = 90))+#,
      #limits = c(date_summary$min_date,date_summary$max_date + weeks(6)))+ #extend xlim so labels aren't cut off
      #limits = c(date_summary$min_date,date_summary$max_date + months(params$label.months)))+ #extend xlim so labels aren't cut off
      geom_dl(aes(label = location), method = list(dl.trans(x = x + 1.1), "last.bumpup", cex = 1.2, fontface = "bold")) +
      #scale_color_brewer(palette = "Set3")
      scale_color_manual(values = plot_colors())+
      coord_cartesian(clip = "off")+
      theme(plot.margin = unit(c(0,3.5,0,1), "cm"))
      #geom_dl(aes(label = location), method = list(dl.combine("last.points")), cex = 0.8)

    #%>%direct.label("last.qp")

    #Levels: All Alma Illery Braddock CHC East End Hazelwood Hill House McKeesport Steel Valley West End Wilkinsburg

    # # margin of white space between plots
    # margin = theme(plot.margin = unit(rep(1, times = 2), "cm"))
    # grid.arrange(grobs = lapply(p1, "+", margin), nrow = 1, widths = c(1.5,2))  # output plot
    grid.arrange(grobs = p1, nrow = 1, widths = c(1.5,2))  # output plot

    }, height = 500, width = combined_plot_width)


   #######################################

   # PLOTS OF INDIVIDUAL SITES
   # ------------------------------
     # output title for individual locations
     output$text.locations = renderText("Graphs for individual sites")


   # output two plots for each location - one showing total number of patients, other with rate
  output$plot.individualLocations = renderPlot({
    # find max number of patients
    max_patients = data()%>%filter(location != "All")%>%group_by(location)%>%summarize(max_patients_loc = max(all_patients))%>%filter(max_patients_loc == max(max_patients_loc))
    max_patients = max_patients$max_patients_loc

    # find max range of rates for single location
    temp_data = data()%>%filter(location!="All")%>%group_by(location)%>%
      summarize(rate_range = max(rate)- min(rate),   # find ranges of rates for each location
                middle_rate = min(rate) + 0.5 * rate_range)    # find middle between max and min rate for each locaiton
    max_range = temp_data%>%filter(rate_range == max(rate_range)) # calculate max range
    max_range = round(max_range$rate_range[1], 3) + 0.001            # isolate max range as a number, then round and add a little to make sure all data is in range for the site with the maximum range
    
    # make variable for barplot y max
    y_ranges = temp_data%>%mutate(ymin = middle_rate - 0.5 * max_range, ymax = middle_rate + 0.5 * max_range)  # create new ranges for y axes


    p3 = list()   # initialize list of all patients bar graph
    p4 = list()# initialize list of line plot

    # for each location, create both plots
    for( i in 1:(nLocations()-1)){

      location_i = data()$location[i]                   # get name of ith location

      # create all patient bar graph
      p1 = create_patient_barplot(data(), location_i, max_patients, plot_colors()[i+1])
      #create rate line plots
      p2 = create_rate_plot(data(), location_i, y_ranges$ymin[i], y_ranges$ymax[i], plot_colors()[i+1])

      # save the plots in a list
      p3[[i]] = p1
      p4[[i]] = p2
    }

    # combine site plots into one list
    plts = rbind(p3, p4)

    # add margin of white space between plots
    margin = theme(plot.margin = unit(rep(1, times = nLocations()), "cm"))

    # arrange plots into one output
    grid.arrange(grobs = lapply(plts, "+", margin), widths = c(1.5, 2), heights = 4*rep(1, times = nLocations()))


  }, height =  nLocations() * 460 , width = combined_plot_width) # make plots output nice and big



 }) # isolate end



#######################################
# PDF DOWNLOAD
# ------------------------------

# make button so report can be downloaded as pdf
  output$download.report <- downloadHandler(
    # file name should be reporttype_report_todaysDate.pdf
    filename = function() {
      paste(report_type(), "_report_", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "rate_report.Rmd")
      file.copy("rate_report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(report.type = report_type(), rate.data = data(), 
                     patient.notes = patient_notes(), page.per.site = input$page.per.site)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  #output$debug <- renderText({print(data())})
}


# run app
shinyApp(ui = ui, server = server)

#sharing apps shinyapps.io

