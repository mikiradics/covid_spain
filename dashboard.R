library(shiny)
library(shinyWidgets)
library(datasets)
library(data.table)
library(dplyr)
library(ggplot2) 
library(leaflet)
# library(rnaturalearth)
library(rgeos)
library(RColorBrewer)
library(geosphere)
library(plotly)
library(ggrepel)
library(tidyr)

Sys.setlocale(locale = "English")

stack_data <- fread("data/reg_data_total_v2_int_ext_total.csv")
prov_data <- fread("data/prov_data_total_int_ext_total.csv")
rest_data <- fread("data/rest_data.csv")
response_indices <- fread("data/responseindicestoplot.csv")
cases <- fread("data/cases_app_v2.csv")
cases_prov <- fread("data/cases_provincial_app_v2.csv")
region_coords <- fread("data/regions_coords.csv")
flows_mitma <- fread("data/flows_mitma.csv")
# stack_data <- merge(stack_data, region_coords, by = "region")
provinces <- rgdal::readOGR("data/provincias-espanolas.geojson", encoding = "UTF-8")
# holidays <- fread("data/holidays.csv")
flows_mitma_mobile_data <- fread("data/flows_mitma_mobile_data.csv")
internal_external <- fread("data/internal_external.csv")

flows_mitma[flows_mitma$counts == 0][, c("counts")] <- NA
flows_mitma <- drop_na(flows_mitma)

myTheme <- function(){
  theme_classic() +
    theme(
      axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"), size = 8),
      axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"), size = 10),
      plot.margin = unit(c(0, 0, 0, 0), "pt"),
      panel.grid.major = element_line(color = 'grey', linetype = 'dotted', size = 0.1),
      panel.grid.minor = element_line(color = 'grey', linetype = 'dotted', size = 0.1)
    )}

ui <- navbarPage("Mobility in Spain during the Covid-19 pandemic",
                 tabPanel("Comparing different indicators across regions in Spain",
                          sidebarLayout(    
                            sidebarPanel(
                              p("This tab serves for regional analysis. Data of the plots can be downloaded in .csv format by clicking on the download buttons."),
                              pickerInput(inputId = "region",
                                          label = "Region:",
                                          choices = unique(stack_data$region),
                                          selected = "Andalusia",
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              checkboxGroupInput(inputId = "indicator",
                                                 label = "Mobility indicators to display:",
                                                 choices = unique(stack_data$time),
                                                 selected = "JRC index"),
                              sliderInput(inputId = "time_period",
                                          label = "Time period of analysis",
                                          min = as.Date("2020-01-01","%Y-%m-%d"),
                                          max = as.Date("2021-12-31","%Y-%m-%d"),
                                          value = c(as.Date("2020-03-01"),as.Date("2021-07-30")),
                                          timeFormat="%Y-%m-%d"),
                              materialSwitch(inputId = "bg", 
                                             label = "Show restrictions and response indices", 
                                             status = "info"),
                              materialSwitch(inputId = "legend_reg",
                                             label = "Show legends",
                                             status = "info"),
                              # materialSwitch(inputId = "holidays", 
                              #                label = "Show holidays", 
                              #                status = "info"),
                              checkboxGroupInput(inputId = "cas_type",
                                                 label = "Cases / deaths to show:",
                                                 # choices = unique(cases$description),
                                                 choiceNames = c("Number of daily new infections per 100,000 inhabitants (7-day rolling average)", 
                                                                 "Number of daily new deaths per 100,000 inhabitants (7-day rolling average)"),
                                                 choiceValues = c("daily_cases_per100k_mavg", "daily_deaths_per100k_mavg"),
                                                 selected = "daily_cases_per100k_mavg"),
                              pickerInput(inputId = "responseindex",
                                          label = "Response indices to show:",
                                          choices = unique(response_indices$variable),
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              downloadButton('downloadData1','Save data on mobility'),
                              downloadButton('downloadData2','Save data on infections')
                            ),
                            mainPanel(
                              plotlyOutput("plot", width = 1200),                 # mobility indices
                              plotlyOutput("plot_below", width = 1200),           # number of cases
                              # verbatimTextOutput("summary")
                            ))
                 ),
                 
                 tabPanel("Comparing different indicators across provinces in Spain",
                          sidebarLayout(    
                            sidebarPanel(
                              p("This tab serves for provincial analysis. Data of the plots can be downloaded in .csv format by clicking on the download buttons."),
                              pickerInput(inputId = "province",
                                          label = "Province:",
                                          choices = unique(prov_data$location),
                                          selected = "Seville",
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              checkboxGroupInput(inputId = "indicator_prov",
                                                 label = "Mobility indices to display:",
                                                 choices = unique(prov_data$time),
                                                 selected = "JRC index"),
                              sliderInput(inputId = "time_period_province",
                                          label = "Time period of the analysis",
                                          min = as.Date("2020-01-01","%Y-%m-%d"),
                                          max = as.Date("2021-12-31","%Y-%m-%d"),
                                          value = c(as.Date("2020-03-01"),as.Date("2021-07-30")),
                                          timeFormat="%Y-%m-%d"),
                              materialSwitch(inputId = "intext_prov",
                                             label = "Show internal and external movements",
                                             status = "info"),
                              materialSwitch(inputId = "bg_prov",
                                             label = "Show restrictions and response indices",
                                             status = "info"),
                              # materialSwitch(inputId = "holidays_prov",
                              #                label = "Show holidays",
                              #                status = "info"),
                              materialSwitch(inputId = "legend_prov",
                                             label = "Show legends",
                                             status = "info"),
                              pickerInput(inputId = "responseindex_prov",
                                          label = "Response indices to show:",
                                          choices = unique(response_indices$variable),
                                          # choiceNames = c("Stringency Index", "Stringency Legacy Index", "Government Response Index",
                                          #                 "Containment Health Index", "Economic Support Index"),
                                          # choiceValues = c("StringencyIndexForDisplay", "StringencyLegacyIndexForDisplay", "GovernmentResponseIndexForDisplay",
                                          #                  "ContainmentHealthIndexForDisplay", "EconomicSupportIndexForDisplay"),
                                          # selected = "StringencyIndexForDisplay",
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              downloadButton('downloadData3','Save data on mobility'),
                              downloadButton('downloadData4','Save data on infections')),
                            mainPanel(
                              plotlyOutput("plot_province", width = 1200),                 # mobility indices
                              plotlyOutput("plot_province_below", width = 1200),          # number of cases
                              
                            ))
                 ),
                 
                 tabPanel("MITMA macromodel flow map",
                          sidebarLayout(    
                            sidebarPanel(
                              p("The annual total number of trips between provinces can be visualized on this tab (data source: National Transport Model of MITMA, data can be downloaded in .csv format by clicking on the download button)."),
                              width=2,
                              pickerInput(inputId = "origen",
                                          label = "Origin (province):",
                                          choices = unique(flows_mitma$Nombre_Zona_origin),
                                          selected = "Sevilla",
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              pickerInput(inputId = "destination",
                                          label = "Destination (province):",
                                          choices = unique(flows_mitma$Nombre_Zona_destination),
                                          selected = unique(flows_mitma$Nombre_Zona_destination),
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              pickerInput(inputId = "mode",
                                          label = "Selected mode:",
                                          choices = unique(flows_mitma$mode),
                                          options = list(`actions-box` = TRUE),
                                          multiple = F),
                              downloadButton('downloadData5','Save data')
                            ),
                            
                            mainPanel(
                              leafletOutput("flowmap", width = "100%", height = 800)
                            )
                          )
                 ),
                 
                 tabPanel("MITMA mobile data flow map",
                          sidebarLayout(    
                            sidebarPanel(
                              p("The daily number of trips between provinces can be visualized on this tab (data source: MITMA cell information mobile data which can be downloaded in .csv format by clicking on the download button)."),
                              width=2,
                              pickerInput(inputId = "origen_mobile",
                                          label = "Origin (province):",
                                          choices = unique(flows_mitma$Nombre_Zona_origin),
                                          selected = "Sevilla",
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              pickerInput(inputId = "destination_mobile",
                                          label = "Destination (province):",
                                          choices = unique(flows_mitma$Nombre_Zona_destination),
                                          selected = unique(flows_mitma$Nombre_Zona_destination),
                                          options = list(`actions-box` = TRUE),
                                          multiple = T),
                              sliderInput(inputId = "time_period_mobile",
                                          label = "Time period of analysis",
                                          min = as.Date("2020-01-01","%Y-%m-%d"),
                                          max = as.Date("2021-12-31","%Y-%m-%d"),
                                          value = c(as.Date("2020-03-01"),as.Date("2021-07-30")),
                                          timeFormat="%Y-%m-%d"),
                              downloadButton('downloadData6','Save data')
                            ),
                            
                            mainPanel(
                              leafletOutput("flowmap2", width = "100%", height = 800)
                            )
                          )
                 ),
                 
                 tabPanel("Contact and details",
                          sidebarLayout(    
                            sidebarPanel(
                              width = 2,
                              h2("Contact"),
                              p(strong("Author:"), "Miklos Radics",
                                br(),
                                strong("E-mail:"), "miklos.radics@ec.europa.eu")
                              
                              
                              
                            ),
                            
                            mainPanel(
                              h1("Last data update"),
                              p("1 October 2021",
                                br(),
                                br()),
                              h1("Background"),
                              p("This dashboard is an outcome of an exploratory data analysis under my doctoral studies that helps us
                            to analyse the effects of the pandemic and the applied restricting measures on mobility across Spain at 
                            national, autonomous community and provincial levels.",
                                br(),
                                "Currently four tabs are available. Two interactive plots provide access to regional and provincial analysis 
                          where mobility and government response indices as well as data on infections can be set and filtered. 
                          Further two interactive maps describe movements between provinces before the pandemic (MITMA model data) 
                          and during the pandemic (MITMA mobile data).",
                                br()),
                              h1("Data sources"),
                              p("The following three main types of data sources can be visualized:",
                                div("- data on how people moved around in Spain since the pandemic started 
                              (open data based on mobile phone locations, Apple and Google data about travel habits of their users);"),
                                div("- data about how the pandemic evolved over time (number of coronavirus infections and deaths);"),
                                div("- data about restrictions."),
                                br()),
                              p("Data on mobility:",
                                div(
                                  "- ",
                                  a("MITMA cell information mobile data ", 
                                    href = "https://www.mitma.gob.es/ministerio/covid-19/evolucion-movilidad-big-data"),
                                  "was made open by the Spanish Ministry of Transport, 
                              Mobility and Urban Agenda (MITMA, Ministerio de Transportes, Movilidad y Agenda Urbana) to foster research about the pandemic."
                                ),
                                div(
                                  "- ",
                                  a("Apple Mobility Trends Reports ", 
                                    href = "https://www.apple.com/covid19/mobility"),
                                  "show the relative volume of directions requests in Apple Maps per country/region, sub-region or city 
                              (defined as the greater metropolitan area) compared to a baseline volume on January 13, 2020."
                                ),
                                div(
                                  "- ",
                                  a("Google COVID-19 Community Mobility Reports  ", 
                                    href = "https://www.google.com/covid19/mobility/data_documentation.html?hl=en"),
                                  "show how visits to places, such as grocery stores, transit stations, workplaces and parks, are changing in each administrative unit."
                                ),
                                div(
                                  "- The ",
                                  a("National Transport Model of MITMA ", 
                                    href = "https://www.mitma.es/ministerio/proyectos-singulares/hermes/movilidad-mnt"),
                                  "is a tool that supports long distance mobility related strategic planning and provide data about 
                              the total number of annual trips between provinces for the modes of private car, bus, train, aviation."
                                ),
                                br()),
                              p(
                                "Data on restrictions:",
                                div("The results of the",
                                    a("Oxford Covid-19 Government Response Tracker (OxCGRT)",
                                      href = "https://www.bsg.ox.ac.uk/research/research-projects/covid-19-government-response-tracker"
                                    ),
                                    "are used, which is a research project that aims to collect systematic information on policy measures 
                                of governments of more than 180 countries. Four indices were developed on a 0-100 scale to 
                                help understand the raw data: overall government response index, containment and health index, stringency index and economic support index."),
                                br()
                              ),
                              p(
                                "Data on cases and vaccination:",
                                div(
                                  "- Data on the number of coronavirus infections and deaths in Spain are accessed through the API of ",
                                  a("the Covid-19 Tracking Project.",
                                    href = "https://covid19tracking.narrativa.com/"
                                  )),
                                div(
                                  "- Data on vaccination is downloaded from the data repository of ",
                                  a("Datadista.",
                                    href = "https://github.com/datadista/datasets/blob/c18aab0b363850a81cfaee13a589fce18ec286d7/COVID%2019/readme.md" 
                                  )
                                ),
                                br()
                              ),
                              p(
                                "Other data sources:",
                                div(
                                  "Socio-demographic data was retrieved from the website of the ",
                                  a("National Statistics Institute of Spain.",
                                    href = "https://www.ine.es/dyngs/INEbase/operacion.htm?c=Estadistica_C&cid=1254736177011&menu=resultados&secc=1254736195458&idp=1254734710990"
                                  )),
                                br()
                              ),
                              h1("Published results"),
                              p("A report on the results of the analysis will be available online soon.")
                            )
                          )
                 )
                 
)




server <- shinyServer(
  
  function(input,output){
    
    ###########
    # regions
    ###########
    
    datasetInput <- reactive({
      stack_data %>% 
        filter(region %in% input$region) %>%
        filter(date > input$time_period[1] & date < input$time_period[2]) %>%
        filter(time %in% input$indicator)
    })
    
    restricts <- reactive({
      rest_data %>%
        filter((start >= input$time_period[1] & end <= input$time_period[2]) |
                 (start < input$time_period[1] & end > input$time_period[1]) |
                 (start < input$time_period[2] & end > input$time_period[2]))
    })
    
    response <- reactive({
      response_indices %>%
        filter(variable %in% input$responseindex) %>%
        filter(Date > input$time_period[1] & Date < input$time_period[2])
    })
    
    casesfunc <- reactive({
      cases %>%
        filter(english_name %in% input$region) %>%
        filter(description == input$cas_type) %>%
        filter(Date > input$time_period[1] & Date < input$time_period[2])
    })
    
    death_axis <- reactive({
      if (length(cases %>% filter(description == "daily_deaths_per100k_mavg"))>0){
        deaths_axis <-  TRUE
      }
    })
    
    # plot time series
    output$plot <- renderPlotly({
      ylabel <- paste0("Mobility indicator(s) in the selected region(s)")
      dataset <- datasetInput() %>% rename(c(Date = date, Value = indicator_ma, Index = time, Location = region))
      bg_data <- restricts()       #restrictions data to visualize
      bg_data$start[1] <- input$time_period[1]
      bg_data$end[nrow(bg_data)] <- input$time_period[2]
      
      if(nrow(response()) > 0){
        y_min = min(min(datasetInput()$indicator_ma, na.rm = T), min(response()$value, na.rm = T))
        y_max = max(max(datasetInput()$indicator_ma, na.rm = T), max(response()$value, na.rm = T))
      } else {
        y_min = min(datasetInput()$indicator_ma, na.rm = T)
        y_max = max(datasetInput()$indicator_ma, na.rm = T)
      }
      
      layer_rect <- geom_rect(
        data = bg_data,
        aes(xmin = start, xmax = end, ymin = y_min, ymax = y_max, fill = comment),
        alpha = 0.4   #transparency
      )
      
      response_data <- response() %>% rename(c(Response_Index = variable, Value = value))
      layer_response <- geom_line(
        data = response_data,
        aes(x=Date, y=Value, colour=Response_Index)
      )
      
      layer_line <- geom_line(
        data = dataset,
        aes(x=Date, y=Value, colour=Location, linetype=Index)
      )
      
      if (input$bg==T){
        plot_reg <- ggplot() + layer_rect
        plot_reg <- plot_reg + layer_line
        if (nrow(response()) > 0){
          plot_reg <- plot_reg + layer_response
        }
        plot_reg <- plot_reg + labs(y = ylabel, x = "") + 
          scale_fill_manual(values = c("Before pandemic" = "#fafffa", "First cases" = "#BFD4DB", "Community transmission" = "#fcffe4",
                                       "State of alarm" = "#f4cccc", "Halting of all non-essential activity" = "#f4cccc", "Lifting of some restrictions" = "#fcffe4",
                                       "De-escalation" = "#BFD4DB", "New normality" = "#fafffa", "Resurgence" = "#BFD4DB",
                                       "State of emergency reimposed" = "#f4cccc", "Gradual return to normal" = "#fafffa"))
        plot_reg <- plot_reg + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")  
        
        
        # geom_text(data=bg_data, aes(x=(start), y=0, label=comment), size=4, angle = 90, hjust = "bottom") +
        # +
        # theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = "top")+
        #+
        # if (input$holidays == T){
        #   geom_text(data=holidays, aes(x=(start), y=0, label=comment), size=4, angle = 90, hjust = "bottom")
        # }
      } else {
        plot_reg <- ggplot() + layer_line + labs(y = ylabel, x = "")
        plot_reg <- plot_reg + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
        
        # +
        # theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = "top")+
        # myTheme() #+
        # if (input$holidays == T){
        #   geom_text(data=holidays, aes(x=(start), y=0, label=comment), size=4, angle = 90, hjust = "bottom")
        # }
        
      }
      
      plot_reg <- plot_reg + myTheme()
      plot_reg <- ggplotly(plot_reg)
      
      if (input$legend_reg == T){
        plot_reg <- plot_reg %>% layout(showlegend = TRUE)                           # horizontal legend - (legend = list(orientation = 'h', x = 0, y = -0.2))
      }
      else {
        plot_reg <- plot_reg %>% layout(showlegend = FALSE)
      }
      plot_reg
    })
    
    
    
    output$plot_below <- renderPlotly({
      ylabel <- "Number of cases / deaths (7-day rolling average)"
      cases_data <- casesfunc() %>% rename(c(Value = value, Description = description, Location = english_name))
      # deaths_axis <- death_axis()
      bg_data <- restricts()
      bg_data$start[1] <- input$time_period[1]
      bg_data$end[nrow(bg_data)] <- input$time_period[2]
      
      if(nrow(response()) > 0){
        y_min_below = min(min(casesfunc()$value, na.rm = T), min(response()$value, na.rm = T))
        y_max_below = max(max(casesfunc()$value, na.rm = T), max(response()$value, na.rm = T))
      } else {
        y_min_below = min(casesfunc()$value, na.rm = T)
        y_max_below = max(casesfunc()$value, na.rm = T)
      }
      
      layer_rect <- geom_rect(
        data = bg_data,
        aes(xmin = start, xmax = end, ymin = y_min_below, ymax = y_max_below, fill = comment),
        alpha = 0.4
      )
      
      # scaleFactor <- max(filter(cases, description=="daily_cases_per100k_mavg")$value) /  max(filter(cases, description=="daily_deaths_per100k_mavg")$value)
      # cases_data[description=="daily_deaths_per100k_mavg"]$value <- scaleFactor*cases_data[description=="daily_deaths_per100k_mavg"]$value
      
      response_data <- response() %>% rename(c(Response_Index = variable, Value = value))
      layer_response <- geom_line(
        data = response_data,
        aes(x=Date, y=Value, colour=Response_Index)
      )
      
      layer_line_cases <- geom_line(
        data = cases_data,
        aes(x=Date, y=Value, colour=Location, linetype=Description)
      )
      
      
      if (input$bg==T) {
        plot <- ggplot()
        plot <- plot + layer_rect
        plot <- plot + layer_line_cases
        if (nrow(response()) > 0){
          plot <- plot + layer_response
        }
        plot <- plot + labs(y = ylabel, x = "")
        plot <- plot + scale_fill_manual(values = c("Before pandemic" = "#fafffa", "First cases" = "#BFD4DB", "Community transmission" = "#fcffe4",
                                                    "State of alarm" = "#f4cccc", "Halting of all non-essential activity" = "#f4cccc", "Lifting of some restrictions" = "#fcffe4",
                                                    "De-escalation" = "#BFD4DB", "New normality" = "#fafffa", "Resurgence" = "#BFD4DB",
                                                    "State of emergency reimposed" = "#f4cccc", "Gradual return to normal" = "#fafffa"))
        # plot <- plot + geom_text(data=bg_data, aes(x=(start), y=0, label=comment), size=4, angle = 90, hjust = "bottom")
        plot <- plot + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
        # plot <- plot + theme(axis.text.x=element_text(angle=60, hjust=1), legend.position="bottom")
        # plot <- plot+ myTheme()
        # if ("daily_deaths_per100k_mavg" %in% input$cas_type){
        #   plot <- plot + scale_y_continuous(sec.axis=sec_axis(~./scaleFactor, name="Number of deaths (7-day rolling average)"))
        # plot <- plot + scale_y_continuous(position = "right")
        # }
        # if (input$holidays == T){
        #   plot <- plot + geom_text(data=holidays, aes(x=(start), y=0, label=comment), size=4, angle = 90, hjust = "bottom")
        # }
      }
      else {
        plot <- ggplot()
        plot <- plot + layer_line_cases
        plot <- plot + labs(y = ylabel, x = "")
        plot <- plot + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
        # plot <- plot + theme(axis.text.x=element_text(angle=60, hjust=1), legend.position="bottom")
        # plot <- plot + myTheme()
        # if ("daily_deaths_per100k_mavg" %in% input$cas_type){
        # plot <-  plot + scale_y_continuous(sec.axis=sec_axis(~./scaleFactor, name="Number of deaths (7-day rolling average)"))
        # plot <-  plot + scale_y_continuous(position = "right")
        # }
        # if (input$holidays == T){
        #   plot <- plot + geom_text(data=holidays, aes(x=(start), y=0, label=comment), size=4, angle = 90, hjust = "bottom")
        # }
        
      }
      
      plot <- plot + myTheme()
      plot <- ggplotly(plot)
      
      if (input$legend_reg == T){
        plot <- plot %>% layout(showlegend = TRUE)                           # horizontal legend - (legend = list(orientation = 'h', x = 0, y = -0.2))
      }
      else {
        plot <- plot %>% layout(showlegend = FALSE)
      }
      plot
      
    })
    
    
    
    
    output$downloadData1 <- downloadHandler(
      filename = function() {
        paste("data-mobility-regional.csv", sep="")
      },
      content = function(file) {
        df <- datasetInput() %>% rename(c(value = indicator, value_rolling_average = indicator_ma, index = time))
        write.csv(df, file)
      }
    )  
    
    output$downloadData2 <- downloadHandler(
      filename = function() {
        paste("data-infections-regional.csv", sep="")
      },
      content = function(file) {
        df <- casesfunc() %>% rename(c(date = Date, region = english_name, value_rolling_average = value))
        df <- df[, c("date", "value_rolling_average", "region", "description")]
        write.csv(df, file)
      }
    )
    
    
    
    
    ###########
    # provinces
    ###########
    
    datasetInputProv <- reactive({
      prov_data %>% 
        filter(location %in% input$province) %>%
        filter(date > input$time_period_province[1] & date < input$time_period_province[2]) %>%
        filter(time %in% input$indicator_prov)
    })
    
    restricts_prov <- reactive({
      rest_data %>%
        filter((start >= input$time_period_province[1] & end <= input$time_period_province[2]) |
                 (start < input$time_period_province[1] & end > input$time_period_province[1]) |
                 (start < input$time_period_province[2] & end > input$time_period_province[2]))
    })
    
    province_int_ext <- reactive({
      internal_external %>% 
        filter(location_google %in% input$province) %>%
        filter(date > input$time_period_province[1] & date < input$time_period_province[2])
    })
    
    response_prov <- reactive({
      response_indices %>%
        filter(variable %in% input$responseindex_prov) %>%
        filter(Date > input$time_period_province[1] & Date < input$time_period_province[2])
    })
    
    casesfunc_prov <- reactive({
      cases_prov %>%
        filter(location_google %in% input$province) %>%
        filter(as.Date(date) > input$time_period_province[1] & as.Date(date) < input$time_period_province[2])
    })
    
    output$plot_province <- renderPlotly({
      dataset_prov <- datasetInputProv() %>% rename(c(Date = date, Value = indicator_ma, Index = time, Location = location))
      bg_data_prov <- restricts_prov()       #restrictions data to visualize
      bg_data_prov$start[1] <- input$time_period_province[1]
      bg_data_prov$end[nrow(bg_data_prov)] <- input$time_period_province[2]
      
      if (input$intext_prov == T){
        y_max <- max(province_int_ext()$share_of_internal_trips, na.rm = T)
        y_min <- min(province_int_ext()$share_of_internal_trips, na.rm = T)
        if (nrow(response_prov()) >0){
          y_max <- max(max(province_int_ext()$share_of_internal_trips, na.rm = T), max(response_prov()$value, na.rm = T))
          y_min <- min(min(province_int_ext()$share_of_internal_trips, na.rm = T), min(response_prov()$value, na.rm = T))
        }
      }
      if (input$intext_prov == F){
        y_max <- max(datasetInputProv()$indicator_ma, na.rm = T)
        y_min <- min(datasetInputProv()$indicator_ma, na.rm = T)
        if (nrow(response_prov()) >0){
          y_max <- max(max(datasetInputProv()$indicator_ma, na.rm = T), max(response_prov()$value, na.rm = T))
          y_min <- min(min(datasetInputProv()$indicator_ma, na.rm = T), min(response_prov()$value, na.rm = T))
        }
      }
      
      layer_rect_prov <- geom_rect(
        data = bg_data_prov,
        aes(xmin = start, xmax = end, ymin = y_min, ymax = y_max, fill = comment),
        alpha = 0.4, show.legend = F   #transparency
      )
      
      response_data_prov <- response_prov() %>% rename(c(Response_Index = variable, Value = value))
      layer_response_prov <- geom_line(
        data = response_data_prov,
        aes(x=Date, y=Value, colour=Response_Index)
      )
      
      layer_line_prov <- geom_line(
        data = dataset_prov,
        aes(x=Date, y=Value, colour=Location, linetype=Index)
      )
      
      int_ext <- 	province_int_ext() %>% rename(c(Date = date, Share_of_internal_trips = share_of_internal_trips, Location = Nombre_Zona))
      int_ext_plot <- geom_line(
        data = int_ext,
        aes(x=Date, y=Share_of_internal_trips, colour=Location)
      )
      
      
      plot_prov <- ggplot()
      if (input$intext_prov == T){
        plot_prov <- plot_prov + int_ext_plot
        if (input$bg_prov==T) {
          plot_prov <- ggplot() + layer_rect_prov + int_ext_plot
          # plot_prov <- plot_prov + ylim(min(int_ext$Share_of_internal_trips), max(int_ext$Share_of_internal_trips))
          if (nrow(response_data_prov >0)){
            plot_prov <- ggplot() + layer_rect_prov + int_ext_plot + layer_response_prov
          }
        }
        plot_prov <- plot_prov + labs(y = "Share of internal trips in the selected province(s) [%]", x = "", size = 6)
        plot_prov <- plot_prov + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
        if (input$bg_prov==T) {
          plot_prov <- plot_prov + scale_fill_manual(values = c("Before pandemic" = "#fafffa", "First cases" = "#BFD4DB", "Community transmission" = "#fcffe4",
                                                                "State of alarm" = "#f4cccc", "Halting of all non-essential activity" = "#f4cccc", "Lifting of some restrictions" = "#fcffe4",
                                                                "De-escalation" = "#BFD4DB", "New normality" = "#fafffa", "Resurgence" = "#BFD4DB",
                                                                "State of emergency reimposed" = "#f4cccc", "Gradual return to normal" = "#fafffa"))
        }
        plot_prov <- plot_prov + theme(axis.text.x=element_text(angle=60, size = 6, hjust=1), 
                                       axis.text.y=element_text(size = 6),
                                       axis.title.y = element_text(size = 8) 
        )
        # if (input$holidays_prov == T){
        #   plot_prov <- plot_prov + geom_text(data=holidays, aes(x=(start), y=0, label=comment), size=4, angle = 90, hjust = "bottom")
        # }
        # plot_prov <- plot_prov + theme(legend.position="top")
        plot_prov <- plot_prov + myTheme()
      }
      if (input$intext_prov == F){
        plot_prov <- plot_prov + layer_line_prov
        if (input$bg_prov==T) {
          plot_prov <- ggplot() + layer_rect_prov + layer_line_prov
          if (nrow(response_data_prov) >0){
            plot_prov <- ggplot() + layer_rect_prov + layer_line_prov + layer_response_prov
          }
        }
        plot_prov <- plot_prov + labs(y = "Mobility indicator(s) in the selected provinces(s)", x = "")
        plot_prov <- plot_prov + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
        if (input$bg_prov==T) {
          plot_prov <- plot_prov + scale_fill_manual(values = c("Before pandemic" = "#fafffa", "First cases" = "#BFD4DB", "Community transmission" = "#fcffe4",
                                                                "State of alarm" = "#f4cccc", "Halting of all non-essential activity" = "#f4cccc", "Lifting of some restrictions" = "#fcffe4",
                                                                "De-escalation" = "#BFD4DB", "New normality" = "#fafffa", "Resurgence" = "#BFD4DB",
                                                                "State of emergency reimposed" = "#f4cccc", "Gradual return to normal" = "#fafffa"))
          # y_ <- rep(c(-10, 10, 20, 30), times = nrow(bg_data_prov))[1:nrow(bg_data_prov)]
          # plot_prov <- plot_prov + geom_text(data=bg_data_prov, aes(x=(start), y=y_, label=comment), size=2, angle = 90, hjust = "bottom")
        }
        plot_prov <- plot_prov + theme(axis.text.x=element_text(angle=60, size = 6, hjust=1),
                                       axis.text.y=element_text(size = 6),
                                       axis.title.y = element_text(size = 8)
        )
        # if (input$holidays_prov == T){
        #   plot_prov <- plot_prov + geom_text(data=holidays, aes(x=(start), y=0, label=comment), size=2, angle = 90, hjust = "bottom")
        # }
        plot_prov <- plot_prov + theme(legend.position="top")
        plot_prov <- plot_prov + myTheme()
      }
      
      plot_prov <- ggplotly(plot_prov)
      
      if (input$legend_prov == T){
        plot_prov <- plot_prov %>% layout(showlegend = TRUE)                           # horizontal legend - (legend = list(orientation = 'h', x = 0, y = -0.2))
      }
      else {
        plot_prov <- plot_prov %>% layout(showlegend = FALSE)
      }
      # plot_prov <- plot_prov %>%  add_annotations( x = bg_data_prov$start, 
      #                                              y = c(0),
      #                                              # xref = bg_data_prov$start,
      #                                              # yref = "y",
      #                                              text = bg_data_prov$comment,
      #                                              font = list(color = '#000000'),
      #                                              textangle = 90)
      # 
      # add_annotations( x = as.list(bg_data_prov$start), 
      #                              y = 0,
      #                              xref = "x",
      #                              yref = "y",
      #                              text = c('Easy Visible'),
      #                              font = list(color = '#000000'), showarrow = F, textangle = 90)
      
      
      plot_prov
      
    })
    
    output$plot_province_below <- renderPlotly({
      cases_data_prov <- casesfunc_prov() %>% rename(c(Date = date, Daily_cases_per100k_inhabitants = daily_cases_per100k_mavg, Location = location_google))
      bg_data_prov <- restricts_prov()
      bg_data_prov$start[1] <- input$time_period_province[1]
      bg_data_prov$end[nrow(bg_data_prov)] <- input$time_period_province[2]
      
      if (nrow(response_prov() > 0)){
        y_max_below <- max(max(casesfunc_prov()$daily_cases_per100k_mavg, na.rm = T), max(response_prov()$value, na.rm = T))
        y_min_below <- min(min(casesfunc_prov()$daily_cases_per100k_mavg, na.rm = T), min(response_prov()$value, na.rm = T))
      } else {
        y_max_below <- max(casesfunc_prov()$daily_cases_per100k_mavg, na.rm = T)
        y_min_below <- min(casesfunc_prov()$daily_cases_per100k_mavg, na.rm = T)
      }
      
      layer_rect_prov <- geom_rect(
        data = bg_data_prov,
        aes(xmin = start, xmax = end, ymin = y_min_below, ymax = y_max_below, fill = comment),
        alpha = 0.4
      )
      
      response_data_prov_below <- response_prov() %>% rename(c(Response_Index = variable, Value = value))
      layer_response_prov_below <- geom_line(
        data = response_data_prov_below,
        aes(x=Date, y=Value, colour=Response_Index)
      )
      
      layer_line_cases_prov <- geom_line(
        data = cases_data_prov,
        aes(x=Date, y=Daily_cases_per100k_inhabitants, colour=Location)
      )
      
      if (input$bg_prov==T) {
        plot_prov_below <- ggplot()
        plot_prov_below  <- plot_prov_below + layer_rect_prov
        plot_prov_below  <- plot_prov_below + layer_line_cases_prov
        if (nrow(response_data_prov_below) > 0){
          plot_prov_below  <- plot_prov_below  + layer_response_prov_below
        }
        plot_prov_below  <- plot_prov_below + labs(y = "Number of cases (7-day rolling average)", x = "")
        plot_prov_below  <- plot_prov_below + scale_fill_manual(values = c("Before pandemic" = "#fafffa", "First cases" = "#BFD4DB", "Community transmission" = "#fcffe4",
                                                                           "State of alarm" = "#f4cccc", "Halting of all non-essential activity" = "#f4cccc", "Lifting of some restrictions" = "#fcffe4",
                                                                           "De-escalation" = "#BFD4DB", "New normality" = "#fafffa", "Resurgence" = "#BFD4DB",
                                                                           "State of emergency reimposed" = "#f4cccc", "Gradual return to normal" = "#fafffa"))
        # y_ <- rep(c(-10, 10, 20, 30), times = nrow(bg_data_prov))[1:nrow(bg_data_prov)]
        # plot_prov_below <- plot_prov_below + geom_text(data=bg_data_prov, aes(x=(start), y=y_, label=comment), size=2, angle = 90, hjust = "bottom")
        plot_prov_below  <- plot_prov_below + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
        plot_prov_below  <- plot_prov_below + theme(axis.text.x=element_text(angle=60, hjust=1), legend.position="bottom")
        plot_prov_below <- plot_prov_below + theme(axis.text.x=element_text(angle=60, size = 6, hjust=1), 
                                                   axis.text.y=element_text(size = 6),
                                                   axis.title.y = element_text(size = 8)) 
        # if (input$holidays_prov == T){
        #   plot_prov_below <- plot_prov_below + geom_text(data=holidays, aes(x=(start), y=0, label=comment), size=2, angle = 90, hjust = "bottom")
        # }
        plot_prov_below  <- plot_prov_below + myTheme()
      }
      else {
        plot_prov_below <- ggplot()
        plot_prov_below <- plot_prov_below + layer_line_cases_prov
        plot_prov_below <- plot_prov_below + labs(y = "Number of cases (7-day rolling average)", x = "")
        plot_prov_below <- plot_prov_below + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
        plot_prov_below <- plot_prov_below + theme(axis.text.x=element_text(angle=60, hjust=1), legend.position="bottom")
        plot_prov_below <- plot_prov_below + theme(axis.text.x=element_text(angle=60, size = 6, hjust=1), 
                                                   axis.text.y=element_text(size = 6),
                                                   axis.title.y = element_text(size = 8)) 
        # if (input$holidays_prov == T){
        #   plot_prov_below <- plot_prov_below + geom_text_repel(data=holidays, aes(x=start, y=0, label=comment), size=20, angle = 90)
        # }
        plot_prov_below <- plot_prov_below + myTheme()
        plot_prov_below
      }
      
      plot_prov_below <- ggplotly(plot_prov_below)
      
      if (input$legend_prov == T){
        plot_prov_below <- plot_prov_below %>% layout(showlegend = TRUE)                          # horizontal legend - (legend = list(orientation = 'h', x = 0, y = -0.2)) 
      }
      else {
        plot_prov_below <- plot_prov_below %>% layout(showlegend = FALSE)
      }
      plot_prov_below
      
    })
    
    output$downloadData3 <- downloadHandler(
      filename = function() {
        paste("data-mobility-provincial.csv", sep="")
      },
      content = function(file) {
        if (input$intext_prov == F){
          df <- datasetInputProv()  %>% rename(c(value = indicator, value_rolling_average = indicator_ma, index = time))  
        } else
        {
          df <- province_int_ext() %>% rename(c(location = location_google))
          df <- df[, c("date", "trips_internal", "trips_not_internal", "share_of_internal_trips", "location")]
        }
        
        write.csv(df, file)
      }
    )  
    
    output$downloadData4 <- downloadHandler(
      filename = function() {
        paste("data-infections-provincial.csv", sep="")
      },
      content = function(file) {
        df_cases <- casesfunc_prov() %>% rename(c(number_of_cases = num_casos, daily_cases_per100k_inhabitants = daily_cases_per100k,
                                                  daily_cases_per100k_inhabitants_rolling_average = daily_cases_per100k_mavg, location = location_google, population2020 = Population2020))
        write.csv(df_cases, file)
      }
    )
    
    ##########
    # flow map - macro model
    ##########
    
    flowfunc <- reactive({
      flows_mitma %>%
        filter(Nombre_Zona_origin %in% input$origen) %>%
        filter(Nombre_Zona_destination %in% input$destination) %>%
        filter(mode %in% input$mode)
      
    })
    
    output$flowmap <- renderLeaflet({ 
      
      flows_data <- flowfunc()
      
      flows <- gcIntermediate( flows_data[,c("xcoord_origin", "ycoord_origin")],  flows_data[,c("xcoord_destination", "ycoord_destination")], sp = TRUE, addStartEnd = TRUE)
      
      flows$counts <- flows_data$counts                         
      
      flows$origins <- flows_data$Nombre_Zona_origin
      
      flows$destinations <- flows_data$Nombre_Zona_destination
      
      hover <- paste0(flows$origins, " to ",
                      flows$destinations, ': ',
                      as.character(flows$counts))
      
      pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origins)
      
      # leaflet(reg_data) %>% 
      #   setView(lat  = 40.416775, lng = -3.703790, zoom = 5) %>%
      #   addTiles() %>%
      #   addCircles(lng = ~lon, lat = ~lat, weight = 1,
      #              radius = ~value * 1000, popup = ~region
      
      
      leaflet(flows) %>%
        addProviderTiles('CartoDB.Positron') %>%
        setView(lat  = 40.416775, lng = -3.703790, zoom = 6) %>%
        addPolygons(data = provinces, color="grey", weight = 1, opacity = 0.5, fillOpacity = 0, label = ~provincia, group = "Provinces" )  %>%
        addPolylines(data = flows, weight = ~counts/max(flows$counts)*40, label = hover,
                     group = ~origins, color = ~pal(origins)) %>%
        addLayersControl(overlayGroups = c("Provinces"),
                         options = layersControlOptions(collapsed = FALSE))
      
      # addLegend("bottomright", pal = pal, values = ~counts,
      #           title = "Number of trips",
      #           labFormat = labelFormat(prefix = "$"),
      #           opacity = 1)
      
    })
    
    output$downloadData5 <- downloadHandler(
      filename = function() {
        paste("data-model.csv", sep="")
      },
      content = function(file) {
        flows_to_save <- flowfunc() %>% rename(c(name_province_origin = Nombre_Zona_origin, name_province_destination = Nombre_Zona_destination, number_of_trips = counts))
        flows_to_save <- flows_to_save[, c("name_province_origin", "NUTS_ID_origin", "name_province_destination", "NUTS_ID_destination", "mode", "number_of_trips")]
        write.csv(flows_to_save, file)
      }
    )
    
    ############
    # flowmap - mobile data
    ############
    
    mobile_data <- reactive({
      flows_mitma_mobile_data %>%
        filter(date > input$time_period_mobile[1] & date < input$time_period_mobile[2]) %>%
        filter(Nombre_Zona_origin %in% input$origen_mobile) %>%
        filter(Nombre_Zona_destination %in% input$destination_mobile) %>%
        group_by(Nombre_Zona_origin, Nombre_Zona_destination, xcoord_origin, ycoord_origin, xcoord_destination, ycoord_destination) %>%
        summarize(trips = sum(trips))
    })
    
    output$flowmap2 <- renderLeaflet({ 
      
      flows_mitma_mobile_data <- mobile_data()
      
      flows_mobile <- gcIntermediate(flows_mitma_mobile_data[,c("xcoord_origin", "ycoord_origin")], flows_mitma_mobile_data[,c("xcoord_destination", "ycoord_destination")], sp = TRUE, addStartEnd = TRUE)
      
      flows_mobile$counts <- flows_mitma_mobile_data$trips
      flows_mobile$origins <- flows_mitma_mobile_data$Nombre_Zona_origin
      flows_mobile$destinations <- flows_mitma_mobile_data$Nombre_Zona_destination
      
      hover <- paste0(flows_mobile$origins, " to ",
                      flows_mobile$destinations, ': ',
                      as.character(flows_mobile$counts))
      
      pal <- colorFactor(brewer.pal(4, 'Set2'), flows_mobile$origins)
      
      # leaflet(reg_data) %>% 
      #   setView(lat  = 40.416775, lng = -3.703790, zoom = 5) %>%
      #   addTiles() %>%
      #   addCircles(lng = ~lon, lat = ~lat, weight = 1,
      #              radius = ~value * 1000, popup = ~region
      
      
      leaflet(flows_mobile) %>%
        addProviderTiles('CartoDB.Positron') %>%
        setView(lat  = 40.416775, lng = -3.703790, zoom = 6) %>%
        addPolygons(data = provinces, color="grey", weight = 1, opacity = 0.5, fillOpacity = 0, label = ~provincia, group = "Provinces" )  %>%
        addPolylines(data = flows_mobile, weight = ~counts/max(flows_mobile$counts)*40, label = hover,
                     group = ~origins, color = ~pal(origins)) %>%
        addLayersControl(overlayGroups = c("Provinces"),
                         options = layersControlOptions(collapsed = FALSE))
      
      
      
      
    })
    
    output$downloadData6 <- downloadHandler(
      filename = function() {
        paste("data-mobile.csv", sep="")
      },
      content = function(file) {
        flows_to_save_mobile <- mobile_data() %>% rename(c(name_province_origin = Nombre_Zona_origin, name_province_destination = Nombre_Zona_destination, number_of_trips = trips))
        flows_to_save_mobile <- flows_to_save_mobile[, c("name_province_origin", "name_province_destination", "number_of_trips")]
        write.csv(flows_to_save_mobile, file)
      }
    )
    
  })

shinyApp(ui = ui, server = server)