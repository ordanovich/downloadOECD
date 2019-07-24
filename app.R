library(dplyr)
library(magrittr)
library(OECD)
library(tidyr)
library(DT)
library(plotly)
library(sparkline)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(htmlwidgets)
library(openxlsx)
library(rmarkdown)



myModal <- function() {
  div(id = "test",
      modalDialog(downloadButton("download1","Download table as csv"),
                  br(),
                  br(),
                  downloadButton("download2","Download table as xlsx"),
                  
                  easyClose = TRUE, title = "Download Table")
  )
}


dataset_list <- OECD::get_datasets()

##--------------------------------------

header <- dashboardHeader(disable=T)

sidebar <- dashboardSidebar(disable=T)

body <- dashboardBody(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  tags$head(
    tags$style(HTML(
      ".tabbable ul li:nth-child(1) { float: left; }
      .tabbable ul li:nth-child(2) { float: left; }
      .tabbable > .nav > li > a  {background-color: white;  color:black}"
    ))
  ),
  
  
  fluidRow(
    
    column(width = 12,
           
           boxPlus(
             title = "Set criteria for data retrieval", 
             closable = FALSE,
             width = 12,
             
             fluidRow(
               
               column(
                 
                 width = 4,
                 
                 radioButtons(inputId = "oecd_quickSpain",
                              label = "Quick search for Spain only",
                              choices = c("Enable", "Disable"),
                              selected = "Enable",
                              inline = T)
                 
                 
               ),
               
               column(
                 
                 width = 4,
                 
                 selectInput(inputId = "oecd_dataset",
                             label = "Select table",
                             choices = sort(dataset_list$title),
                             selected = "Public sector debt by instrument coverage"
                 )
                 
               ),
               
               column(
                 
                 width = 4,
                 
                 htmlOutput("oecd_time_range")
                 
               )),
             
             fluidRow(
               
               column(width = 12,
                      
                      fluidRow(
                        
                        column(width = 4,
                               uiOutput("geovars")
                        ),
                        
                        column(width = 4,
                               uiOutput("geovars_values")
                        )
                        
                      )
                      
               )
             )
             
             
           )
           
           
    )
    
  ),
  
  fluidRow(
    
    column(width = 6,
           
           gradientBox(
             title = "Full table",
             width = 12,
             icon = "fa fa-table",
             gradientColor = "purple",
             boxToolSize = "xs",
             closable = FALSE,
             collapsible = TRUE,
             
             fluidRow(
               
               column(width = 12,
                      
                      radioButtons(inputId = "retrieve_clean_data",
                                   label = "Clean up original values",
                                   choices = c("Yes", "No"),
                                   selected = "Yes",
                                   inline = T)
                      
               )
               
             ),
             
             
             footer = fluidRow(
               
               column(width = 12,
                      
                      uiOutput("t")
                      
               )
               
             )
             
             
           )
    ),
    
    
    column(width = 6,
           
           gradientBox(
             title = "Overview",
             width = 12,
             icon = "fa fa-table",
             gradientColor = "maroon",
             boxToolSize = "xs",
             closable = FALSE,
             collapsible = TRUE,
             
             fluidRow(
               
               column(width = 12,
                      
                      uiOutput("grouping_var")
                      )
             ),
             
             footer = fluidRow(
               
               column(width = 12,
                      
                      uiOutput("p"),
                      
                      downloadButton("report", "Generate report")
               )
               
             )
             
             
           )
    )
    
  )
)



shinyApp(
  
  ui = dashboardPage(header, sidebar, body),
  
  server = function(input, output, session) { 
    
    options(shiny.usecairo=T)
    
    oecd_dataset_id <- reactive({
      
      dataset_list %>% 
        mutate_if(is.factor, as.character) %>% 
        filter(title == input$oecd_dataset) %>% 
        select(id) -> id; id$id
      
    })
    
    
    oecd_data_str <- reactive({
      
      get_data_structure(oecd_dataset_id())
      
    })
    
    oecd_time_range <- reactive({
      
      
      tt_range_TIME <- as.numeric(oecd_data_str()$TIME$id[nchar(oecd_data_str()$TIME$id) == 4])
      tt_range_YEA <- as.numeric(oecd_data_str()$YEA$id[nchar(oecd_data_str()$YEA$id) == 4])
      
      unique(append(tt_range_TIME, tt_range_YEA))
      
      
    })
    
    output$oecd_time_range <- renderUI({
      
      
      sliderInput(inputId = "oecd_time_range",
                  label = "Select time period",
                  min = min(oecd_time_range()),
                  max = max(oecd_time_range()),
                  value = c(round(quantile(oecd_time_range())[4],0),
                            round(quantile(oecd_time_range())[5],0)),
                  sep = "")
      
    })
    
    oecd_data_raw <- reactive({
      
      if(input$oecd_quickSpain == "Enable"){
        
        OECD::get_dataset(
          
          dataset =  oecd_dataset_id(),
          filter = "ESP",
          start_time = input$oecd_time_range[1],
          end_time = input$oecd_time_range[2])
        
      }
      else if(input$oecd_quickSpain == "Disable"){
        
        OECD::get_dataset(
          
          dataset =  oecd_dataset_id(),
          start_time = input$oecd_time_range[1],
          end_time = input$oecd_time_range[2])
      }
      
      
    })
    
    
    
    oecd_data_clean <- reactive({
      
      
      
      dstruc_df <- lapply(names(oecd_data_str()),
                          function(current_name)
                            transform(oecd_data_str()[[current_name]],
                                      variable = current_name))
      
      dstruc_df <- lapply(dstruc_df, function(df) df %>% mutate_if(is.factor, as.character))
      dstruc_df <- data.table::rbindlist(dstruc_df[-1])
      
      df <- oecd_data_raw()
      
      nn_df <- names(df); nn_df[!(nn_df %in% c("obsTime", "obsValue"))] -> nn_df
      
      
      
      for (i in nn_df) {
        
        df  %>% merge(dstruc_df %>%
                        filter(variable == i) %>%
                        select(id, label),
                      by.x = i,
                      by.y = "id") -> df
        
      }
      
      
      names(df)[!(names(df) %in% append(nn_df, c("obsTime", "obsValue")))] <- oecd_data_str()$VAR_DESC[oecd_data_str()$VAR_DESC$id %in% nn_df, "description"]
      
      df[!(names(df) %in% nn_df)] -> df
      
      df$obsYear <- as.integer(substr(df$obsTime, start = 1, stop = 4))
      
      names(df)[names(df) == 'obsTime'] <- 'Period'
      names(df)[names(df) == 'obsValue'] <- 'Value'
      names(df)[names(df) == 'obsYear'] <- 'Year'
      
      df[ , order(names(df))]
      
    })
    
    output$t_clean <- renderDataTable(
      
      datatable(oecd_data_clean() %>% 
                  dplyr::filter(UQ(as.name(input$geovars_clean)) %in% input$geovars_values_clean),
                extensions = 'Buttons',
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    "copy",
                    list(
                      extend = "collection",
                      text = 'Download entire dataset',
                      action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
                    )
                  )
                )
      )
    )
    
    output$t_raw <- renderDataTable(
      
      datatable(
        as.data.frame(oecd_data_raw() %>% dplyr::filter(UQ(as.name(input$geovars_raw)) %in% input$geovars_values_raw)),
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            "copy",
            list(
              extend = "collection",
              text = 'Download entire dataset',
              action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
}")
            )
          )
        )
      )
    )
    
    output$t <- renderUI({
      
      if(input$retrieve_clean_data == "Yes"){dataTableOutput("t_clean") %>% withSpinner(color="#0dc5c1")}
      else if(input$retrieve_clean_data == "No"){dataTableOutput("t_raw") %>% withSpinner(color="#0dc5c1")}
      
    })
    
    output$geovars_clean <- renderUI({
      
      nn <- sort(names(oecd_data_clean()))
      
      selectInput(inputId = "geovars_clean",
                  label = "Select variable containing geolocation data",
                  choices = nn,
                  selected = "Country")
      
    })
    
    output$geovars_raw <- renderUI({
      
      nn <- sort(names(oecd_data_raw()))
      
      selectInput(inputId = "geovars_raw",
                  label = "Select variable containing geolocation data",
                  choices = nn,
                  selected = "LOCATION")
      
    })
    
    output$geovars <- renderUI({
      
      if(input$retrieve_clean_data == "Yes"){htmlOutput("geovars_clean")}
      else if(input$retrieve_clean_data == "No"){htmlOutput("geovars_raw")}
      
    })
    
    
    output$geovars_values_clean <- renderUI({
      
      oecd_data_clean() %>% select(UQ(as.name(input$geovars_clean))) %>% set_colnames(c("G")) %>% distinct() -> dd
      
      selectizeInput(inputId = "geovars_values_clean",
                  label = "Select geographic units",
                  choices = sort(dd$G),
                  selected = sample(dd$G,1),
                  multiple = T)
      
    })
    
    output$geovars_values_raw <- renderUI({
      
      oecd_data_raw() %>% select(UQ(as.name(input$geovars_clean))) %>% set_colnames(c("G")) %>% distinct() -> dd
      
      selectizeInput(inputId = "geovars_values_raw",
                  label = "Select geographic units",
                  choices = sort(dd$G),
                  selected = sample(dd$G,1),
                  multiple = T)
      
    })
    
    
    output$geovars_values <- renderUI({
      
      if(input$retrieve_clean_data == "Yes"){htmlOutput("geovars_values_clean")}
      else if(input$retrieve_clean_data == "No"){htmlOutput("geovars_values_raw")}
      
    })
    
    output$groupingvars_clean <- renderUI({
      
      nn <- sort(names(oecd_data_clean()))
      
      selectInput(inputId = "groupingvars_clean",
                  label = "Select grouping variable",
                  choices = nn,
                  selected = "Country")
      
    })
    
    output$groupingvars_raw <- renderUI({
      
      nn <- sort(names(oecd_data_raw()))
      
      selectInput(inputId = "groupingvars_raw",
                  label = "Select grouping variable",
                  choices = nn,
                  selected = "LOCATION")
      
    })
    
    output$grouping_var <- renderUI({
      
      if(input$retrieve_clean_data == "Yes"){htmlOutput("groupingvars_clean")}
      else if(input$retrieve_clean_data == "No"){htmlOutput("groupingvars_raw")}
      
    })
    
    
    
    output$p_clean <- renderPlotly({
      
      
      oecd_data_clean() %>%
        dplyr::filter(UQ(as.name(input$geovars_clean)) %in% input$geovars_values_clean) %>%
        select(Value, UQ(as.name(input$groupingvars_clean))) %>% 
        set_colnames(c("Value", "G")) -> dd
      
      p <- plot_ly(dd, x = ~Value, color = ~G, type = "box", showlegend=FALSE)
      
      p
      
    })
    
    output$p_raw <- renderPlotly({
      
      oecd_data_raw() %>%
        dplyr::filter(UQ(as.name(input$geovars_raw)) %in% input$geovars_values_raw) %>%
        select(Value, UQ(as.name(input$groupingvars_raw))) %>% 
        set_colnames(c("Value", "G")) -> dd
      
      p <- plot_ly(dd, x = ~Value, color = ~G, type = "box", showlegend=FALSE)
      
      p
      
    })
    
    
    output$p <- renderUI({
      
      if(input$retrieve_clean_data == "Yes"){plotlyOutput("p_clean") %>% withSpinner(color="#0dc5c1")}
      else if(input$retrieve_clean_data == "No"){plotlyOutput("p_raw") %>% withSpinner(color="#0dc5c1")}
      
    })
    
    
    
    observeEvent(input$test, {
      print("hello")
      showModal(myModal())
    })
    
    
    output$download1 <- downloadHandler(
      filename = function() {
        paste(input$oecd_dataset, ".csv", sep="")
      },
      content = function(file) {
        
        if(input$retrieve_clean_data == "Yes"){ write.csv( oecd_data_clean() %>% 
                                                             dplyr::filter(UQ(as.name(input$geovars_clean)) %in% input$geovars_values_clean),
                                                           file)}
        
        else if(input$retrieve_clean_data == "No"){ write.csv( oecd_data_raw() %>% 
                                                                 dplyr::filter(UQ(as.name(input$geovars_raw)) %in% input$geovars_values_raw), file)}
        
      }
    )
    
    output$download2 <- downloadHandler(
      filename = function() {
        paste(input$oecd_dataset, ".xlsx", sep="")
      },
      content = function(file) {
        if(input$retrieve_clean_data == "Yes"){ write.xlsx( oecd_data_clean() %>% 
                                                             dplyr::filter(UQ(as.name(input$geovars_clean)) %in% input$geovars_values_clean),
                                                           file)}
        
        else if(input$retrieve_clean_data == "No"){ write.xlsx( oecd_data_raw() %>% 
                                                                 dplyr::filter(UQ(as.name(input$geovars_raw)) %in% input$geovars_values_raw), file)}
        
      }
    )
    
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        if(input$retrieve_clean_data == "Yes"){ 
        # Set up parameters to pass to Rmd document
        params <- list(n = oecd_data_clean() %>%
                         dplyr::filter(UQ(as.name(input$geovars_clean)) %in% input$geovars_values_clean) %>%
                         select(Value, UQ(as.name(input$groupingvars_clean))) %>% 
                         set_colnames(c("Value", "G")),
                       y1 = input$oecd_time_range[1],
                       y2 = input$oecd_time_range[2])
        }
        
        
        else if(input$retrieve_clean_data == "No"){ 
          # Set up parameters to pass to Rmd document
          params <- list(n = oecd_data_raw() %>%
                           dplyr::filter(UQ(as.name(input$geovars_raw)) %in% input$geovars_values_raw) %>%
                           select(Value, UQ(as.name(input$groupingvars_raw))) %>% 
                           set_colnames(c("Value", "G")),
                         y1 = input$oecd_time_range[1],
                         y2 = input$oecd_time_range[2] )
        }
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    
    
  }
)

