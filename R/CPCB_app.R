library(shiny)
library(Cairo)
library(DT)
library(plotly)
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(htmltools)
library(shinyjs)
library(lubridate)
library(zoo)
library(caTools)
library(xts)
library(stringr)
library(readr)
library(openair)
library(xlsx)
library(openxlsx)

ui <- fluidPage(
  h1("Analyse CPCB data"),
  tags$head(
    tags$style(HTML(".sidebar {
                    height : 10vh; overflow-y : auto; font-size : 14px;
                    }" ,
                    ".shiny-output-error-validation {
                    color : red; font-size : 14px;
                    }"
    ))),
  sidebarLayout(position = "left",
                sidebarPanel(width = 3,
                             conditionalPanel(condition = "input.tabs1 == 6",
                                              tags$hr(),
                                              h4("Alarms! Check for any malfunction."),
                                              tags$hr()),
                             conditionalPanel(condition = "input.tabs1 == 4",
                                              tags$hr(),
                                              selectInput("palleInp", "Map this pollutant",
                                                          "Select"),
                                              tags$hr()),
                             conditionalPanel(condition = "input.tabs1 == 3",
                                              tags$hr(),
                                              h4("Summary Statistics"),
                                              tags$hr()),
                             conditionalPanel(condition = "input.tabs1 == 2",
                                              tags$hr(),
                                              h4("Time series plots"),
                                              tags$hr()
                             ),
                             conditionalPanel(condition = "input.tabs1 == 1",
                                              tags$hr(),
                                              fileInput("file1",
                                                        "CPCB 1 hour data",
                                                        multiple = FALSE,
                                                        accept = c('.xlsx')),
                                              tags$hr(),
                                              checkboxInput('remove_9', 'Removes negative values'),
                                              tags$hr(),
                                              h5("Add a multiple for removing outliers (mean + y *(sd))"),
                                              numericInput("e",
                                                           "y * sd",
                                                           value = 3.0),
                                              tags$hr(),
                                              h5("Add % of data completeness required in a day or month"),
                                              numericInput("per",
                                                           "percent",
                                                           value = 75),
                                              tags$hr(),
                                              actionButton("hourly", "HOUR"),
                                              tags$hr(),
                                              downloadButton('download',"Download as csv"),
                                              tags$hr())),
                mainPanel(
                  tags$head(
                    tags$style(type = 'text/css',
                               ".nav-tabs {
                               font-size: 18px
                               } ")),
                  tabsetPanel(id = "tabs1",
                              tabPanel(value = 1,
                                       title = "Joined File",
                                       dataTableOutput("table1")),
                              tabPanel(
                                value = 3,
                                title = "Summary",
                                dataTableOutput("table")
                              )))
  ))


server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30*1024^2, shiny.launch.browser = TRUE)
  
  file_name_CPCB <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }else {
      name_CPCB <- gsub(".xlsx$", "", basename(input$file1$name))
      return(name_CPCB)
    }
  })
  
  CPCB_f <- reactive({
    name_CPCB <- file_name_CPCB()
    completeFun <- function(data, desiredCols) {
      completeVec <- complete.cases(data[, desiredCols])
      return(data[completeVec, ])
    }
    ### Function to check for Mean+3SD and Mean-3SD; caution needs to have all the columns without NA values
    LLD <- function(x, y, z, ey) {
      if (is.na(x) ) {
        return (NA)
      }
      else if (x > (y + (ey * z)) || x < (y - (ey * z)) ){
        return (NA)
      }
      else if (is.null(x) || x == '' || is.null(y) || y == '' || is.null(z) || z == '') {
        return (NA)
      } else
      {
        return (x)
      }
    }
    per <- input$per
    
    if (is.null(input$file1)) {
      return(NULL)
    } else {
      trial <- read.xlsx2("D:/Dropbox/ILKConsultancy/ambient_data/data/Bapuji Nagar/site_16320200330134440.xlsx",
                         1, startRow = 17)
      trial$date <- gsub(":00", ":00:00", trial$To.Date, fixed = TRUE)
      
      ### This folder contains files with different columns on below other so 
      # splitting it based on empty rows after a set of parameters
      trial$tbl_id <- cumsum(!nzchar(trial$date))
      trial <- trial[nzchar(trial$date), ]
      trial[ , c('From.Date', 'To.Date')] <- list(NULL)
      dt_s <- split(trial[, -ncol(trial)], trial$tbl_id)
      
      ### Three dataframes representing different parameters; Also the start and 
      # the end date was used from each fiel to create a time series dataframe
      PM <- data.frame(dt_s$`0`) %>%
        mutate(date = as.POSIXct(date, format = '%d-%m-%Y %H:%M:%S', tz = "Asia/Kolkata"))
      ye <- format(PM[1, "date"], format = "%Y")
      x1 <- as.POSIXct(paste0(ye, "-01-01 01:00:00"), 
                       format = '%Y-%m-%d %H:%M:%S', tz = "Asia/Kolkata")
      ye <- format(tail(PM$date, n = 3)[1], format = "%Y")
      x2 <- as.POSIXct(paste0(ye, "-12-31 23:00:00"), 
                       format = '%Y-%m-%d %H:%M:%S', tz = "Asia/Kolkata")
      date <- seq(
        from = as.POSIXct(x1, tz = "Asia/Kolkata"),
        to = as.POSIXct(x2, tz = "Asia/Kolkata"),
        by = "60 min"
      ) 
      tseries_df <- data.frame(date)
      
      ### Join the three idfferent dataframe into a single one
      site1_join <- left_join(tseries_df, PM, by = "date")
      
      make_df <- function(y, tseries_df) {
        df <- data.frame(y)
        if(!nrow(df))
        {
          df <- tseries_df
        } else {
          names(df) <- as.matrix(df[1, ])
          df <- df[-1, ]
          df[] <- lapply(df, function(x) type.convert(as.character(x)))
          df <- Filter(function(x)! all(is.na(x)), df)
          df <- df %>%
            dplyr::select("date" = `To Date`, everything()) %>%
            mutate(date = as.POSIXct(date, format = '%d-%m-%Y %H:%M:%S', 
                                     tz = "Asia/Kolkata"))
        }
      }
      Ben <- make_df(dt_s$`2`, tseries_df)
      Beny <- make_df(dt_s$`4`, tseries_df)
      Bent <- make_df(dt_s$`6`, tseries_df)
      all <- list(site1_join, Ben, Beny, Bent) %>% reduce(left_join, by = "date")
      # data_list1 <- subset(data_list1, no_hour >= ((per / 100) * 24))
      all
    }
  })
 
  data_joined <- eventReactive(input$hourly, {
    data <- CPCB_f()
    name_CPCB <- file_name_CPCB()
    return(data)
  })
  output$table1 <- DT::renderDataTable({
    data_joined <- data_joined() 
    cols <- names(data_joined)[2:ncol(data_joined)]
    data_joined[ , cols] <- sapply(X = data_joined[ , cols], 
                                   FUN = function(x) as.numeric(as.character(x)))
    setDT(data_joined)
    data_joined[,(cols) := round(.SD, 2), .SDcols = cols]
    datatable(data_joined, options = list("pageLength" = 25)) %>% formatDate(1, "toLocaleString")
  })
  
  output$download <- downloadHandler(
    filename <- function() {"CPCB_data.csv"},
    content <- function(fname) {
      data_joined <- data_joined()
      write.csv(data_joined, fname)
    })
  
  output$table <- DT::renderDataTable({
    data <- data_joined()
    data <- data %>%
      select(everything(), - date)
    columns <- 1:ncol(data)
    data[, columns] <- lapply(columns, function(x) as.numeric(as.character(data[[x]])))
    tmp1 <- do.call(data.frame,
                    list(Mean = apply(data, 2, mean, na.rm = TRUE),
                         SD = apply(data, 2, sd, na.rm = TRUE),
                         Median = apply(data, 2, median, na.rm = TRUE),
                         IQR = apply(data, 2, IQR, na.rm = TRUE),
                         Min = apply(data, 2, min, na.rm = TRUE),
                         Max = apply(data, 2, max, na.rm = TRUE),
                         p1  = apply(data, 2, quantile, probs = c(0.01),
                                     na.rm = TRUE),
                         p10 = apply(data, 2, quantile, probs = c(0.1),
                                     na.rm = TRUE),
                         p25 = apply(data, 2, quantile, probs = c(0.25),
                                     na.rm = TRUE),
                         p75 = apply(data, 2, quantile, probs = c(0.75),
                                     na.rm = TRUE),
                         p90 = apply(data, 2, quantile, probs = c(0.9),
                                     na.rm = TRUE),
                         p99 = apply(data, 2, quantile, probs = c(0.99),
                                     na.rm = TRUE),
                         Total_non_NA = apply(data, 2,
                                              function(x)
                                              {length(which(!is.na(x)))})))
    tmp <- data.frame(tmp1)
    tmp$Mean   <- round(as.numeric(as.character(tmp$Mean)), digits = 2)
    tmp$IQR    <- round(as.numeric(as.character(tmp$IQR)), digits = 2)
    tmp$Median <- round(as.numeric(as.character(tmp$Median)), digits = 2)
    tmp$Min    <- round(as.numeric(as.character(tmp$Min)), digits = 2)
    tmp$Max    <- round(as.numeric(as.character(tmp$Max)), digits = 2)
    tmp$p10    <- round(as.numeric(as.character(tmp$p10)), digits = 2)
    tmp$SD     <- round(as.numeric(as.character(tmp$SD)), digits = 2)
    tmp$p90    <- round(as.numeric(as.character(tmp$p90)), digits = 2)
    tmp$p75    <- round(as.numeric(as.character(tmp$p75)), digits = 2)
    tmp$p99    <- round(as.numeric(as.character(tmp$p99)), digits = 2)
    tmp$p1     <- round(as.numeric(as.character(tmp$p1)), digits = 2)
    tmp$p25    <- round(as.numeric(as.character(tmp$p25)), digits = 2)
    tmp
    tmp <- t(tmp)
    datatable(tmp, options = list("pageLength" = 13))
  })
}
## Run app
shinyApp(ui, server)






