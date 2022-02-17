
# Team #:9
######################################################################################
#load library
library(readxl)
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

###################################### Q2,Q3 #############################################
#Q2.read file
setwd("/Users/yuxuanli/Desktop/备份/R For Analytics")
df <- read_xlsx("window_breakage_data.xlsx", sheet=1, col_names=T)
#remove spaces in column names
colnames(df) <- make.names(colnames(df), unique = TRUE)

#Q3.coerce the column
colnames_df <- as.vector(colnames(df))
categorical_vars <- c("Batch", "Window.color",
                      "Window.Type", "Glass.Supplier",
                      "Glass.Supplier.Location", "Pass.Fail")
numeric_vars <- colnames_df[! colnames_df%in%categorical_vars]
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)
df[numeric_vars] <- lapply(df[numeric_vars], as.numeric)
# data fram with only numeric variables and with only categorical variables
numeric_df <- df[,numeric_vars]
categorical_df <- df[,categorical_vars]

################################# functions & variables used in shiny (for reference) ############################
## 6. source function
source("DataQualityReportOverall.R")
DataCompleteness <- DataQualityReportOverall(df)
## 8. use dply in some fashion
library(dplyr)
glimpse(df)
window_type_stat <- df %>%
  select("Breakage.Rate","Window.Type") %>%
  filter(Window.Type != "NA") %>%
  group_by(Window.Type) %>%
  summarize("Breakage Rate Mean" = mean(Breakage.Rate), 
            "Breakage Rate Median" = median(Breakage.Rate),
            "Breakage Rate SDev" = sd(Breakage.Rate),
            "Breakage Rate IQR" = IQR(Breakage.Rate)) %>%
  arrange(desc("Median Breakage Rate")) %>%
  top_n(3)

## 9 use apply() families
# we included apply() and lapply() in shiny, please see Q9 in shiny.
#lapply() and sapply(), find the max of silicon viscosity and cut speed. lapply() returns in list, sapply() returns in vector
list_df <- list(A=df$Silicon.Viscosity, B=df$Cut.speed)
lapply_max_viscosity_speed <- lapply(list_df, FUN=max,na.rm=T)
sapply_max_viscosity_speed <- sapply(list_df,FUN=max, na.rm=T)

#mapply(), compare the mean and standard deviation of window size in Pass group and Fail group
windowsize_pass <- list(mean=mean(df$Window.Size[df$Pass.Fail =="Pass"],na.rm=T),
                        std=sd(df$Window.Size[df$Pass.Fail =="Pass"],na.rm=T))
windowsize_fail <- list(mean=mean(df$Window.Size[df$Pass.Fail=="Fail"],na.rm=T),
                        std=sd(df$Window.Size[df$Pass.Fail =="Fail"],na.rm=T))
mapply_for_comparison <- mapply(FUN=identical,windowsize_pass,windowsize_fail)

## 10 use aggregate()
aggregation.df <- aggregate(cbind(Glass.thickness,Yield)~Batch+Glass.Supplier, data=df, FUN=mean)


##################################################### shiny! ##################################################################
ui <- fluidPage (theme = shinytheme("superhero"),
                 navbarPage(title = "Window Manufacture Summary Dashboard",
                            tabPanel("Plots",sidebarPanel(
                              ################# Q4 ggplot input ###########################
                              selectInput('hist_x',"Choose a variable for histogram:", names(numeric_df), 
                                          selected = names(numeric_df)[[1]]),
                              sliderInput("binCount", "Bins for histogram",
                                          min = 1, max = 10, value = 5),
                              selectInput('density_x',"Choose a variable for density plot:", names(numeric_df), 
                                          selected = names(numeric_df)[[1]]),
                              selectInput('box_x', label = "X axis of Box Plot:", names(categorical_df), 
                                          selected = names(categorical_df)[[4]]),
                              selectInput('box_y', label = "Y axis of Box Plot:", names(numeric_df), 
                                          selected = names(numeric_df)[[1]]),
                              selectInput('bar_x', label = "X axis of Bar Graoh:", names(categorical_df), 
                                          selected = names(categorical_df)[[4]]),
                              selectInput('scatter_x', label = "X axis of Scatter Plot:", names(numeric_df), 
                                          selected = names(numeric_df)[[5]]),
                              selectInput('scatter_y', label = "Y axis of Scatter Plot:", names(numeric_df),
                                          selected = names(numeric_df)[[2]]),
                              actionButton(inputId = "click", label = "Click Me To Plot!")),
                              
                              ################################### Q4 ggplot main panel ##############################
                              mainPanel (
                                h2("Histogram"),
                                plotOutput("hist"),
                                h2("Density Plot"),
                                plotOutput("density"),
                                h2("Box Plot"),
                                plotOutput("box"),
                                h2("Bar Graph"),
                                plotOutput("bar"),
                                h2("Scatter Plot"),
                                plotOutput("scatter"))),
              
                          tabPanel("Tables Summary",
                            sidebarPanel( 
                               ####################### Q5  table inputs #####################################################
                               selectInput('groupby', label = "Choose a variable to show its count", names(categorical_df),
                                           selected =names(categorical_df)[[3]]),
                               actionButton(inputId = "click2", label = "Show Count Table"),
                               selectInput('summary_var', label = "Choose variables to show five number summary", names(numeric_df),
                                           selected =names(categorical_df)[[1]]),
                               actionButton(inputId = "click3", label = "Show Statistics")),
                            ############################ Q5  table main panel #################################################
                             mainPanel( 
                               h2("Count Table"),
                               dataTableOutput("count"),
                               h2("Five Number Summary"),
                               dataTableOutput("five"))),
                                       
                           tabPanel("Data Explore",
                              ################################# Q9 apply() input #####################################
                              sidebarPanel( numericInput('row_col', label = "Mean of rows(1) columns(2)", 2, 
                                                         min = 1, max = 2, step = 1)),
                              ############################ Q6 Q7 Q8 Q9 Q10 main panel #################################
                              mainPanel(  
                                h2("Data Completeness"),
                                verbatimTextOutput("completeness"),
                                h2("Best Window Type"),
                                verbatimTextOutput("best"),
                                h2("Window Type Break Rate Statistics"),
                                dataTableOutput("type_break_stat"),
                                h2("Mean of Rows/Columns"),
                                verbatimTextOutput("rc"),
                                h2("Maximum Silicon Viscosity and Cut Speed"),
                                verbatimTextOutput("max"),
                                h2("Aggregate glass thickness,yield on batch and glass supplier"),
                                dataTableOutput("agg")))))
                 

server <- function(input, output) {
  
  #################################### Q4.ggplot2 ########################################### 
  #histogram
  observeEvent(input$click,{
    output$hist <- renderPlot({
      ggplot(df, aes_string(x=input$hist_x)) + 
        geom_histogram(color="darkorange1",fill="darkorange1",binwidth = input$binCount)
    })
  })
  
  #density
  observeEvent(input$click,{
    output$density <- renderPlot({
      ggplot(data=df) + geom_density(aes_string(x = input$density_x),
                                     fill="darkorange1", color="darkorange1") 
    })
  })
  
  #box
  observeEvent(input$click,{
    output$box <- renderPlot({
      ggplot(data=df, aes_string(x=input$box_x,y=input$box_y)) + 
        geom_boxplot(color="darkorange1",fill="darkorange3") 
    })
  })
  
  #bar graph
  observeEvent(input$click,{
    output$bar <- renderPlot({
      ggplot(data=df, aes_string(x = input$bar_x)) +
        geom_bar(color="darkorange1",fill="darkorange1") 
    })
  })
  
  #scatter
  observeEvent(input$click,{
    output$scatter <- renderPlot ({
      ggplot(data=df,aes_string(x=input$scatter_x, y=input$scatter_y)) + 
        geom_point(fill="darkorange1", color="darkorange1")
      
    })
  })
  
  
  ################ Q5. Show at least one table that summarize the data ####################
  
  ### Q5 data
  summary_data <- reactive({
    numeric_df[,input$summary_var]})
  
  #count table
  observeEvent(input$click2,{
    output$count <- renderDataTable({df %>%
        group_by("Group by"=get(input$groupby)) %>%
        summarise("Count"=n())
    })
  })
  
  #summary statistics table
  observeEvent(input$click3,{
    output$five <- renderDataTable({summary(summary_data())
    })
  })
  
  
  ######################### Q6.source function #######################################
  output$completeness <- renderPrint({DataCompleteness
  })
  
  ########################### Q7. use loop###################################### 
  # find window type that has the best quality among others
  min_count = 500
  min_i = 0
  df1 <- df[!is.na(df$Window.Type),]
  windowType=unique(df1$Window.Type)
  for (i in 1:length((windowType))) {
    count = count(df1[df1$Window.Type==windowType[i] & df1$Pass.Fail == "Fail", ])
    if (count < min_count){
      min_count = count
      min_i = i
    }
  }
  output$best <- renderPrint(windowType[i])
  
  ######################### Q8.use dyplr #############################################
  output$type_break_stat <- renderDataTable({window_type_stat
  })
  
  
  ######################### Q9.use apply() families #######################################
  #apply()
  output$rc <- renderPrint({apply(numeric_df, MARGIN=as.numeric(input$row_col),
                                  FUN=mean, na.rm=TRUE)
  })
  #lapply()
  output$max <- renderPrint({
    lapply(list(df$Silicon.Viscosity, df$Cut.speed), FUN=max, na.rm=T)
  })
  
  ########################### Q10. use aggregate()######################################
  ## 10 use aggregate()
  aggregation_data <- reactive({
    aggregation.df
  })
  
  output$agg <- renderDataTable({aggregation_data()
    })
  
  
}


shinyApp(ui = ui, server = server)

