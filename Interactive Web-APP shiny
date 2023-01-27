################################################################################
# MGMT 590 Using R for Analytics
# Team Lab #3
# Due: 12/05/2021 11:59pm
#
#
# Team #:9
### we added progress indicator under lab2:scatter plot. #############

######################################################################################
#load library
library(readxl)
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library (vegan)
library (cluster)
library(caret)
library(lpSolveAPI)
library(lpSolve)


###################################### Initialization #############################################
setwd("//Users/yuxuanli/Desktop/备份/R For Analytics/FinalProject")
#read file
df <- read.csv("chocolate_590.csv")


#coerce the column
colnames_df <- as.vector(colnames(df))
categorical_vars <- c("company_location", "country_of_bean_origin",
                      "review_date","sugar","cocoa_butter","lecithin",
                      "salt","sweetener_without_sugar","vanilla")
numeric_vars <- colnames_df[! colnames_df%in%categorical_vars]
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)
df[numeric_vars] <- lapply(df[numeric_vars], as.numeric)

numeric_df <- df[,numeric_vars]
categorical_df <- df[,categorical_vars]
################################# functions & variables used in shiny (for reference) ############################
## source function
source("DataQualityReportOverall.R")
DataCompleteness <- DataQualityReportOverall(df)

## use dply in some fashion
library(dplyr)
glimpse(df)
rating_stat <- df %>%
  select("rating","vanilla") %>%
  filter(vanilla != "NA") %>%
  group_by(vanilla) %>%
  summarize("rating Mean" = mean(rating), 
            "rating Median" = median(rating),
            "rating SDev" = sd(rating),
            "rating IQR" = IQR(rating)) %>%
  arrange(desc("Median rating")) %>%
  top_n(3)

##use apply() families
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

##aggregate()
aggregation.df <- aggregate(cbind(Glass.thickness,Yield)~Batch+Glass.Supplier, data=df, FUN=mean)

##################################################### Lab3-q5 ##################################################################
##### find and remove high correlation#######################3
descrCor <-  cor(numeric_df_clean[,2:ncol(numeric_df_clean)])   
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85)
summary(descrCor[upper.tri(descrCor)])                  
# which columns in your correlation matrix have a correlation greater than some
# Find them and remove them
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
filteredDescr <- numeric_df_clean[,2:ncol(numeric_df_clean)][,-highlyCorDescr] 
# summarize the correlations
descrCor2 <- cor(filteredDescr)  
summary(descrCor2[upper.tri(descrCor2)])
#new data frame
new_df <- cbind(numeric_df_clean$Breakage.Rate, filteredDescr)
#rename the target variable to "y"
names(new_df)[1] <- "y"

##################################################### shiny! ##################################################################
ui <- fluidPage (theme = shinytheme("superhero"),
                 navbarPage(title = "Window Manufacture Summary Dashboard",
            ############################## Lab2 ###########################
                            tabPanel("Plots",sidebarPanel(
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
                                          selected = names(numeric_df)[[1]]),
                              selectInput('scatter_y', label = "Y axis of Scatter Plot:", names(numeric_df),
                                          selected = names(numeric_df)[[2]]),
                              actionButton(inputId = "click", label = "Click Me To Plot!")),
                              
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
              
          ####################### Lab2 #####################################################
                          tabPanel("Tables Summary",
                            sidebarPanel( 
                               selectInput('groupby', label = "Choose a variable to show its count", names(categorical_df),
                                           selected =names(categorical_df)[[3]]),
                               actionButton(inputId = "click2", label = "Show Count Table"),
                               selectInput('summary_var', label = "Choose variables to show five number summary", names(numeric_df),
                                           selected =names(categorical_df)[[1]]),
                               actionButton(inputId = "click3", label = "Show Statistics")),

                             mainPanel( 
                               h2("Count Table"),
                               dataTableOutput("count"),
                               h2("Five Number Summary"),
                               dataTableOutput("five"))),
                        
          ################################# Lab2#####################################               
                           tabPanel("Data Explore",
                              sidebarPanel( numericInput('row_col', label = "Mean of rows(1) columns(2)", 2, 
                                                         min = 1, max = 2, step = 1)),
                
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
                                dataTableOutput("agg"))),
                          
                          
                ############################ Lab3-Q4  Cluster Tab #####################################################      
                  tabPanel("Descriptive Analysis",
                           sidebarPanel( 
                             ####################### Q5  table inputs #####################################################
                             selectInput('Xcol', label = "Choose the X variable", numeric_vars,
                                         selected =numeric_vars[[5]]),
                             selectInput('Ycol', label = "Choose the Y variable", numeric_vars,
                                         selected =numeric_vars[[7]]),
                             numericInput('clusters', 'Cluster count', 3, min = 2, max = 9),
                           actionButton(inputId = "Run", label = "Click me to plot graph!"),
                           actionButton(inputId = "Run2", label = "Click me to plot elbow graph!"),
                           actionButton(inputId = "Run3", label = "Click me to plot silhoutte plot!")),
                           ############################ Q5  table main panel #################################################
                           mainPanel(
                             plotOutput('plot1'),
                             plotOutput("plot2"),
                             plotOutput("plot3"))),
                          
                ############################ Lab3-Q5  Linear Regression Tab #####################################################      
                          tabPanel("Linear Regression",
                                   # sidebarPanel(
                                     # selectInput('lr_x1', label = "select x1", names(numeric_df), 
                                     #                        selected = names(numeric_df)[[1]]),
                                     # selectInput('lr_x2', label = "select x2", names(numeric_df), 
                                     #             selected = names(numeric_df)[[2]]),
                                     # selectInput('lr_y', label = "select y", names(numeric_df), 
                                     #         selected = names(numeric_df)[[6]])),
                          mainPanel(
                            h2("Model Fit"),
                            verbatimTextOutput("mf"),
                            h2("Adjusted R"),
                            verbatimTextOutput("adjR"),
                            h2("Estimated Coefficients"),
                            verbatimTextOutput("ec")
                            
                          )),
                ############################ Lab3-Q6  Optimization Tab #####################################################
                          tabPanel("Model Optimization",
                                    sidebarPanel(
                                    numericInput("yieldx","YieldX:", 0),
                                   numericInput("window_size","Window.Size:", 0),
                                   numericInput("glass_thickness","Glass.thickness :", 0),          
                                   numericInput("ambient_temp","Ambient.Temp :", 0),
                                   numericInput("cut_speed","Cut.speed :", 0),
                                   numericInput("edge_rate","Edge.Deletion.rate :", 0),
                                   numericInput("spacer_distance","Spacer.Distance :", 0),
                                   numericInput("silicon_viscosity","Silicon.Viscosity :", 0),
                                   selectInput("notation", "Comparable:",
                                               c(">=" = ">=",
                                                 ">" = ">",
                                                 "=" = "=",
                                                 "<" = "<",
                                                 "<=" = "<=")),
                                   numericInput("value","Constrain Value:", 0.000000),
                                   actionButton(inputId = "add_btn", label = "Add Constraint"),
                                   actionButton(inputId = "solve_btn", label = "Show Solution")),
                                
                                  
                                   mainPanel(
                                     h2("Solve Model"),
                                     verbatimTextOutput("solve"),
                                     h2("Minimized Breakage Rate"),
                                     verbatimTextOutput("mini"),
                                     h2("Optimal Variable Solution"),
                                     verbatimTextOutput("solution"),
                                     h2("Optimal Parameter Setting"),
                                     plotOutput("dist")
                                     
                                   ))
                          
                          ))
                 

server <- function(input, output) {
  
  #################################### Lab2-ggplot ########################################### 
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
  
  #################################### Lab3-q3 progress indicator added ########################################### 
    #scatter
  observeEvent(input$click,{
    output$scatter <- renderPlot ({
      
      progress <- Progress$new()
      on.exit(progress$close())
      
      progress$set(message = 'making plot...',
                   value=0)
      n <-5
      for (i in 1:n) {
        progress$inc(1/n, detail = paste("Doing part", i))
        Sys.sleep(0.1)
      }
      ggplot(data=df,aes_string(x=input$scatter_x, y=input$scatter_y)) + 
        geom_point(fill="darkorange1", color="darkorange1")
      
    })
  })
  
  
  ################ Lab 2: Showtable that summarize the data ####################
  
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
  
  
  ######################### Lab2 source function #######################################
  output$completeness <- renderPrint({DataCompleteness
  })
  
  ########################### Lab2 use loop###################################### 
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
  
  ######################### Lab2 use dyplr #############################################
  output$type_break_stat <- renderDataTable({window_type_stat
  })
  
  
  ######################### Lab2 use apply() families #######################################
  #apply()
  output$rc <- renderPrint({apply(numeric_df, MARGIN=as.numeric(input$row_col),
                                  FUN=mean, na.rm=TRUE)
  })
  #lapply()
  output$max <- renderPrint({
    lapply(list(df$Silicon.Viscosity, df$Cut.speed), FUN=max, na.rm=T)
  })
  
  ########################### Lab2 use aggregate()######################################
  ## 10 use aggregate()
  aggregation_data <- reactive({
    aggregation.df
  })
  
  output$agg <- renderDataTable({aggregation_data()
    })
  
  
  ########################### Lab3-q4 ######################################

  selectedData <- reactive({
    df<-na.omit(df)
    df[, c(input$Xcol, input$Ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  observeEvent(input$Run, {output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })})
  
  observeEvent(input$Run2, {output$plot2 <- renderPlot({
    set.seed(123)
    df<-na.omit(df) 
    df <- df[numeric_vars]
    scaled_data = as.matrix(scale(df))
    # Compute and plot wss for k = 2 to k = 15.
    k.max <- 9
    
    wss <- sapply(2:k.max, 
                  function(k){kmeans(df, k, nstart=50,iter.max = 15 )$tot.withinss})
    plot(2:k.max, wss,
         type="b", pch = 19, frame = FALSE, 
         xlab="Number of clusters K",
         ylab="Total within-clusters sum of squares")
    
  })})
  
  observeEvent(input$Run3, {output$plot3 <- renderPlot({
    set.seed(123)
    df<-na.omit(df) 
    df <- df[numeric_vars]
    silhouette_score <- function(k){
      km <- kmeans(df, centers = k, nstart=25)
      ss <- silhouette(km$cluster, dist(df))
      mean(ss[, 3])
    }
    k <- 2:9
    avg_sil <- sapply(k, silhouette_score)
    plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
    
    
  })})
  
  ########################### Lab3-5(a) fit a linear regression model ######################################
 
  descrCor <-  cor(numeric_df_clean[,2:ncol(numeric_df_clean)])                           # correlation matrix
  highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85) # num Xs with cor > t
  summary(descrCor[upper.tri(descrCor)])                    # summarize the cors
  # which columns in your correlation matrix have a correlation greater than some
  # specified absolute cutoff. Find them and remove them
  highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
  filteredDescr <- numeric_df_clean[,2:ncol(numeric_df_clean)][,-highlyCorDescr] 
  # summarize the cors again
  descrCor2 <- cor(filteredDescr)  
  summary(descrCor2[upper.tri(descrCor2)])
  #new data
  new_df <- cbind(numeric_df_clean$Breakage.Rate, filteredDescr)
  #rename the target variable to "y"
  names(new_df)[1] <- "y"
  
   fit <- reactive({
     lm(y ~., data=new_df)})

    output$mf <- renderPrint({
      summary(fit())
    })
  

  ################# 5(b) display statistics (adjusted R and coefficients)######################################
  
  output$adjR <- renderPrint({summary(fit())$adj.r.squared})
  options(scipen=0)
  output$ec <- renderPrint({summary(fit())$coefficients[,c("Estimate","Pr(>|t|)")]})

  ########################### Lab3-6 ######################################
  #initialize every time user click "Show Solution"
  init_solver = function(){
    lps.model <<- make.lp(nrow=0, ncol=9)
    set.type(lps.model, columns=1:9, type="real")
    name.lp(lps.model, name="Window Breakage LP")
    lp.control(lps.model, sense="min")
    fitted = lm(y ~., data=new_df)
    coe <<- summary(fitted)$coefficients[ , "Estimate"]
    coe_vec <- as.vector(summary(fitted)$coefficients[ , "Estimate"])
    # add objective function 
    set.objfn(lps.model, obj=coe_vec)
    add.constraint(lps.model, c(1,0,0,0,0,0,0,0,0), "=", 1)
    for(i in 2:9){
      upper = max(new_df[ , i])
      lower = min(new_df[ , i])
      vector = c(0,0,0,0,0,0,0,0,0)
      vector[i] = 1
      add.constraint(lps.model, vector, "<=", upper)
      add.constraint(lps.model, vector, ">=", lower)
    }
  }
  init_solver()
  
  # to see if the user input is valid input that between min and max or not
  observeEvent(input$add_btn,{
    vec = c(1, input$yieldx,input$window_size,input$glass_thickness,
            input$ambient_temp,input$cut_speed,input$edge_rate,
            input$spacer_distance,input$silicon_viscosity)
    notation = input$notation
    value = input$value
    
    largest = 0
    smallest = 0
    for(i in 2:9){
      if(vec[i]!=0){
        upper = max(new_df[ , i])
        lower = min(new_df[ , i])
        if (vec[i] > 0){
          largest = largest  + vec[i]*upper
          smallest = smallest + vec[i]*lower
        }else{
          largest = largest + vec[i]*lower
          smallest = smallest + vec[i]*upper
        }
      }
    }
    
    largest = floor(largest)
    smallest = ceiling(smallest)
    if(notation %in% c(">",">=")){
      if(largest < value){
        output$solve = renderPrint(cat("Constraint Conflict: the rhs value must <",largest))
        return
      }
    }else if(notation %in% c("<", "<=")){
      if(smallest > value){
        output$solve = renderPrint(cat("Constraint Conflict: the rhs value must >", smallest))
        return
      }
    }else{
      if(value > largest || value < smallest){
        output$solve = renderPrint(cat("Constraint Conflict: the rhs value must between [",smallest,", ",largest,"]"))
        return
      }
    }
    # if the user input constraint is valid, then add constraint
    add.constraint(lps.model, c(1, input$yieldx,input$window_size,input$glass_thickness,
                               input$ambient_temp,input$cut_speed,input$edge_rate,
                               input$spacer_distance,input$silicon_viscosity) , input$notation, input$value)
    output$solve = renderPrint("Constraint added!")
    })
  
  
  observeEvent(input$solve_btn,{
    solve(lps.model)
    const = get.constraints(lps.model)
    res = get.objective(lps.model)
    vars = get.variables(lps.model)
    var_name <- c("Intercept","YieldX","WindowSize","GlassThickness","AmbientTemp",
                  "CutSpeed","EdgeDeletionRate","SpacerDistance","SiliconViscosity")

    plot_data <- data.frame(decision_variable=var_name, values=vars)
    print(plot_data)
    output$solve <- renderPrint(const)
    output$mini <- renderPrint(res)
    output$solution <- renderPrint(plot_data)
    output$dist <- renderPlot({
    ggplot(data=plot_data, aes(x=decision_variable, y=values)) +
    geom_bar(stat="identity",color="darkorange1",fill="darkorange1")})
    init_solver()
  })
    
  

  
}


shinyApp(ui = ui, server = server)

