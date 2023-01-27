# Designed and developed a scalable, automated interactive web-app (R Shiny ) for chocolate retailers, gauging popularity of chocolate bars.
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
library(maps)


###################################### Initialization #############################################
# setwd("//Users/yuxuanli/Desktop/备份/R For Analytics/FinalProject")
#read file
df <- read_xlsx("chocolate_590_1.xlsx")


#coerce the column
colnames_df <- as.vector(colnames(df))
categorical_vars <- c("company_location", "country_of_bean_origin",
                      # "review_date",
                      "sugar","cocoa_butter","lecithin",
                      "salt","sweetener_without_sugar","vanilla")
numeric_vars <- colnames_df[! colnames_df%in%categorical_vars]
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)
df[numeric_vars] <- lapply(df[numeric_vars], as.numeric)

numeric_df <- df[,numeric_vars]
categorical_df <- df[,categorical_vars]

############################ map data preprocess#####################################################3
df_map <- df

df_map$company_location <- as.character(df_map$company_location)
df_map$company_location <- ifelse(df_map$company_location == "U.S.A", "USA",
                                  ifelse(df_map$company_location == "U.k.", "UK",
                                         ifelse(df_map$company_location == "Wales", "UK",
                                                ifelse(df_map$company_location == "South korea", "South Korea",
                                                       ifelse(df_map$company_location == "Puerto rico", "Puerto Rico",
                                                              ifelse(df_map$company_location == "New zealand", "New Zealand",
                                                                     ifelse(df_map$company_location =="Costa rica", "Costa Rica",
                                                                            ifelse(df_map$company_location == 'Scotland', "UK",
                                                                                   ifelse(df_map$company_location == "Sao tome", "Sao Tome and Principe",
                                                                                          ifelse(df_map$company_location == "St. lucia", "Saint Lucia",
                                                                                                 ifelse(df_map$company_location == "South africa", "South Africa",
                                                                                                        ifelse(df_map$company_location == "Dominican republic", "Dominican Republic",
                                                                                                               ifelse(df_map$company_location == "Sao tome & principe", "Sao Tome and Principe",
                                                                                                                      ifelse(df_map$company_location =="St.vincent-grenadines", "Sao Tome and Principe",
                                                                                                                             ifelse(df_map$company_location == "Czech republic", "Czech Republic",
                                                                                                                                    ifelse(df_map$company_location=="U.a.e.", "United Arab Emirates",
                                                                                                                                           ifelse(df_map$company_location == "El salvador", "El Salvador", df_map$company_location)))))))))))))))))




###################################### data for model fit: preprocess - dummy coding for optimization #############################################
df1 <- df
names(df1)[1] <- "y"
dummies <- dummyVars( y~ ., data = df1)            # create dummies for Xs
ex <- data.frame(predict(dummies, newdata = df1))  # actually creates the dummies
df_dummies <- cbind(df1$y, ex)                              # combine target var with Xs
names(df_dummies)[1] <- "y"                               # name target var 'y'
################################# data for model fit: preprocess - identify and remove high cor ##########################################
## find high correlations and remove them
descrCor <-  cor(df_dummies[,2:ncol(df_dummies)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85) # num Xs with cor > t
summary(descrCor[upper.tri(descrCor)])                    # summarize the cors
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
filteredDescr <- df_dummies[,2:ncol(df_dummies)][,-highlyCorDescr] 
descrCor2 <- cor(filteredDescr)  
summary(descrCor2[upper.tri(descrCor2)])
#new data
new_df <- cbind(df_dummies$y, filteredDescr)
names(new_df)[1] <- "y"
################################# data for model fit: preprocess - identify and remove linear combo ##########################################
y <- new_df$y
temp_d <- cbind(rep(1, nrow(new_df)), new_df[2:ncol(new_df)])
names(temp_d)[1] <- "ones"
comboInfo <- findLinearCombos(temp_d)
temp_d <- temp_d[, -comboInfo$remove]
temp_d <- temp_d[, c(2:ncol(temp_d))]
# new data
new_df2 <- cbind(y, temp_d)
print(names(new_df2))

################################# data for optimization ##########################################
unique_locations<-as.vector(names(new_df2[,4:65]))
unique_origins<-as.vector(names(new_df2[,66:125]))

colnames_newdf2<-colnames(new_df2[,2:ncol(new_df2)])
var_name <- c("Intercept",colnames_newdf2)


##################################################### shiny! ##################################################################
ui <- fluidPage (theme = shinytheme("united"),
                 navbarPage(title = "Chocolate Bar Rating Dashboard",
                            
                            #################################### Intro Tab#################################################################################
                            ############################### Business background ###################################################
                            tabPanel("Harry's Fine Candies", mainPanel(
                              column(12, 
                                     img(src = "gif10.gif", height = 150, width = 150),
                                     
                                     img(src = "gif11.gif", height = 150, width = 150),
                                     img(src = "gif4.gif", height = 150, width = 150),
                                     img(src = "gif5.gif", height = 150, width = 150),
                                     img(src = "gif6.gif", height = 150, width = 150),
                                     img(src = "gif7.gif", height = 150, width = 150),
                                     
                                     br(),
                                     img(src = "logo.png", height = 300, width = 600),
                                     align="center"),
                              
                              h1("Committed to Excellence",style = "font-family: 'times'; font-si16pt",align = "center"),
                              br(),
                              h2("Harry's Fine Candies was founded in 1932 by Harry Smith in West Lafayette, Indiana. 
                              Harry prided himself on offering a high-end menu that included just three types of candy; lollipops, gummies, and licorice.
                                  Today, the menu remains the same and this commitment to quality and consistency has established Harry's as 
                                  the premiere spot for high-end candies and allowed them to grow to 25 locations across the United States!
                                ", 
                                 style = "font-family: 'times'; font-si12pt"),
                              
                              width=12, 
                              br(),
                              img(src = "gif2.gif", height = 150, width = 200),
                              img(src = "gif3.gif", height = 150, width = 200),
                              img(src = "gif8.gif", height = 150, width = 200),
                              img(src = "gif1.gif", height = 150, width = 200),
                              img(src = "gif12.gif", height = 150, width = 200),
                              align="center"
                            )),
                            ############################### Project Overview ###################################
                            tabPanel("Project Overview", mainPanel(
                              h1("Problem Statement",style = "font-family: 'times'; font-si16pt"),
                              h2("Harry's Fine Candies has recently received customer requests for high-end chocolate. 
                              To meet customer demand, Harry's has decided to offer high-end chocolate options at their retail locations. 
                              Before they engage in this practice, Harry's wants to better understand the chocolate industry to ensure that the
                              chocolate they're offering is truly the best in the world. As a result, Harry's has asked for our help in indentifying
                                 the world's finest chocolate and the characteristics that make it so outstanding.", 
                                 style = "font-family: 'times'; font-si16pt"),
                              br(),
                              h1("Business Inquiries",style = "font-family: 'times'; font-si16pt"),
                              h2("Harry's leadership tasked us to answer 3 key questions regarding the chocolate industry:",style = "font-family: 'times'; font-si16pt"),
                              h3("1. What part of the world sells the finest chocolate?
                               ", style = "font-family: 'times'; font-si16pt"),
                              h3("2. What is the geographical origin of the cocoa beans used in world's finest chocolate?",
                                 style = "font-family: 'times'; font-si16pt"),
                              h3("3. What is the ingredient makeup of the the world's finest chocolate?", 
                                 style = "font-family: 'times'; font-si16pt"),
                              width=12
                            )),
                            ############################### Team Member ################################### 
                            
                            tabPanel("Team Members", mainPanel(
                              br(),
                              h1("Team Members",style = "font-family: 'times'; font-si16pt"),
                              br(),
                              column(4,img(src = "Drew.jpg", height = 400, width = 400),
                                     h1("Drew Bertram", style = "font-family: 'times'; font-si16pt",align = "center")),
                              column(4,img(src = "Yuxuan.jpeg", height = 400, width = 400),
                                     h1("Yuxuan Li", style = "font-family: 'times'; font-si16pt",align = "center")),
                              column(4,img(src = "Kriti.jpg", height = 400, width = 400),
                                     h1("Kriti Sayal", style = "font-family: 'times'; font-si16pt",align = "center")),
                              width=12
                            )),
                            ############################### Data Introduction ################################### 
                            tabPanel("Data Introduction", mainPanel(
                              column(12, 
                                     img(src = "dataintro.png", height = 600, width = 1000),
                                     align="center"))),
                            
                            
                            ###################################### Map Tab ############################################################
                            tabPanel("World Map",
                                     fluidRow(
                                       sidebarPanel( 
                                         helpText("Create demographic map based on chocolate company location/cocoa bean origin "),
                                         selectInput('x1', label = "Show rating by:", c("company_location",
                                                                                        "country_of_bean_origin"
                                         ),
                                         selected ="company_location"))),
                                     
                                     fluidRow(ailgn="center",
                                              mainPanel(
                                                plotOutput("World_map", width = 1200, height = 550),
                                              ))),
                            ######################################### EDA Tab ########################################
                            tabPanel("Plots",
                                     
                                     sidebarPanel( width = 3,
                                                   selectInput('hist_x',"Choose a variable for histogram:", names(c(numeric_df,categorical_df)), 
                                                               selected = names(numeric_df)[[2]]),
                                                   sliderInput("binCount", "Bins for histogram",
                                                               min = 1, max = 10, value = 5),
                                                   selectInput('density_x',"Choose a variable for density plot:", names(numeric_df), 
                                                               selected = names(numeric_df)[[2]]),
                                                   selectInput('box_x', label = "X axis of Box Plot:", names(df), 
                                                               selected = names(categorical_df)[[4]]),
                                                   selectInput('box_y', label = "Y axis of Box Plot:", names(numeric_df), 
                                                               selected = names(numeric_df)[[1]]),
                                                   selectInput('scatter_x', label = "X axis of Scatter Plot:", names(numeric_df), 
                                                               selected = names(numeric_df)[[2]]),
                                                   selectInput('scatter_y', label = "Y axis of Scatter Plot:", names(numeric_df),
                                                               selected = names(numeric_df)[[1]]),
                                                   actionButton(inputId = "click", label = "Click Me To Plot!")),
                                     
                                     mainPanel (h2("Exploratory Data Analysis"),
                                                fluidRow(
                                                  splitLayout(cellWidths = c("50%", "50%"), 
                                                              column(12, plotOutput("hist")),
                                                              column(12, plotOutput("density"))),
                                                  br(),
                                                  splitLayout(cellWidths = c("50%", "50%"), 
                                                              column(12, plotOutput("box")),
                                                              column(12,plotOutput("scatter"))
                                                  )))),
                            
                            
                            # tabPanel("Data Explore",
                            #          sidebarPanel( numericInput('row_col', label = "Mean of rows(1) columns(2)", 2, 
                            #                                     min = 1, max = 2, step = 1)),
                            #          
                            #          mainPanel(
                            #            h2("Window Type Break Rate Statistics")
                            #            dataTableOutput("stat1")))
                            # ))
                            ######################################### Summary Table Tab ########################################
                            tabPanel("Tables Summary",
                                     sidebarPanel( 
                                       selectInput('groupby', label = "Choose a variable to show its count", names(categorical_df),
                                                   selected =names(categorical_df)[[3]]),
                                       actionButton(inputId = "click2", label = "Show Count Table"),
                                       selectInput('summary_var', label = "Choose variables to show five number summary", names(numeric_df),
                                                   selected =names(numeric_df)[[1]]),
                                       actionButton(inputId = "click3", label = "Show Statistics")),
                                     
                                     mainPanel( 
                                       h2("Count Table"),
                                       dataTableOutput("count"),
                                       h2("Five Number Summary"),
                                       dataTableOutput("five"))),
                            
                            
                            ################################################# Model fit Tab ####################3#######################################
                            # tabPanel("Linear Regression",
                            #          # sidebarPanel(
                            #          #   selectInput('unique_locations', label = "Choose a Country Location", unique_locations),
                            #          #   selectInput('unique_origins', label = "Choose a Country Location", unique_locations)),
                            #          
                            #          mainPanel(
                            #            h2("Model Fit"),
                            #            verbatimTextOutput("mf"),
                            #            h2("Adjusted R"),
                            #            verbatimTextOutput("adjR"),
                            #            h2("Estimated Coefficients"),
                            #            verbatimTextOutput("ec")
                            #            
                            #          )),
                            #################################################### Optimization Tab ############################################################
                            tabPanel("Model Optimization",
                                     sidebarPanel(
                                       selectInput("location","choose a company location:",c("ANY",unique_locations)),
                                       selectInput("origin","choose a bean origin:",c("ANY",unique_origins)),
                                       numericInput("cocaP","Cocoa Percent :", 0),
                                       numericInput("ingredient","Counts of Ingredients :", 0),          
                                       # numericInput("location","Company Location :", 0),
                                       # numericInput("origin","Country of Bean Origin :", 0),
                                       # numericInput("date","Review Date :", 0),
                                       numericInput("sugar","Sugar :", 0),
                                       numericInput("butter","Cocoa Butter :", 0),
                                       numericInput("lecithin","Lecithin :", 0),
                                       numericInput("salt","Salt :", 0),
                                       numericInput("sweetener","Sweetener Without Sugar :", 0),
                                       numericInput("vanilla","Vanilla :", 0),
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
                                       # h2("Solve Model"),
                                       # verbatimTextOutput("solve"),
                                       h2("Maximized Rating"),
                                       verbatimTextOutput("max"),
                                       h2("Optimal Variable Solution"),
                                       verbatimTextOutput("solution"),
                                       h2("Optimal Parameter Setting"),
                                       plotOutput("dist")
                                     )),
                            
                 ))


server <- function(input, output) {
  
  ########################################## Map #################################################### 
  selectedData<- reactive({
    if (input$x1 == "company_location"){
      world_data <- ggplot2::map_data('world')
      world_data <- fortify(world_data)
      names(world_data) <- c("long","lat","group" ,"order","company_location" ,"subregion")
      world_data <- subset(world_data, company_location %in% unique(df_map$company_location))
      world_data <- world_data[!duplicated(world_data$company_location),]
      merged_df_map <- df_map%>% left_join(world_data, by= "company_location")
      merged_df_map<-merged_df_map %>% 
        group_by(company_location) %>%
        mutate(median = round(median(rating),0))
      merged_df_map}
    else {world_data <- ggplot2::map_data('world')
    world_data <- fortify(world_data)
    names(world_data) <- c("long","lat","group" ,"order","country_of_bean_origin" ,"subregion")
    world_data <- subset(world_data, country_of_bean_origin %in% unique(df_map$country_of_bean_origin))
    world_data <- world_data[!duplicated(world_data$country_of_bean_origin),]
    merged_df_map <- df_map%>% left_join(world_data, by= "country_of_bean_origin")
    merged_df_map<-merged_df_map %>% 
      group_by(country_of_bean_origin) %>%
      mutate(median = round(median(rating),0))
    merged_df_map}}
  )
  
  
  output$World_map <- renderPlot({
    world <- map_data("world")
    ggplot() +
      geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region),
        color = "white", fill = "lightgray", size = 0.1
      ) +
      geom_point(
        data = selectedData(),
        aes(long, lat, color = median),
        alpha = 0.7, size=10
      )}) 
  #################################### ggplot ########################################### 
  #histogram
  observeEvent(input$click,{
    output$hist <- renderPlot({
      ggplot(df, aes_string(x=input$hist_x)) + 
        geom_bar(color="darkorange1",fill="darkorange1",binwidth = input$binCount)+
        theme(axis.text.x = element_text(angle = 90))
      
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
        geom_boxplot(color="darkorange1",fill="darkorange3") +
        theme(axis.text.x = element_text(angle = 90))
      
    })
  })
  
  #bar graph
  observeEvent(input$click,{
    output$bar <- renderPlot({
      ggplot(data=df, aes_string(x = input$bar_x)) +
        geom_bar(color="darkorange1",fill="darkorange1") 
    })
  })
  
  #################################### progress indicator added ########################################### 
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
  
  ################################### table summary#######################################################3
  
  
  #count table
  observeEvent(input$click2,{
    output$count <- renderDataTable({df %>%
        group_by("Group by"=get(input$groupby)) %>%
        summarise("Count"=n())
    })
  })
  
  observeEvent(input$click3,{
    output$five <- renderDataTable({summary(numeric_df[,input$summary_var])
      
    })
  })
  
  
  ########################### fit a linear regression model ######################################
  
  ## find high correlations and remove them
  descrCor <-  cor(df_dummies[,2:ncol(df_dummies)])                           # correlation matrix
  highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85) # num Xs with cor > t
  summary(descrCor[upper.tri(descrCor)])                    # summarize the cors
  highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
  filteredDescr <- df_dummies[,2:ncol(df_dummies)][,-highlyCorDescr] 
  descrCor2 <- cor(filteredDescr)  
  summary(descrCor2[upper.tri(descrCor2)])
  #new data
  new_df <- cbind(df_dummies$y, filteredDescr)
  names(new_df)[1] <- "y"
  
  ###find linear combos and remove them
  y <- new_df$y
  temp_d <- cbind(rep(1, nrow(new_df)), new_df[2:ncol(new_df)])
  names(temp_d)[1] <- "ones"
  comboInfo <- findLinearCombos(temp_d)
  temp_d <- temp_d[, -comboInfo$remove]
  temp_d <- temp_d[, c(2:ncol(temp_d))]
  # new data
  new_df2 <- cbind(y, temp_d)
  
  ## fit the model
  fit <- reactive({
    lm(y ~., data=new_df2)})
  
  output$mf <- renderPrint({
    summary(fit())
  })
  
  output$adjR <- renderPrint({summary(fit())$adj.r.squared})
  
  options(scipen=0)
  output$ec <- renderPrint({summary(fit())$coefficients[,c("Estimate","Pr(>|t|)")]})
  
  ################################Optimization#########################################3               
  
  # #initialize every time user click "Show Solution"
  init_solver = function(){
    lps.model <<- make.lp(nrow=0, ncol=130)
    set.type(lps.model, columns=1:3, type="real")
    set.type(lps.model, columns=4:130, type="binary")
    name.lp(lps.model, name="Chocolate Rating Optimization")
    lp.control(lps.model, sense="max")
    fitted = lm(y ~., data=new_df2)
    coe <<- summary(fitted)$coefficients[ , "Estimate"]
    coe_vec <- as.vector(summary(fitted)$coefficients[ , "Estimate"])
    # add objective function
    print(length(coe_vec))
    set.objfn(lps.model, obj=coe_vec)
    add.constraint(lps.model, c(1,rep(0,129)), "=", 1)
    for(i in 2:3){
      upper = max(new_df2[ , i])
      lower = min(new_df2[ , i])
      vector = rep(0,130)
      vector[i] = 1
      add.constraint(lps.model, vector, "<=", upper)
      add.constraint(lps.model, vector, ">=", lower)
    }
    # country constraint
    add.constraint(lps.model, c(rep(0,3),rep(1,62),rep(0,65)), "=", 1)
    # # date constraint
    # add.constraint(lps.model, c(rep(0,65),rep(1,14),rep(0,65)), "=", 1)
    # origin constraint
    add.constraint(lps.model, c(rep(0,65),rep(1,60),rep(0,5)), "=", 1)}
  
  init_solver()
  
  observeEvent(input$add_btn,{
    input_location = rep(0, 62)
    # input_date = rep(0,14)
    input_origin = rep(0, 60)
    vec = c(0, input$cocaP,input$ingredient,
            rep(0,62),
            rep(0,60),
            input$butter,input$lecithin,input$salt,input$sweetener,input$vanilla)
    notation = input$notation
    value = input$value
    
    largest = 0
    smallest = 0
    for(i in 2:130){
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
        return()
      }
    }else if(notation %in% c("<", "<=")){
      if(smallest > value){
        output$solve = renderPrint(cat("Constraint Conflict: the rhs value must >", smallest))
        return()
      }
    }else{
      if(value > largest || value < smallest){
        output$solve = renderPrint(cat("Constraint Conflict: the rhs value must between [",smallest,", ",largest,"]"))
        return()
      }
    }
    # if the user input constraint is valid, then add constraint
    add.constraint(lps.model, vec, input$notation, input$value)
    if(input$location!="ANY"){
      input_location[match(input$location, unique_locations)] = 1
      # chosen location
      add.constraint(lps.model, c(rep(0,3),input_location,rep(0,65)), "=", 1)
    }
    if(input$origin!="ANY"){
      input_origin[match(input$origin,unique_origins)] = 1
      # chosen origin
      add.constraint(lps.model, c(rep(0,65),input_origin,rep(0,5)), "=", 1)
    }
    output$solve = renderPrint("Constraint added!")
  })
  
  observeEvent(input$solve_btn,{
    solve(lps.model)
    const = get.constraints(lps.model)
    res = get.objective(lps.model)
    vars = get.variables(lps.model)
    colnames_newdf2<-colnames(new_df2[,2:ncol(new_df2)])
    var_name <- c("Intercept",colnames_newdf2)
    
    plot_data <- data.frame(decision_variable=var_name, values=vars)
    print(plot_data)
    # output$solve <- renderPrint(const)
    output$max <- renderPrint(res)
    output$solution <- renderPrint(plot_data)
    output$dist <- renderPlot({
      ggplot(data=plot_data, aes(x=decision_variable, y=values)) +
        geom_bar(stat="identity",color="darkorange1",fill="darkorange1")})
    init_solver()
  })
  
  
  
  
  
}


shinyApp(ui = ui, server = server)

