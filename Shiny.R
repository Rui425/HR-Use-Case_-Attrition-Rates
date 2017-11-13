## Shiny 
library(dplyr)
library(corrplot)
library(ggplot2)
library(caret)
library(party)
library(fastAdaboost)
library(shinydashboard)
library(leaflet)
library(shiny)
library(rpart.plot)
library(e1071)

EmployeePerformance = read.csv("~/EmployeePerformance.csv")
data1 = EmployeePerformance
head(data1)
######  Prediction
## Attrition
model_attri = train(
  Attrition ~ Age + MonthlyIncome + TotalWorkingYears + YearsInCurrentRole +
    WorkLifeBalanceScore + Department + JobLevel + MaritalStatus + OverTime + 
    StockOptionLevel, data1,
  method = 'cforest',
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)
model_attri$results
pred_attri = predict(model_attri,data1)
sum(data1$Attrition == pred_attri)/length(pred_attri)
table(pred_attri,data1$Attrition)

## Performance
modelPerfor = train(
  PerformanceRating ~ Age + Department + JobLevel + LeadershipScore + CultureScore + CompensationScore, 
  data1,
  method = 'rpart',
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)
modelPerfor$results
predPerfor = predict(modelPerfor,data1)
sum(predPerfor == data1$PerformanceRating)/dim(data1)[1]
prp(modelPerfor$finalModel,box.palette = "Reds", tweak = .8)



####### Shiny ######## 
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)



body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Candidate Evaluation"),
            conditionalPanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 100, left = "150", right = "auto", bottom = "auto",
                             width = 600, height = "auto",
                             
                             h2("Control Panel"),
                             fluidRow(
                               h3("   Candidate's Personal Information"),
                               column(6,numericInput("age", "Age:", 30,
                                                     min = min(range(data1$Age)), max = max(range(data1$Age)))),
                               column(6,numericInput("monthlyinc", "Monthly income", 7000,
                                                     min = min(range(data1$MonthlyIncome)), max = max(range(data1$MonthlyIncome)))),
                               column(6,numericInput("mworkingYear", "Total Working Years:", 10,
                                                     min = min(range(data1$TotalWorkingYears)), max = max(range(data1$TotalWorkingYears)))),
                               column(6,numericInput("CurrentYear", "Years in Current Role:", 10,
                                                     min = min(range(data1$YearsInCurrentRole)), max = max(range(data1$YearsInCurrentRole)))),
                               
                               column(6, selectInput("joblevel", "Applied Job Level:", selected = unique(data1$JobLevel)[1],
                                                     c("Associate" = "Associate",
                                                       "Junior Associate Manager" = "Junior Associate Manager",
                                                       "Executive" = "Executive",
                                                       "VP to C-Level" = "VP to C-Level  "))),
                               column(6, selectInput("department", "Department:", selected = unique(data1$Department)[1],
                                                     c("Sales" = "Sales",
                                                       "Research & Development" = "Research & Development",
                                                       "Human Resources" = "Human Resources"))),
                               column(6, selectInput("marriage", "Merital Status:", selected = unique(data1$MaritalStatus)[1],
                                                     c("Single" = "Single",
                                                       "Married" = "Married",
                                                       "Divorced" = "Divorced")))
                             ),
                             fluidRow(
                               h3("   Candidate's Evaluated Scores"),
                               column(6, sliderInput("worklifescore","Work & Life Balance:",
                                                     min = 0, max = 1, value = .5)),
                               column(6, selectInput("overtime", "Over Time:", selected = unique(data1$OverTime)[1],
                                                     c("Yes" = "Yes",
                                                       "No" = "No"))),
                               column(6, selectInput("stockoption", "Stock Option Level:", selected = unique(data1$StockOptionLevel)[1],
                                                     c("No Option Plan" = "No Option Plan",
                                                       "Full Option Plan" = "Full Option Plan",
                                                       "Small Option Plan" = "Small Option Plan",
                                                       "Large Option Plan" = "Large Option Plan")))
                               
                             ),
                             fluidRow(
                               box(
                                 title = "Results", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, verbatimTextOutput("summary")
                                 # plotOutput("Result")
                               )
                               
                             )
            )
    ),
    
    tabItem(tabName = "Attrition Rate",
            h2("Leave or not?")
    )
  )
)


# Put them together into a dashboardPage
ui = dashboardPage(
  dashboardHeader(title = "Human Resource System"),
  sidebar,
  body
)



#newdat = data.frame(matrix( , nrow=1, ncol=6))
#names(newdat) = c("Age","Department","JobLevel","LeadershipScore", "CultureScore","CompensationScore")
server = function(input, output) {
  sliderValues <- reactive({
    data.frame(Age = input$age,
               MonthlyIncome = input$monthlyinc,
               TotalWorkingYears = input$mworkingYear,
               YearsInCurrentRole = input$CurrentYear,
               WorkLifeBalanceScore = input$worklifescore,
               Department = input$department,
               JobLevel = input$joblevel,
               MaritalStatus = input$marriage,
               OverTime = input$overtime,
               StockOptionLevel = input$stockoption,
               stringsAsFactors = FALSE)
    
  })
  output$summary <- renderPrint({
    dataset <- sliderValues()
    result = predict(model_attri,dataset)
    print(result)
  })
  # Show the values in an HTML table ----
  # output$Result = renderDataTable({sliderValues()})
}

shinyApp(ui = ui, server = server)
