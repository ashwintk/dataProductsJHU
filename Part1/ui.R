library(shiny)

shinyUI(
  pageWithSidebar(#Page with sidebar
    headerPanel("Body Mass Index Calculator"), #Heading
    sidebarPanel(
      tabsetPanel(id = "tab_panel", #Two tabs one each for units of measurement
        tabPanel(#Standard
          title = "Standard",
          h4("Enter your height in feet & inches"),
          numericInput('feet','feet:',0,min = 1,max=20,step=1),
          numericInput('inches','inches',0,min = 1,max=11,step=1),
          h4("Enter your weight in pounds"),
          numericInput(inputId = 'pounds',label = "", value = 0),
          submitButton('Compute your BMI')
          ),
        tabPanel(#Metric
          title = "Metric",
          h4("Enter your height centimeters"),
          numericInput('height',label = "",value = 0),
          h4("Enter your weight in kilograms"),
          numericInput('weight',label = "",value = 0),
          submitButton('Compute your BMI')
          )
        ),
      br(),
      h4("BMI Table"),
      tableOutput("bmiTbl"), #Display BMI Range as table
      br(),#Display all the risk factors associated with obesity
      h4("Along with being overweight or obese, the following conditions will put you at greater risk for heart disease and other conditions:"),
      br(),
      tags$ul(
        tags$li("Hypertension"),
        tags$li("High LDL cholesterol"),
        tags$li("Low LDL cholesterol"),
        tags$li("High triglycerides"),
        tags$li("High blood glucose"),
        tags$li("History of heart diseases"),
        tags$li("Physical inactivity"),
        tags$li("Cigarette smoking")
        )
      ), mainPanel(#Main Panel, to display results and graph
        h3("Results of your BMI computation are as follows:"),
        br(),
        h4("You have entered the following values"),
        verbatimTextOutput("inputHeight"),
        verbatimTextOutput("inputWeight"),
        h4("Your BMI is"),
        verbatimTextOutput("outputBMI"),
        h4("Your BMI Category is"),
        uiOutput("bmiCategory"),
        br(),
        plotOutput("bmiPlot",height = 500,width = 500)
        )
    )
  )