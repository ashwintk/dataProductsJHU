library(shiny)

shinyUI(
  pageWithSidebar(#Page with sidebar
    headerPanel("Body Mass Index and daily calorie intake calculator"), #Heading
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
      ),br(),br(),
      tags$a(href="index.html","User guide can be found here")
    ), mainPanel(#Main Panel, to display results and graph
      tabsetPanel(id = "typeOfOutput", #Two tabs one each for units of measurement
                  tabPanel(#BMI Calculations
                    title = "BMI Calculations",
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
                    plotOutput("bmiPlot",height = 500,width = 500),
                    br(),
                    br(),
                    tags$a(href="http://www.nhlbi.nih.gov/health/educational/lose_wt/BMI/bmicalc.htm","Formula for BMI calculations and BMI categories have been gathered from National Health Institute")
                  ),tabPanel(#Calorie Calculations
                    title="Daily Calorie Intake Estimator",
                    h3("Enter the following Information to calculate your daily calorie intake"),
                    br(),
                    radioButtons(inputId = "sex",label = "Sex: ",c("Male" = "Male","Female" = "Female")), br(),
                    numericInput(inputId = "age",label = "Age",1,min = 1,max = 100,step = 1),br(),
                    radioButtons(inputId = "activity_indicator",label = "Choose relevant activity",c(
                      "Basal Metabolic Rate (BMR)"="1",
                      "Sedentary - Little or no exercise and no desk job"="2",
                      "Lightly Active - Light exercise or sports 1-3 days a week"="3",
                      "Moderately Active - Moderate exercise or sports 3-5 days a week"="4",
                      "Very Active - Hard exercise or sports 6-7 days a week" = "5",
                      "Extremely Active - Hard daily exercise or sports and physical job"="6"
                    )), br(),
                    submitButton("Compute your daily required calorie intakes"),
                    h3("The following are your calorie intake results based on your input"),
                    verbatimTextOutput("maintainWeight"),
                    verbatimTextOutput("looseOneLBPerWeek"),
                    verbatimTextOutput("looseTwoLBPerWeek"),
                    verbatimTextOutput("gainOneLBPerWeek"),
                    verbatimTextOutput("gainTwoLBPerWeek"),br(),
                    tags$p("The following are sources which were referred in computation of BMR and Daily calorie Requirements"),
                    tags$ul(
                      tags$ol(
                        tags$a(href="http://en.wikipedia.org/wiki/Basal_metabolic_rate#BMR_estimation_formulas","1. Basal Metabolic Rate - Mifflin - St Jeor equation")
                      ),
                      tags$ol(
                        tags$a(href="http://www.calculator.net/calorie-calculator.html","2. Calorie Intake Calculator")
                      ),
                      tags$ol(
                        tags$a(href="http://www.caloriesperhour.com/tutorial_BMR.php","3. Calculating BMR and RMR")
                      )
                    )
                  )
      )
      
    )
  )
)