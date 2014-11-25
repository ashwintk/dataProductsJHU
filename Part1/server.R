#This function outputs the BMI category for a person
assesBMICategory <- function(bmi){
  if(bmi<18.5) #underweight
    "<span style='color: orange'>Underweight</span>"
  else if(bmi>18.5 && bmi <24.9) #Normal weight
    "<span style='color: green'>Normal (healthy weight)</span>"
  else if(bmi>25.0 && bmi <29.9) #Overweight
    "<span style='color: orange'>Overweight</span>"
  else #Obese
    "<span style='color: red'>Obese</span>"
}
#this function computes BMI for a given height and weight. 
#Type is used to denote whether the system used for measurement is standard or metric
computeBMI <- function(ht,wt,type){
  if(type==1) #Standard BMI = ( Weight in Pounds / ( Height in inches x Height in inches ) ) x 703
    (wt/ht^2) * 703
  else # Metric BMI = ( Weight in Kilograms / ( Height in Meters x Height in Meters ) )
    wt/ht^2
}
#This function generates the BMI table which is displayed on the UI
generateBMITable <- function(){
  data.frame(bmi = c("Below 18.5","18.5–24.9","25.0–29.9","30.0 and Above"), category=c("Underweight","Normal","Overweight","Obesity"))
}
#This function creates a contour plot based on the system used for measurement and the user's BMI
generateBMIPlotData <- function(type,bmi){
  bmi_levels <- matrix()
  if(type==1){#standard System
    ht <- seq(48,84, length.out=1000) #Generate 1000 values for height 
    wt <- seq(90,300, length.out=1000) #Generate 1000 values for weight 
    ht_wt <- expand.grid(x=ht, y=wt)
    bmi_levels <- matrix(computeBMI(ht_wt$x,ht_wt$y,1),length(ht),length(wt)) #Create a matrix containing BMI 
    contour(ht,wt,bmi_levels,levels = c(18.5,25,30), drawlabels=FALSE,
            xlab="Height",ylab="Weight",
            main="BMI categories by height and weight") #Create a contour plot from the data generated above
    if(bmi<18.5){#add labels appropriately
      text(55,200,"Obese",cex=2,srt=45)
      text(65,165,"Overweight",cex=2,srt=40)
      text(70,150,"Normal",cex=2,srt=35)
      text(75,120,"Underweight - (You)",cex=2,srt=18)
    }else if(bmi>18.5 && bmi <24.9){
      text(55,200,"Obese",cex=2,srt=45)
      text(65,165,"Overweight",cex=2,srt=40)
      text(70,150,"Normal - (You)",cex=2,srt=35)
      text(75,120,"Underweight",cex=2,srt=18)
    }else if(bmi>25.0 && bmi <29.9){
      text(55,200,"Obese",cex=2,srt=45)
      text(65,165,"Overweight - (You)",cex=2,srt=40)
      text(70,150,"Normal",cex=2,srt=35)
      text(75,120,"Underweight",cex=2,srt=18)
    }else{
      text(55,200,"Obese - (You)",cex=2,srt=45)
      text(65,165,"Overweight",cex=2,srt=40)
      text(70,150,"Normal",cex=2,srt=35)
      text(75,120,"Underweight",cex=2,srt=18)
    }
  }else{#Metric System
    ht <- seq(1,5, length.out=1000)#Generate 1000 values for height
    wt <- seq(60,400, length.out=1000)#Generate 1000 values for weight
    ht_wt <- expand.grid(x=ht, y=wt)
    bmi_levels <- matrix(computeBMI(ht_wt$x,ht_wt$y,2),length(ht),length(wt))#Create a matrix containing BMI 
    contour(ht,wt,bmi_levels,levels = c(18.5,25,30), drawlabels=FALSE,
            xlab="Height (meters)",ylab="Weight (kilograms)",
            main="BMI categories by height and weight")
    if(bmi<18.5){#add labels appropriately
      text(1.5,200,"Obese",cex=2,srt=45)
      text(2.7,200,"Overweight",cex=2,srt=60)
      text(2.6,150,"Normal",cex=2,srt=45)
      text(4,120,"Underweight- (You)",cex=2,srt=18)
    }else if(bmi>18.5 && bmi <24.9){
      text(1.5,200,"Obese",cex=2,srt=45)
      text(2.7,200,"Overweight",cex=2,srt=60)
      text(2.6,150,"Normal- (You)",cex=2,srt=45)
      text(4,120,"Underweight",cex=2,srt=18)
    }else if(bmi>25.0 && bmi <29.9){
      text(1.5,200,"Obese",cex=2,srt=45)
      text(2.7,200,"Overweight- (You)",cex=2,srt=60)
      text(2.6,150,"Normal",cex=2,srt=45)
      text(4,120,"Underweight",cex=2,srt=18)
    }else{
      text(1.5,200,"Obese - (You)",cex=2,srt=45)
      text(2.7,200,"Overweight",cex=2,srt=60)
      text(2.6,150,"Normal",cex=2,srt=45)
      text(4,120,"Underweight",cex=2,srt=18)
    }
  }
}
#This method computes base metabolic rate based on Mifflin - St Jeor equation
computeBMR <- function(heightInCentiMeter,weightInKilograms,sex,age){
  if(sex=="Male"){
    10*weightInKilograms + 6.25*heightInCentiMeter - 5*age + 5
  }else{
    10*weightInKilograms + 6.25*heightInCentiMeter - 5*age -161
  }
}
#This method returns float values corresponding to the activity indicators
getActivityFactor <- function(act_ind){
  if(act_ind=="1")#Basal Metabolic Rate (BMR)
    1
  else if(act_ind=="2")#Sedentary - Little or no exercise and no desk job
    1.2
  else if(act_ind=="3")#Lightly Active - Light exercise or sports 1-3 days a week
    1.375
  else if(act_ind=="4")#Moderately Active - Moderate exercise or sports 3-5 days a week
    1.55
  else if(act_ind=="5")#Very Active - Hard exercise or sports 6-7 days a week
    1.725
  else if(act_ind=="6")#Extremely Active - Hard daily exercise or sports and physical job
    1.9
}

library(shiny)
library(rCharts)

shinyServer(
  function(input,output){
    output$bmiTbl <- renderTable({generateBMITable()}) #Display BMI Category table
    observe({
      if(input$typeOfOutput=="BMI Calculations"){
        if(input$tab_panel=="Standard"){#if unit of measurement is standard
          hieghtInInches <- (as.numeric(input$feet)*12)+as.numeric(input$inches) #convert height to inches
          bmi <- computeBMI(hieghtInInches,as.numeric(input$pounds),1) #compute BMI
          if(is.nan(bmi)){ #if BMI is NaN
            output$outputBMI <- renderPrint({"Check the values you have entered and try again"})
          }else{ #Display User entered values, BMI, their BMI category and plot      
            output$inputHeight <- renderPrint({paste("Height: ",input$feet,"feet,",input$inches,"inches",sep=" ")})
            output$inputWeight <- renderPrint({paste("Weight: ",input$pounds,"pounds",sep=" ")})
            output$outputBMI <- renderPrint({round(bmi,1)})
            output$bmiCategory <- renderPrint({assesBMICategory(bmi)})
            output$bmiPlot <- renderPlot({generateBMIPlotData(1,bmi)})
          }
        }else{#if unit of measurement is metric
          hieghtInMeters <- as.numeric(input$height)/100 #convert hieght to meters
          bmi <- computeBMI(hieghtInMeters,as.numeric(input$weight),2)#compute BMI
          if(is.nan(bmi)){#if BMI is NaN
            output$outputBMI <- renderPrint({"Check the values you have entered and try again"})
          }else{#Display User entered values, BMI, their BMI category and plot
            output$inputHeight <- renderPrint({paste("Height: ",input$height,"centimeters",sep=" ")})
            output$inputWeight <- renderPrint({paste("Weight: ",input$weight,"kilograms",sep=" ")})
            output$outputBMI <- renderPrint({round(bmi,1)})
            output$bmiCategory <- renderPrint({assesBMICategory(bmi)})
            output$bmiPlot <- renderPlot({generateBMIPlotData(2,bmi)})
          }
        }
      }else if(input$typeOfOutput=="Daily Calorie Intake Estimator"){
        heightInCentiMeters <- 0
        weightInKgs <- 0
        if(input$tab_panel=="Standard"){
          heightInCentiMeters <- ((as.numeric(input$feet)*12)+as.numeric(input$inches)) *2.54
          weightInKgs <- as.numeric(input$pounds)*0.453592
        }else{
          heightInCentiMeters <- as.numeric(input$height)
          weightInKgs <- as.numeric(input$weight)
        }
        BMR <- computeBMR(heightInCentiMeters,weightInKgs,input$sex,input$age)
        activityFactor <- getActivityFactor(input$activity_indicator)
        output$maintainWeight <- renderPrint({paste("You need ",round(BMR*activityFactor,0)," calories/day to maintain your weight",sep="")})
        output$looseOneLBPerWeek <- renderPrint({paste("You need ",round(BMR*activityFactor,0) - 500," calories/day to loose 1 lb/week",sep="")})
        output$looseTwoLBPerWeek <- renderPrint({paste("You need ",round(BMR*activityFactor,0) - 1000," calories/day to loose 2 lb/week",sep="")})
        output$gainOneLBPerWeek <- renderPrint({paste("You need ",round(BMR*activityFactor,0) + 500," calories/day to gain 1 lb/week",sep="")})
        output$gainTwoLBPerWeek <- renderPrint({paste("You need ",round(BMR*activityFactor,0) + 1000," calories/day to gain 2 lb/week",sep="")})
      }   
    }
    )
  }
)