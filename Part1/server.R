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
library(shiny)
shinyServer(
  function(input,output){
    output$bmiTbl <- renderTable({generateBMITable()}) #Display BMI Category table
    observe({
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
    })
    
  }
  )