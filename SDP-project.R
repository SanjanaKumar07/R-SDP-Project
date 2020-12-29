setwd("C:\\Rcourse\\")
DataSet=read.csv("Salary_Data.csv")
set.seed(25)
temp=sample(c("train","test"),size = nrow(DataSet),replace = T,prob = c(0.8,0.2))
trainingdata=DataSet[temp=="train",]
testingdata=DataSet[temp=="test",]
Model = lm(Salary~YearsExperience,DataSet)
ui<-fluidPage(
  numericInput(inputId = "exp",label = "enter experience here",value = 0),
  actionButton(inputId = "button",label = "Submit"),textOutput(outputId="salary"))
server<-function(input,output){ observeEvent(input$exp,
                                             output$salary <-renderPrint((predict.lm(Model,data.frame("YearsExperience" =as.numeric(input$exp))))))}
shinyApp(ui=ui,server=server)