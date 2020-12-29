library(sas7bdat)
walmartData <- read.sas7bdat(file = "walmart.sas7bdat")
dim(walmartData)
set.seed(25)
sample
temp = sample(c("train","test"),size = nrow(walmartData),replace = T,prob = c(0.8,0.2))
trainingData = walmartData[temp == "train",]
testingData = walmartData[temp == "test",]
dim(trainingData)
dim(testingData)
print(walmartData)
multiModel = lm(Customer_Satisfaction~Product_Quality+E_Commerce+Technical_Support+Complaint_Resolution+Advertising+Product_Line+Salesforce_Image+Competitive_Pricing+Warranty_Claims+Packaging+Order_Billing+Price_Flexibility+Delivery_Speed,trainingData)
summary(multiModel)
prediction = predict.lm(multiModel,testingData[,2:14])
prediction = as.data.frame(prediction)
library(DMwR)
DMwR::regr.eval(testingData[,1],prediction)
stepTrainModel = step(multiModel,direction = "both")
summary(stepTrainModel)
