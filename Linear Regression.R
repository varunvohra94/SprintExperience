d<- read.table("data_final.csv", header = TRUE, sep = ",")
d1<-d[4:15]
set.seed(33) 
train_rows = sample(1:nrow(d1), 0.7 *nrow(d1)) 

train_dataSet    = d1[ train_rows,]  
validate_dataSet = d1[-train_rows,] 

d2 <- train_dataSet[,-7]
fitTrain <- lm(train_dataSet[,7] ~ . , data=d2)
summary(fitTrain)

predicted_values = predict(fitTrain, validate_dataSet[,-7])

library(arules)
temp = discretize(predicted_values, method = "cluster", categories= 3)
predicted_values_class =  factor(temp,labels= c("B", "G", "E"))

temp2 = discretize(validate_dataSet[,10], method = "cluster", categories= 3)
validation_values_class =  factor(temp2,labels= c("B", "G", "E"))

mean_error = mean(predicted_values_class != validation_values_class)
mean_error_percentage = 100*mean_error;
cat("Mean Error = ",mean_error_percentage,"%\n")

