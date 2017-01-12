library(aod)
#setwd("C:/Users/Faariya/Documents/GA STATE/masters program/CLASSES/sprint")
d1 <- read.table("data_final.csv", header = TRUE, sep = ",")
d2 <- d1[,c(-1,-2,-3)]

threshold = mean(d2[,7])
rows = which(d2[,7] > threshold)
output_class = rep('1',length(d2[,7]))
output_class[rows] = '0'
output_class=as.factor(output_class)
#print(output_class[1:10])

d3 <- data.frame(output_class, d2[,-7])
mylogit <- glm(output_class ~., data = d3, family = "binomial")
summary(mylogit)
set.seed(50)
training_rows <- sample(1:nrow(d3),0.60*nrow(d3))
training_data <- d3[training_rows,]
testing_data <- d3[-training_rows,]
mylogit2 <- glm(output_class ~., data = training_data, family = "binomial")
summary(mylogit2)
#plot(mylogit2)
predicted_values = round(predict(mylogit2, testing_data[,-1],type="response"), digits=0)
predicted_values[1:10]
mean_error = mean(predicted_values != testing_data[,1]);
mean_percent = mean_error*100
mean_percent
