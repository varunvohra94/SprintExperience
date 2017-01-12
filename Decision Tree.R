data <- read.table("data_final.csv", header = TRUE, sep = ",")
y <- data[,16]
library("arules")
check_category <- discretize(y,method = "cluster",categories = 3)
y_class =  factor(check_category,labels = c("E","G","B"))
y_class[1:10]
y_class2 <- as.numeric(y_class)
hist(y_class2)
max(data[,16])
min(data[,16])
mean(data[,16])
data <- data.frame(data[,-16],y_class)
data <- data[-c(11,15)]
del <- c(11,15)
library("C50")
t1 <- C5.0(data[,-14],data[,14])
summary(t1)
plot(t1)
set.seed(42)
train_dataRows <- sample(1:nrow(data),0.60*nrow(data))
train_data <- data[train_dataRows,]
test_data <- data[-train_dataRows,]
t2 <- C5.0(train_data[,-14],train_data[,14])
summary(t2)
plot(t2)
predicted_values = predict(t2, test_data[,-14], type = "class")
mean_error = mean(predicted_values != test_data[,14]);
mean_percent = mean_error*100
mean_percent
