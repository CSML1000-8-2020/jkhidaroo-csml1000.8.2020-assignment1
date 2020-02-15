library(ranger)
library(caret)
library(data.table)
library(dplyr)

breastcancer_data <- read.csv("data.csv")
dim(breastcancer_data)
head(breastcancer_data,6)
summary(breastcancer_data)
names(breastcancer_data)

# summarize the class distribution
percentage <- prop.table(table(breastcancer_data$diagnosis)) * 100
cbind(freq=table(breastcancer_data$diagnosis), percentage=percentage)
#remove id and x
target <- ifelse(breastcancer_data$diagnosis=="B", 1, 0)
target
model_1 = select (breastcancer_data,-c(X,id,diagnosis))
nobs <- nrow(model_1)
nobs
model_1
model_2=scale(model_1)
model_3<-data.frame(cbind(model_2,target))
summary(model_3)

#model start
library(caTools)
set.seed(123)
split = sample.split(model_3$target, SplitRatio = 0.75)
train_data = subset(model_3, split == TRUE)
test_data = subset(model_3, split == FALSE)

dim(train_data)

Logistic_Model <- glm(target ~ perimeter_mean + 
                        + smoothness_mean + compactness_mean + concavity_mean + concave.points_mean +
                        symmetry_mean ,data=train_data, family = binomial)
summary(Logistic_Model)
predictTrain = predict(Logistic_Model, type="response")
summary(predictTrain)
tapply(predictTrain, train_data$target, mean)
table(train_data$target, predictTrain > 0.5)
install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(predictTrain, train_data$target)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
predictTest = predict(Logistic_Model, type = "response", newdata = test_data)
summary(predictTest)


