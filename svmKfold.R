# load the library
library(caTools)
library(ggplot2) # visualization
library(e1071)
library(caret)

# ensure the results are repeatable, but also change to see if results are similar
set.seed(123)

# load the data
# Concatenate data file subdir and name.
full_file_name <- paste("./input/diagnosis_data.csv",sep="")
show(full_file_name)
diagnosis_data_full <- read.csv(full_file_name)

# Remove the columns that do not contain features which can be analyzed: ID; Diagnosis(DV); X(all NA)
diagnosis_data_features <- diagnosis_data_full[,2:32]
diagnosis_data_features = scale(diagnosis_data_features)

# check data
str(diagnosis_data_features)
summary(diagnosis_data_features)

# Split into training set and test set
split = sample.split(diagnosis_data_features$diagnosis, SplitRatio = 0.75)
training_set = subset(diagnosis_data_features, split == TRUE)
test_set = subset(diagnosis_data_features, split == FALSE)

classifier = svm(formula = diagnosis ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-1])

# Making the Confusion Matrix
cm = table(test_set[, 1], y_pred)
cm
# Applying k-Fold Cross Validation
folds = createFolds(training_set$diagnosis, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = svm(formula = diagnosis ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred = predict(classifier, newdata = test_fold[-1])
  cm = table(test_fold[, 1], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))
accuracy
cv

