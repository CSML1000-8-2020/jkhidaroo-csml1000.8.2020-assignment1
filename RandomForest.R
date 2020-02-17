# load the library
library(randomForest)
library(caTools)
library(ggplot2) # visualization
library(faraway)


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

# training_set[-3] = scale(training_set[-3])
# test_set[-3] = scale(test_set[-3])

# Fitting Random Forest Classification to the Training set

classifier = randomForest(x = training_set[-1],
                          y = training_set$diagnosis,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-1], type = 'class')

# Making the Confusion Matrix
cm = table(test_set[,1], y_pred)
cm

# Get importance
importance    <- importance(classifier)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

rankImportance

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance),  y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') + coord_flip() +  theme_few()

rankTop5 <- by(rankImportance, rankImportance["Importance"], tail, n=5)
rankTop5


