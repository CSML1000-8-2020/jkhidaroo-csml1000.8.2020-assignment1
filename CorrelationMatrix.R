# load the library
library(GGally)
library(ggcorrplot)
library(mlbench)
library(caret)

# ensure the results are repeatable
set.seed(789)

# load the data
# Concatenate data file subdir and name.
full_file_name <- paste("./input/diagnosis_data.csv",sep="")
show(full_file_name)
diagnosis_data_full <- read.csv(full_file_name)

# Remove the columns that do not contain features which can be analyzed: ID; Diagnosis(DV); X(all NA)
diagnosis_data_features <- diagnosis_data_full[,3:32]
#diagnosis_data_features = scale(diagnosis_data_features)
# check data
str(diagnosis_data_features)
summary(diagnosis_data_features)

# Correlation Matrix
# calculate correlation matrix
correlationMatrix <- cor(diagnosis_data_features)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)
# print column names of highly correlated attributes
colnames(diagnosis_data_features[,c(highlyCorrelated)])
# plot the correlation matrix
ggcorrplot(correlationMatrix, 
           hc.order = TRUE, 
           type = "lower",
           outline.col = "white",
           insig = "blank")

