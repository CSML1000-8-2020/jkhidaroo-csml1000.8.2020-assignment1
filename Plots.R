# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6262849/
# NeuralNetTools package that can be used for the interpretation of supervised neural network models created in R. 
# Functions in the package can be used to visualize a model using a neural network interpretation diagram, 
# evaluate variable importance by disaggregating the model weights, 
# and perform a sensitivity analysis of the response variables to changes in the input variables. 
# Methods are provided for objects from many of the common neural network packages in R, 
# including caret, neuralnet, nnet, and RSNNS
install.packages("NeuralNetTools", dependencies = TRUE)
library('NeuralNetTools')

NID <- plotnet(fit_net)
NID
importantce_analysis_garson <-garson(fit_net) + theme(axis.text.x = element_text(angle = 90))
importantce_analysis_garson
importantce_analysis_olden  <- olden(fit_net) + theme(axis.text.x = element_text(angle = 90))
importantce_analysis_olden
sensitivity_lekprofile <- lekprofile(fit_net) + theme(axis.text.x = element_text(angle = 90))
sensitivity_lekprofile
sensitivity_lekprofile_Group <- lekprofile(fit_net, group_show = TRUE)
sensitivity_lekprofile_Group
sensitivity_lekprofile_vals <- lekprofile(fit_net, group_vals=6, group_show = TRUE)
sensitivity_lekprofile_vals
