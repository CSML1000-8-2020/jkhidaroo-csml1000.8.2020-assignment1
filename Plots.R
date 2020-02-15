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
