# based on the importance analysis result, we rebuild the ANN with less input variables from 30 to 10
# we found that this will preserve the accuracy as told by the confusionMatrix
# while we reduced the ANN parameters from 161 to 61. 
# model becomes more resilent and able to catch the most essential information 
fit_net_10<-nnet(c2~perimeter_worst+radius_worst+area_worst+texture_worst+texture_se+compactness_worst+smoothness_worst+concavity_mean+concave.points_worst+concave.points_se,data=trainset,size=5, decay=5e-4, maxit=2000)
predictions_10 <- data.frame(cbind(round(predict(fit_net_10, testset),digits=0), testset$c2))
table(predictions_10$X1,predictions_10$X2)
NID_10 <- plotnet(fit_net_10)
NID_10
importantce_analysis_garson_10 <-garson(fit_net_10) + theme(axis.text.x = element_text(angle = 90))
importantce_analysis_garson_10
importantce_analysis_olden_10  <- olden(fit_net_10) + theme(axis.text.x = element_text(angle = 90))
importantce_analysis_olden_10
sensitivity_lekprofile_10 <- lekprofile(fit_net_10) + theme(axis.text.x = element_text(angle = 90))
sensitivity_lekprofile_10
sensitivity_lekprofile_Group_10 <- lekprofile(fit_net_10, group_show = TRUE)
sensitivity_lekprofile_Group_10
sensitivity_lekprofile_vals_10 <- lekprofile(fit_net_10, group_vals=6, group_show = TRUE)
sensitivity_lekprofile_vals_10
