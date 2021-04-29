# predict(reg_model[[type]],
#         newdata = remove_missing_levels(reg_model[[type]],injuries_list[[scen]][[type]]),type='response')
# Accra
# 1343 rows
# 1113 rows between 15 y 69 
# 1076 rows remove truck and other
#   928 whw
#   148 nov
# 966 rows after add_distance_column function
#   835 whw
#   131 nov
sum(injuries_for_model[[1]][[type]]$count)
summary(injuries_for_model[[1]][[type]]$count)
hist(injuries_for_model[[1]][[type]]$count)
table(injuries_for_model[[1]][[type]]$count)

aux = test
aux_predict = predict(aux, newdata = injuries_for_model[[1]][[type]], 
                      type = "response")

sum(aux_predict) # Same number of trips. 835 in total
summary(aux_predict)
hist(aux_predict)
table(aux_predict)

aux_predict_missinglabels = predict(aux, 
                                    newdata = remove_missing_levels(aux,
injuries_for_model[[1]][[type]]), 
                      type = "response")

sum(aux_predict_missinglabels) # Same number of trips. 835 in total
summary(aux_predict_missinglabels)
hist(aux_predict_missinglabels)
table(aux_predict_missinglabels)

aux2 = trim_glm_object(test)
aux2_predict = predict(aux2, newdata = injuries_for_model[[1]][[type]], 
                      type = "response")
sum(aux2_predict) # Same number of trips. 835 in total
summary(aux2_predict)
hist(aux2_predict)
table(aux2_predict)

# Predict from injuries_list instead of injuries_for model
new_data = injuries_list$Baseline$whw %>% 
  mutate(injury_reporting_rate = 1,
         weight = 10) # set weight to 10 years
aux_pred_list <- predict(aux, newdata = new_data, 
                         type = "response")
sum(aux_pred_list) # Same number of trips. 835 in total


results <- data.frame(true_values = injuries_for_model[[1]][[type]]$count,
           predicted = round(aux_predict,2) ,
           predicted_missing = round(aux_predict_missinglabels,2),
           predicted2 = round(aux2_predict,2),
           predicted_weigth = round(aux_pred_list, 2))
View(results)
