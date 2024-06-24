#regression 
children_difficult_wave1 <- schooltransitiondata %>% select(1, 98:122)
children_difficult_wave1_filtered <- children_difficult_wave1 %>% filter(complete.cases(.))
children_difficult_wave1_filtered_sum <- children_difficult_wave1_filtered %>% 
  rowwise() %>%
  mutate(total_score_wave1 = sum(c_across(2:26), na.rm = TRUE)) %>%
  ungroup()

children_difficult_wave2 <- schooltransitiondata %>% select(1, 575:599)
children_difficult_wave2_filtered <- children_difficult_wave2 %>% filter(complete.cases(.))
children_difficult_wave2_filtered_sum <- children_difficult_wave2_filtered %>% 
  rowwise() %>%
  mutate(total_score_wave2 = sum(c_across(2:26), na.rm = TRUE)) %>%
  ungroup()

children_difficult_merge <- merge(children_difficult_wave1_filtered_sum, children_difficult_wave2_filtered_sum, by = "ID", all = FALSE)

#check the difficult score distribution a bit
ggplot(children_difficult_merge, aes(x = total_score_wave1)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(children_difficult_merge$total_score_wave1, na.rm = TRUE), 
                                        sd = sd(children_difficult_merge$total_score_wave1, na.rm = TRUE)),
                color = "red", size = 1) +
  labs(title = "Distribution of Wave 1 Difficult Score", x = "Total Score", y = "Density") +
  theme_minimal()

ggplot(children_difficult_merge, aes(x = total_score_wave2)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(children_difficult_merge$total_score_wave2, na.rm = TRUE), 
                                        sd = sd(children_difficult_merge$total_score_wave2, na.rm = TRUE)),
                color = "red", size = 1) +
  labs(title = "Distribution of Wave 2 Difficult Score", x = "Total Score", y = "Density") +
  theme_minimal()

#back to work
discrepancy_score <- rbind(MaternalDataset1, PaternalDataset1)
regression_dataframe <- merge(children_difficult_merge, discrepancy_score, by = "ID", all = FALSE)

install.packages("lavaan")
library(lavaan)

model_warmth <- '
total_score_wave2 ~ warmth_Discrepancies + total_score_wave1
'
fit_warmth <- sem(model_warmth, data = regression_dataframe)
summary(fit_warmth)

model_hostility <- '
total_score_wave2 ~ hostility_Discrepancies + total_score_wave1
'
fit_hostility <- sem(model_hostility, data = regression_dataframe)
summary(fit_hostility)

#testing wave 2 discrepancy as predictor
discrepancy_score_test <- rbind(MaternalDataset2, PaternalDataset2)
regression_dataframe_test <- merge(children_difficult_merge, discrepancy_score_test, by = "ID", all = FALSE)

fit_warmth_test <- sem(model_warmth, data = regression_dataframe_test)
summary(fit_warmth_test)

fit_hostility_test <- sem(model_hostility, data = regression_dataframe_test)
summary(fit_hostility_test)
#when using wave 2 discrepancy score to predict, we find the p value = .342 in the regression model of warmth discrepency, and we find the p value = .396 in the regression model of hostility discrepency

#Testing what happens if we address the covariance between discrepancy scores and wave 1 total scores
model_warmth_test1 <- '
total_score_wave2 ~ 1 + warmth_Discrepancies + total_score_wave1
warmth_Discrepancies ~ total_score_wave1
'
fit_warmth_test1 <- sem(model_warmth_test1, data = regression_dataframe)
summary(fit_warmth_test1)

model_hostility_test1 <- '
total_score_wave2 ~ 1 + hostility_Discrepancies + total_score_wave1
hostility_Discrepancies ~ total_score_wave1
'
fit_hostility_test1 <- sem(model_hostility_test1, data = regression_dataframe)
summary(fit_hostility_test1)
#p value stays the same
