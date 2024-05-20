# install.packages(c("farff", "dplyr"))

library(farff)
library(dplyr)
library(mlr3)
library(mlr3learners)
library(ggplot2)
library(quantreg)
getwd()

freq_data = readARFF("datasets/freMTPL2freq.arff")
sev_data = readARFF("datasets/freMTPL2sev.arff")
joined_data = right_join(freq_data, sev_data, by = join_by(IDpol == IDpol))
joined_data = joined_data[complete.cases(joined_data), ]
# remove NAs resulting from joining incomplete data
joined_data = joined_data[complete.cases(joined_data), ]

total_claim_amount = joined_data %>%
  group_by(IDpol) %>%
  summarise(TotalClaimAmount = sum(ClaimAmount)) %>%
  select(IDpol, TotalClaimAmount)

insurance_data = joined_data %>%
  group_by(IDpol) %>%
  filter(row_number() == 1) %>%
  select(-ClaimAmount) %>%
  full_join(total_claim_amount, by = join_by(IDpol)) %>%
  mutate(ClaimPerYear = TotalClaimAmount / Exposure)

# Exploratory Analysis

# Boxplots der Versicherungsansprüche und der Ansprüche je Jahr zeigen wenige, jedoch extreme Ausreißer
ggplot(insurance_data, aes(x = TotalClaimAmount)) +
  geom_boxplot() +
  theme_bw()

ggplot(insurance_data, aes(x = ClaimPerYear)) +
  geom_boxplot() +
  theme_bw()

# Interquartilsabstand (25% bis 75% Quantil) beträgt nur 2224 EUR
# Der Großteil der Kunden nimmt somit Ansprüche von bis zu 2224 EUR wahr.
IQR(insurance_data$ClaimPerYear)
# Der höchste Versicherungsanspruch eines Kunden pro Jahr beträgt jedoch 18.524.548 EUR
range(insurance_data$ClaimPerYear)
quantiles = quantile(round(insurance_data$ClaimPerYear), probs = seq(0, 1, 0.05))

# Insgesamt handelt es sich um 95 Ausreißer, die mehr als 1.5 Standardabweichungen
# vom Mittelwert der Versicherungsansprüche je Kunde je Jahr entfernt sind.

# Eine Grenze von knapp 1.000.000 dient zur Teilung des Datensatzes in einen regulären
# und einen extremen Teil

# Extreme value model
library(extRemes)

insurance_df = insurance_data %>% 
  ungroup(IDpol) %>%
  select(c(Area, VehPower, VehAge, DrivAge, BonusMalus, VehBrand, VehGas, Density, Region, ClaimPerYear))
insurance_df = data.frame(insurance_df)

# set.seed(123)
# fitGEV <- fevd(
#   ClaimPerYear, 
#   data = insurance_df,
#   type = "GP",
#   threshold = 2000000)
# fitGEV$results
# plot(fitGEV)

threshold_vector = seq(5000, 100000, 5000)
AIC_vector = lapply(threshold_vector, FUN = function(i) {
  fitGEV <- fevd(
    ClaimPerYear, 
    data = insurance_df,
    type = "GP",
    threshold = i)
  return(2 * fitGEV$results$value + 2)}
)
AIC_vector

# Ellbogenkriterium bezüglich AIC: Ab Threshold von 20000 beginnt der Fit sich nur graduell zu verbessern
dev.off()
threshold_df = data.frame(threshold_vector, unlist(AIC_vector))
ggplot(data.frame("threshold" = threshold_vector, "AIC" = unlist(AIC_vector))) +
  geom_line(aes(x = threshold, y = AIC))

# Wir entscheiden uns für eine Threshold von 20000 EUR
fitGEV <- fevd(
  ClaimPerYear, 
  data = insurance_df,
  type = "GP",
  threshold = 25000)
dev.off()
# Die empirischen Quantile stimmen sehr gut mit den theoretischen Quantilen überein
# Die empirischen Quantile einer zusätzlichen, künstlichen Stichprobe (stochastisch!)
# liegen sehr nahe an den empirischen Quantilen aus unseren Daten
plot(fitGEV)
#

# Für Ansprüche kleiner als 20000 EUR trainieren wir ein herkömmliches Modell

# remove outlier
# outlier_IDs = which(
#   insurance_data$ClaimPerYear >= 10000)

# outlier_data = insurance_data[outlier_IDs, ]
# print(outlier_data)

# insurance_data = insurance_data[!row_number(insurance_data) %in% outlier_IDs, ]

model_data = insurance_data %>% filter(ClaimPerYear <= 25000)

# 80% der Ansprüche fallen unter einen Betrag von 4300 EUR
# 90% der Ansprüche fallen unter einen Betrag von 9208 EUR
quantile(round(model_data$ClaimPerYear), probs = seq(0.1, 1, 0.05))
plot(density(model_data$ClaimPerYear))
# insurance_data = insurance_data %>%
# mutate(ClaimPerYear = replace(ClaimPerYear, ClaimPerYear >= 1000000, "Not Candy"))
# quantile(round(insurance_data$ClaimPerYear), probs = seq(0.1, 1, 0.05))

# Da diese extremen Ausreißer nach oben von zentraler Bedeutung für die finanzielle 
# Gesundheit der Versicherungsgesellschaft sind, dürfen wir sie NICHT entfernen.

# Ein geeignetes Modell modelliert nicht nur den bedingten Erwartungswert der
# Zielvariable sondern deren Verteilung, welche Ausreißer berücksicht.


# Modellwahl

# 
# # 
# str(insurance_data)
# model_data = insurance_data %>% select(-c(IDpol, ClaimNb, Exposure, TotalClaimAmount))
# n = nrow(model_data)
# set.seed(12)
# train_set = sample(n, (2/3) * n)
# test_set =  setdiff(1:n, train_set)
# data_train = model_data[train_set, ]
# data_test = model_data[test_set, ]
# 
# library(quantregRanger)

# 
# rqfit <- rq("ClaimPerYear ~ .", data = data_train, method = "fn")
# summary(rqfit)
# pred_quantreg = predict(rqfit, data_test)
# data_test$Prediction = pred_quantreg
# 
# ggplot(data_test) +
#   geom_point(aes(x = ClaimPerYear, y = Prediction))

# n = nrow(insurance_data)
# set.seed(123)
# train_set = sample(n, (2/3) * n)
# test_set =  setdiff(1:n, train_set)
# insurance_train = insurance_data[train_set, ]
# insurance_test = insurance_data[test_set, ]

# library(grf)
# ?quantile_forest
# X_train = model.matrix(
#   ~. , 
#   data = data_train[, -which(names(data_train) %in% "ClaimPerYear")])
# Y_train = data_train$ClaimPerYear
# X_test = model.matrix(~. , data = data_test[, -which(names(data_test) %in% "ClaimPerYear")])
# Y_test = data_test$ClaimPerYear
# 
# mod = quantile_forest(X_train, Y_train)
# outlier_data
# library(ranger)
# mod = ranger(
#   "ClaimPerYear ~ .",
#   data = outlier_data,
#   quantreg = TRUE)
# pred_outliers = predict(mod, outlier_data, quantiles = c(0.1, 0.5, 0.9))
# pred_outliers
# 
# 
# pareto_dist = gpd(outlier_data$ClaimPerYear, 
#     threshold = min(outlier_data$ClaimPerYear), 
#     method = "ml",
#     information = c("observed"))
# 
# plot(pareto_dist)
# 
# pareto_dist$par.ests
# pareto_dist$information

# baseline_model_0.1 = quantile(data_train$ClaimPerYear, probs = 0.1)
# baseline_model_0.5 = quantile(data_train$ClaimPerYear, probs = 0.5)
# baseline_model_0.9 = quantile(data_train$ClaimPerYear, probs = 0.9)
# # baseline_model_0.95 = quantile(data_train$ClaimPerYear, probs = 0.95)
# 
# pred <- as.data.frame(predict(mod, X_test))
# 
# data_test$PredictedClaimPerYear0.1 = pred[, 1]
# data_test$PredictedClaimPerYear0.5 = pred[, 2]
# data_test$PredictedClaimPerYear0.9 = pred[, 3]
# data_test$PredictedRange = data_test$PredictedClaimPerYear0.9 - data_test$PredictedClaimPerYear0.1
# data_test$IDpol <- factor(data_test$IDpol, levels = unique(data_test$IDpol))
# # data_test$IDpol = as.numeric(data_test$IDpol)
# data_test = data_test %>% arrange(PredictedRange)
# 
# ggplot(subset(data_test, ClaimPerYear <= 15000)) +
#   # geom_point(aes(x = ClaimPerYear, y = PredictedClaimPerYear0.5)) +
#   # geom_errorbar(
#   #   aes(x = IDpol, 
#   #       ymin = PredictedClaimPerYear0.1, 
#   #       ymax = PredictedClaimPerYear0.9), 
#   #   width=.01)
#   geom_ribbon(
#     aes(
#       ymin = PredictedClaimPerYear0.1, 
#       ymax = PredictedClaimPerYear0.9,  
#       x = IDpol, 
#       fill = "band"), 
#     alpha = 0.3) +
#   geom_line(aes(x = IDpol, y = PredictedClaimPerYear0.5))
# 

# pred <- predict(mod, data_test, quantiles = c(0.1, 0.5, 0.9, 0.95), type = "quantiles")
# pred$predictions
# 
# data_test$PredictedClaimPerYear0.1 = pred$predictions[, 1]
# data_test$PredictedClaimPerYear0.5 = pred$predictions[, 2]
# data_test$PredictedClaimPerYear0.9 = pred$predictions[, 3]
# # data_test$PredictedClaimPerYear0.95 = pred$predictions[, 4]
# 
# #data_test$PredictedRange = abs(data_test$PredictedClaimPerYear0.9 - data_test$PredictedClaimPerYear0.1)
# data_test$Error = abs(data_test$ClaimPerYear - data_test$PredictedClaimPerYear0.5)
# # data_test = data_test %>% arrange(PredictedRange)
# data_test$IDpol <- factor(data_test$IDpol, levels = unique(data_test$IDpol))
# library(ggplot2)
# 
# # Evaluierung per Quantilsverlust (Pinball-Loss)
# pinball_loss = function(quantile, response, prediction) {
#   if (response >= prediction) {
#     loss = quantile * abs(response - prediction)
#   } else {
#     loss = (1 - quantile) * abs(response - prediction)
#   }
#   return(loss)
# }
# 
# loss_0.1 = 0
# loss_0.5 = 0
# loss_0.9 = 0
# loss_0.95 = 0
# 
# baseline_loss_0.1 = 0
# baseline_loss_0.5 = 0
# baseline_loss_0.9 = 0
# baseline_loss_0.95 = 0
# 
# for (i in 1:nrow(data_test)) {
#   loss_0.1 = loss_0.1 + pinball_loss(0.1, data_test[i, "ClaimPerYear"], data_test[i, "PredictedClaimPerYear0.1"])
#   loss_0.5 = loss_0.5 + pinball_loss(0.5, data_test[i, "ClaimPerYear"], data_test[i, "PredictedClaimPerYear0.5"])
#   loss_0.9 = loss_0.9 + pinball_loss(0.9, data_test[i, "ClaimPerYear"], data_test[i, "PredictedClaimPerYear0.9"])
#   loss_0.95 = loss_0.95 + pinball_loss(0.95, data_test[i, "ClaimPerYear"], data_test[i, "PredictedClaimPerYear0.95"])
#   
#   baseline_loss_0.1 = baseline_loss_0.1 + pinball_loss(0.1, data_test[i, "ClaimPerYear"], baseline_model_0.1)
#   baseline_loss_0.5 = baseline_loss_0.5 + pinball_loss(0.5, data_test[i, "ClaimPerYear"], baseline_model_0.5)
#   baseline_loss_0.9 = baseline_loss_0.9 + pinball_loss(0.9, data_test[i, "ClaimPerYear"], baseline_model_0.9)
#   baseline_loss_0.95 = baseline_loss_0.95 + pinball_loss(0.95, data_test[i, "ClaimPerYear"], baseline_model_0.95)
# }
# 
# loss_0.1 > baseline_loss_0.1
# loss_0.5 > baseline_loss_0.5
# loss_0.9 > baseline_loss_0.9
# loss_0.95 > baseline_loss_0.95
# 
# D_squared_0.05 = 1 - ((loss_0.05)^2 / (baseline_loss_0.05)^2)
# D_squared_0.5 = 1 - (loss_0.5 / baseline_loss_0.5)
# D_squared_0.95 = 1 - (loss_0.95 / baseline_loss_0.95)
# 
# 
# 
# for (i in 1:nrow())
# insurance_test$pinball_loss_0.05 = 
# 
# pinball_loss(0.9, insurance_test[10, ]$ClaimPerYear, insurance_test[10, ]$PredictedClaimPerYear0.5)
# 
# 
# test_df = insurance_test[insurance_test$ClaimPerYear <= 4300, ]

# 80% der Ansprüche fallen unter einen Betrag von 4300 EUR
# ggplot(data = insurance_test[insurance_test$ClaimPerYear <= 4300, ]) +
#   geom_errorbar(
#     aes(x = IDpol, 
#         ymin = PredictedClaimPerYear0.05, 
#         ymax = PredictedClaimPerYear0.95), 
#     width=.01,
#     position=position_dodge(.9), alpha = 0.7) +
#   geom_point(aes(x = IDpol, y = PredictedClaimPerYear0.5), color = "blue") 
#   # geom_point(aes(x = IDpol, y = ClaimPerYear), color = "black") 
#   # geom_point(aes(x = IDpol, y = Error, color = "red"))
#   
# sum(
#   (insurance_test$ClaimPerYear >= insurance_test$PredictedClaimPerYear0.05) & 
#   (insurance_test$ClaimPerYear <= insurance_test$PredictedClaimPerYear0.95)) / 
#   nrow(insurance_test)
# 
# max(test_df$PredictedRange)
# # Training a model in mlr3
# insurance_task = mlr3::as_task_regr(
#   insurance_data, target = "ClaimPerYear", id = "insurance")
# 
# splits = partition(insurance_task)
# lrn_rpart = lrn("regr.ranger")
# lrn_rpart$train(insurance_task, splits$train)
# 
# insurance_train = insurance_data[splits$train, ]
# insurance_test = insurance_data[splits$test, ]
# 
# insurance_data
# str(insurance_data)
# insurance_data$VehGas = as.factor(insurance_data$VehGas)
# 
# insurance_data = insurance_data %>%
#   select(-c(TotalClaimAmount, Exposure))
# 
# prediction = lrn_rpart$predict(insurance_task, splits$test)
# insurance_test$PredictedClaimPerYear = prediction$response
# 
# prediction$score(msr("regr.mae"))
# 
# ggplot(insurance_test) +
#   geom_point(aes(x = IDpol, y = abs(PredictedClaimPerYear - ClaimPerYear)))
# 

# mlr3 

model_data = model_data %>%
  ungroup(IDpol) %>%
  select(-c(IDpol, ClaimNb, Exposure, TotalClaimAmount))

model_data$VehGas = as.factor(model_data$VehGas)
model_data

# library(cor)
install.packages("lares")
library(lares)
corr_cross(model_data, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)
corr_var(model_data, # name of dataset
         ClaimPerYear, # name of variable to focus on
         top = 5 # display top 5 correlations
) 

# cor(as.matrix(model_data))
cor(as.matrix(model_data))

tsk_insurance = as_task_regr(model_data, target = "ClaimPerYear", id = "insurance")
splits = partition(tsk_insurance)
library(mlr3learners)
library(mlr3verse)
learner_list = c(
  lrn("regr.featureless"), 
  lrn("regr.lm"), 
  lrn("regr.rpart"),
  lrn("regr.ranger"))

# poe = po("encode", method = "one-hot")
# learner_list = lapply(learner_list, FUN = function(x) {po("encode") %>>% x})

resamplings = rsmp("cv", folds = 3)
design = benchmark_grid(tsk_insurance, learner_list, resamplings)
set.seed(123)
bmr = benchmark(design)
aggr = bmr$aggregate()
aggr

library("mlr3verse")

lrn_rpart = lrn("regr.rpart")
lrn_rpart$train(tsk_insurance, splits$train)
lrn_ranger = lrn("regr.ranger")
lrn_ranger$train(tsk_insurance, splits$train)

library(rpart.plot)
lrn_rpart$model
rpart.plot(lrn_rpart$model)

pred_rpart = lrn_rpart$predict(tsk_insurance, splits$test)
pred_ranger = lrn_ranger$predict(tsk_insurance, splits$test)
pred_rpart$score(msr("regr.mae"))
pred_ranger$score(msr("regr.mae"))

insurance_test = insurance_data[splits$test, ]
insurance_test$PredictionRanger = pred_ranger$response
insurance_test$PredictionRpart = pred_rpart$response

ggplot(data = insurance_test, aes(x = ClaimPerYear, y = PredictionRpart)) +
  geom_point(size = 1, shape = 21, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.5) +
  theme_bw()

ggplot(data = insurance_test, aes(x = ClaimPerYear, y = PredictionRanger)) +
  geom_point(size = 1, shape = 21, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.5) +
  theme_bw()



