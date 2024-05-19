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

outlier_IDs = which(
  insurance_data$ClaimPerYear >= 19000)

outlier_data = insurance_data[outlier_IDs, ]
print(outlier_data)

insurance_data = insurance_data[!row_number(insurance_data) %in% outlier_IDs, ]

# 80% der Ansprüche fallen unter einen Betrag von 4300 EUR
# 90% der Ansprüche fallen unter einen Betrag von 9208 EUR
quantile(round(insurance_data$ClaimPerYear), probs = seq(0.1, 1, 0.05))
plot(density(insurance_data$ClaimPerYear))
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
outlier_data
# library(ranger)
# mod = ranger(
#   "ClaimPerYear ~ .",
#   data = outlier_data,
#   quantreg = TRUE)
# pred_outliers = predict(mod, outlier_data, quantiles = c(0.1, 0.5, 0.9))
# pred_outliers


pareto_dist = gpd(outlier_data$ClaimPerYear, 
    threshold = min(outlier_data$ClaimPerYear), 
    method = "ml",
    information = c("observed"))

plot(pareto_dist)

pareto_dist$par.ests
pareto_dist$information

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
ggplot(data = insurance_test[insurance_test$ClaimPerYear <= 4300, ]) +
  geom_errorbar(
    aes(x = IDpol, 
        ymin = PredictedClaimPerYear0.05, 
        ymax = PredictedClaimPerYear0.95), 
    width=.01,
    position=position_dodge(.9), alpha = 0.7) +
  geom_point(aes(x = IDpol, y = PredictedClaimPerYear0.5), color = "blue") 
  # geom_point(aes(x = IDpol, y = ClaimPerYear), color = "black") 
  # geom_point(aes(x = IDpol, y = Error, color = "red"))
  
sum(
  (insurance_test$ClaimPerYear >= insurance_test$PredictedClaimPerYear0.05) & 
  (insurance_test$ClaimPerYear <= insurance_test$PredictedClaimPerYear0.95)) / 
  nrow(insurance_test)

max(test_df$PredictedRange)
# Training a model in mlr3
insurance_task = mlr3::as_task_regr(
  insurance_data, target = "ClaimPerYear", id = "insurance")

splits = partition(insurance_task)
lrn_rpart = lrn("regr.ranger")
lrn_rpart$train(insurance_task, splits$train)

insurance_train = insurance_data[splits$train, ]
insurance_test = insurance_data[splits$test, ]

insurance_data
str(insurance_data)
insurance_data$VehGas = as.factor(insurance_data$VehGas)

insurance_data = insurance_data %>%
  select(-c(TotalClaimAmount, Exposure))

prediction = lrn_rpart$predict(insurance_task, splits$test)
insurance_test$PredictedClaimPerYear = prediction$response

prediction$score(msr("regr.mae"))

ggplot(insurance_test) +
  geom_point(aes(x = IDpol, y = abs(PredictedClaimPerYear - ClaimPerYear)))


# mlr3 

insurance_data$VehGas = as.factor(insurance_data$VehGas)

tsk_insurance = as_task_regr(insurance_data, target = "ClaimPerYear", id = "insurance")
splits = partition(tsk_insurance)
lrn_rpart = lrn("regr.rpart")
lrn_ranger = lrn("regr.ranger")
lrn_featureless = lrn("regr.featureless")

lrn_rpart$train(tsk_insurance, splits$train)
lrn_ranger$train(tsk_insurance, splits$train)
lrn_featureless$train(tsk_insurance, splits$train)

pred_rpart = lrn_rpart$predict(tsk_insurance, splits$test)
pred_ranger = lrn_ranger$predict(tsk_insurance, splits$test)
pred_featureless = lrn_featureless$predict(tsk_insurance, splits$test)

pred_rpart$score(msr("regr.mae"))
pred_ranger$score(msr("regr.mae"))
pred_featureless$score(msr("regr.mae"))

insurance_test = insurance_data[splits$test, ]
insurance_test$Prediction = pred_ranger$response

ggplot(data = insurance_test, aes(x = ClaimPerYear, y = Prediction)) +
  geom_point()

# pareto
library(evir)
# install.packages("evir")
?gpd



gpd(
  data_train, 
  threshold = 200000,
  method = "ml",
  information = c("observed"))


# 
install.packages("extRemes")
library(extRemes)
# Simulate normal
# simnorm <- rnorm(length(retd), mean(retd), sd(retd))
# Estimate generalized pareto distribution for the tail using Maximum Likelihood
# I use 1.65 as a threshold
pareto_fit <- fevd(
  outlier_data$ClaimPerYear, 
  threshold = min(outlier_data$ClaimPerYear),
  type = "GP", use.phi= T, method = c("MLE"))
# Estimate generalized pareto distribution for the left tail using Maximum Likelihood
# tmp_threshold <- quantile(100*retd, .1) # set the Threshold
# Estimate generalized pareto distribution for the right tail using Maximum Likelihood
# tmpfit <- fevd(-100*retd, threshold= -tmp_threshold, type = "GP", use.phi= T, method = c("MLE"))
# tmp_threshold <- quantile(100*retd, .9)  # set the Threshold
# tmpfit <- fevd(100*retd, threshold= tmp_threshold, type = "GP", use.phi= T, method = c("MLE"))


plot(pareto_fit)



library(extRemes)
data(Fort)
names(Fort)

bmFort <- blockmaxxer(Fort, blocks = Fort$year, which="Prec")
names(bmFort)

plot(Fort$year, Fort$Prec, xlab = "Year",
     ylab = "Precipitation (inches)",
     cex = 1.25, cex.lab = 1.25,
     col = "darkblue", bg = "lightblue", pch = 21)

points(bmFort$year, bmFort$Prec, col="darkred", cex=1.5)

# Fit a GEV distribution to annual maximum Precipitation
# in Fort Collins, Colorado, U.S.A.
fitGEV <- fevd(Prec, data = bmFort)
fitGEV
plot(fitGEV)
plot(fitGEV, "trace")

