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
# joined_data = joined_data[complete.cases(joined_data), ]

total_claim_amount = joined_data %>%
  group_by(IDpol) %>%
  summarise(TotalClaimAmount = sum(ClaimAmount)) %>%
  select(IDpol, TotalClaimAmount)

insurance_data = joined_data %>%
  group_by(IDpol) %>%
  filter(row_number() == 1) %>%
  ungroup(IDpol) %>%
  select(-ClaimAmount) %>%
  full_join(total_claim_amount, by = join_by(IDpol)) %>%
  mutate(ClaimPerYear = TotalClaimAmount / Exposure) %>%
  select(-c(TotalClaimAmount, Exposure, ClaimNb, Density, Region, IDpol)) %>%
  mutate(VehGas = as.factor(VehGas))



# Exploratory Analysis

# Boxplots der Versicherungsansprüche und der Ansprüche je Jahr zeigen wenige, jedoch extreme Ausreißer
# ggplot(insurance_data, aes(x = TotalClaimAmount)) +
  # geom_boxplot() +
  # theme_bw()

ggplot(insurance_data, aes(x = ClaimPerYear)) +
  geom_boxplot() +
  theme_bw()

# Interquartilsabstand (25% bis 75% Quantil) beträgt nur 2224 EUR
# Der Großteil der Kunden nimmt somit Ansprüche von bis zu 2224 EUR wahr.
IQR(insurance_data$ClaimPerYear)
# Der höchste Versicherungsanspruch eines Kunden pro Jahr beträgt jedoch 18.524.548 EUR
range(insurance_data$ClaimPerYear)
quantile(round(insurance_data$ClaimPerYear), probs = seq(0, 1, 0.05))

library(lares)
# Innerhalb der erklärenden Variablen finden sich diverse Korrelationen, beispielsweise zwischen Alter
# des Fahrers und des Schadenfreiheitsrabattes, sowie zwischen Merkmalen des Fahrzeuges.
corr_cross(insurance_data, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)
# Zwischen der Zielvariable und den erklärenden Variablen finden sich extrem schwache Assoziationen
# Zwar lassen lineare Korrelationen nicht ausschließen, dass es nichtlineare Zusammenhänge gibt,
# jedoch sind die vorhanden linearen Assoziationen nicht sehr vielversprechend für ein zu trainierendes Modell

# Die höchste lineare Korrelation mit der Zielvariable hat das Alter des Fahrers, jedoch nur mit 2%.
# Dies lässt vermuten, dass die Findung einer multivariaten Verteilung hier schwierig sein sollte.
corr_var(insurance_data, # name of dataset
         ClaimPerYear, # name of variable to focus on
         top = 5 # display top 5 correlations
) 

feature_pred = x2y(insurance_data)
plot(feature_pred)


dev.off()
# Die empirischen Quantile stimmen sehr gut mit den theoretischen Quantilen überein
# Die empirischen Quantile einer zusätzlichen, künstlichen Stichprobe (stochastisch!)
# liegen sehr nahe an den empirischen Quantilen aus unseren Daten
# plot(fitGEV)

# library(h2o)
# lasso_vars(insurance_data,
#            ClaimPerYear)

# Extreme value model
library(extRemes)

# insurance_df = insurance_data %>% 
# ungroup(IDpol) %>%
# select(-IDpol)
# select(c(Area, VehPower, VehAge, DrivAge, BonusMalus, VehBrand, VehGas, Density, Region, ClaimPerYear))
insurance_df = data.frame(insurance_data)

threshold_vector = seq(5000, 1000000, 10000)
AIC_vector = lapply(threshold_vector, FUN = function(i) {
  fitGEV <- fevd(
    ClaimPerYear, 
    data = insurance_df,
    type = "GP",
    threshold = i)
  return(2 * fitGEV$results$value + 2)}
)
AIC_vector

# Ellbogenkriterium bezüglich AIC: Ab Threshold von 250.000 beginnt der Fit sich nur graduell zu verbessern
dev.off()
threshold_df = data.frame(threshold_vector, unlist(AIC_vector))
ggplot(data.frame("threshold" = threshold_vector, "AIC" = unlist(AIC_vector))) +
  geom_line(aes(x = threshold, y = AIC))

# Wir entscheiden uns für einen Extremwert-Threshold von 250000 EUR
fitGEV <- fevd(
  ClaimPerYear, 
  data = insurance_df,
  type = "GP",
  threshold = 250000)
dev.off()

fitGEV
# Die empirischen Quantile stimmen sehr gut mit den theoretischen Quantilen überein
# Die empirischen Quantile einer zusätzlichen, künstlichen Stichprobe (stochastisch!)
# liegen sehr nahe an den empirischen Quantilen aus unseren Daten
set.seed(12)
plot(fitGEV)


find_empirical_dist = function(x) {
  n = length(x)
  sorted_x = sort(x)
  cumsum_frac = seq(1:n) / n
  emp_dist = data.frame(x = sorted_x, y = cumsum_frac)
  return(emp_dist)
}

empiricalProb = function(quantile, dist) {
  return(sum(dist$x <= quantile) / nrow(dist))
}

emp_dist = find_empirical_dist(model_data$ClaimPerYear)
empiricalProb(5000, emp_dist)


# Für Ansprüche kleiner als 25000 EUR trainieren wir ein herkömmliches Modell

model_data = insurance_data %>% filter(ClaimPerYear <= 250000)

library(gamlss)
?gamlss
gamlssNP("ClaimPerYear ~ .")

quantile(round(model_data$ClaimPerYear), probs = seq(0.1, 1, 0.05))
dev.off()
plot(hist(model_data$ClaimPerYear))

# mlr3 

# model_data = model_data %>%
  # ungroup(IDpol) %>%
  # select(-c(IDpol))

model_data$VehGas = as.factor(model_data$VehGas)
model_data

# library(cor)
# # install.packages("lares")
# library(lares)
# corr_cross(model_data, # name of dataset
#            max_pvalue = 0.05, # display only significant correlations (at 5% level)
#            top = 10 # display top 10 couples of variables (by correlation coefficient)
# )
# corr_var(model_data, # name of dataset
#          ClaimPerYear, # name of variable to focus on
#          top = 5 # display top 5 correlations
# ) 


tsk_insurance = as_task_regr(model_data, target = "ClaimPerYear", id = "insurance")
splits = partition(tsk_insurance)
library(mlr3learners)

library(mlr3verse)
learner_list = c(
  lrn("regr.featureless"), 
  lrn("regr.lm"), 
  lrn("regr.rpart"),
  lrn("regr.kknn"),
  lrn("regr.svm"),
  lrn("regr.ranger"))

resamplings = rsmp("cv", folds = 3)
# poe = po("encode", method = "one-hot")
learner_list = lapply(learner_list, FUN = function(x) {po("encode") %>>% x})

design = benchmark_grid(tsk_insurance, learner_list, resamplings)
set.seed(123)
bmr = benchmark(design)
aggr = bmr$aggregate()
aggr

library("mlr3verse")

# Nicht überraschend finden wir kein einziges passendes Modell; der reine Durchschnitt der Zielvariable
# zeigt ähnliche Performance wie Bäume oder Random Forests
# Ich schlussfolgere, dass die Schätzung einer multivariate Verteilung hier nicht zum Ziel führt
# Es verbleibt die Möglichkeit, die marginale Dichte der Zielvariable zu modellieren; auch deshalb, weil
# für unsere Extremwerterteilung > 250.000 EUR eine passende theoretische Verteilung gefunden werden konnte.

# 
# # Nun fehlt noch die univariate Dichteschätzung unterhalb des Extremwertes
# 
# claims_lower = model_data %>%
#   filter(ClaimPerYear < 250000) 
# 
# # Wir sehen, dass die Verteilung immer noch sehr stark rechtschief ist
# ggplot(claims_lower, aes(x = ClaimPerYear)) +
#   geom_density() +
#   theme_bw()
# 
# # Sehen wir uns noch einmal die Quantile der marginalen Verteilung an
# quantile(claims_lower$ClaimPerYear, probs = seq(0, 1, 0.1))
# # Ganze 90% der Daten befinden sich im Intervall von EUR bis 8677 EUR Schadensansprüchen
# 
# 
# # Wir partitionieren die Daten wiederum in einen mittleren und einen niedrigen Anteil
# library(fitdistrplus)
# claims_medium = model_data %>%
#   filter(ClaimPerYear < 250000) 
# # Die Verteilung hat zwar einen "heavy Tail", sollte jedoch leichter zu modellieren sein
# ggplot(claims_medium, aes(x = ClaimPerYear)) +
#   geom_density() +
#   theme_bw()
# 
# descdist(model_data$ClaimPerYear, discrete = FALSE)
# fit.weibull <- fitdist(claims_medium$ClaimPerYear, "beta")
# 
# # Wir entscheiden uns für einen Extremwert-Threshold von 250000 EUR
# fitGEV <- fevd(
#   ClaimPerYear, 
#   data = insurance_df,
#   type = "GP",
#   threshold = 10000)
# fitGEV
# dev.off()
# # Die empirischen Quantile stimmen sehr gut mit den theoretischen Quantilen überein
# # Die empirischen Quantile einer zusätzlichen, künstlichen Stichprobe (stochastisch!)
# # liegen sehr nahe an den empirischen Quantilen aus unseren Daten
# plot(fitGEV)
# # 
# # # 
# # claims_lower = model_data %>%
# #   filter(ClaimPerYear < 9000) 
# # 
# # # Wir sehen, dass die Verteilung immer noch sehr stark rechtschief ist
# # ggplot(claims_lower, aes(x = ClaimPerYear)) +
# #   geom_density() +
# #   theme_bw()
# # 
# # 
# # install.packages("TDA")
# # library("TDA")
# # ## Generate Data from the unit circle
# # # n <- 300
# # # X <- circleUnif(n)
# # 
# # ## Construct a grid of points over which we evaluate the function
# # by <- 100
# # Xseq <- seq(0, 20000, by = by)
# # # Yseq <- seq(-1.7, 1.7, by = by)
# # Grid <- expand.grid(Xseq, Yseq)
# # 
# # ## kernel density estimator
# # k <- 50
# # KNN <- knnDE(X, Grid, k)
# # 
# # 
# # set.seed(7)
# # fit.knn <- train(Species~., data=train, method="knn",
# #                  metric=metric ,trControl=trainControl)
# # knn.k1 <- fit.knn$bestTune # keep this Initial k for testing with knn() function in next section
# # print(fit.knn)
# # 
# # 
# # claims_lower = claims_lower$ClaimPerYear
# # 
# # library(gamlss)
# # fit <- fitDist(claims_lower, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)
# fit
# summary(fit)
# plot(fit)
# # descdist(claims_lower, discrete = FALSE)
# # It seems our data can be described by a beta distribution
# 
# fit.beta <- fitdist(claims_lower, "beta")
# 
# 
# fit.norm <- fitdist(x, "norm")
# 
# ggplot(claims_medium, aes(x = ClaimPerYear)) +
#   geom_density()
