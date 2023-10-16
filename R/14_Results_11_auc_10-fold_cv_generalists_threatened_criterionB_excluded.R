# Load required packages
library(lme4)
library(dplyr)
library(pROC)
library(ggplot2)

# Set working directory
setwd("My_working_directory")

# Import data
ext.risk<-read.delim("species_data.txt")

# Filter data for generalists
ext.risk.mammals<-ext.risk %>% filter(habitat_breadth>1)

# Recode red list categories as numeric
ext.risk.mammals[,32] <- ifelse(ext.risk.mammals[,32] == "LC", 0,
                                ifelse(ext.risk.mammals[,32] == "NT", 0,
                                       ifelse(ext.risk.mammals[,32] == "VU", 1,
                                              ifelse(ext.risk.mammals[,32] == "EN", 1,
                                                     ifelse(ext.risk.mammals[,32] == "CR", 1, NA)))))

# Exclude threatened species under criterionB
ext.risk.mammals$threatened_under_criterionB <- ifelse(ext.risk.mammals[,31] == 1 & ext.risk.mammals[,32] == 1, 1, 0)
threatened.under.criterionB.excluded<-ext.risk.mammals %>% filter(threatened_under_criterionB<1)

# Convert categorical variables to factor
threatened.under.criterionB.excluded$order<-as.factor(threatened.under.criterionB.excluded$order)
threatened.under.criterionB.excluded$rl_cat<-as.factor(threatened.under.criterionB.excluded$rl_cat)

# Select variables from dataset
vars<-c("order",
        "patch_area",
        "change_patch_area", 
        "habitat_intactness_p5",
        "habitat_intactness_p10",
        "habitat_intactness_p50",
        "habitat_intactness_p90", 
        "habitat_intactness_p95",
        "change_habitat_intactness_p5",
        "change_habitat_intactness_p10",
        "change_habitat_intactness_p50", 
        "change_habitat_intactness_p90",
        "change_habitat_intactness_p95",
        "patch_intactness_p5", 
        "patch_intactness_p10",
        "patch_intactness_p50",
        "patch_intactness_p90", 
        "patch_intactness_p95",
        "change_patch_intactness_p5", 
        "change_patch_intactness_p10",
        "change_patch_intactness_p50",
        "change_patch_intactness_p90", 
        "change_patch_intactness_p95",
        "range_size",
        "gestation_length",
        "weaning_age",
        "rl_cat")

# Replace values greater than 0 with 0 in columns of change
threatened.under.criterionB.excluded.gain.excluded <- threatened.under.criterionB.excluded %>%
  mutate_at(vars(change_patch_area,
                 change_habitat_intactness_p5,
                 change_habitat_intactness_p10,
                 change_habitat_intactness_p50,
                 change_habitat_intactness_p90,
                 change_habitat_intactness_p95,
                 change_patch_intactness_p5,
                 change_patch_intactness_p10,
                 change_patch_intactness_p50,
                 change_patch_intactness_p90,
                 change_patch_intactness_p95), ~replace(., . > 0, 0)) %>%
  # Replace negative values with absolute values
  mutate_at(vars(change_patch_area,
                 change_habitat_intactness_p5,
                 change_habitat_intactness_p10,
                 change_habitat_intactness_p50,
                 change_habitat_intactness_p90,
                 change_habitat_intactness_p95,
                 change_patch_intactness_p5,
                 change_patch_intactness_p10,
                 change_patch_intactness_p50,
                 change_patch_intactness_p90,
                 change_patch_intactness_p95),
            abs)

# Remove species with missing (NA) values
threatened.under.criterionB.excluded.gain.excluded.na.omit<-na.omit(threatened.under.criterionB.excluded.gain.excluded[,vars])

# Scale the numerical variables
num_vars <- sapply(threatened.under.criterionB.excluded.gain.excluded.na.omit, is.numeric)
ext.risk.mammals.data.scaled <- threatened.under.criterionB.excluded.gain.excluded.na.omit
ext.risk.mammals.data.scaled[, num_vars] <- scale(threatened.under.criterionB.excluded.gain.excluded.na.omit[, num_vars])

# Create data frames for extinction risk (ER) models
data.pa<-data.frame(or=ext.risk.mammals.data.scaled$order,
                    pa=ext.risk.mammals.data.scaled$patch_area,
                    rpa=ext.risk.mammals.data.scaled$change_patch_area,
                    rs=ext.risk.mammals.data.scaled$range_size,
                    gl=ext.risk.mammals.data.scaled$gestation_length,
                    wa=ext.risk.mammals.data.scaled$weaning_age,
                    rl.cat=ext.risk.mammals.data.scaled$rl_cat)

data.hi<-data.frame(or=ext.risk.mammals.data.scaled$or,
                    hi=ext.risk.mammals.data.scaled$habitat_intactness_p95,
                    rhi=ext.risk.mammals.data.scaled$change_habitat_intactness_p95,
                    rs=ext.risk.mammals.data.scaled$range_size,
                    gl=ext.risk.mammals.data.scaled$gestation_length,
                    wa=ext.risk.mammals.data.scaled$weaning_age,
                    rl.cat=ext.risk.mammals.data.scaled$rl_cat)

data.pi<-data.frame(or=ext.risk.mammals.data.scaled$or,
                    pi=ext.risk.mammals.data.scaled$patch_intactness_p95,
                    rpi=ext.risk.mammals.data.scaled$change_patch_intactness_p95,
                    rs=ext.risk.mammals.data.scaled$range_size,
                    gl=ext.risk.mammals.data.scaled$gestation_length,
                    wa=ext.risk.mammals.data.scaled$weaning_age,
                    rl.cat=ext.risk.mammals.data.scaled$rl_cat)

#----
# Set number of folds for cross-validation
k <- 10

# Calculate the number of observations in the dataset
n <- dim(ext.risk.mammals.data.scaled)[1]

# Set seed for reproducibility 
set.seed(17)

# Generate indices for random sampling
indices <- sample(rep(1:k, ceiling(n/k))[1:n])

#----
# Calculate AUC values for pa
all.response.pa <- all.predictor.pa <- aucs.pa <- c()
for (i in 1:k) {
  test.pa = data.pa[indices==i,]
  learn.pa = data.pa[indices!=i,]
  model.pa <- glmer(rl.cat~pa+rpa+rs+gl+wa+(1|or), family=binomial, data=learn.pa)
  model.pred.pa <- predict(model.pa, newdata=test.pa, type="response", allow.new.levels=TRUE)
  aucs.pa <- c(aucs.pa, roc(test.pa$rl.cat, model.pred.pa, quiet=TRUE)$auc)
  all.response.pa <- c(all.response.pa, test.pa$rl.cat)
  all.predictor.pa <- c(all.predictor.pa, model.pred.pa)
}

# Convert AUC values into a data frame
hc.pa.auc.values<-as.data.frame(aucs.pa)

# Calculate mean and standard error
hc.pa.auc.values.model <- lm(aucs.pa ~ 1, hc.pa.auc.values)
coef.hc.pa.auc.values.model<-data.frame(coef(summary(hc.pa.auc.values.model)))

# Calculate confidence interval
confint.hc.pa.auc.values.model<-as.data.frame(confint(hc.pa.auc.values.model))

# Create data frame with mean AUC value and confidence interval
auc.pa<-data.frame(auc=coef.hc.pa.auc.values.model[,1], ci_lower=confint.hc.pa.auc.values.model$`2.5 %`, ci_upper=confint.hc.pa.auc.values.model$`97.5 %`)

#----
# Calculate AUC values for hi
all.response.hi <- all.predictor.hi <- aucs.hi <- c()
for (i in 1:k) {
  test.hi = data.hi[indices==i,]
  learn.hi = data.hi[indices!=i,]
  model.hi <- glmer(rl.cat~hi+rhi+rs+gl+wa+(1|or), family=binomial, data=learn.hi)
  model.pred.hi <- predict(model.hi, newdata=test.hi, type="response", allow.new.levels=TRUE)
  aucs.hi <- c(aucs.hi, roc(test.hi$rl.cat, model.pred.hi, quiet=TRUE)$auc)
  all.response.hi <- c(all.response.hi, test.hi$rl.cat)
  all.predictor.hi <- c(all.predictor.hi, model.pred.hi)
}

# Convert AUC values into a data frame
hc.hi.auc.values<-as.data.frame(aucs.hi)

# Calculate mean and standard error
hc.hi.auc.values.model <- lm(aucs.hi~1, hc.hi.auc.values)
coef.hc.hi.auc.values.model<-data.frame(coef(summary(hc.hi.auc.values.model)))

# Calculate confidence interval
confint.hc.hi.auc.values.model<-as.data.frame(confint(hc.hi.auc.values.model))

# Create data frame with mean AUC value and confidence interval
auc.hi<-data.frame(auc=coef.hc.hi.auc.values.model[,1], ci_lower=confint.hc.hi.auc.values.model$`2.5 %`, ci_upper=confint.hc.hi.auc.values.model$`97.5 %`)

#----
# Calculate AUC values for pi
all.response.pi <- all.predictor.pi <- aucs.pi <- c()
for (i in 1:k) {
  test.pi = data.pi[indices==i,]
  learn.pi = data.pi[indices!=i,]
  model.pi <- glmer(rl.cat~pi+rpi+rs+gl+wa+(1|or), family=binomial, data=learn.pi)
  model.pred.pi <- predict(model.pi, newdata=test.pi, type="response", allow.new.levels=TRUE)
  aucs.pi <- c(aucs.pi, roc(test.pi$rl.cat, model.pred.pi, quiet=TRUE)$auc)
  all.response.pi <- c(all.response.pi, test.pi$rl.cat)
  all.predictor.pi <- c(all.predictor.pi, model.pred.pi)
}

# Convert AUC values into a data frame
hc.pi.auc.values<-as.data.frame(aucs.pi)

# Calculate mean and standard error
hc.pi.auc.values.model <- lm(aucs.pi~1, hc.pi.auc.values)
coef.hc.pi.auc.values.model<-data.frame(coef(summary(hc.pi.auc.values.model)))

# Calculate confidence interval
confint.hc.pi.auc.values.model<-as.data.frame(confint(hc.pi.auc.values.model))

# Create data frame with mean AUC value and confidence interval
auc.pi<-data.frame(auc=coef.hc.pi.auc.values.model[,1], ci_lower=confint.hc.pi.auc.values.model$`2.5 %`, ci_upper=confint.hc.pi.auc.values.model$`97.5 %`)

#----
# Combine data frames with mean AUC values and confidence intervals together
auc.pahipi<-rbind(auc.pa, auc.hi, auc.pi)

# Assign landscape model and habitat breadth
auc.pahipi$parameter<-c("pa", "hi", "pi")
land.models<-c("Patch-matrix", "Continuum", "Hybrid")
auc.pahipi$land.model<-land.models
auc.pahipi$habitat_breadth <- "Generalists"

# Save AUC values and confidence intervals
write.table(auc.pahipi, file="auc_10-fold_cv_pahipi_generalists_threatened_criterionB_excluded.csv", sep="\t", row.names=F)
