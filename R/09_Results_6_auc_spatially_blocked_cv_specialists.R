# Load required packages
library(lme4)
library(dplyr)
library(pROC)
library(ggplot2)

# Set working directory
setwd("My_working_directory")

# Import data
ext.risk<-read.delim("species_data.txt")

# Filter data for specialists
ext.risk.mammals<-ext.risk %>% filter(habitat_breadth==1)

# Recode red list categories as numeric
ext.risk.mammals[,32] <- ifelse(ext.risk.mammals[,32] == "LC", 0,
                                ifelse(ext.risk.mammals[,32] == "NT", 0,
                                       ifelse(ext.risk.mammals[,32] == "VU", 1,
                                              ifelse(ext.risk.mammals[,32] == "EN", 1,
                                                     ifelse(ext.risk.mammals[,32] == "CR", 1, NA)))))

# Convert categorical variables to factor
ext.risk.mammals$order<-as.factor(ext.risk.mammals$order)
ext.risk.mammals$realm<-as.factor(ext.risk.mammals$realm)
ext.risk.mammals$rl_cat<-as.factor(ext.risk.mammals$rl_cat)

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
        "realm",
        "rl_cat")

# Replace values greater than 0 with 0 in columns of change
ext.risk.mammals.gain.excluded <- ext.risk.mammals %>%
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
ext.risk.mammals.gain.excluded.na.omit<-na.omit(ext.risk.mammals.gain.excluded[,vars])

# Create data frame for extinction risk (ER) models
ext.risk.mammals.data<-data.frame(or=ext.risk.mammals.gain.excluded.na.omit$order, 
                                  pa=ext.risk.mammals.gain.excluded.na.omit$patch_area,
                                  rpa=ext.risk.mammals.gain.excluded.na.omit$change_patch_area,
                                  hi=ext.risk.mammals.gain.excluded.na.omit$habitat_intactness_p95,
                                  rhi=ext.risk.mammals.gain.excluded.na.omit$change_habitat_intactness_p95,
                                  pi=ext.risk.mammals.gain.excluded.na.omit$patch_intactness_p95,
                                  rpi=ext.risk.mammals.gain.excluded.na.omit$change_patch_intactness_p95,
                                  rs=ext.risk.mammals.gain.excluded.na.omit$range_size,
                                  gl=ext.risk.mammals.gain.excluded.na.omit$gestation_length,
                                  wa=ext.risk.mammals.gain.excluded.na.omit$weaning_age,
                                  rlm=ext.risk.mammals.gain.excluded.na.omit$realm,
                                  rl.cat=ext.risk.mammals.gain.excluded.na.omit$rl_cat)

# Scale the numerical variables
num_vars <- sapply(ext.risk.mammals.data, is.numeric)
ext.risk.mammals.data.scaled <- ext.risk.mammals.data
ext.risk.mammals.data.scaled[, num_vars] <- scale(ext.risk.mammals.data[, num_vars])

#----
# Define training  and testing datasets
# Using all realms but Afrotropic as training set and Afrotropic as testing set
train.but.afrotropic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Australasia" | rlm=="Indomalayan" | rlm=="Nearctic" | rlm=="Neotropic" | rlm=="Palearctic")
test.afroropic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Afrotropic")

# Using all realms but Australasia as training set and Australasia as testing set
train.but.australasia <- ext.risk.mammals.data.scaled %>% filter(rlm=="Afrotropic" | rlm=="Indomalayan" | rlm=="Nearctic" | rlm=="Neotropic" | rlm=="Palearctic")
test.australasia <- ext.risk.mammals.data.scaled %>% filter(rlm=="Australasia")

# Using all realms but Indomalayan as training set and Indomalayan as testing set
train.but.indomalayan <- ext.risk.mammals.data.scaled %>% filter(rlm=="Afrotropic" | rlm=="Australasia" | rlm=="Nearctic" | rlm=="Neotropic" | rlm=="Palearctic")
test.indomalayan <- ext.risk.mammals.data.scaled %>% filter(rlm=="Indomalayan")

# Using all realms but Nearctic as training set and Nearctic as testing set
train.but.nearctic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Afrotropic" | rlm=="Australasia" | rlm=="Indomalayan" | rlm=="Neotropic" | rlm=="Palearctic")
test.nearctic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Nearctic")

# Using all realms but Neotropic as training set and Neotropic as testing set
train.but.neotropic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Afrotropic" | rlm=="Australasia" | rlm=="Indomalayan" | rlm=="Nearctic" | rlm=="Palearctic")
test.neotropic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Neotropic")

# Using all realms but Palearctic as training set and Palearctic as testing set
train.but.palearctic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Afrotropic" | rlm=="Australasia" | rlm=="Indomalayan" | rlm=="Nearctic" | rlm=="Neotropic")
test.palearctic <- ext.risk.mammals.data.scaled %>% filter(rlm=="Palearctic")

#----
# ER ~ pa
hc.pa.but.afrotropic<-glmer(rl.cat~pa+rpa+rs+gl+wa+(1|or), family=binomial, data=train.but.afrotropic)
hc.pa.but.australasia<-glmer(rl.cat~pa+rpa+rs+gl+wa+(1|or), family=binomial, data=train.but.australasia)
hc.pa.but.indomalayan<-glmer(rl.cat~pa+rpa+rs+gl+wa+(1|or), family=binomial, data=train.but.indomalayan)
hc.pa.but.nearctic<-glmer(rl.cat~pa+rpa+rs+gl+wa+(1|or), family=binomial, data=train.but.nearctic)
hc.pa.but.neotropic<-glmer(rl.cat~pa+rpa+rs+gl+wa+(1|or), family=binomial, data=train.but.neotropic)
hc.pa.but.palearctic<-glmer(rl.cat~pa+rpa+rs+gl+wa+(1|or), family=binomial, data=train.but.palearctic)

# Calculate predicted probability  for each species in test dataset
predicted.pa.but.afrotropic <- predict(hc.pa.but.afrotropic, test.afroropic, type="response", allow.new.levels = TRUE)
predicted.pa.but.australasia <- predict(hc.pa.but.australasia, test.australasia, type="response", allow.new.levels = TRUE)
predicted.pa.but.indomalayan <- predict(hc.pa.but.indomalayan, test.indomalayan, type="response", allow.new.levels = TRUE)
predicted.pa.but.nearctic <- predict(hc.pa.but.nearctic, test.nearctic, type="response", allow.new.levels = TRUE)
predicted.pa.but.neotropic <- predict(hc.pa.but.neotropic, test.neotropic, type="response", allow.new.levels = TRUE)
predicted.pa.but.palearctic <- predict(hc.pa.but.palearctic, test.palearctic, type="response", allow.new.levels = TRUE)

# Calculate AUC values
hc.pa.but.afrotropic.auc<-auc(test.afroropic$rl.cat, predicted.pa.but.afrotropic)
hc.pa.but.australasia.auc<-auc(test.australasia$rl.cat, predicted.pa.but.australasia)
hc.pa.but.indomalayan.auc<-auc(test.indomalayan$rl.cat, predicted.pa.but.indomalayan)
hc.pa.but.nearctic.auc<-auc(test.nearctic$rl.cat, predicted.pa.but.nearctic)
hc.pa.but.neotropic.auc<-auc(test.neotropic$rl.cat, predicted.pa.but.neotropic)
hc.pa.but.palearctic.auc<-auc(test.palearctic$rl.cat, predicted.pa.but.palearctic)

# Convert AUC values into a data frame
hc.pa.auc.values<-as.data.frame(rbind(hc.pa.but.afrotropic.auc[1], hc.pa.but.australasia.auc[1], hc.pa.but.indomalayan.auc[1], hc.pa.but.nearctic.auc[1], hc.pa.but.neotropic.auc[1], hc.pa.but.palearctic.auc[1]))

# Assign new name to the column of the data frame
colnames(hc.pa.auc.values) <- c("pa.auc.values")

# Calculate mean and standard error
hc.pa.auc.values.model <- lm(pa.auc.values ~ 1, hc.pa.auc.values)
coef.hc.pa.auc.values.model<-data.frame(coef(summary(hc.pa.auc.values.model)))

# Calculate confidence interval
confint.hc.pa.auc.values.model<-as.data.frame(confint(hc.pa.auc.values.model))

# Create data frame with mean AUC value and confidence interval
auc.pa<-data.frame(auc=coef.hc.pa.auc.values.model[,1], ci_lower=confint.hc.pa.auc.values.model$`2.5 %`, ci_upper=confint.hc.pa.auc.values.model$`97.5 %`)

#----
# ER ~ hi
hc.hi.but.afrotropic<-glmer(rl.cat~hi+rhi+rs+gl+wa+(1|or), family=binomial, data=train.but.afrotropic)
hc.hi.but.australasia<-glmer(rl.cat~hi+rhi+rs+gl+wa+(1|or), family=binomial, data=train.but.australasia)
hc.hi.but.indomalayan<-glmer(rl.cat~hi+rhi+rs+gl+wa+(1|or), family=binomial, data=train.but.indomalayan)
hc.hi.but.nearctic<-glmer(rl.cat~hi+rhi+rs+gl+wa+(1|or), family=binomial, data=train.but.nearctic)
hc.hi.but.neotropic<-glmer(rl.cat~hi+rhi+rs+gl+wa+(1|or), family=binomial, data=train.but.neotropic)
hc.hi.but.palearctic<-glmer(rl.cat~hi+rhi+rs+gl+wa+(1|or), family=binomial, data=train.but.palearctic)

# Calculate predicted probability for each species in test dataset
predicted.hi.but.afrotropic <- predict(hc.hi.but.afrotropic, test.afroropic, type="response", allow.new.levels = TRUE)
predicted.hi.but.australasia <- predict(hc.hi.but.australasia, test.australasia, type="response", allow.new.levels = TRUE)
predicted.hi.but.indomalayan <- predict(hc.hi.but.indomalayan, test.indomalayan, type="response", allow.new.levels = TRUE)
predicted.hi.but.nearctic <- predict(hc.hi.but.nearctic, test.nearctic, type="response", allow.new.levels = TRUE)
predicted.hi.but.neotropic <- predict(hc.hi.but.neotropic, test.neotropic, type="response", allow.new.levels = TRUE)
predicted.hi.but.palearctic <- predict(hc.hi.but.palearctic, test.palearctic, type="response", allow.new.levels = TRUE)

# Calculate AUC values
hc.hi.but.afrotropic.auc<-auc(test.afroropic$rl.cat, predicted.hi.but.afrotropic)
hc.hi.but.australasia.auc<-auc(test.australasia$rl.cat, predicted.hi.but.australasia)
hc.hi.but.indomalayan.auc<-auc(test.indomalayan$rl.cat, predicted.hi.but.indomalayan)
hc.hi.but.nearctic.auc<-auc(test.nearctic$rl.cat, predicted.hi.but.nearctic)
hc.hi.but.neotropic.auc<-auc(test.neotropic$rl.cat, predicted.hi.but.neotropic)
hc.hi.but.palearctic.auc<-auc(test.palearctic$rl.cat, predicted.hi.but.palearctic)

# Convert AUC values into a data frame
hc.hi.auc.values<-as.data.frame(rbind(hc.hi.but.afrotropic.auc[1], hc.hi.but.australasia.auc[1], hc.hi.but.indomalayan.auc[1], hc.hi.but.nearctic.auc[1], hc.hi.but.neotropic.auc[1], hc.hi.but.palearctic.auc[1]))

# Assign new name to the column of the data frame
colnames(hc.hi.auc.values) <- c("hi.auc.values")

# Calculate mean and standard error
hc.hi.auc.values.model <- lm(hi.auc.values ~ 1, hc.hi.auc.values)
coef.hc.hi.auc.values.model<-data.frame(coef(summary(hc.hi.auc.values.model)))

# Calculate confidence interval
confint.hc.hi.auc.values.model<-as.data.frame(confint(hc.hi.auc.values.model))

# Create data frame with mean AUC value and confidence interval
auc.hi<-data.frame(auc=coef.hc.hi.auc.values.model[,1], ci_lower=confint.hc.hi.auc.values.model$`2.5 %`, ci_upper=confint.hc.hi.auc.values.model$`97.5 %`)

#----
# ER ~ pi
hc.pi.but.afrotropic<-glmer(rl.cat~pi+rpi+rs+gl+wa+(1|or), family=binomial, data=train.but.afrotropic)
hc.pi.but.australasia<-glmer(rl.cat~pi+rpi+rs+gl+wa+(1|or), family=binomial, data=train.but.australasia)
hc.pi.but.indomalayan<-glmer(rl.cat~pi+rpi+rs+gl+wa+(1|or), family=binomial, data=train.but.indomalayan)
hc.pi.but.nearctic<-glmer(rl.cat~pi+rpi+rs+gl+wa+(1|or), family=binomial, data=train.but.nearctic)
hc.pi.but.neotropic<-glmer(rl.cat~pi+rpi+rs+gl+wa+(1|or), family=binomial, data=train.but.neotropic)
hc.pi.but.palearctic<-glmer(rl.cat~pi+rpi+rs+gl+wa+(1|or), family=binomial, data=train.but.palearctic)

# Calculate predicted probability for each species in test dataset
predicted.pi.but.afrotropic <- predict(hc.pi.but.afrotropic, test.afroropic, type="response", allow.new.levels = TRUE)
predicted.pi.but.australasia <- predict(hc.pi.but.australasia, test.australasia, type="response", allow.new.levels = TRUE)
predicted.pi.but.indomalayan <- predict(hc.pi.but.indomalayan, test.indomalayan, type="response", allow.new.levels = TRUE)
predicted.pi.but.nearctic <- predict(hc.pi.but.nearctic, test.nearctic, type="response", allow.new.levels = TRUE)
predicted.pi.but.neotropic <- predict(hc.pi.but.neotropic, test.neotropic, type="response", allow.new.levels = TRUE)
predicted.pi.but.palearctic <- predict(hc.pi.but.palearctic, test.palearctic, type="response", allow.new.levels = TRUE)

# Calculate AUC values
hc.pi.but.afrotropic.auc<-auc(test.afroropic$rl.cat, predicted.pi.but.afrotropic)
hc.pi.but.australasia.auc<-auc(test.australasia$rl.cat, predicted.pi.but.australasia)
hc.pi.but.indomalayan.auc<-auc(test.indomalayan$rl.cat, predicted.pi.but.indomalayan)
hc.pi.but.nearctic.auc<-auc(test.nearctic$rl.cat, predicted.pi.but.nearctic)
hc.pi.but.neotropic.auc<-auc(test.neotropic$rl.cat, predicted.pi.but.neotropic)
hc.pi.but.palearctic.auc<-auc(test.palearctic$rl.cat, predicted.pi.but.palearctic)

# Convert AUC values into a data frame
hc.pi.auc.values<-as.data.frame(rbind(hc.pi.but.afrotropic.auc[1], hc.pi.but.australasia.auc[1], hc.pi.but.indomalayan.auc[1], hc.pi.but.nearctic.auc[1], hc.pi.but.neotropic.auc[1], hc.pi.but.palearctic.auc[1]))

# Assign new name to the column of the data frame
colnames(hc.pi.auc.values) <- c("pi.auc.values")

# Calculate mean and standard error
hc.pi.auc.values.model <- lm(pi.auc.values ~ 1, hc.pi.auc.values)
coef.hc.pi.auc.values.model<-data.frame(coef(summary(hc.pi.auc.values.model)))

# Claculate confidence interval
confint.hc.pi.auc.values.model<-as.data.frame(confint(hc.pi.auc.values.model))

# Create data frame with AUC value and confidence interval
auc.pi<-data.frame(auc=coef.hc.pi.auc.values.model[,1], ci_lower=confint.hc.pi.auc.values.model$`2.5 %`, ci_upper=confint.hc.pi.auc.values.model$`97.5 %`)

#----
# Combine data frames with mean AUC values and confidence intervals together
auc.pahipi<-rbind(auc.pa, auc.hi, auc.pi)

# Assign landscape model and habitat breadth
auc.pahipi$parameter<-c("pa", "hi", "pi")
land.models<-c("Patch-matrix", "Continuum", "Hybrid")
auc.pahipi$land.model<-land.models
auc.pahipi$habitat_breadth <- "Specialists"

# Save AUC values and confidence intervals
write.table(auc.pahipi, file="auc_spatially_blocked_cv_pahipi_specialists.csv", sep="\t", row.names=F)
