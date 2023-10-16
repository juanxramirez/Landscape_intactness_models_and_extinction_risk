# Load required packages
library(lme4)
library(dplyr)
library(ggeffects)
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

# Create data frame for extinction risk (ER) models
ext.risk.mammals.data<-data.frame(or=threatened.under.criterionB.excluded.gain.excluded.na.omit$order,
                                  pa=threatened.under.criterionB.excluded.gain.excluded.na.omit$patch_area,
                                  rpa=threatened.under.criterionB.excluded.gain.excluded.na.omit$change_patch_area,
                                  hi=threatened.under.criterionB.excluded.gain.excluded.na.omit$habitat_intactness_p95,
                                  rhi=threatened.under.criterionB.excluded.gain.excluded.na.omit$change_habitat_intactness_p95,
                                  pi=threatened.under.criterionB.excluded.gain.excluded.na.omit$patch_intactness_p95,
                                  rpi=threatened.under.criterionB.excluded.gain.excluded.na.omit$change_patch_intactness_p95,
                                  rs=threatened.under.criterionB.excluded.gain.excluded.na.omit$range_size,
                                  gl=threatened.under.criterionB.excluded.gain.excluded.na.omit$gestation_length,
                                  wa=threatened.under.criterionB.excluded.gain.excluded.na.omit$weaning_age,
                                  rl.cat=threatened.under.criterionB.excluded.gain.excluded.na.omit$rl_cat)

# Scale the numerical variables
num_vars <- sapply(ext.risk.mammals.data, is.numeric)
ext.risk.mammals.data.scaled <- ext.risk.mammals.data
ext.risk.mammals.data.scaled[, num_vars] <- scale(ext.risk.mammals.data[, num_vars])

#----
# Set ER models
pa.glmer<-glmer(rl.cat~pa+rpa+rs+gl+wa+(1|or), family=binomial, data=ext.risk.mammals.data.scaled)
hi.glmer<-glmer(rl.cat~hi+rhi+rs+gl+wa+(1|or), family=binomial, data=ext.risk.mammals.data.scaled)
pi.glmer<-glmer(rl.cat~pi+rpi+rs+gl+wa+(1|or), family=binomial, data=ext.risk.mammals.data.scaled)

# Extract coefficients for pa, rpa, hi, rhi, pi, and rpi while holding other predictors constant at their mean
coef.pa<-fixef(pa.glmer)["pa"]
coef.rpa<-fixef(pa.glmer)["rpa"]
coef.hi<-fixef(hi.glmer)["hi"]
coef.rhi<-fixef(hi.glmer)["rhi"]
coef.pi<-fixef(pi.glmer)["pi"]
coef.rpi<-fixef(pi.glmer)["rpi"]

# Extract the confidence interval for the estimated coefficient of the predictor of interest
coef.pa.ci<-confint(pa.glmer, method="Wald")["pa",]
coef.rpa.ci<-confint(pa.glmer, method="Wald")["rpa",]
coef.hi.ci<-confint(hi.glmer, method="Wald")["hi",]
coef.rhi.ci<-confint(hi.glmer, method="Wald")["rhi",]
coef.pi.ci<-confint(pi.glmer, method="Wald")["pi",]
coef.rpi.ci<-confint(pi.glmer, method="Wald")["rpi",]

# Create data frame with extracted coefficients
pa.effectsize.table <- data.frame(parameter = "pa", std_coefficient=coef.pa[[1]], ci_lower=coef.pa.ci[[1]], ci_upper=coef.pa.ci[[2]])
rpa.effectsize.table <- data.frame(parameter = "rpa", std_coefficient=coef.rpa[[1]], ci_lower=coef.rpa.ci[[1]], ci_upper=coef.rpa.ci[[2]])
hi.effectsize.table <- data.frame(parameter = "hi", std_coefficient=coef.hi[[1]], ci_lower=coef.hi.ci[[1]], ci_upper=coef.hi.ci[[2]])
rhi.effectsize.table <- data.frame(parameter = "rhi", std_coefficient=coef.rhi[[1]], ci_lower=coef.rhi.ci[[1]], ci_upper=coef.rhi.ci[[2]])
pi.effectsize.table <- data.frame(parameter = "pi", std_coefficient=coef.pi[[1]], ci_lower=coef.pi.ci[[1]], ci_upper=coef.pi.ci[[2]])
rpi.effectsize.table <- data.frame(parameter = "rpi", std_coefficient=coef.rpi[[1]], ci_lower=coef.rpi.ci[[1]], ci_upper=coef.rpi.ci[[2]])

# Combine data frames together
pahipi.effectsize.table<-rbind(pa.effectsize.table, hi.effectsize.table, pi.effectsize.table)
rparhirpi.effectsize.table<-rbind(rpa.effectsize.table, rhi.effectsize.table, rpi.effectsize.table)

# Assign landscape model and habitat breadth
land.models<-c("Patch-matrix", "Continuum", "Hybrid")
pahipi.effectsize.table$land.model<-land.models
pahipi.effectsize.table$habitat_breadth <- "Specialists"
rparhirpi.effectsize.table$land.model<-land.models
rparhirpi.effectsize.table$habitat_breadth <- "Specialists"

# Save effect sizes
write.table(pahipi.effectsize.table, file="effectsize_pahipi_specialists_threatened_criterionB_excluded.csv", sep="\t", row.names=F)
write.table(rparhirpi.effectsize.table, file="effectsize_rparhirpi_specialists_threatened_criterionB_excluded.csv", sep="\t", row.names=F)
