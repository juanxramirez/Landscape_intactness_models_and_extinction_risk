# Load required packages
library(lme4)
library(dplyr)
library(ggplot2)

# Set working directory
setwd("My_working_directory")

# Import data
highHFP.drivers<-read.delim("species_data.txt")

# Extract data for specialists
specialists<-highHFP.drivers %>% filter(habitat_breadth==1)

# Recode red list categories as numeric
specialists[,32] <- ifelse(specialists[,32] == "LC", 0,
                           ifelse(specialists[,32] == "NT", 0,
                                  ifelse(specialists[,32] == "VU", 1,
                                         ifelse(specialists[,32] == "EN", 1,
                                                ifelse(specialists[,32] == "CR", 1, NA)))))

# Convert categorical variables to factor
specialists$order<-as.factor(specialists$order)
specialists$rl_cat<-as.factor(specialists$rl_cat)

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
specialists.gain.excluded <- specialists %>%
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
specialists.gain.excluded.na.omit<-na.omit(specialists.gain.excluded[,vars])

# Create data frame for extinction risk (ER) models
data.specialists<-data.frame(or=specialists.gain.excluded.na.omit$order,
			     hip5=specialists.gain.excluded.na.omit$habitat_intactness_p5,
			     hip10=specialists.gain.excluded.na.omit$habitat_intactness_p10,
			     hip50=specialists.gain.excluded.na.omit$habitat_intactness_p50,
			     hip90=specialists.gain.excluded.na.omit$habitat_intactness_p90,
			     hip95=specialists.gain.excluded.na.omit$habitat_intactness_p95,
			     rhip5=specialists.gain.excluded.na.omit$change_habitat_intactness_p5,
			     rhip10=specialists.gain.excluded.na.omit$change_habitat_intactness_p10,
			     rhip50=specialists.gain.excluded.na.omit$change_habitat_intactness_p50,
			     rhip90=specialists.gain.excluded.na.omit$change_habitat_intactness_p90,
			     rhip95=specialists.gain.excluded.na.omit$change_habitat_intactness_p95,
			     pip5=specialists.gain.excluded.na.omit$patch_intactness_p5,
			     pip10=specialists.gain.excluded.na.omit$patch_intactness_p10,
			     pip50=specialists.gain.excluded.na.omit$patch_intactness_p50,
			     pip90=specialists.gain.excluded.na.omit$patch_intactness_p90,
			     pip95=specialists.gain.excluded.na.omit$patch_intactness_p95,
			     rpip5=specialists.gain.excluded.na.omit$change_patch_intactness_p5,
			     rpip10=specialists.gain.excluded.na.omit$change_patch_intactness_p10,
			     rpip50=specialists.gain.excluded.na.omit$change_patch_intactness_p50,
			     rpip90=specialists.gain.excluded.na.omit$change_patch_intactness_p90,
			     rpip95=specialists.gain.excluded.na.omit$change_patch_intactness_p95,
			     rs=specialists.gain.excluded.na.omit$range_size,
			     gl=specialists.gain.excluded.na.omit$gestation_length,
			     wa=specialists.gain.excluded.na.omit$weaning_age,
			     rl.cat=specialists.gain.excluded.na.omit$rl_cat)

# Scale the numerical variables
num_vars <- sapply(data.specialists, is.numeric)
data.specialists.scaled <- data.specialists
data.specialists.scaled[, num_vars] <- scale(data.specialists[, num_vars])

#----
# ER ~ habitat intactness (hi) + reduction in habitat intactness (rhi) + ...
hip5.glmer<-glmer(rl.cat~hip5+rhip5+rs+gl+wa+(1|or), family=binomial, data=data.specialists.scaled)
hip10.glmer<-glmer(rl.cat~hip10+rhip10+rs+gl+wa+(1|or), family=binomial, data=data.specialists.scaled)
hip50.glmer<-glmer(rl.cat~hip50+rhip50+rs+gl+wa+(1|or), family=binomial, data=data.specialists.scaled)
hip90.glmer<-glmer(rl.cat~hip90+rhip90+rs+gl+wa+(1|or), family=binomial, data=data.specialists.scaled)
hip95.glmer<-glmer(rl.cat~hip95+rhip95+rs+gl+wa+(1|or), family=binomial, data=data.specialists.scaled)

# Extract coefficients for hi while holding other predictors constant at their mean
coef.hip5<-fixef(hip5.glmer)["hip5"]
coef.hip10<-fixef(hip10.glmer)["hip10"]
coef.hip50<-fixef(hip50.glmer)["hip50"]
coef.hip90<-fixef(hip90.glmer)["hip90"]
coef.hip95<-fixef(hip95.glmer)["hip95"]

# Extract the confidence interval for the estimated coefficient of the predictor of interest
coef.hip5.ci<-confint(hip5.glmer, method="Wald")["hip5",]
coef.hip10.ci<-confint(hip10.glmer, method="Wald")["hip10",]
coef.hip50.ci<-confint(hip50.glmer, method="Wald")["hip50",]
coef.hip90.ci<-confint(hip90.glmer, method="Wald")["hip90",]
coef.hip95.ci<-confint(hip95.glmer, method="Wald")["hip95",]

# Create a new data frame with a single row and column
hip5.effectsize.table <- data.frame(parameter = "5th", std_coefficient=coef.hip5[[1]], ci_lower=coef.hip5.ci[[1]], ci_upper=coef.hip5.ci[[2]])
hip10.effectsize.table <- data.frame(parameter = "10th", std_coefficient=coef.hip10[[1]], ci_lower=coef.hip10.ci[[1]], ci_upper=coef.hip10.ci[[2]])
hip50.effectsize.table <- data.frame(parameter = "50th", std_coefficient=coef.hip50[[1]], ci_lower=coef.hip50.ci[[1]], ci_upper=coef.hip50.ci[[2]])
hip90.effectsize.table <- data.frame(parameter = "90th", std_coefficient=coef.hip90[[1]], ci_lower=coef.hip90.ci[[1]], ci_upper=coef.hip90.ci[[2]])
hip95.effectsize.table <- data.frame(parameter = "95th", std_coefficient=coef.hip95[[1]], ci_lower=coef.hip95.ci[[1]], ci_upper=coef.hip95.ci[[2]])

# Combine the multiple group of rows together
hi.effectsizes.table<-rbind(hip5.effectsize.table, hip10.effectsize.table, hip50.effectsize.table, hip90.effectsize.table, hip95.effectsize.table)

# Assign group, landscape model and habitat breadth
hi.effectsizes.table$group <- "hi"
hi.effectsizes.table$land.model <- "Continuum"
hi.effectsizes.table$habitat_breadth <- "Specialists"

#----
# ER ~ patch intactness (pi) + reduction in patch intactness (rpi) + ...
pip5.glmer<-glmer(rl.cat~pip5+rpip5+rs+gl+wa+(1|or), family=binomial, data=data.specialists.scaled)
pip10.glmer<-glmer(rl.cat~pip10+rpip10+rs+gl+wa+(1|or), family=binomial, data=data.specialists.scaled)
pip50.glmer<-glmer(rl.cat~pip50+rpip50+rs+gl+wa+(1|or), family=binomial, data=data.specialists.scaled)
pip90.glmer<-glmer(rl.cat~pip90+rpip90+rs+gl+wa+(1|or), family=binomial, data=data.specialists.scaled)
pip95.glmer<-glmer(rl.cat~pip95+rpip95+rs+gl+wa+(1|or), family=binomial, data=data.specialists.scaled)

# Extract coefficients for pi while holding other predictors constant at their mean
coef.pip5<-fixef(pip5.glmer)["pip5"]
coef.pip10<-fixef(pip10.glmer)["pip10"]
coef.pip50<-fixef(pip50.glmer)["pip50"]
coef.pip90<-fixef(pip90.glmer)["pip90"]
coef.pip95<-fixef(pip95.glmer)["pip95"]

# Extract the confidence interval for the estimated coefficient of the predictor of interest
coef.pip5.ci<-confint(pip5.glmer, method="Wald")["pip5",]
coef.pip10.ci<-confint(pip10.glmer, method="Wald")["pip10",]
coef.pip50.ci<-confint(pip50.glmer, method="Wald")["pip50",]
coef.pip90.ci<-confint(pip90.glmer, method="Wald")["pip90",]
coef.pip95.ci<-confint(pip95.glmer, method="Wald")["pip95",]

# Create a new data frame with a single row and column
pip5.effectsize.table <- data.frame(parameter = "5th", std_coefficient=coef.pip5[[1]], ci_lower=coef.pip5.ci[[1]], ci_upper=coef.pip5.ci[[2]])
pip10.effectsize.table <- data.frame(parameter = "10th", std_coefficient=coef.pip10[[1]], ci_lower=coef.pip10.ci[[1]], ci_upper=coef.pip10.ci[[2]])
pip50.effectsize.table <- data.frame(parameter = "50th", std_coefficient=coef.pip50[[1]], ci_lower=coef.pip50.ci[[1]], ci_upper=coef.pip50.ci[[2]])
pip90.effectsize.table <- data.frame(parameter = "90th", std_coefficient=coef.pip90[[1]], ci_lower=coef.pip90.ci[[1]], ci_upper=coef.pip90.ci[[2]])
pip95.effectsize.table <- data.frame(parameter = "95th", std_coefficient=coef.pip95[[1]], ci_lower=coef.pip95.ci[[1]], ci_upper=coef.pip95.ci[[2]])

# Combine the multiple group of rows together
pi.effectsizes.table<-rbind(pip5.effectsize.table, pip10.effectsize.table, pip50.effectsize.table, pip90.effectsize.table, pip95.effectsize.table)

# Assign group, landscape model and habitat breadth
pi.effectsizes.table$group <- "pi"
pi.effectsizes.table$land.model <- "Hybrid"
pi.effectsizes.table$habitat_breadth <- "Specialists"

# Combine effect sizes for hi and pi together
hipi.effectsize.table<-rbind(hi.effectsizes.table, pi.effectsizes.table)

# Save effect sizes
write.table(hipi.effectsize.table, file="effectsize_hipi_specialists.csv", sep="\t", row.names=F)
