# Load required packages
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)

# Set working directory
setwd("My_working_directory")

# Import data
highHFP.drivers<-read.delim("species_data.txt")

# Recode red list categories as numeric
highHFP.drivers[,32] <- ifelse(highHFP.drivers[,32] == "LC", 0,
                              ifelse(highHFP.drivers[,32] == "NT", 0,
                                     ifelse(highHFP.drivers[,32] == "VU", 1,
                                            ifelse(highHFP.drivers[,32] == "EN", 1,
                                                   ifelse(highHFP.drivers[,32] == "CR", 1, NA)))))

# Convert categorical variables to factor
highHFP.drivers$order<-as.factor(highHFP.drivers$order)
highHFP.drivers$rl_cat<-as.factor(highHFP.drivers$rl_cat)

# Select variables from dataset
vars<-c("order",
        "habitat_breadth",
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
highHFP.drivers.gain.excluded <- highHFP.drivers %>%
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

# Remove species with missing values
highHFP.drivers.gain.excluded.na.omit<-na.omit(highHFP.drivers.gain.excluded[,vars])

# Extract data for generalist species
generalists<-highHFP.drivers.gain.excluded.na.omit %>% filter(habitat_breadth>1)

# Extract data for specialist species
specialists<-highHFP.drivers.gain.excluded.na.omit %>% filter(habitat_breadth==1)

# Create data frame for generalists
data.generalists<-data.frame(PA=scale(generalists$patch_area),
                             RPA=scale(generalists$change_patch_area),
                             HI=scale(generalists$habitat_intactness_p95),
                             RHI=scale(generalists$change_habitat_intactness_p95),
                             PI=scale(generalists$patch_intactness_p95),
                             RPI=scale(generalists$change_patch_intactness_p95),
                             RS=scale(generalists$range_size),
                             GL=scale(generalists$gestation_length),
                             WA=scale(generalists$weaning_age))

# Create data frame for specialists
data.specialists<-data.frame(PA=scale(specialists$patch_area),
                             RPA=scale(specialists$change_patch_area),
                             HI=scale(specialists$habitat_intactness_p95),
                             RHI=scale(specialists$change_habitat_intactness_p95),
                             PI=scale(specialists$patch_intactness_p95),
                             RPI=scale(specialists$change_patch_intactness_p95),
                             RS=scale(specialists$range_size),
                             GL=scale(specialists$gestation_length),
                             WA=scale(specialists$weaning_age))

# Visual inspection of the data normality using Q-Q plots (quantile-quantile plots)
# Q-Q plots for pa
ggqqplot(data.generalists$PA, ylab = "Patch area")
ggqqplot(data.specialists$PA, ylab = "Patch area")

# Q-Q plots for hi
ggqqplot(data.generalists$HI, ylab = "Habitat intactness")
ggqqplot(data.specialists$HI, ylab = "Habitat intactness")

# Q-Q plots for pi
ggqqplot(data.generalists$PI, ylab = "Patch intactness")
ggqqplot(data.specialists$PI, ylab = "Patch intactness")

# Compute correlation matrix
corr.generalists <- cor(data.generalists,  method="spearman")
corr.specialists <- cor(data.specialists,  method="spearman")

# Save correlation matrix
write.table(corr.generalists, file="corr_generalists.csv", sep="\t", row.names=F)
write.table(corr.specialists, file="corr_specialists.csv", sep="\t", row.names=F)
