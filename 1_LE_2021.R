# libraries required:
library(tidyverse)
library(DemoDecomp)
library(dplyr)
library(reshape)
library(data.table)

## Set the working directory
setwd("C:/Users/yzhu/OneDrive - ARG/SIMAH project/Charlotte Probst/Life expectancy decomposition/")

## Load the functions that go with this code
source("SIMAH_code/life_expectancy/2b_decomp_functions.R")

#############################################################################################################
#before calculating the life expectancy, we have to get the mortality data into the right format

#Select ACS vs CPS

#load aggregated mortality data:
dMort <- read.csv("SIMAH_workplace/mortality/3_out data/allethn_sumCOD_1821_LE_decomp.csv")
table(dMort$year)


#read in population data
# ACS <- read.csv("ACS_popcounts_2000_2021_updated.csv") # question for updated data
# table(ACS$year)
# ACS_pred <-  read.csv("ACS_popcounts_predicted2020.csv")  
# CPS <- read.csv("CPS_2000_2020_agegp.csv")
# ACS_weights <- readRDS("rep_weights_2020.RDS")
# MSIM <- read.csv("pop_counts_simulation_2000_2020.csv")

ACS <- read.csv("SIMAH_workplace/ACS/ACS_popcounts_2000_2021_updated.csv")
ACS_pred <-  read.csv("SIMAH_workplace/ACS/ACS_popcounts_predicted2020.csv")  
CPS <- read.csv("SIMAH_workplace/CPS/3_out CPS data/CPS_2000_2020_agegp.csv")
ACS_weights <- readRDS("SIMAH_workplace/ACS/rep_weights_2020.RDS")
#MSIM <- read.csv("SIMAH_workplace/microsim/pop_counts_simulation_2000_2020.csv") # missing

#############################################################################################################
# Specify which population counts and which level of detail should be computed
k.pop_type <- "ACS" # "ACS", "ACS_pred" or "CPS". ACS Weights are treated separately below. 
k.run <- "detail" # "ses" or "detail" or "total"
k.weights <- FALSE 

#load population data (raw vs modeled)
if(k.pop_type=="ACS"){
   dPop <- ACS
   #dPop <- dPop %>% 
     #filter(state == "USA") %>% 
      #select(-c(state)) 
}else if(k.pop_type=="ACS_pred"){
   dPop <- ACS_pred 
   dPop <- dPop %>% filter(state == "USA") %>% 
      select(-c(state)) 
   
}else if (k.pop_type == "CPS") {
   dPop <- CPS
}
str(ACS)

dMort <- inner_join(dMort, dPop)

# variable type should be factor and not character
glimpse(dMort)
dMort <- dMort %>% mutate_at(vars(race, edclass), as.factor)

## Prepare vectors of variable names containing all causes of death
v.totals <- names(dMort)[grepl("mort", names(dMort))]
v.rates <- str_replace(v.totals, "mort", "rate") 
v.rates <- c(v.rates[1], paste0("mx_", v.rates[2:(length(v.rates))]))   

## Aggregate: summarize the data to collapse one demographic dimension 
## Specify all factor variables you want to keep (and omit the one 
## you want to collapse)
dMort_t <- aggregate(.~ year + sex +                  age_gp, data =  dMort, FUN=sum)
dMort_s <- aggregate(.~ year + sex + edclass +        age_gp, data =  dMort, FUN=sum)
dMort_d <- aggregate(.~ year + sex + edclass + race + age_gp, data =  dMort, FUN=sum)

str(dMort_d)

if (k.run == "total") {
   dMort_run <- dMort_t 
   sel.vars <- c("year", "age_gp", "sex", v.rates) 
   # Generate a variable to loop over
   dMort_run$group <- dMort_run$sex
} else if (k.run == "ses") {
   dMort_run <- dMort_s
   sel.vars <- c("year", "age_gp", "sex", "edclass", v.rates) 
   # Generate a variable to loop over
   dMort_run$group <- apply(dMort_run[ , c("sex", "edclass") ] , 1 , paste , collapse = "_" )
} else if (k.run == "detail") {
   dMort_run <- dMort_d
   sel.vars <- c("year", "age_gp", "sex", "edclass", "race", v.rates) 
   # Generate a variable to loop over
   dMort_run$group <- apply(dMort_run[ , c("sex", "edclass", "race") ] , 1 , paste , collapse = "_" )
}


# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
   dMort_run[, v.rates[i]] <- (dMort_run[, v.totals[i]]/dMort_run$TPop)
} 

# now you can loop over the unique values of this new variable
v.group <- unique(dMort_run$group)

v.year1 <- c(2018:2021)
for (j in (1:length(v.year1))){
  year1 <- v.year1[j]
  for(i in 1:length(v.group)) {
    US_y1 <- filter(dMort_run, year==year1 &  group == v.group[i]) 
    US_y1 <- US_y1[, sel.vars] 
    US_y1$ax = c(3.5,rep(2.5,11), 6.99) # midpoint of age range. Check age groups again. 
    US_y1_vector <- unlist(select(US_y1,starts_with("mx_")))
    le_results <-  life_table_causes(nmx_by_cause_vector = US_y1_vector, age_vector = pull(US_y1,age_gp),
                                     ax_vector = pull(US_y1,ax))
    le_results <- as.data.frame(c(le_results + min(pull(US_y1, age_gp)))) # I am adding the age at baseline here
    le_results$year <- v.year1[j]
    le_results$group <- v.group[i]
    
    if (j == 1 & i == 1) {
      dle_results <- le_results
    } else {
      dle_results <- rbind(dle_results, le_results)
    }      
  }
}


if (k.run == "total") {
   names(dle_results) <-  c("Life_expectancy", "Year", "Sex")
} else if (k.run == "ses") {
   dle_results <- separate(dle_results, col = group, into = c("sex","edclass"), sep = "_")
   dle_results$edclass <- factor(dle_results$edclass, 
                                 levels = c( "LEHS", "SomeC", "College"))
   dle_results <- dle_results[order(dle_results$year, dle_results$sex, dle_results$edclass), ]
   names(dle_results) <-  c("Life_expectancy", "Year", "Sex", "SES" )
   
} else if (k.run == "detail") {
   dle_results <- separate(dle_results, col = group, into = c("sex","edclass", "race"), sep = "_")
   dle_results$edclass <- factor(dle_results$edclass, 
                                 levels = c( "LEHS", "SomeC", "College"))
   dle_results <- dle_results[order(dle_results$year, dle_results$sex, dle_results$edclass, dle_results$race), ]
   names(dle_results) <-  c("Life_expectancy", "Year", "Sex", "SES", "Race" )
}

write.csv(dle_results, 
          paste0("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/", 
                 "LifeExpectancy_1821_", k.run, "_", k.pop_type, ".csv"), 
          row.names = FALSE)
