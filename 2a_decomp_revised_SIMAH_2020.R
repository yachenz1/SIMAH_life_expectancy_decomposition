# Calculate LE decomposition for 2019 to 2020 and 2020 to 2021 - change years accordingly when uploading and saving data
# by SES and race and ethnicity 
# Mortality data NCHS
# Population data: ACS 
# Project: SIMAH


# libraries required:
library("dplyr")
library("DemoDecomp")
library("tidyverse")

## Set the working directory
setwd("C:/Users/yzhu/OneDrive - ARG/SIMAH project/Charlotte Probst/Life expectancy decomposition/")


## Load the functions that go with this code
source("SIMAH_code/life_expectancy/2b_decomp_functions.R")

#load aggregated mortality data:
dMort <- read.csv("SIMAH_workplace/mortality/3_out data/allethn_sumCOD_1821_LE_decomp.csv")
#read in population data

ACS <- read.csv("SIMAH_workplace/ACS/ACS_popcounts_2000_2021.csv")
ACS_pred <-  read.csv("SIMAH_workplace/ACS/ACS_popcounts_predicted2020.csv")  
CPS <- read.csv("SIMAH_workplace/CPS/3_out CPS data/CPS_2000_2020_agegp.csv")
ACS_weights <- readRDS("SIMAH_workplace/ACS/rep_weights_2020.RDS")

#############################################################################################################
# Specify which population counts and which level of detail should be computed
k.pop_type <- "CPS" # "ACS", "ACS_pred" or "CPS". ACS Weights are treated separately below. 
k.run <- "detail" # "ses" or "detail"
k.weights <- FALSE 

#load population data (raw vs modeled)
if(k.pop_type=="ACS"){
  dPop <- ACS
}else if(k.pop_type=="ACS_pred"){
  dPop <- ACS_pred 
}else if (k.pop_type == "CPS") {
  dPop <- CPS
}


if (k.pop_type != "CPS") {
  dPop <- dPop %>% filter(state == "USA", year > 2017) %>%
    select(!state)
} else {
  dPop <- dPop %>% filter(year > 2017)
}
dMort <- dMort %>% inner_join(dPop)

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
dMort_s <- aggregate(.~ year + sex + edclass +        age_gp, data =  dMort, FUN=sum)
dMort_d <- aggregate(.~ year + sex + edclass + race + age_gp, data =  dMort, FUN=sum)

if (k.run == "ses") {
  dMort_run <- dMort_s
  sel.vars <- c("year", "age_gp", "sex", "edclass", v.rates) 
  # Generate a variable to loop over
  dMort_run$group <- apply(dMort_run[ , c("sex", "edclass") ] , 1 , paste , collapse = "_" )
} else if (k.run == "detail") {
  dMort_run <- dMort_d
  sel.vars <- c("year", "age_gp", "sex", "edclass", "race", v.rates) 
  # Generate a variable to loop over
  dMort_run$group <- as.factor(apply(dMort_run[ , c("sex", "edclass", "race") ] , 1 , paste , collapse = "_" ))
}

# Calculate the rates for all relevant causes of death
for (i in 1:length(v.totals)){
  dMort_run[, v.rates[i]] <- (dMort_run[, v.totals[i]]/dMort_run$TPop)
} 

# now you can loop over the unique values of the group variable for selected years
v.group <- unique(dMort_run$group)
v.year1 <- c(2019) 
v.year2 <- c(2020)
# v.year1 <- c(2020) 
# v.year2 <- c(2021)

for (j in (1:length(v.year1))){
  year1 <- v.year1[j]
  year2 <- v.year2[j]
  for(i in 1:length(v.group)) {
    US_y1 <- filter(dMort_run, year==year1 &  group == v.group[i]) ## this one would then be i
    US_y2 <- filter(dMort_run, year==year2 &  group == v.group[i])
    
    US_y1 <- US_y1[, sel.vars] #to delete several columns at once. comment: This kept Trate. in our out?
    US_y2 <- US_y2[, sel.vars]
    US_y1$ax = c(3.5,rep(2.5,11), 6.99) # midpoint of age range. Check age groups again. 
    US_y2$ax = c(3.5,rep(2.5,11), 6.99)
    
    # to start the first step of the decomposition
    US_y1_vector <- unlist(select(US_y1,starts_with("mx_")))
    US_y2_vector <- unlist(select(US_y2,starts_with("mx_")))
    decomp_results <-  horiuchi(func = life_table_causes,
                                pars1 = US_y1_vector,
                                pars2 = US_y2_vector,
                                age_vector = pull(US_y1,age_gp),
                                ax_vector = (pull(US_y1,ax) + pull(US_y2,ax))/2,
                                N = 100)
    decomp_results_mat <- matrix(decomp_results,ncol = length(v.totals[-1])) # -1 to remove Tmort
    
    #Sum over age - this gives the contribution of each cause in terms of years of LE. Note that this should sum to difference LE between the beg and start periods
    cause_contributions <- apply(decomp_results_mat,2,sum)
    cause_contributions <-  array(cause_contributions, dim = c(1, length(cause_contributions)))
    temp_contrib <- as.data.frame(cause_contributions) 
    colnames(temp_contrib) <- v.totals[-1]
    temp_contrib$group <- v.group[i]
    temp_contrib$start_year <- v.year1[j]
    temp_contrib$end_year <- v.year2[j]
    temp_contrib$LE1 <-  life_table_causes(nmx_by_cause_vector = US_y1_vector, age_vector = pull(US_y1,age_gp),
                                           ax_vector = pull(US_y1, ax)) + min(pull(US_y1, age_gp))
    temp_contrib$LE2 <-  life_table_causes(nmx_by_cause_vector = US_y2_vector, age_vector = pull(US_y2,age_gp),
                                           ax_vector = pull(US_y2, ax)) + min(pull(US_y2, age_gp))
    
    if (i == 1 & j == 1) {
      dResults_contrib <- temp_contrib
    } else {
      dResults_contrib <- rbind(temp_contrib, dResults_contrib)
    }
  } 
}

if (k.run == "ses") {
  dResults_contrib <- separate(dResults_contrib, col = group, into = c("sex","edclass"), sep = "_")
  dResults_contrib$edclass <- factor(dResults_contrib$edclass, 
                                     levels = c( "LEHS", "SomeC", "College"))
  dResults_contrib <- dResults_contrib[order(dResults_contrib$end_year, dResults_contrib$sex, dResults_contrib$edclass), ]
} else if (k.run == "detail") {
  dResults_contrib <- separate(dResults_contrib, col = group, into = c("sex","edclass", "race"), sep = "_")
  dResults_contrib$edclass <- factor(dResults_contrib$edclass, 
                                     levels = c( "LEHS", "SomeC", "College"))
  dResults_contrib <- dResults_contrib[order(dResults_contrib$end_year, dResults_contrib$sex, dResults_contrib$edclass, dResults_contrib$race), ]
}

write.csv(dResults_contrib, 
          paste0("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/Results_contrib_", 
                 v.year1[1], "_", max(v.year2), "_",
                 k.run, "_", k.pop_type, ".csv"),
          row.names = FALSE)

write.csv(dMort_run, 
          paste0("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/dMort_",
                 v.year1[1], "_", max(v.year2), "_", 
                 k.run, "_", k.pop_type, ".csv"), 
          row.names = FALSE)

## Get aggregated mortality data 
if (k.run == "detail" & k.pop_type == "ACS") {
  dMort_out <- aggregate(.~ year + sex + edclass + race , data =  dMort_run, FUN=sum)
  dMort_out <- dMort_out %>% 
    mutate_at(vars(race, sex, edclass), as.factor)
  names(dMort_out) <- gsub(pattern = "mort", replacement = "", x = names(dMort_out)) 
  levels(dMort_out$edclass) <- list("High" = "College", "Mid." = "SomeC", "Low" = "LEHS")
  levels(dMort_out$sex) <- list(Men = "1", Women = "2")
  dMort_out <- dMort_out %>% select(!c(group, age_gp)) %>% subset(year > 2018, race !="Other") %>%
    dplyr::rename("Total" = "t",
                  "Covid 19" = "cov",
                  "Influenza and pneumonia" = "flu", 
                  "Other infectious diseases" = "othinf", 
                  "Alcohol poisoning" = "alcpoi",
                  "Opioid poisoning" = "opioid",
                  "Suicide" = "sij",
                  "Motor vehicle accident" = "mvacc", 
                  "Unintentional injury*" = "uij",   
                  "Other injury" = "othj",   
                  "AUD" = "aud",
                  "Liver disease & cirrhosis" = "liver", 
                  "Kidney disease" = "kidney",
                  "Diabetes mellitus" = "dm",
                  "Dementia" = "dementia",
                  "Cerebrovascular diseases" = "stroke", 
                  "Diseases of the heart" = "heart", 
                  "Cancer" = "cancer", 
                  "Chronic LRD" = "resp", 
                  "Other NCDs" = "othncd",
                  "Rest" = "rest",
                  "Total population (ACS)" = "TPop",
                  "Year" = "year",
                  "Education" = "edclass", 
                  "Race/ethnicity" = "race",
                  "Sex" = "sex") %>%
    mutate_if(is.numeric, round, 0) 
  #dMort_out <- t(dMort_out)
  write.csv(dMort_out, 
            paste0("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/dMort_summary",
                   v.year1[1], "_", max(v.year2), "_", 
                   k.run, "_", k.pop_type, ".csv"), 
            row.names = F)
  gather(data = dMort_out, 
         key = Cause, - Year, -Sex, -"Race/ethnicity", -Education,
         value = deaths)
}

####################################################
####### calculate Results for 2020 weights  ######## - we do not have weights. anything beyond this part of the code was not run
if (k.weights != TRUE) {
  stop("No computaion of weights requested")
}

dPop_weights <- list()

# join with TPop for different weights and calculate mortality from different causes
for(i in 1:length(ACS_weights)){
  if (k.run == "ses") {
    dPop_weights[[i]] <- ACS_weights[[i]] %>%
      group_by(year, sex, age_gp, edclass) %>% summarise(TPop=sum(TPop)) %>% 
      mutate(age_gp=as.integer(age_gp))
  } else if (k.run == "detail"){
    dPop_weights[[i]] <- ACS_weights[[i]] %>%
      group_by(year, sex, race, age_gp, edclass) %>% summarise(TPop=sum(TPop)) %>% 
      mutate(age_gp=as.integer(age_gp))
  }
}

dMort2020 <- dMort_run %>% filter(year==2020) %>% dplyr::select(-c(TPop, trate)) %>% 
  select(!contains("mx_"))
mortlist <- list()

for(i in 1:length(dPop_weights)){
  mortlist[[i]] <- left_join(dMort2020, dPop_weights[[i]]) %>% 
    pivot_longer(cols=c(tmort:restmort)) %>% 
    mutate(rate = value/TPop,
           name = gsub("mort","rate",name),
           name = ifelse(name=="trate","trate",
                         paste0("mx_",name))) %>% 
    dplyr::select(-value) %>% 
    pivot_wider(names_from=name, values_from=rate) 
}

# Generate a variable to loop over
if (k.run == "ses") {
  for(i in 1:length(mortlist)){
    mortlist[[i]]$group <- apply(mortlist[[i]][ , c("sex", "edclass") ] , 1 , paste , collapse = "_" )
    mortlist[[i]]$weight <- i
  }
} else if (k.run == "detail") {
  for(i in 1:length(mortlist)){
    mortlist[[i]]$group <- apply(mortlist[[i]][ , c("sex", "edclass", "race") ] , 1 , paste , collapse = "_" )
    mortlist[[i]]$weight <- i
  }
}

# only get the results for 2019 - 2020 (results above will be the same for 2018-2019)
year1 <- 2019
year2 <- 2020
dResults_contrib_list <- list()

for(k in 1:length(mortlist)){
  
  for(i in 1:length(v.group)) {
    US_y1 <- filter(dMort_run, year==year1 &  group == v.group[i]) ## this one would then be i
    US_y2 <- filter(mortlist[[k]], year==year2 &  group == v.group[i])
    
    US_y1 <- US_y1[, sel.vars] #to delete several columns at once. comment: This kept Trate. in our out?
    US_y2 <- US_y2[, sel.vars]
    
    US_y1$ax = c(3.5,rep(2.5,11), 6.99) # midpoint of age range. Check age groups again. 
    US_y2$ax = c(3.5,rep(2.5,11), 6.99)
    
    # to start the first step of the decomposition
    US_y1_vector <- unlist(select(US_y1,starts_with("mx_")))
    US_y2_vector <- unlist(select(US_y2,starts_with("mx_")))
    
    decomp_results <-  horiuchi(func = life_table_causes,
                                pars1 = US_y1_vector,
                                pars2 = US_y2_vector,
                                age_vector = pull(US_y1,age_gp),
                                ax_vector = (pull(US_y1,ax) + pull(US_y2,ax))/2,
                                N = 100)
    decomp_results_mat <- matrix(decomp_results,ncol = length(v.totals[-1])) # -1 to remove Tmort
    
    #Sum over age - this gives the contribution of each cause in terms of years of LE. Note that this should sum to difference LE between the beg and start periods
    cause_contributions <- apply(decomp_results_mat,2,sum)
    cause_contributions <-  array(cause_contributions, dim = c(1, length(cause_contributions)))
    temp_contrib <- as.data.frame(cause_contributions) 
    colnames(temp_contrib) <- v.totals[-1]
    temp_contrib$group <- v.group[i]
    temp_contrib$start_year <- v.year1[j]
    temp_contrib$end_year <- v.year2[j]
    temp_contrib$LE1 <-  life_table_causes(nmx_by_cause_vector = US_y1_vector, age_vector = pull(US_y1,age_gp),
                                           ax_vector = pull(US_y1, ax)) + min(pull(US_y1, age_gp))
    temp_contrib$LE2 <-  life_table_causes(nmx_by_cause_vector = US_y2_vector, age_vector = pull(US_y2,age_gp),
                                           ax_vector = pull(US_y2, ax)) + min(pull(US_y2, age_gp))
    
    if (i == 1 & j == 1) {
      dResults_contrib_list[[paste(k)]] <- temp_contrib
    } else {
      dResults_contrib_list[[paste(k)]] <- rbind(temp_contrib, dResults_contrib_list[[paste(k)]])
    }
  }
}

if (k.run == "ses") {
  for(i in 1:length(dResults_contrib_list)){
    dResults_contrib_list[[i]] <- separate(dResults_contrib_list[[i]], col = group, into = c("sex","edclass"), sep = "_")
    dResults_contrib_list[[i]]$edclass <- factor(dResults_contrib_list[[i]]$edclass, 
                                                 levels = c( "LEHS", "SomeC", "College"))
    dResults_contrib_list[[i]]$sex <- as.factor(dResults_contrib_list[[i]]$sex)
    dResults_contrib_list[[i]] <- 
      dResults_contrib_list[[i]][order(
        dResults_contrib_list[[i]]$start_year, 
        dResults_contrib_list[[i]]$sex, 
        dResults_contrib_list[[i]]$edclass), ]
    dResults_contrib_list[[i]]$weight <- i
    dResults_contrib_list[[i]]$group <- NULL
    #  names(dResults_contrib) <-  c("Life_expectancy", "Year", "Sex", "SES" )
  }
} else if (k.run == "detail") {
  for(i in 1:length(dResults_contrib_list)){
    dResults_contrib_list[[i]] <- 
      separate(dResults_contrib_list[[i]],
               col = group, 
               into = c("sex","edclass", "race"), sep = "_")
    dResults_contrib_list[[i]]$edclass <- 
      factor(dResults_contrib_list[[i]]$edclass,
             levels = c( "LEHS", "SomeC", "College"))
    dResults_contrib_list[[i]]$sex <- as.factor(dResults_contrib_list[[i]]$sex)
    dResults_contrib_list[[i]] <- 
      dResults_contrib_list[[i]][order(
        dResults_contrib_list[[i]]$start_year, 
        dResults_contrib_list[[i]]$sex, 
        dResults_contrib_list[[i]]$edclass, 
        dResults_contrib_list[[i]]$race),]
    dResults_contrib_list[[i]]$weight <- i
    dResults_contrib_list[[i]]$group <- NULL
    #  names(dResults_contrib) <-  c("Life_expectancy", "Year", "Sex", "SES", "race )
  }  
}

dResults_weights <- do.call(rbind, dResults_contrib_list)

write.csv(dResults_weights, 
          paste0("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/Results_contrib_", 
                 v.year1[1], "_", max(v.year2), "_",
                 k.run, "_", "ACSweights.csv"),
          row.names = F)

write.csv(mortlist, 
          paste0("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/dMort_",
                 v.year1[1], "_", max(v.year2), "_", 
                 k.run, "_", "ACSweights.csv"), 
          row.names = F)

