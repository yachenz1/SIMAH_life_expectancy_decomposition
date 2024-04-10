# libraries required:
library("tidyverse")
library("dplyr")
library("reshape")
library("data.table")
library("RColorBrewer")

## Set the working directory
setwd("C:/Users/yzhu/Desktop/SIMAH project/Charlotte Probst/Life expectancy decomposition/")

# Read results for life expectancy 
  #dle_results_weight <- read.csv(("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/Results_contrib_2018_2020_detail_ACSweights.csv"))
#LE_ACS_ses <- read.csv("~/Desktop/CAMH/Fall 2023/SIMAH II/Output/LifeExpectancy_1821_ses_ACS.csv")

LE_ACS_detail <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/LifeExpectancy_1821_detail_ACS.csv")

  #LE_ACS_pred_detail <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/LifeExpectancy_0020_detail_ACS_pred.csv")
  #LE_CPS_detail <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/LifeExpectancy_0020_detail_CPS.csv")
#LE_ACS_ses <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/LifeExpectancy_0020_ses_ACS.csv")
#LE_ACS_pred_ses <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/LifeExpectancy_0020_ses_ACS_pred.csv")
#LE_CPS_ses <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/LifeExpectancy_0020_ses_CPS.csv")

# Graph results by sex and SES or by sex, ses, and race
k.run <- "detail" # "ses" or "detail"
k.pop_type <- "ACS" # "ACS", "ACS_pred" or "CPS". ACS Weights are treated separately below. 

# for race and SES graphs
if(k.run == "detail") {
   if(k.pop_type=="ACS"){
      dle_results <- LE_ACS_detail
   }else if(k.pop_type=="ACS_pred"){
      dle_results <- LE_ACS_pred_detail
   }else if (k.pop_type == "CPS") {
      dle_results <- LE_CPS_detail
   } 
} else if (k.run == "ses") {
   if(k.pop_type=="ACS"){
      dle_results <- LE_ACS_ses
   }else if(k.pop_type=="ACS_pred"){
      dle_results <- LE_ACS_pred_ses
   }else if (k.pop_type == "CPS") {
      dle_results <- LE_CPS_ses
   }
}

table(dle_results$Year)
str (dle_results)
#dle_results <- dle_results %>% filter(Race != "Other", Year >2014)
dle_results <- dle_results %>% filter(Year >2017)
dle_results <- dle_results %>% mutate_at(vars(Sex, SES), as.factor)

levels(dle_results$SES) <- list("High" = "College", "Middle" = "SomeC", "Low" = "LEHS")
levels(dle_results$Sex) <- list(Men = "1", Women = "2")

color.vec <- c("#cf82a6", "#a14d72", "#732946")
color.vec <- c("#69AA9E", "#447a9e",  "#d72c40") # high  middle low
color.vec <- brewer.pal(4,"YlGnBu")[-1]
color.vec <- c("#90be6d", "#f9c74f", "#f94144")


# Plot on life expectancy by SES over time
le_graph <- ggplot(data = dle_results[dle_results$Race != "Other",], aes(x = Year, y = Life_expectancy, colour = SES)) + 
  facet_grid(rows = vars(Sex), cols = vars(Race)) +
  ylab("Life expectancy at age 18") +
  theme_light()+
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text=element_text(size = 12), 
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "bottom") +
  scale_color_manual(name = "Educational attainment", 
                     breaks = c("High", "Middle", "Low"), values = color.vec, 
                     labels = c("High", "Middle", "Low")) +   
  geom_line(aes(color = SES), linewidth = .9, alpha = .6) +
  geom_point(size = 2, aes(color = SES)) 
le_graph
ggsave(paste0("SIMAH_workplace/life_expectancy/3_graphs/2021_decomp/LE_", k.run, k.pop_type, ".jpg"), 
       dpi=600, width=20, height=15, units="cm")
dle_results %>%
  group_by(Year) %>%
  summarize(mean_life_expectancy = mean(Life_expectancy, na.rm = TRUE))


## Display results for weights - we do not have weights, code was not run beyond this point
dle_results_weight <- dle_results_weight %>% 
   select(sex, edclass,	race, end_year, LE2, weight) %>% 
   filter(race != "Other", end_year == 2020)
names(dle_results_weight) <- c("Sex", "SES", "Race", "Year", 
                               "Life_expectancy", "weight")
ggplot(data = dle_results_weight, 
       aes(x = SES, y = Life_expectancy, colour = SES, group = SES)) + 
   geom_boxplot(aes(fill=SES)) +
   facet_grid(rows = vars(Sex), cols = vars(Race)) +
   scale_y_continuous(breaks=seq(65,100, 5))

dle_ranges <- dle_results_weight %>% group_by(Sex, SES, Race, Year) %>%
   summarise(low = min(Life_expectancy),
   high = max(Life_expectancy)) %>% mutate(difference = high-low)

df_list <- list(LE_ACS_detail, dle_ranges, LE_ACS_pred_detail, LE_CPS_detail)
LE_detail_combined <- df_list %>% reduce(inner_join, 
                                         by = c("Year", "Sex", "SES", "Race"))
names(LE_detail_combined) <- c("Life_expectancy_ACS", 
                               "Year", "Sex", "SES", "Race", 
                               "ACS_weight_min", 
                               "ACS_weight_max", 
                               "ACS_range",
                               "Life_expectancy_ACS_pred", 
                               "Life_expectancy_CPS")
col.order <- c("Year", "Sex", "SES", "Race", 
               "Life_expectancy_ACS", "ACS_weight_min", "ACS_weight_max", "ACS_range",
               "Life_expectancy_ACS_pred", "Life_expectancy_CPS")
LE_detail_combined <- LE_detail_combined[,col.order]
LE_detail_combined <- LE_detail_combined %>% 
   mutate(LE_dif_ACS_pred = Life_expectancy_ACS - Life_expectancy_ACS_pred,
          LE_dif_CPS = Life_expectancy_ACS - Life_expectancy_CPS) %>%
   mutate(across(where(is.numeric), round, 1)) %>%
   filter(Year>2018)   %>%
   mutate_at(vars(Sex, SES, Race), as.factor)
levels(dle_ranges$SES) <- list("High" = "College", "Middle" = "SomeC", "Low" = "LEHS")
levels(dle_ranges$Sex) <- list(Men = "1", Women = "2")


write.csv(LE_detail_combined, 
          paste0("SIMAH_workplace/life_expectancy/2_out_data/2020_decomp/", 
                 "LifeExpectancy_combined_", k.run, "_1920.csv"), 
          row.names = F)   