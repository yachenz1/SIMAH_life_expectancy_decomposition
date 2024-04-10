library(tidyverse)
library(tidyr)

## Set the working directory
setwd("C:/Users/yzhu/OneDrive - ARG/SIMAH project/Charlotte Probst/Life expectancy decomposition/")

dle_results <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/LifeExpectancy_1821_detail_ACS.csv")

dle_results_1 <- dle_results %>% filter(Race != "Other") %>%
  mutate(SES = replace(SES, SES == "College", "High"),
         SES = replace(SES, SES == "SomeC", "Middle"),
         SES = replace(SES, SES == "LEHS", "Low"),
         Sex = replace(Sex, Sex == 1, "Men"),
         Sex = replace(Sex, Sex == 2, "Women")) %>%
  spread(Race, Life_expectancy) %>%
  mutate(`White-Black` = White-Black,
         `Hispanic-White` = Hispanic-White) %>%
  gather(key = "Racial_Disparity", value = "Diff_Life_Expectancy", 7:8) 
#%>%
 # mutate(Racial_Disparity = replace(Racial_Disparity, Racial_Disparity == "White-Black", "Black"),
  #       Racial_Disparity = replace(Racial_Disparity, Racial_Disparity == "White-Hispanic", "Hispanic"))
  
# White-Black disparities during 2019-2020
dle_results_1 %>% filter(Year %in% 2019:2020 & Racial_Disparity == "White-Black") %>% 
  select(Year, Sex, SES, Racial_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2020` - `2019`)

# Hispanic-White disparities during 2019-2020
dle_results_1 %>% filter(Year %in% 2019:2020 & Racial_Disparity == "Hispanic-White") %>% 
  select(Year, Sex, SES, Racial_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2020` - `2019`)


# White-Black disparities during 2020-2021
dle_results_1 %>% filter(Year %in% 2020:2021 & Racial_Disparity == "White-Black") %>% 
  select(Year, Sex, SES, Racial_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2021` - `2020`)

# White-Hispanic disparities during 2020-2021
dle_results_1 %>% filter(Year %in% 2020:2021 & Racial_Disparity == "Hispanic-White") %>% 
  select(Year, Sex, SES, Racial_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2021` - `2020`)


color.vec <- c("#90be6d", "#f9c74f", "#f94144")

# Plot on racial/ethnic disparity in life expectancy over time
le_graph <- ggplot(data = dle_results_1, aes(x = Year, y = Diff_Life_Expectancy, colour = SES)) + 
  facet_grid(rows = vars(Sex), cols = vars(Racial_Disparity)) +
  ylab("Difference in LE comparing White to Black/Hispanic") +
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
  geom_point(size = 2, aes(color = SES)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
le_graph

k.run <- "detail"
k.pop_type <- "ACS"
ggsave(paste0("SIMAH_workplace/life_expectancy/3_graphs/2021_decomp/LE_racial_disparity_", k.run, k.pop_type,"_updated", ".jpg"), 
       dpi=600, width=20, height=15, units="cm")




dle_results_2 <- dle_results %>% filter(Race != "Other") %>%
  mutate(SES = replace(SES, SES == "College", "High"),
         SES = replace(SES, SES == "SomeC", "Middle"),
         SES = replace(SES, SES == "LEHS", "Low"),
         Sex = replace(Sex, Sex == 1, "Men"),
         Sex = replace(Sex, Sex == 2, "Women")) %>%
  spread(SES, Life_expectancy) %>%
  mutate(`High-Low` = High-Low,
         `High-Middle` = High-Middle) %>%
  gather(key = "SES_Disparity", value = "Diff_Life_Expectancy", 7:8) 
#%>%
 # mutate(SES_Disparity = replace(SES_Disparity, SES_Disparity == "High-Low", "Low"),
  #       SES_Disparity = replace(SES_Disparity, SES_Disparity == "High-Middle", "Middle"))

# High-Low disparities during 2019-2020
dle_results_2 %>% filter(Year %in% 2019:2020 & SES_Disparity == "High-Low") %>% 
  select(Year, Sex, Race, SES_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2020` - `2019`)

# High-Middle disparities during 2019-2020
dle_results_2 %>% filter(Year %in% 2019:2020 & SES_Disparity == "High-Middle") %>% 
  select(Year, Sex, Race, SES_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2020` - `2019`)


# High-Low disparities during 2020-2021
dle_results_2 %>% filter(Year %in% 2020:2021 & SES_Disparity == "High-Low") %>% 
  select(Year, Sex, Race, SES_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2021` - `2020`)

# High-Middle disparities during 2020-2021
dle_results_2 %>% filter(Year %in% 2020:2021 & SES_Disparity == "High-Middle") %>% 
  select(Year, Sex, Race, SES_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2021` - `2020`)


le_graph <- ggplot(data = dle_results_2, aes(x = Year, y = Diff_Life_Expectancy, colour = Race)) + 
  facet_grid(rows = vars(Sex), cols = vars(SES_Disparity)) +
  ylab("Difference in LE comparing High to Low/Middle education") +
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
  scale_color_manual(name = "Race and ethnicity",
                     breaks = c("White", "Hispanic", "Black"), values = color.vec,
                     labels = c("White", "Hispanic", "Black")) +
  geom_line(aes(color = Race), linewidth = .9, alpha = .6) +
  geom_point(size = 2, aes(color = Race)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

le_graph
ggsave(paste0("SIMAH_workplace/life_expectancy/3_graphs/2021_decomp/LE_SES_disparity_", k.run, k.pop_type, "_updated", ".jpg"), 
       dpi=600, width=20, height=15, units="cm")



#####
# during 2019-2020
dle_results %>% filter(Year %in% 2019:2020) %>% 
  filter(Race != "Other") %>%
  mutate(SES = replace(SES, SES == "College", "High"),
         SES = replace(SES, SES == "SomeC", "Middle"),
         SES = replace(SES, SES == "LEHS", "Low"),
         Sex = replace(Sex, Sex == 1, "Men"),
         Sex = replace(Sex, Sex == 2, "Women")) %>%
  select(Year, Sex, SES, Race, Life_expectancy) %>%
  spread(Year, Life_expectancy) %>% 
  mutate(Change_in_LE = `2020` - `2019`) %>%
  View()



  spread(Race, Life_expectancy) %>%
  mutate(`White-Black` = White-Black,
         `White-Hispanic` = White-Hispanic) %>%
  gather(key = "Racial_Disparity", value = "Diff_Life_Expectancy", 7:8)
  
  
  
  # during 2020-2021
  dle_results %>% filter(Year %in% 2020:2021) %>% 
    filter(Race != "Other") %>%
    mutate(SES = replace(SES, SES == "College", "High"),
           SES = replace(SES, SES == "SomeC", "Middle"),
           SES = replace(SES, SES == "LEHS", "Low"),
           Sex = replace(Sex, Sex == 1, "Men"),
           Sex = replace(Sex, Sex == 2, "Women")) %>%
    select(Year, Sex, SES, Race, Life_expectancy) %>%
    spread(Year, Life_expectancy) %>% 
    mutate(Change_in_LE = `2021` - `2020`) %>%
    View()  
  
  
### Summary statistics  
decomp_results_2020_ACS_detail_cat %>% 
  filter(Category != "COVID-19") %>% 
  group_by(Race, Sex, Education) %>% 
  summarize(sumLE = sum(Contribution)) %>% 
  View()
  
decomp_results_2020_ACS_detail_cat %>% 
  filter(Education == "Low" & Category != "COVID-19") %>% 
  group_by(Race, Sex) %>% 
  summarize(sumLELow = sum(Contribution)) %>% 
  left_join(decomp_results_2020_ACS_detail_cat %>% 
              filter(Education == "High" & Category != "COVID-19") %>% 
              group_by(Race, Sex) %>% 
              summarize(sumLEHigh = sum(Contribution)), by = c("Race", "Sex")) %>%
  mutate(DIFF = sumLELow - sumLEHigh)


Percent_COVID_LE <- decomp_results_2020_ACS_detail_cat %>%
  group_by(Race, Sex, Education) %>%
  summarize(totalLE = sum(Contribution)) %>%
  left_join(decomp_results_2020_ACS_detail_cat %>%
              filter(Category == "COVID-19") %>%
              group_by(Race, Sex, Education) %>%
              summarize(COVIDLE = sum(Contribution)), by = c("Race", "Sex", "Education")) %>% 
  mutate(percent = COVIDLE / totalLE * 100) 

Percent_COVID_LE %>% filter(Race %in% c("Hispanic", "White") & Education == "High")
Percent_COVID_LE %>% filter(Race %in% c("Black", "White") & Education == "Low")



decomp_results_2020_ACS_detail_cat %>% 
  filter(Category %in% c("CVD", "Cancer", "Other NCD")) %>% 
  group_by(Sex, Race) %>% 
  summarize(sumLE = sum(Contribution)) %>% 
  View()


decomp_results_2020_ACS_detail_cat %>% 
  filter(Category %in% c("CVD", "Cancer", "Other NCD")) %>% 
  group_by(Sex, Education, Race) %>% 
  summarize(sumLE = sum(Contribution)) %>% 
  View()

decomp_results_2020_ACS_detail_cat %>% 
  filter(Category %in% c("CVD", "Cancer", "Other NCD")) %>%
  View()

decomp_results_2020_ACS_detail_cat %>% filter(Category == "Deaths of despair") %>% View()
