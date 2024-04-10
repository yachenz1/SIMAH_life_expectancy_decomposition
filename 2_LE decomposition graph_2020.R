# Plot LE decomposition results for 2018 to 2020 
# by SES and race and ethnicity 
# Project: SIMAH

# libraries required:
library("dplyr")
library("RColorBrewer")
library("tidyverse")

## Set the working directory
setwd("C:/Users/yzhu/OneDrive - ARG/SIMAH project/Charlotte Probst/Life expectancy decomposition/")

# Graph results by sex and SES or by sex, ses, and race
k.run <- "detail" # "ses" or "detail"
k.pop_type <- "ACS" # "ACS", "ACS_pred" or "CPS". ACS Weights are treated separately below. 


if(k.run == "detail") {
  if(k.pop_type=="ACS"){
    dDecomp <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/Results_contrib_2019_2020_detail_ACS.csv")
  }else if(k.pop_type=="ACS_pred"){
    dDecomp <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/Results_contrib_2019_2020_detail_ACS_pred.csv")
  }else if (k.pop_type == "CPS") {
    dDecomp <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/Results_contrib_2019_2020_detail_CPS.csv")
  }
} else if (k.run == "ses") {
  if(k.pop_type=="ACS"){
    dDecomp <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/Results_contrib_2019_2020_ses_ACS.csv")
  }else if(k.pop_type=="ACS_pred"){
    dDecomp <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/Results_contrib_2019_2020_ses_ACS_pred.csv")
  }else if (k.pop_type == "CPS") {
    dDecomp <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/Results_contrib_2019_2020_ses_CPS.csv")
  }
}


if (k.run == "detail"){
  dgathered <- gather(data = dDecomp, key = "mort", value = "value", 
                      -sex , -edclass, -race, -start_year, -end_year, -LE1, -LE2)
} else if (k.run == "ses") {
  dgathered <- gather(data = dDecomp, key = "mort", value = "value", 
                      -sex , -edclass, -start_year, -end_year, -LE1, -LE2)
}

# have to be factor variables
glimpse(dgathered)
dgathered <- dgathered %>% 
  mutate(mort = gsub(pattern = "mort", replacement = "", x = dgathered$mort)) %>% 
  mutate_at(vars(sex, edclass, mort), as.factor) %>%
  mutate(Category = factor(case_when(mort == 'cov' ~ 'COVID-19',
                                     # mort == 'flu' | 
                                     # mort == 'othinf' ~ 'Other infectious disease',
                                     mort == 'mvacc' |
                                       mort == 'uij' |
                                       mort == 'othj' ~ 'Unintentional injury',
                                     mort == 'aud' |
                                       mort == 'alcpoi' | 
                                       mort == 'liver' |
                                       mort == 'sij' | 
                                       mort == 'opioid' ~ 'Deaths of despair', 
                                     mort == 'heart' ~ 'CVD', 
                                     mort == 'cancer' ~ 'Cancer',
                                     mort == 'kidney' |
                                       mort == 'dm' |
                                       mort == 'dementia' |
                                       mort == 'resp' | 
                                       mort == 'stroke' |
                                       mort == 'othncd' ~ 'Other NCD', 
                                     mort == 'flu' | 
                                       mort == 'othinf' |
                                       mort == 'rest' ~ 'Rest'), 
                           levels = c("COVID-19", "CVD", "Cancer", "Other NCD", "Unintentional injury", "Deaths of despair", "Opioid poisoning", "Rest"))
  )

if (k.run == "ses") {
  names(dgathered) <- c("Sex", "Education", "start_year", "end_year", "LE1", "LE2", "Cause_of_death", "Contribution", "Category")     
} else if (k.run == "detail") {
  dgathered <- mutate_at(dgathered, vars(race), as.factor)
  names(dgathered) <- c("Sex", "Education", "Race", "start_year", "end_year", "LE1", "LE2", "Cause_of_death", "Contribution", "Category")     
  dgathered <- filter(dgathered, Race != "Other") 
}


levels(dgathered$Education) <- list("High" = "College", "Middle" = "SomeC", "Low" = "LEHS")
levels(dgathered$Sex) <- list(Men = "1", Women = "2")
levels(dgathered$Cause_of_death) <- list("Covid 19" = "cov",
                                         "Influenza and pneumonia" = "flu", 
                                         "Other infectious diseases" = "othinf", 
                                         
                                         "Alcohol poisoning" = "alcpoi",
                                         "Opioid poisoning" = "opioid",
                                         "Suicide" = "sij",
                                         "Motor vehicle accident" = "mvacc", 
                                         "Unintentional injury*" = "uij",   
                                         "Other injury" = "othj",   
                                         
                                         "Alcohol use disorder" = "aud",
                                         "Liver disease & cirrhosis" = "liver", 
                                         "Kidney disease" = "kidney",
                                         "Diabetes mellitus" = "dm",
                                         "Dementia" = "dementia",
                                         "Cerebrovascular diseases" = "stroke", 
                                         "Diseases of the heart" = "heart", 
                                         "Cancer" = "cancer", 
                                         "Chronic lower respiratory diseases" = "resp", 
                                         "Other NCDs" = "othncd",
                                         
                                         "Rest" = "rest"
)


infectious <- 5
injury1 <- 3
injury2 <- 3
ncd1 <- 3
ncd2 <- 7

other <- 1
color.vec <- c(rev(brewer.pal(infectious,"Blues"))[2:4],
               rev(brewer.pal(injury1,"Reds")),
               rev(brewer.pal(injury2,"Greens")), #Oranges
               rev(brewer.pal(ncd1,"RdPu")), #YlOrRd
               rev(brewer.pal(ncd2,"YlOrRd")),
               
               c("grey"))

write.csv(dgathered, paste0("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/decomp_results_2020_", 
                            k.pop_type, "_", k.run, ".csv"))
dgathered <- filter(dgathered, start_year == 2019) 
## Plot showing changes in every year (will not be included in publication)
dcomp_plot <- ggplot(data = dgathered, 
                     aes(x = Education, y = Contribution, fill = Cause_of_death)) +
  geom_bar(position = position_stack(reverse = T), stat = "identity") +
  scale_fill_manual("Cause of death", values = color.vec)+ 
  facet_grid( rows = vars(Sex),  scales = "free")  + 
  coord_flip() +
  theme_light()+
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text=element_text(size = 12), 
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12),
        legend.position = "right") +
  xlab("Contribution in years of life expectancy")

if (k.run == "detail") {
  dcomp_plot <- dcomp_plot + facet_grid(rows = vars(Sex, Race))
}

ggsave(plot = dcomp_plot, 
       filename =  
         paste0("SIMAH_workplace/life_expectancy/3_graphs/2021_decomp/decomp_plot_2020_",
                k.pop_type, "_",
                k.run, ".jpeg"), 
       dpi=600, width=30, height=15, units="cm", device = "jpeg")

# Generate a more simplified graph
color.vec2 <- rev(brewer.pal(9,"YlGnBu")[-1])
color.vec2 <- c("#8ab17d", "#e76f51", "#f4a261", "#e9c46a", "#2a9d8f", "#287271", "#264653", "#ced4da")
color.vec2 <- c("#90be6d", "#577590", "#4d908e", "#43aa8b", "#f9c74f", "#f94144", "#f3722c", "#ced4da")
color.vec2 <- c("#90be6d", "#577590", "#4d908e", "#43aa8b", "#f9c74f", "#f94144", "#ced4da")

dcomp_plot <- ggplot(data = dgathered, 
                     aes(x = Education, y = Contribution, fill = Category)) +
  geom_bar(position = position_stack(reverse = T), stat = "identity") +
  scale_fill_manual("Cause of death", values = color.vec2)+ 
  facet_grid( rows = vars(Sex),  scales = "free")  + 
  coord_flip() +
  theme_light()+
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text=element_text(size = 12), 
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12),
        legend.position = "right") +
  ylab("Losses/gains in years of life expectancy") +
  xlab("") + 
  #scale_y_continuous(limits = c(-6.5, 5), breaks = seq(-6, 5, by = 1)) 
  scale_y_continuous(limits = c(-6.5, 2.5), breaks = seq(-6, 2.5, by = 1))

if (k.run == "detail") {
  dcomp_plot <- dcomp_plot + facet_grid(rows = vars(Sex, Race))
}
dcomp_plot
ggsave(plot = dcomp_plot, 
       filename =  
         paste0("SIMAH_workplace/life_expectancy/3_graphs/2021_decomp/decomp_plot_2020_categories",
                k.pop_type, "_",
                k.run, ".jpeg"), 
       dpi=600, width=30, height=15, units="cm", device = "jpeg")

dcat <- dgathered %>%
  group_by(Sex , Education, Race, start_year, Category) %>%
  summarise(Contribution = sum(Contribution)) %>%
  arrange(Contribution)


write.csv(dcat, paste0("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/decomp_results_2020_", 
                       k.pop_type, "_", k.run, "_cat.csv"))

https://www.youtube.com/watch?v=rBp3eYHrsfo 
library(plotly)
ggplotly(dcomp_plot)
