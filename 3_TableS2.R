dMort2019_2021 <- dMort %>% group_by(year, sex, race, edclass) %>% 
  summarize(`Motor vehicle accident` = sum(mvaccmort), 
            `Opioid poisoning` = sum(opioidmort), 
            `Alcohol poisoning` = sum(alcpoimort), 
            `Unintentional injury` = sum(uijmort), 
            `Suicide` = sum(sijmort), 
            `Other injury` = sum(othjmort), 
            `Covid 19` = sum(covmort), 
            `Influenza and pneumonia` = sum(flumort), 
            `Other infectious diseases` = sum(othinfmort), 
            `Diseases of the heart` = sum(heartmort), 
            Cancer = sum(cancermort), 
            `Cerebrovascular diseases` = sum(strokemort), 
            `Chronic LRD` = sum(respmort), 
            Dementia = sum(dementiamort), 
            `Diabetes mellitus` = sum(dmmort), 
            `Kidney disease` = sum(kidneymort), 
            `Liver disease & cirrhosis` = sum(livermort), 
            AUD = sum(audmort), 
            `Other NCDs` = sum(othncdmort), 
            Rest = sum(restmort), 
            `Total deaths` = sum(tmort), 
            `Total population (ACS)` = sum(TPop))

dMort2019_2021[,5:26] <- round(dMort2019_2021[,5:26], 0)



dMort2019 <- dMort2019_2021 %>% 
  filter(year == 2019 & race != "Other") %>% 
  mutate(sex = ifelse(sex == 1, "Men", "Women"),
         edclass = case_when(edclass == "College" ~ "High",
                             edclass == "LEHS" ~ "Low",
                             edclass == "SomeC" ~ "Mid.")) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column()



dMort2020 <- dMort2019_2021 %>% 
  filter(year == 2020 & race != "Other") %>% 
  mutate(sex = ifelse(sex == 1, "Men", "Women"),
         edclass = case_when(edclass == "College" ~ "High",
                             edclass == "LEHS" ~ "Low",
                             edclass == "SomeC" ~ "Mid.")) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column()



dMort2021 <- dMort2019_2021 %>% 
  filter(year == 2021 & race != "Other") %>% 
  mutate(sex = ifelse(sex == 1, "Men", "Women"),
         edclass = case_when(edclass == "College" ~ "High",
                             edclass == "LEHS" ~ "Low",
                             edclass == "SomeC" ~ "Mid.")) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column()


dMort_combined <- rbind(dMort2019, dMort2020, dMort2021)

write.csv(dMort_combined, 
          paste0("C:/Users/yzhu/OneDrive - ARG/SIMAH project/Charlotte Probst/Life expectancy decomposition/SIMAH_workplace/mortality/3_out data/", 
                 "TableS2.csv"), 
          row.names = FALSE)



dMort_totals <- dMort %>% group_by(year) %>% 
  summarize(`Motor vehicle accident` = sum(mvaccmort), 
            `Opioid poisoning` = sum(opioidmort), 
            `Alcohol poisoning` = sum(alcpoimort), 
            `Unintentional injury` = sum(uijmort), 
            `Suicide` = sum(sijmort), 
            `Other injury` = sum(othjmort), 
            `Covid 19` = sum(covmort), 
            `Influenza and pneumonia` = sum(flumort), 
            `Other infectious diseases` = sum(othinfmort), 
            `Diseases of the heart` = sum(heartmort), 
            Cancer = sum(cancermort), 
            `Cerebrovascular diseases` = sum(strokemort), 
            `Chronic LRD` = sum(respmort), 
            Dementia = sum(dementiamort), 
            `Diabetes mellitus` = sum(dmmort), 
            `Kidney disease` = sum(kidneymort), 
            `Liver disease & cirrhosis` = sum(livermort), 
            AUD = sum(audmort), 
            `Other NCDs` = sum(othncdmort), 
            Rest = sum(restmort), 
            `Total deaths` = sum(tmort), 
            `Total population (ACS)` = sum(TPop)) %>%
  t() %>%
  janitor::row_to_names(row_number = 1) %>%
  as.data.frame() %>%
  rownames_to_column()


dMort_totals[,2:5] <- round(dMort_totals[,2:5], 0)


write.csv(dMort_totals, 
          paste0("C:/Users/yzhu/OneDrive - ARG/SIMAH project/Charlotte Probst/Life expectancy decomposition/SIMAH_workplace/mortality/3_out data/", 
                 "TableS2_totals.csv"), 
          row.names = FALSE)
