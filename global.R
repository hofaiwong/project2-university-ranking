setwd("~/Desktop/Project 2/world-university-ranking")

## global.R ##
library(dplyr)
source('helpers.r')

school_and_country_table = read.csv("./data/school_and_country_table.csv", stringsAsFactors = F) %>%
  rename(., university_name = school_name)
cleanupCountry = read.csv("./data/cleanupCountry.csv", stringsAsFactors = F)
cleanupRename = read.csv("./data/cleanupRename.csv", stringsAsFactors = F)
cleanupCoord = read.csv("./data/cleanupCoord.csv", stringsAsFactors = F)


cwur = read.csv("./data/cwurData.csv", stringsAsFactors = F) %>%
  rename(., university_name = institution, total_score = score) %>%
  mutate(., university_name = ifelse (grepl('^The ', university_name), 
                                      gsub('The ','',university_name), 
                                      university_name)) %>%
  left_join(., cleanupRename, by = c('university_name')) %>%
  mutate(., new_name = ifelse(is.na(new_name), university_name, new_name))

shanghaiData = read.csv("./data/shanghaiData.csv", stringsAsFactors = F) %>%
  left_join(., rbind(school_and_country_table,cleanupCountry), by = c('university_name')) %>%
  mutate(., university_name = ifelse (grepl('^The ', university_name), 
                                      gsub('The ','',university_name), 
                                      university_name)) %>%
  left_join(., cleanupRename, by = c('university_name')) %>%
  mutate(., new_name = ifelse(is.na(new_name), university_name, new_name))

timesData = read.csv("./data/timesData.csv", stringsAsFactors = F) %>%
  mutate(., university_name = ifelse (grepl('^The ', university_name), 
                                      gsub('The ','',university_name), 
                                      university_name)) %>%
  left_join(., cleanupRename, by = c('university_name')) %>%
  mutate(., new_name = ifelse(is.na(new_name), university_name, new_name),
         international = as.numeric(international),
         income = as.numeric(income),
         total_score = as.numeric(total_score))

#expenditure = read.csv("education_expenditure_supplementary_data.csv", stringsAsFactors = F)
#attainment = read.csv("educational_attainment_supplementary_data.csv", stringsAsFactors = F)

#Data frame of unique universities ranked in 2015
rankings = unique(rbind(cwur[,c('new_name','country','year')],
                     timesData[,c('new_name','country','year')],
                     shanghaiData[,c('new_name','country','year')]) %>%
  filter(., year == 2015) %>%
  mutate(., country = gsub('Republic of Ireland', 'Ireland', country)) %>%
  mutate(., country = gsub('USA','United States of America', country)) %>%
  mutate(., country = gsub('Unisted States of America','United States of America', country)) %>%
  mutate(., country = gsub('Unted Kingdom', 'United Kingdom', country)) %>%
  mutate(., country = gsub('UK', 'United Kingdom', country)) %>%
  mutate(., country = gsub('Slovak Republic', 'Slovakia', country)) %>%
  mutate(., country = gsub('Russian Federation', 'Russia', country))) %>%
  left_join(., cleanupCoord[,c('new_name','lon','lat')], by = c('new_name')) %>%
  left_join(., cwur[,c(1,4:15)], by = c('new_name', 'year')) %>%
  left_join(., timesData[,c(1,4:15)], by = c('new_name', 'year')) %>%
  left_join(., shanghaiData[,c(1,3:11,13)], by = c('new_name', 'year')) %>%
  rename(., rank_cwur = world_rank.x,
         national_rank_cwur = national_rank.x,
         citations_cwur = citations.x,
         total_score_cwur = total_score.x,
         rank_times = world_rank.y,
         citations_times = citations.y,
         total_score_times = total_score.y,
         rank_shanghai = world_rank,
         national_rank_shanghai = national_rank.y,
         total_score_shanghai = total_score) %>%
  mutate(., rank_times = gsub('=','',rank_times)) %>%
  mutate(., rank_times = as.integer(sapply(rank_times, split_rank)),
         rank_shanghai = as.integer(sapply(rank_shanghai, split_rank))) %>%
  arrange(., desc(new_name))

countries = sort(unique(rankings$country))
universities = sort(unique(rankings$new_name))

df.2015.mean = rankings[,c(2,6:23,25,28,30:36)] %>%
  group_by(country) %>% 
  summarise_each(funs(f = round(mean(., na.rm=TRUE))))

df.2015.country = rankings %>%
  group_by(., country) %>%
  summarise(., top_cwur = min(rank_cwur, na.rm=T), 
            top_times = min(rank_times, na.rm=T),
            top_shanghai = min(rank_shanghai, na.rm=T),
            median_cwur = round(median(rank_cwur, na.rm=T)),
            median_times = round(median(rank_times, na.rm=T)),
            median_shanghai = round(median(rank_shanghai, na.rm=T)),
            count_cwur = sum(!is.na(rank_cwur)),
            count_times = sum(!is.na(rank_times)),
            count_shanghai = sum(!is.na(rank_shanghai))) %>%
  left_join(., df.2015.mean, by = 'country')


df.2015.uni = rankings %>%
  filter(., !is.na(lon) & !is.na(lat)) ####---> Need to add more coordinates!!!!
