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

universities_year = unique(rbind(cwur[,c('new_name','country','year')],
                     timesData[,c('new_name','country','year')],
                     shanghaiData[,c('new_name','country','year')])) %>%
  mutate(., country = gsub('Republic of Ireland', 'Ireland', country)) %>%
  mutate(., country = gsub('USA','United States of America', country)) %>%
  mutate(., country = gsub('Unisted States of America','United States of America', country)) %>%
  mutate(., country = gsub('Unted Kingdom', 'United Kingdom', country)) %>%
  mutate(., country = gsub('UK', 'United Kingdom', country)) %>%
  mutate(., country = gsub('Slovak Republic', 'Slovakia', country)) %>%
  mutate(., country = gsub('Russian Federation', 'Russia', country))

universities_year = unique(universities_year) %>%
  left_join(., cleanupCoord[,c('new_name','lon','lat')], by = c('new_name'))

rankings = universities_year %>%
  left_join(., cwur[,c('new_name','world_rank', 'year')], by = c('new_name', 'year')) %>%
  left_join(., timesData[,c('new_name','world_rank', 'year')], by = c('new_name', 'year')) %>%
  left_join(., shanghaiData[,c('new_name','world_rank', 'year')], by = c('new_name', 'year')) %>%
  rename(., rank_cwur = world_rank.x, 
         rank_times = world_rank.y,
         rank_shanghai = world_rank) %>%
  mutate(., rank_times = gsub('=','',rank_times)) %>%
  mutate(., rank_times = as.integer(sapply(rank_times, split_rank)),
         rank_shanghai = as.integer(sapply(rank_shanghai, split_rank)))

universities = unique(rankings[,c(1,2)])

countries = sort(unique(rankings$country))

df.2015.country = rankings[rankings$year==2015,] %>%
  group_by(., country) %>%
  summarise(., top_cwur = min(rank_cwur, na.rm=T), 
            top_times = min(rank_times, na.rm=T),
            top_shanghai = min(rank_shanghai, na.rm=T),
            median_cwur = round(median(rank_cwur, na.rm=T)),
            median_times = round(median(rank_times, na.rm=T)),
            median_shanghai = round(median(rank_shanghai, na.rm=T)),
            count_cwur = sum(!is.na(rank_cwur)),
            count_times = sum(!is.na(rank_times)),
            count_shanghai = sum(!is.na(rank_shanghai)))

df.2015.uni = rankings %>%
  filter(., year==2015 & !is.na(lon) & !is.na(lat)) ####---> Need to add more coordinates!!!!