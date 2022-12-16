pcr_summary_long <- readRDS('./Data/PCR_compiled.rds')


##check S1 demographics
s1_demographics <- read_xlsx('./Data/scope_demographics deidentified S1.xlsx') %>%
  mutate(ID=as.numeric(ID), 
         ID= paste0('S1_',ID)) 


s2_demographics_b <- read_excel('./Data/confidential/New Participants SCOPE_season 2.xlsx', sheet='repeat participant cheat sheet') %>%
  rename( S1_id=`season 1 scope ID`, ID=`season 2 scope ID`) %>%
  mutate(S1_id= paste0('S1_',S1_id),
         ID= paste0('S2_',ID) )
 
s2_demographics_repeat <- s2_demographics_b %>%
  left_join(s1_demographics, by=c('S1_id'='ID')) %>%
  select(ID, Age, Gender, Race, Ethnicity)

s2_demographics_new <- read_excel('./Data/confidential/New Participants SCOPE_season 2.xlsx', sheet='new participants season 2') %>%
  rename(ID = `scope ID`, Race = `How would you describe your ethnicity?`,
         Ethnicity = `Would you consider yourself Hispanic or Non-Hispanic?`,
         dob= `What is your date of birth?`,
  ) %>%
  mutate(Age = lubridate::time_length(difftime(as.Date('2022-01-01'), dob), "years"),
         ID= paste0('S2_',ID)) %>%
  select(ID, Age, Gender, Race, Ethnicity) #chronic_heart_disease,congestive_heart_failure,enlarged_or_thick_heart,chronic_liver_disease,liver_cirrhosis,liver_cancer,chronic_lung_disease,chronic_obstructive_pulm,emphysema,

s2_demographics <- bind_rows(s2_demographics_new, s2_demographics_repeat) 

demographics_all <- bind_rows(s1_demographics, s2_demographics)

pcr_survey_combo <- pcr_summary_long %>%
  left_join(demographics_all, by=c('ID')) %>%
  mutate(piab_pos = if_else(piab<40,1,0), season=substr(ID,1,2),
         Gender=if_else(Gender %in% c('M', 'Male'),'M',
                        if_else(Gender %in% c('F','Female'),'F','999'))) %>%
  group_by(season, Gender) %>%
  summarize(prev=mean(piab_pos), Nswab=n(), Npos=sum(piab_pos))

pcr_survey_combo
