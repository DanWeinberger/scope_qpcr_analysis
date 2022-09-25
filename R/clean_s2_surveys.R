##Clean S2 surveys
library(dplyr)

clean_s2_surveys <- function(){
s2_surv <- read.csv('./Data/confidential/Season_2_Surveys/fornightly_DATA_2022-09-19.csv') %>%
  rename(scope_id=participant_id, 
         child_contact=child_contact_2weeks,
         child_contact_u12m=child_contact_age___1,
         child_contact_13_23m=child_contact_age___2,
         child_contact_24_59m=child_contact_age___3,
         child_contact_5_10y=child_contact_age___4,
         child_contact_over10y=child_contact_age___5,
         activity_community_center=activities_describe___1,
         activity_friends=activities_describe___2,
         activity_family=activities_describe___3,
         acitivity_fitness=activities_describe___4,
         activity_other=activities_describe___5
         ) %>%
  mutate(ID = paste0('S2_',gsub("\\..*","",floor(scope_id))) , time=visit_number)

table(s2_surv$visit_number)

d1.ds <- readRDS( './Data/PCR_compiled.rds')

s1_demographics <- read_excel('./Data/scope_demographics deidentified S1.xlsx') %>%
  rename(`season 1 scope ID`=ID) %>%
  mutate(`season 1 scope ID`=as.numeric(`season 1 scope ID`))

s2_demographics_b <- read_excel('./Data/confidential/New Participants SCOPE_season 2.xlsx', sheet='repeat participant cheat sheet')

s2_demographics_b2 <- left_join(s2_demographics_b,s1_demographics, by="season 1 scope ID") %>%
  rename(ID=`season 2 scope ID`) %>%
  select(ID, Age, Gender, Race, Ethnicity)

s2_demographics_a <- read_excel('./Data/confidential/New Participants SCOPE_season 2.xlsx', sheet='new participants season 2') %>%
  rename(ID = `scope ID`, Race = `How would you describe your ethnicity?`,
         Ethnicity = `Would you consider yourself Hispanic or Non-Hispanic?`,
         dob= `What is your date of birth?`
         ) %>%
  mutate(Age = lubridate::time_length(difftime(as.Date('2022-01-01'), dob), "years") ) %>%
  select(ID, Age, Gender, Race, Ethnicity)

s2_demographics <- bind_rows(s2_demographics_a, s2_demographics_b2) %>%
   mutate(ID= paste0('S2_',ID))


s2_pcr <- d1.ds[grep('S2', d1.ds$ID),]

s2_a <- merge(s2_pcr, s2_surv, by=c('ID','time'), all=T) %>%
  mutate(piab_pos = 1*(piab<40))

N_surveys <- dcast(s2_a[c('ID','time','child_contact')], ID~time, fun.aggregate = length)

s2_a <- s2_a %>%
  filter(!is.na(time))

s2_a$child_contact[is.na(s2_a$child_contact)] <- 9999
s2_a$child_contact[is.infinite(s2_a$child_contact)] <- 9999

s2_a$child_contact_u12m[is.na(s2_a$child_contact_u12m)] <-9999
s2_a$child_contact_13_23m[is.na(s2_a$child_contact_13_23m)] <-9999
s2_a$child_contact_24_59m[is.na(s2_a$child_contact_24_59m)] <-9999
s2_a$child_contact_5_10y[is.na(s2_a$child_contact_5_10y)] <-9999
s2_a$child_contact_over10y[is.na(s2_a$child_contact_over10y)] <-9999

s2_a <- merge(s2_a,s2_demographics, by='ID', all=T)

s2_a <- s2_a %>%
  group_by(ID, time) %>%
  mutate(repN=row_number()) %>%
  filter(repN==1 & !is.na(Household)) %>%
  ungroup()

N_contacts <- dcast(s2_a[c('ID','time','child_contact')], ID~time, fun.aggregate = max, na.rm=T, fill=9999)

out.list <- list('survey_and_pcr_s2'=s2_a,'contacts_wide_s2'=N_contacts)

return(out.list)

}
