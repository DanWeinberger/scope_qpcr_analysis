##Clean S2 surveys
library(dplyr)

clean_s2_surveys <- function(){
s2_surv <- read.csv('./Data/confidential/Season_2_Surveys/Uptodat Fortnightly_Wyllie_DATA_2022-07-21_1314 .csv') %>%
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
  mutate(ID = paste0('S2_',gsub("\\..*","",scope_id)) , time=visit_number)

table(s2_surv$visit_number)

d1.ds <- readRDS( './data/PCR_compiled.rds')

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


N_contacts <- dcast(s2_a[c('ID','time','child_contact')], ID~time, fun.aggregate = max, na.rm=T, fill=9999)

out.list <- list('survey_and_pcr_s2'=s2_a,'contacts_wide_s2'=N_contacts)

return(out.list)

}
