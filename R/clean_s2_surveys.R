##Clean S2 surveys
library(dplyr)

#84.3,84.4,84.5,84.6.The participants were at the hospital at the time to produce saliva.
#Participants 55.3,53.2,56.3 they couldn’t produce saliva.
#ID 37,38 no participants was attribute to that household.

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


# s1_data <- readRDS('./Data/confidential/s1_survey_clean.rds') %>%
#   `[[`('survey_and_pcr_s1') %>%
#   dplyr::select(ID, Age, Gender, Race, Ethnicity,smoke_dic,pneu_vax_dic) %>%
#   unique() %>%
#   filter(!is.na(ID))

s1_demographics <- readRDS('./Data/confidential/s1_baseline_vars.rds') %>%
  rename(`season 1 scope ID`=ID, Age=Age2, Gender=Gender2, Race=Race2,Ethnicity=Ethnicity2) %>%
  mutate(`season 1 scope ID`=as.numeric(`season 1 scope ID`)) %>%
  dplyr::select(-MRN_cleaned) 

s2_demographics_b <- read_excel('./Data/confidential/New Participants SCOPE_season 2.xlsx', sheet='repeat participant cheat sheet')

s2_demographics_b2 <- left_join(s2_demographics_b,s1_demographics, by="season 1 scope ID") %>%
  rename(ID=`season 2 scope ID`, s1_match=`season 1 scope ID`) %>%
  rename(pneu_vax1=pneu_vax,
         immuno_meds1=immuno,
         diabetes_meds1=diabetes,
         asthma_meds1=asthma,
         flu_shot1=flu_shot
         ) %>%
  mutate(s1_match = paste0('S1_',s1_match),
         pneu_vax = if_else(pneu_vax1=='Yes',1,
                            if_else(pneu_vax1=='No',0, NA_real_)),
         flu_shot = if_else(flu_shot1=='Yes',1,
                            if_else(flu_shot1=='No',2, NA_real_)),
         asthma = if_else(asthma_meds1=='Yes',1,
                            if_else(asthma_meds1=='No',2, NA_real_)),
         diabetes = if_else(diabetes_meds1=='Yes',1,
                          if_else(diabetes_meds1=='No',2, NA_real_)),
         smoke = if_else(current_smoke=='Yes, cigarettes',1,
                            if_else(current_smoke=='No',2, NA_real_))
         
         ) %>%
  select(ID, s1_match,Age, Gender, Race, Ethnicity, smoke,pneu_vax,pneu_vax_date, education , diabetes,asthma, flu_shot ) 
  

s2_demographics_a <- read_excel('./Data/confidential/New Participants SCOPE_season 2.xlsx', sheet='new participants season 2') %>%
  rename(ID = `scope ID`, Race = `How would you describe your ethnicity?`,
         Ethnicity = `Would you consider yourself Hispanic or Non-Hispanic?`,
         dob= `What is your date of birth?`,
         pneu_vax_date= pcv_date,
         pneu_vax=pcv
         ) %>%
  mutate(Age = lubridate::time_length(difftime(as.Date('2022-01-01'), dob), "years"), pneu_vax_date=as.character(pneu_vax_date) ) %>%
  select(ID, Age, Gender, Race, Ethnicity, education, weight, height, pneu_vax, pneu_vax_date, flu_shot, flu_shot_when,
         diabetes, asthma, smoke, number_cigarettes, smoked, quit_smoking) #chronic_heart_disease,congestive_heart_failure,enlarged_or_thick_heart,chronic_liver_disease,liver_cirrhosis,liver_cancer,chronic_lung_disease,chronic_obstructive_pulm,emphysema,

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
  mutate(repN=row_number(),
         recent_pneumo_vax = if_else(vaccines_new==3, 1, 0),
         recent_flu_vax = if_else(vaccines_new==2, 1, 0),
         recent_covid_vax = if_else(vaccines_new==4, 1, 0)
         )%>%
  filter(repN==1 & !is.na(Household)) %>%
  ungroup()

N_contacts <- dcast(s2_a[c('ID','time','child_contact')], ID~time, fun.aggregate = max, na.rm=T, fill=9999)

out.list <- list('survey_and_pcr_s2'=s2_a,'contacts_wide_s2'=N_contacts)

return(out.list)

}
