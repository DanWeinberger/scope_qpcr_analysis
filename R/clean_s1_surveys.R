#--To do: -harmonize column names between fortnightly and initial questionaire so we can merge -Fix dates -Check missing visitNs

#Intake questionaire
clean_s1_surveys <- function(){
  key1a <- read.csv('./Data/hh_id_key.csv')
  
  master1 <- read_excel('./Data/confidential/SCOPE_Master_20210108.xlsx', sheet='COM SAL')
  
  
  key2 <- read_excel('./Data/confidential/SCOPE Participant MRN and ID.xlsx', skip=1)
  key3 <- cbind.data.frame('id'=c(100:101) , 'Household'=c(51,51) )
  
  key4a <- bind_rows(key1a,key3)
  
  key4a$id <- paste0('S1_', key4a$id)
  key4a$Household <- paste0('S1_', key4a$Household) #SEASON 1 key
  
  key1ab <- bind_rows(key4a)
  
  
q0a <-  read_excel('./Data/confidential/SCOPE Intake questionnaire_September 24, 2021_11.57.xlsx', skip=1)
key2 <- read_excel('./Data/confidential/SCOPE Participant MRN and ID.xlsx', skip=1)

s1_demographics <- read_excel('./Data/scope_demographics deidentified S1.xlsx') 
  #mutate(ID = as.numeric(ID), ID=paste0('S1_',ID))

c1.c <- read.csv( './Data/cleaned_file_s2.csv')

all.ids <- unique(c1.c$Sample)
all.ids <- all.ids[substr(all.ids,1,2)=='S2']
all.ids <- gsub(".*_","",all.ids)
all.ids <- sub('\\..*', '', all.ids)
all.ids <- sort(as.numeric(as.character(unique(all.ids))))

key4b <- cbind.data.frame('id'=all.ids ,'Household'=rep(1:(length(all.ids)/2),each=2) )      
key4b$id <- paste0('S2_', key4b$id)
key4b$Household <- paste0('S2_', key4b$Household) #SEASON 1 key

key2 <- read_excel('./Data/confidential/SCOPE Participant MRN and ID.xlsx', skip=1)
key3 <- cbind.data.frame('id'=c(100:101) , 'Household'=c(51,51) )
key1a <- read.csv('./Data/hh_id_key.csv')

key4a <- bind_rows(key1a,key3)

key4a$id <- paste0('S1_', key4a$id)
key4a$Household <- paste0('S1_', key4a$Household) #SEASON 1 key

key1ab <- bind_rows(key4a, key4b)

q0a$`Date of Visit` <- parse_date_time(q0a$`Date of Visit`, orders=c('d-b-Y','d/b/Y','d-m-Y','m/d/Y','m/d/y','Y-m-d'))

q0b <- read_excel('./Data/confidential/SCOPE Intake questionnaire_January 18, 2022_09.50.xlsx', skip=1)
names(q0b) <- names(q0a)
q0b$`Date of Visit` <- parse_date_time(q0b$`Date of Visit`, orders=c('d-b-Y','d/b/Y','d-m-Y','m/d/Y','m/d/y','Y-m-d'))


q0c <- read_excel('./Data/confidential/SCOPE Intake questionnaire_December 7, 2021_13.05.xlsx', skip=1)
names(q0c) <- names(q0a)
q0c$`Date of Visit` <- parse_date_time(q0c$`Date of Visit`, orders=c('d-b-Y','d/b/Y','d-m-Y','m/d/Y','m/d/y','Y-m-d'))

#combine the intake questionaire datasets
q0 <- bind_rows(q0a, q0b, q0c) 

select.colnames <- c('MRN', 
                     "SCOPE ID (eg. 0007, 0044, 0072 etc) if known/listed", 
                     "Date of Visit",
                     "Visit Location - Selected Choice",
                     "What is your date of birth? (DD-MM-YYYY)", 
                     "1.     Are you experiencing any of the following symptoms today?   - Q8#1 - Nasal congestion",
                     "1.     Are you experiencing any of the following symptoms today?   - Q8#1 - Coughing",
                     "1.     Are you experiencing any of the following symptoms today?   - Q8#1 - Running nose",
                     "1.     Are you experiencing any of the following symptoms today?   - Q8#1 - Sore throat", 
                     "1.     Are you experiencing any of the following symptoms today?   - Q8#1 - Fever", 
                     "1.    \nHave you had a positive test for COVID?",
                     "If\nyes, when did you have this positive test? (DD-MM-YYYY)",
                     "Have\nyou received the pneumonia vaccine?"  , 
                     "If yes,\nwhen did you receive this vaccine? (DD-MM-YYYY)",
                     "Have\nyou been hospitalized for pneumonia in the last month (30 days)?" , 
                     "Have\nyou taken any antibiotics in the last month (30 days)?",  
                     "Weight",
                     "Height (feet, inches)","
           Gender - Selected Choice","What is your highest level of education?", 
                     "How would you describe your ethnicity? - Selected Choice"  ,
                     "What is the relationship between yourself and the person you are enrolling into the study with?", 
                     "How long have you been living together? - Years" ,
                     "How much time do you spend together each day? - ___ hours", 
                     "Have you received the flu shot this season?",
                     "Have you had any of the following symptoms in the past 2 weeks? - Q39#1 - Nasal congestion" ,
                     "Have you had any of the following symptoms in the past 2 weeks? - Q39#1 - Coughing",
                     "Have you had any of the following symptoms in the past 2 weeks? - Q39#1 - Running nose",
                     "Have you had any of the following symptoms in the past 2 weeks? - Q39#1 - Sore throat" ,
                     "Have you had any of the following symptoms in the past 2 weeks? - Q39#1 - Fever",
                     "Have you taken any medications for or been told by a health care provider that you have any of th... - Q40#1 - 1.\tImmunodeficiency (Examples include: HIV/AIDS, corticosteroid medication like “Prednisone”, medicine or therapy to treat a cancerous tumor or mass, have had your spleen removed or have chronic kidney disease)",
                     "Have you taken any medications for or been told by a health care provider that you have any of th... - Q40#1 - 2.\tDiabetes",
                     "Have you taken any medications for or been told by a health care provider that you have any of th... - Q40#1 - 5c. Chronic lung disease: Asthma",
                     "Do you currently smoke cigarettes or e-cigarrettes?",
                     "Do you have any regular contact with children?",
                     "What is the age range of the children you have had contact with? - Selected Choice" ,
                     "How often do you usually have contact with children?" ,
                     "How much contact per day do you have?",
                     "Time of sample collection:" 
)

q0 <- q0[,c(which(names(q0) %in% select.colnames),grep('Are you experiencing any of the following symptoms today', names(q0)) )]

#q0b <- q0b[,c(which(names(q0b) %in% select.colnames),grep('Are you experiencing any of the following symptoms today', names(q0b)) )]

rename.intake <- c('MRN','scope_id', 'visit_date','visit_location',
                  'dob', 'pos_covid_past', 'pos_covid_date', 'pneu_vax', 
                  'pneu_vax_date','pneumonia_hosp', 
                  'recent_abx', 'weight','height',
                  'education','ethnicity', 
                  'hh_relationship', 
                  'relationship_duration','hh_contact_time',
                  'flu_shot', 'recent_nasal', 'recent_cough', 
                  'recent_runny_nose', 'recent_sore_throat', 
                  'recent_fever','immuno_meds','diabetes_meds',
                  'asthma_meds', 'current_smoke', 
                  'contact_children','age_child_contacts', 
                  'frequency_child_contacts', 'time_day_child_contacts', 
                  'time_sample', 'current_nasal','current_cough', 
                  'current_runny_nose', 'current_sore_throat',
                  'current_fever', 'other_symp1','othr_symp2')

names(q0) <- rename.intake

q0$visit_date[q0$visit_date %in% c( '12/APR/2021','12 Apr 2021','12/14/2021')] <- '2021-04-12'
q0$visit_date <- parse_date_time(q0$visit_date, orders=c('d-b-Y','d/b/Y','d-m-Y','m/d/Y','m/d/y','Y-m-d'))
q0$visitN <- '1'

q0$MRN <- as.numeric(gsub('MR','',q0$MRN))

q0 <- q0 %>%
  mutate( MRN=round(MRN),
          MRN=if_else(MRN==414574,4149574, MRN),
          MRN=if_else(MRN==121637,1212637, MRN),
          MRN=if_else(MRN==24915329,2495329, MRN) 
          ) %>%
  arrange(MRN, education) %>%
  group_by(MRN) %>%
  mutate(repN=row_number()) %>%
  ungroup() %>%
  filter(repN==1)

#24915329

#Fortnightly questionnaire and intake cleaning...do not run each time

#Manually clean v2 for age of contact intp

q1b0 <- read_excel('./Data/confidential/SCOPE Fortnightly Questionnaire_September 24, 2021_12.00_clean v2.xlsx', skip=1)
q1b0$`Date of Visit` <- as.character(q1b0$`Date of Visit`)

orig.colnames <- names(q1b0)

#read in newly entered surveys
q1b1 <- read_excel('./data/confidential/SCOPE Fortnightly Questionnaire_December 7, 2021_13.03.xlsx', skip=1)
names(q1b1) <- orig.colnames
q1b1$`Date of Visit` <- as.character(q1b1$`Date of Visit`)

q1b2 <- read_excel('./data/confidential/SCOPE Fortnightly Questionnaire_December 11, 2021_13.30.xlsx', skip=1)
names(q1b2) <- orig.colnames
q1b2$`Date of Visit` <- as.character(q1b2$`Date of Visit`)

q1b3 <- read_excel('./data/confidential/SCOPE Fortnightly Questionnaire_January 18, 2022_09.02.xlsx', skip=1)
names(q1b3) <- orig.colnames
q1b3$`Date of Visit` <- as.character(q1b3$`Date of Visit`)

#Combine forntightly datasets
q1 <- bind_rows(q1b0,q1b1, q1b2,q1b3) %>%
  rename(MRN=MRN,
         visit_date = `Date of Visit`,
         visitN = `Fortnightly visit number:`, 
         scope_id = `SCOPE ID if known/listed (eg. 0002, 0027, 0044, 0072...)`,  
         visit_location=`Visit Location - Selected Choice`,
         recent_social_activities=`Have you taken part in any social activities or outings during the past two weeks?`, 
         social_activity_type=`What sorts of activities have you participated in? - Selected Choice` , 
         contact_children = `Have you had any contact with children in the past two weeks?` , 
         age_child_contacts=`If yes, what is the age range of the children you have had contact with? - Selected Choice`,
         age_child_contacts_detail=`If yes, what is the age range of the children you have had contact with? - Record actual age if given - Text`,
         frequency_child_contacts=`How often do you usually have contact with children?` , 
         time_day_child_contacts=`How much contact per day do you have?`, 
         recent_covid_test=`Have you been tested for COVID in the last two weeks?` , 
         recent_doctor=`Have you had any sick visits to the doctor or been hospitalized in the last two weeks?` , 
         recent_vaccines=`Have you received any new vaccines in the last two weeks? - Selected Choice`   ,
         recent_vaccines_descr=`Have you received any new vaccines in the last two weeks? - Other vaccine, describe: - Text`  ,
         sample_time= `Time of sample collection:` ,
         recent_abx=`Have you taken any antibiotics in the last two weeks?` ,
         recent_nasal=`Have you had any of the following symptoms in the past two weeks? - Q12#1 - Nasal congestion`,
         recent_cough = `Have you had any of the following symptoms in the past two weeks? - Q12#1 - Coughing`,
         recent_runny_nose=`Have you had any of the following symptoms in the past two weeks? - Q12#1 - Running nose`,
         ) %>%
  mutate(visit_date = parse_date_time(visit_date, orders=c('d-b-Y','d/b/Y','d-m-Y','d-m-y','m/d/Y','m/d/y','Y-m-d'))
) %>%
  select(MRN, visit_date, visitN, visit_location,
         recent_social_activities,social_activity_type,contact_children,
         age_child_contacts,age_child_contacts_detail,frequency_child_contacts,
         time_day_child_contacts,recent_covid_test,
         recent_doctor,recent_vaccines,recent_vaccines_descr,
         sample_time,recent_abx,recent_nasal,recent_cough,recent_runny_nose)
  
q0.baseline.vars <- c("pos_covid_past","pos_covid_date","pneu_vax", "pneu_vax_date",'ethnicity','weight','height','education', "immuno_meds" ,"diabetes_meds", "asthma_meds","flu_shot","hh_relationship", "relationship_duration")

q1$MRN <-  gsub("mr","", q1$MRN )
q1$MRN <- as.character(as.numeric(gsub('MR','', q1$MRN)) )

#Combine baseline and fortnightly surveys
q0$MRN <- as.character(q0$MRN)
q1$MRN <- as.character(q1$MRN)
q2 <- bind_rows(q0[,-which(names(q0) %in% q0.baseline.vars)], q1) #combine q0, q1

q2$MRN <-  gsub("_visit#2.pdf","", q2$MRN )
q2$MRN <-  gsub("MR","", q2$MRN )
q2$MRN <-  gsub("mr","", q2$MRN )
q2$MRN <-  as.numeric(q2$MRN)
q2$visitN <- gsub('V','', q2$visitN)
q2$visitN <- gsub('v','', q2$visitN)
q2$visitN <- as.numeric(q2$visitN)


q2$MRN[q2$MRN==17330176] <- 1733076 #fix typo
q2$MRN[q2$MRN==1156975 ] <- 1156974 #fix typo
q2$MRN[is.na(q2$MRN) & q2$visit_date=='2021-03-17'] <- 4149574

q2$MRN <-  gsub("_visit#2.pdf","", q2$MRN )
q2$MRN <-  gsub("MR","", q2$MRN )
q2$MRN <-  gsub("mr","", q2$MRN )
q2$MRN <-  as.numeric(q2$MRN)
q2$MRN <-  gsub("MR","", q2$MRN )

q2$MRN_cleaned <- q2$MRN

q2$MRN_cleaned[q2$MRN_cleaned==' 6450061'] <-  '6450061'

q2$MRN_cleaned[q2$MRN_cleaned=='414574'] <-  '4149574'
q2$MRN_cleaned[q2$MRN_cleaned=='1156975'] <-  '1156974'
q2$MRN_cleaned[q2$MRN_cleaned=='24915329'] <-  '2495329'
q2$MRN_cleaned[q2$MRN_cleaned=='121637'] <-  '1212637'

#deduplicate
q2 <- q2 %>%
  group_by(MRN, visitN) %>%
  mutate(repN=row_number()) %>%
  ungroup() %>%
  filter(repN==1 & visitN %in% c(1,2,3,4,5,6))

#q1 <- merge(q1, q0[,c('MRN',q0.baseline.vars)], by='MRN', all=T)
#Output file for manual cleanring, and read back in cleaned version
write.csv(q2, './data/confidential/survey_data_to_clean.csv')

##Summary of which people are missing which days
q2.m <- reshape2::melt(q2[, c('MRN_cleaned','visitN') ], id.vars=c('MRN_cleaned','visitN'))
q2.c <- reshape2::dcast(q2.m, MRN_cleaned ~visitN, fun.aggregate = length)
write.csv(q2.c,'./Data/confidential/MRN_surveys_missing.csv')
#q2 <- q2[,-which(names(q2) %in% q0.baseline.vars)] #baseline vars are messed up in merge

#Merge in scope ID with the MRN
q2 <- q2 %>%
  mutate(MRN_cleaned=if_else(MRN_cleaned=='506940','5069040',MRN_cleaned),
         MRN_cleaned=if_else(MRN_cleaned=='4559554','5449554',MRN_cleaned),
        # if_else(MRN_cleaned=='2664776','5069040',MRN_cleaned),
        MRN_cleaned=if_else(MRN_cleaned=='2266001','2266061',MRN_cleaned),
        MRN_cleaned=if_else(MRN_cleaned=='31985','319852',MRN_cleaned),
        MRN_cleaned=if_else(MRN_cleaned=='2020325','2070325',MRN_cleaned),
        MRN_cleaned=if_else(MRN_cleaned=='4679497','4179497',MRN_cleaned)
         )

q3 <- key2 %>%
  rename(MRN=`MR#`) %>%
  mutate(MRN=if_else(MRN==" 6450061","6450061", MRN),
    MRN_cleaned =gsub('MR','',MRN)) %>%
  full_join(y=q2, by="MRN_cleaned") %>%
  mutate(ID=paste0('S1_', `ID Number`))

#import qPCR data
d1.ds <- readRDS('./data/PCR_compiled.rds')

d1.ds <- d1.ds[grep('S1', d1.ds$ID),]

e3 <- merge(d1.ds, q3, by.x=c('ID','time'), by.y=c("ID",'visitN'), all=T)  

e3$social_activity_type <- as.character(e3$social_activity_type)
e3$social_activity_type[e3$recent_social_activities=='No'] <- 'No'

e3$frequency_child_contacts <- as.character(e3$frequency_child_contacts)

e3$frequency_child_contacts[e3$contact_children=='No'] <- 'No contact'

e3$piab_pos <- NA
e3$piab_pos[e3$piab >=40 & !is.na(e3$piab)] <- 0 
e3$piab_pos[e3$piab <40 & !is.na(e3$piab)] <- 1 
e3$piab_pos <- as.factor(e3$piab_pos)

e3$smoke_dic <- NA
e3$smoke_dic[grep('Yes',e3$current_smoke)] <- 1
e3$smoke_dic[grep('No',e3$current_smoke)] <- 0

e3$pneu_vax_dic <- NA
e3$pneu_vax_dic[grep('Yes',e3$pneu_vax)] <- 1
e3$pneu_vax_dic[grep('No',e3$pneu_vax)] <- 0

#e3$pneu_vax_date[e3$pneu_vax_date=='N/A'] <- NA

#e3$education <- tolower(e3$education)

#e3$height <- gsub(' ', '', e3$height)
## export

#e3$month <- month(e3$visit_date_cleaned)

e3$contact_children[is.na(e3$contact_children) | e3$contact_children=='NA'] <- 'missing'

e3$child_contact <- 9999
e3$child_contact[e3$contact_children=='Yes'] <- 1
e3$child_contact[e3$contact_children=='No'] <- 0

e3$child_contact_u12m <- 0
e3$child_contact_u12m[grep('<12 m', e3$age_child_contacts)] <- 1
e3$child_contact_u12m[is.na(e3$age_child_contacts) & e3$contact_children!='No'] <- 9999

e3$child_contact_13_23m <- 0
e3$child_contact_13_23m[grep('13-23 mo', e3$age_child_contacts)] <- 1
e3$child_contact_13_23m[is.na(e3$age_child_contacts) & e3$contact_children!='No'] <- 9999

e3$child_contact_24_59m <- 0
e3$child_contact_24_59m[grep('24-59 months', e3$age_child_contacts)] <- 1
e3$child_contact_24_59m[is.na(e3$age_child_contacts) & e3$contact_children!='No'] <- 9999

e3$child_contact_5_10y <- 0
e3$child_contact_5_10y[grep('5-10 years', e3$age_child_contacts)] <- 1
e3$child_contact_5_10y[is.na(e3$age_child_contacts) & e3$contact_children!='No'] <- 9999

e3$child_contact_over10y <- 0
e3$child_contact_over10y[grep('>10 years', e3$age_child_contacts)] <-1
e3$child_contact_over10y[is.na(e3$age_child_contacts) & e3$contact_children!='No'] <- 9999


e3$activities <- 0
e3$activities[e3$recent_social_activities=='No'] <- 0
e3$activities[e3$recent_social_activities=='Yes'] <- 1
e3$activities[is.na(e3$social_activity_type)] <- 9999

e3$activity_family <- 0
e3$activity_family[e3$social_activity_type=='Activities with family'] <- 1
e3$activity_family[is.na(e3$social_activity_type)] <- 9999

e3$activity_friends <- 0
e3$activity_friends[e3$social_activity_type=='Activities with friends'] <- 1
e3$activity_friends[is.na(e3$social_activity_type)] <- 9999

e3$activity_community_center <- 0
e3$activity_community_center[e3$social_activity_type=='Activities at community centers'] <- 1
e3$activity_community_center[is.na(e3$social_activity_type)] <- 9999

e3$acitivity_fitness <- 0
e3$acitivity_fitness[e3$social_activity_type=='Fitness activities'] <- 1
e3$acitivity_fitness[is.na(e3$social_activity_type)] <- 9999

#child_contact_often,child_contact_hours
#these two variables often had coding switched so combine
e3$child_combined <- paste(e3$frequency_child_contacts, e3$time_day_child_contacts)

q0_merge <- q0[,c('MRN','pneu_vax',"pneu_vax_date", 'education','diabetes_meds','immuno_meds','asthma_meds', 'flu_shot')]
e3 <- e3 %>%
  
  filter(!is.na(Household)) %>%
  mutate( idN = as.numeric(gsub('S1_','', ID)),
    child_contact_hours = if_else(grepl('<4',child_combined),1,
                               if_else(grepl('4-8',child_combined),2,
                               if_else(grepl('8+',child_combined),3,999))),
         child_contact_often = if_else(grepl('Daily',child_combined),1,
                                       if_else(grepl('Every few',child_combined),2,
                                               if_else(grepl('Once or twice',child_combined),3,999))),
         
         ) %>%
  left_join(q0_merge, by=c('MRN_cleaned'='MRN'))

s1_demographics$ID <- as.numeric(s1_demographics$ID)
e3 <- merge(e3, s1_demographics, by.x="idN", by.y='ID', all=T)


id.ages <- unique(e3[,c('ID','Age')])
id.ages <- id.ages[!is.na(id.ages$Age),]



write.csv(e3,'./Data/Confidential/export_pfizer.csv')


actitivity_summary <- e3 %>%
  filter(!is.na(ID) & activities!=9999) %>%
  group_by(ID) %>%
  summarize(soc_activity= max(activities, na.rm=T) ,
            soc_activity_family= max(activity_family, na.rm=T) ,
            soc_activity_friend= max(activity_friends, na.rm=T) ,
            soc_activity_comm_ctr= max(activity_community_center, na.rm=T) ,
            soc_activity_fitness= max(acitivity_fitness, na.rm=T) 
  ) %>%
  ungroup() %>%
  summarize(freq_activity=mean(soc_activity, na.rm=T), N_activity=sum(soc_activity, na.rm=T),
            freq_family=mean(soc_activity_family, na.rm=T),N_activity_family=sum(soc_activity_family, na.rm=T),
            freq_friend=mean(soc_activity_friend, na.rm=T),N_activity_friends=sum(soc_activity_friend, na.rm=T),
            freq_comm_ctr=mean(soc_activity_comm_ctr, na.rm=T),N_activity_comm_ctr=sum(soc_activity_comm_ctr, na.rm=T),    
            freq_fitness=mean(soc_activity_fitness, na.rm=T),N_activity_fitness=sum(soc_activity_fitness, na.rm=T)    
            
  )

demographics <- read_excel('./Data/scope_demographics deidentified S1.xlsx') %>%
  mutate(ID =as.numeric(ID)) %>%
  rename(Age2=Age, Gender2=Gender, Race2=Race, Ethnicity2=Ethnicity)

#season1 baseline variables
baseline_s1 <- key2 %>%
  rename(MRN=`MR#`) %>%
  mutate(MRN=if_else(MRN==" 6450061","6450061", MRN),
         MRN_cleaned = as.character(gsub('MR','',MRN))) %>%
  full_join(y=q0, by=c("MRN_cleaned"="MRN") ) %>%
  dplyr::select(MRN_cleaned,`ID Number`,pneu_vax, pneu_vax_date,ethnicity,weight,height,education, immuno_meds ,current_smoke,diabetes_meds, asthma_meds,flu_shot) %>%
  rename(ID=`ID Number`) %>%
  full_join(demographics, by='ID') %>%
  mutate(Race2=if_else(is.na(Race2), ethnicity, Race2)) %>%
  dplyr::select(-ethnicity)


#baseline_s1 <- q0[, c('MRN',q0.baseline.vars)]

saveRDS(baseline_s1,'./Data/confidential/s1_baseline_vars.rds')

out.list <- list('actitivity_summary_s1'=actitivity_summary, 'survey_and_pcr_s1'=e3, 's1_baseline_vars'=baseline_s1)
return(out.list)
}


