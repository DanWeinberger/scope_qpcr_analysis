#--To do: -harmonize column names between fortnightly and initial questionaire so we can merge -Fix dates -Check missing visitNs

#Intake questionaire
clean_s1_surveys <- function(){
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

q0 <- bind_rows(q0a, q0b, q0c) 


select.colnames <- c('MRN', 
                     "SCOPE ID (eg. 0007, 0044, 0072 etc) if known/listed", 
                     "Date of Visit","Visit Location - Selected Choice",
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

rename.intake <- c('MRN','scope_id', 'visit_date','visit_location', 'pos_covid_past', 'pos_covid_date', 'pneu_vax', 'pneu_vax_date','pneumonia_hosp', 'recent_abx', 'weight','height','education','ethnicity', 'hh_relationship', 'relationship_duration','hh_contact_time','flu_shot', 'recent_nasal', 'recent_cough', 'recent_runny_nose', 'recent_sore_throat', 'recent_fever','immuno_meds','diabetes_meds','asthma_meds', 'current_smoke', 'contact_children','age_child_contacts', 'frequency_child_contacts', 'time_day_child_contacts', 'time_sample', 'current_nasal','current_cough', 'current_runny_nose', 'current_sore_throat','current_fever', 'other_symp1','othr_symp2')

names(q0) <- rename.intake

q0$visit_date[q0$visit_date %in% c( '12/APR/2021','12 Apr 2021','12/14/2021')] <- '2021-04-12'
q0$visit_date_clean <- parse_date_time(q0$visit_date, orders=c('d-b-Y','d/b/Y','d-m-Y','m/d/Y','m/d/y','Y-m-d'))
q0$visitN <- '1'


q0$MRN <- as.numeric(gsub('MR','',q0$MRN))



#Fortnightly questionnaire and intake cleaning...do not run each time
q1 <- read_excel('./Data/confidential/SCOPE Fortnightly Questionnaire_September 24, 2021_12.00_clean.xlsx', skip=1)

orig.colnames <- names(q1)

select.colnames.q1 <- c('MRN', "Fortnightly visit number:", "SCOPE ID if known/listed (eg. 0002, 0027, 0044, 0072...)", "Date of Visit", "Visit Location - Selected Choice","Have you taken part in any social activities or outings during the past two weeks?", "What sorts of activities have you participated in? - Selected Choice" , "Have you had any contact with children in the past two weeks?" , "If yes, what is the age range of the children you have had contact with? - Selected Choice","How often do you usually have contact with children?" , "How much contact per day do you have?", "Have you been tested for COVID in the last two weeks?" , "Have you had any sick visits to the doctor or been hospitalized in the last two weeks?" , "Have you received any new vaccines in the last two weeks? - Selected Choice"   ,"Time of sample collection:" ,'Have you taken any antibiotics in the last two weeks?' )

rename.q1 <-c('MRN','scope_id', 'visit_date', 'visitN', 'visit_location','recent_social_activities','social_activity_type', 'contact_children','age_child_contacts', 'frequency_child_contacts', 'time_day_child_contacts','recent_covid_test', 'recent_doctor','recent_abx', 'recent_vaccines', 'sample_time', 'current_nasal','current_cough', 'current_runny_nose', 'current_sore_throat','current_fever', 'other_symp1','othr_symp2','recent_nasal', 'recent_cough', 'recent_runny_nose', 'recent_sore_throat', 'recent_fever' , 'recent_other_symp1','recent_other_symp2')


q0.baseline.vars <- c('scope_id',"pos_covid_past","pos_covid_date","pneu_vax", "pneu_vax_date",'ethnicity','weight','height','education', "immuno_meds" ,"diabetes_meds", "asthma_meds","flu_shot","hh_relationship", "relationship_duration")

q1 <- q1[,c(which(names(q1) %in% select.colnames.q1),grep('Which symptoms are you experiencing today', names(q1)),grep('Have you had any of the following symptoms', names(q1))  )]

names(q1) <- rename.q1

q1$visit_date_clean <- parse_date_time(q1$visit_date, orders=c('d-b-Y','d/b/Y','d-m-Y','d-m-y','m/d/Y','m/d/y','Y-m-d'))

q1$visit_date <- as.Date(q1$visit_date)

q1$MRN <-  gsub("mr","", q1$MRN )
q1$MRN <- as.numeric(gsub('MR','', q1$MRN)) 

q1 <- bind_rows(q0[,-which(names(q0) %in% q0.baseline.vars)], q1) #combine q0, q1

q1$MRN <-  gsub("_visit#2.pdf","", q1$MRN )
q1$MRN <-  gsub("MR","", q1$MRN )
q1$MRN <-  gsub("mr","", q1$MRN )
q1$MRN <-  as.numeric(q1$MRN)
q1$visitN <- gsub('V','', q1$visitN)
q1$visitN <- gsub('v','', q1$visitN)
q1$visitN <- as.numeric(q1$visitN)

#q1 <- merge(q1, q0[,c('MRN',q0.baseline.vars)], by='MRN', all=T)
#Output file for manual cleanring, and read back in cleaned version
write.csv(q1, './data/confidential/survey_data_to_clean.csv')

#read in newly entered surveys
q1b1 <- read_excel('./data/confidential/SCOPE Fortnightly Questionnaire_December 7, 2021_13.03.xlsx', skip=1)
names(q1b1) <- orig.colnames

q1b2 <- read_excel('./data/confidential/SCOPE Fortnightly Questionnaire_December 11, 2021_13.30.xlsx', skip=1)
names(q1b2) <- orig.colnames
q1b2$`Date of Visit` <- as.Date(q1b2$`Date of Visit`, '%m/%d/%Y')

q1b3 <- read_excel('./data/confidential/SCOPE Fortnightly Questionnaire_January 18, 2022_09.02.xlsx', skip=1)
names(q1b3) <- orig.colnames
q1b3$`Date of Visit` <- as.Date(q1b3$`Date of Visit`, '%m/%d/%Y')

q1b <- bind_rows(q1b1, q1b2,q1b3)




q1b <- q1b[,c(which(names(q1b) %in% select.colnames.q1),grep('Which symptoms are you experiencing today', names(q1b)),grep('Have you had any of the following symptoms', names(q1b))  )]

names(q1b) <- rename.q1

q1b$MRN <- as.numeric(q1b$MRN)

q1b$visitN <- as.numeric(q1b$visitN)
#q1b <- unique(q1b) #de-deuplicate

q1b <- q1b %>%
  group_by(MRN, visitN) %>%
  mutate(repN=row_number()) %>%
  ungroup()

q1b <- q1b[q1b$repN==1,]

q2 <- read_excel('./data/confidential/survey_data_to_clean_DMW.xlsx')
q2$visit_date <- as.Date(as.numeric(q2$visit_date), origin='1900-01-01')
q2$visit_date_cleaned <- as.Date(as.numeric(q2$visit_date_cleaned), origin='1900-01-01')
q2$visit_date <- q2$visit_date_cleaned

q0$visitN <- as.numeric(q0$visitN)
q0$MRN <- as.numeric(q0$MRN)

q2 <- bind_rows(q2, q0[,-which(names(q0) %in% q0.baseline.vars)])
q2 <- bind_rows(q2, q1b)

q2$MRN[q2$MRN==17330176] <- 1733076 #fix typo
q2$MRN[q2$MRN==1156975 ] <- 1156974 #fix typo
q2$MRN[is.na(q2$MRN) & q2$visit_date_cleaned=='2021-03-17'] <- 4149574

q2$visit_date_cleaned <- q2$visit_date
q2$MRN_cleaned[is.na(q2$MRN_cleaned)] <-
  q2$MRN[is.na(q2$MRN_cleaned)]

##Summary of which people are missing which days
q2.m <- reshape2::melt(q2[, c('MRN_cleaned','visitN') ], id.vars=c('MRN_cleaned','visitN'))
q2.c <- reshape2::dcast(q2.m, MRN_cleaned ~visitN, fun.aggregate = length)
write.csv(q2.c,'./Data/confidential/MRN_surveys_missing.csv')
q2 <- q2[,-which(names(q2) %in% q0.baseline.vars)] #baseline vars are messed up in merge
q0$MRN <-  gsub("_visit#2.pdf","", q0$MRN )
q0$MRN <-  gsub("MR","", q0$MRN )
q0$MRN <-  gsub("mr","", q0$MRN )
q0$MRN <-  as.numeric(q0$MRN)
q2$MRN <-  gsub("MR","", q2$MRN )
merge_key <- unique(q2[, c('MRN',  "MRN_cleaned" )])

q0 <- as.data.frame(q0)

merge_key <- as.data.frame(merge_key)

q0a <- merge(q0, merge_key, by.x='MRN', by.y='MRN') ##all=T)

q3 <- merge(q2, q0a[,which(names(q0a) %in% c('MRN_cleaned',q0.baseline.vars))], by= "MRN_cleaned", all.x=T)

q3 <- q3[q3$visitN %in% c(1,2,3,4,5,6),]

q3$visit_date_cleaned <- as.Date(q3$visit_date_cleaned)

#q3$age <- round(as.numeric(q3$visit_date_cleaned - as.Date(q3$dob_clean))/365) #only recorded at t=1
#View(q2[, c('visitN','dob_clean',"MRN..Clean.",'Visit_date..Clean.') ])
key2$mr_clean <- gsub('MR', '',key2$`MR#`,)
#This is tricky..on key 2 is minimally cleaned; should match with uncleaned MRN on q3

key2$mr_clean[key2$mr_clean==' 6450061'] <-  '6450061'

q3$MRN_cleaned[q3$MRN_cleaned=='414574'] <-  '4149574'
q3$MRN_cleaned[q3$MRN_cleaned=='1156975'] <-  '1156974'
q3$MRN_cleaned[q3$MRN_cleaned=='24915329'] <-  '2495329'
q3$MRN_cleaned[q3$MRN_cleaned=='121637'] <-  '1212637'

q3 <- unique(q3)
q4 <- merge(q3, key2, by.x="MRN_cleaned", by.y= "mr_clean" , all.x=T)

q4$flag_id <- is.na(q4$`ID Number`)

s1_demographics$ID <- as.numeric(s1_demographics$ID)
q4 <- merge(q4, s1_demographics, by.x="ID Number", by.y='ID', all=T)

#age1 <- read_excel('./Data/confidential/AGE Scope Participants samples pick-up 06-15-2021.xlsx', skip=1)

#age1$MRN_cleaned <- gsub( 'MR', '',age1$`MR#`)
#age1 <- age1[,c('MRN_cleaned', 'AGE' )]
#age1$MRN_cleaned <- as.numeric(age1$MRN_cleaned)

#q4 <- merge(q4, age1, by='MRN_cleaned', all.x=T)


##Flag duplicate survey entries

q4 <- q4 %>%
  group_by(MRN_cleaned, visitN) %>%
  mutate(repN = row_number())

q4 <- q4[q4$repN==1,]

q4 <- q4[!(q4$`ID Number`  %in% c(38,39, 48,49, 58, 59, 102, 103)),]
q4 <- q4[!(q4$`ID Number`==6 & q4$visitN==1),]

q4$ID <- paste0('S1_', q4$`ID Number`)

##Summary of which people are missing which days
q4.1 <- q4

q4.1$child_contact_2_10y <- 0
q4.1$child_contact_2_10y[grep('24-59 months', q4.1$age_child_contacts)] <- 1
q4.1$child_contact_2_10y[grep('5-10 years', q4.1$age_child_contacts)] <- 1
#q4.1$child_contact_2_10y[grep('13-23 months', q4.1$age_child_contacts)] <- 1
#q4.1$child_contact_2_10y[grep('<12 months', q4.1$age_child_contacts)] <- 1

q4.1$child_contact_2_10y[is.na(q4.1$age_child_contacts)] <- NA

q4.m <- reshape2::melt(q4.1[, c('ID','visitN','child_contact_2_10y') ], id.vars=c('ID','visitN'))

q2.c <- reshape2::dcast(q4.m, `ID` ~visitN, fun.aggregate = mean, fill=9999)
names(q2.c)[1] <- 'ID'

q2.c <- q2.c[!(q2.c$ID %in% c(38,39)),] #these IDs were not included in study

q3.c <- merge(q2.c, key1ab, by.x='ID',by.y='id')


#View(q2[is.na(q2$age),])
#miss.dob <- q0a[q0a$dob %in% c('N/A',"19-04-2021") | is.na(q0$dob), c('dob','MRN_cleaned','MRN') ]
#write.csv(miss.dob,'./Data/confidential/check_dob.csv')


# master1$merge_id <- as.numeric(as.character(gsub('CS','',master1$dw_id_full)))
# 
# e1 <- merge(c1.c, master1, by.x='Sample', by.y='merge_id', all.x)
# 
# excel_date_diff <- as.numeric(as.Date("1970-01-01") - as.Date('1899-12-30')) #origin for excel and R is different
# 
# e1$`sample date`[e1$`sample date`=='2/15/25021'] <- as.character(as.numeric(as.Date('2021-02-15')) + excel_date_diff )
# 
# 
# 
# e1$pos <- 1*(e1$piab<40)
# 
# e1$sample_Date <- as.Date(as.numeric(e1$`sample date`), origin=as.Date('1899-12-30')) #use excel origin of 12-30-1899
# 
# hist(e1$sample_Date, breaks=10)
# e1$part_id <- as.numeric(e1$number)
# 
# e2 <- merge(e1, key2, by.x='part_id', by.y="ID Number", all=T)
# e2$`MR#` <- as.numeric(gsub("MR","", e2$`MR#`  ))


##NEED TO FIX THIS MERGE--MR and visitN could be missing
e3 <- merge(clean_pcr$clean_pcr, q4, by.x=c('ID','time'), by.y=c("ID",'visitN'), all=T)  


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

e3$pneu_vax_date[e3$pneu_vax_date=='N/A'] <- NA

e3$education <- tolower(e3$education)

e3$height <- gsub(' ', '', e3$height)
## export

id.ages <- unique(e3[,c('ID','Age')])
id.ages <- id.ages[!is.na(id.ages$Age),]

exp1 <- e3[,c('ID','time','piab', 'lyta','Household','MRN_cleaned', 'education','ethnicity',"hh_relationship"  ,        "relationship_duration","pneu_vax_dic" ,"pneu_vax_date" ,"weight", "height","pos_covid_past" ,          "pos_covid_date","smoke_dic", "contact_children" ,       
              "age_child_contacts"  ,     "frequency_child_contacts" ,"time_day_child_contacts" )]

baseline.chars <- exp1[,c('ID','Household', 'education','ethnicity',"hh_relationship"  ,        "relationship_duration","pneu_vax_dic" ,"pneu_vax_date" ,"weight", "height","pos_covid_past" ,          "pos_covid_date","smoke_dic")]

baseline.chars <- baseline.chars %>%
  group_by(ID) %>%
  mutate(smoke_dic= max(smoke_dic, na.rm=T))

baseline.chars <- baseline.chars %>%
  group_by(ID) %>%
  mutate(repN=row_number())
baseline.chars <- baseline.chars[baseline.chars$repN==1,]
baseline.chars$repN <- NULL

exp2 <- merge(id.ages, exp1[c('ID','time','piab', 'lyta',"contact_children" ,       
                              "age_child_contacts"  ,     "frequency_child_contacts" ,"time_day_child_contacts")], by='ID', all.y=T)

exp3 <- merge(exp2,baseline.chars, by='ID', all.x=T )
exp3 <- exp3[order(exp3$ID, exp3$time),]

write.csv(exp3,'./Data/Confidential/export_pfizer.csv')

e3$month <- month(e3$visit_date_cleaned)


e3$contact_children[is.na(e3$contact_children) | e3$contact_children=='NA'] <- 'missing'

e3$child_contact_u12m <- 'No'
e3$child_contact_u12m[grep('<12 m', e3$age_child_contacts)] <- 'Yes'
e3$child_contact_u12m[is.na(e3$age_child_contacts)] <- 'Not answered'

e3$child_contact_u5y <- 'No'
e3$child_contact_u5y[grep('<12 m', e3$age_child_contacts)] <- 'Yes'
e3$child_contact_u5y[grep('13-23 mo', e3$age_child_contacts)] <- 'Yes'
e3$child_contact_u5y[grep('24-59 months', e3$age_child_contacts)] <- 'Yes'
e3$child_contact_u5y[is.na(e3$age_child_contacts) & e3$contact_children!='No'] <- 'Not answered'


e3$child_contact_u12m <- 'No'
e3$child_contact_u12m[grep('<12 m', e3$age_child_contacts)] <- 'Yes'
e3$child_contact_u12m[is.na(e3$age_child_contacts) & e3$contact_children!='No'] <- 'Not answered'

e3$child_contact_13_23m <- 'No'
e3$child_contact_13_23m[grep('13-23 mo', e3$age_child_contacts)] <- 'Yes'
e3$child_contact_13_23m[is.na(e3$age_child_contacts) & e3$contact_children!='No'] <- 'Not answered'

e3$child_contact_24_59m <- 'No'
e3$child_contact_24_59m[grep('24-59 months', e3$age_child_contacts)] <- 'Yes'
e3$child_contact_24_59m[is.na(e3$age_child_contacts) & e3$contact_children!='No'] <- 'Not answered'

e3$child_contact_5_10y <- 'No'
e3$child_contact_5_10y[grep('5-10 years', e3$age_child_contacts)] <- 'Yes'
e3$child_contact_5_10y[is.na(e3$age_child_contacts) & e3$contact_children!='No'] <- 'Not answered'

e3$child_contact_over10y <- 'No'
e3$child_contact_over10y[grep('>10 years', e3$age_child_contacts)] <-'Yes'
e3$child_contact_over10y[is.na(e3$age_child_contacts) & e3$contact_children!='No'] <- 'Not answered'


e3$soc_act_bin <- 0
e3$soc_act_bin[e3$recent_social_activities=='No'] <- 0
e3$soc_act_bin[e3$recent_social_activities=='Yes'] <- 1

e3$soc_act_family_bin <- 0
e3$soc_act_family_bin[e3$social_activity_type=='Activities with family'] <- 1


e3$soc_act_friend_bin <- 0
e3$soc_act_friend_bin[e3$social_activity_type=='Activities with friends'] <- 1


e3$soc_act_comm_ctr_bin <- 0
e3$soc_act_comm_ctr_bin[e3$social_activity_type=='Activities at community centers'] <- 1

e3$soc_act_fitness_bin <- 0
e3$soc_act_fitness_bin[e3$social_activity_type=='Fitness activities'] <- 1

actitivity_sumamry <- e3 %>%
  group_by(ID) %>%
  summarize(soc_activity= max(soc_act_bin, na.rm=T) ,
            soc_activity_family= max(soc_act_family_bin, na.rm=T) ,
            soc_activity_friend= max(soc_act_friend_bin, na.rm=T) ,
            soc_activity_comm_ctr= max(soc_act_comm_ctr_bin, na.rm=T) ,
            soc_activity_fitness= max(soc_act_fitness_bin, na.rm=T) 
  ) %>%
  ungroup() %>%
  summarize(freq_activity=mean(soc_activity, na.rm=T), N_activity=sum(soc_activity, na.rm=T),
            freq_family=mean(soc_activity_family, na.rm=T),N_activity_family=sum(soc_activity_family, na.rm=T),
            freq_friend=mean(soc_activity_friend, na.rm=T),N_activity_friends=sum(soc_activity_friend, na.rm=T),
            freq_comm_ctr=mean(soc_activity_comm_ctr, na.rm=T),N_activity_comm_ctr=sum(soc_activity_comm_ctr, na.rm=T),    
            freq_fitness=mean(soc_activity_fitness, na.rm=T),N_activity_fitness=sum(soc_activity_fitness, na.rm=T)    
            
  )

out.list <- list('actitivity_summary_s1'=actitivity_sumamry, 'survey_and_pcr_s1'=e3, 'contacts_wide_s1'=q3.c)
return(out.list)
}
