##Clean S2 surveys
library(dplyr)
s2_surv <- read.csv('./Data/confidential/Season_2_Surveys/Uptodat Fortnightly_Wyllie_DATA_2022-07-21_1314 .csv') %>%
  rename(scope_id=participant_id) %>%
  mutate(ID = paste0('S2_',gsub("\\..*","",scope_id)) , time=visit_number)

table(s2_surv$visit_number)

d1.ds <- readRDS( './data/PCR_compiled.rds')

s2_pcr <- d1.ds[grep('S2', d1.ds$ID),]

s2_a <- merge(s2_pcr, s2_surv, by=c('ID','time'), all=T) %>%
  mutate(piab_pos = 1*(piab<40))
