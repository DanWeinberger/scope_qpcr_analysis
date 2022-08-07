##Clean S2 surveys
library(dplyr)
a1 <- read.csv('./Data/confidential/Season_2_Surveys/Uptodat Fortnightly_Wyllie_DATA_2022-07-21_1314 .csv') %>%
  rename(scope_id=participant_id) %>%
  mutate(scope_id = gsub("\\..*","",scope_id) , scope_id=paste0(scope_id,'.',visit_number))

