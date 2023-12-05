
heat.fun_serotype <- function( target='piab', cut1=15, cut2=45, cut3 = 47, cut4=95,cut5=141){
#ds1 <- data.array[target,,]
  
  pcr_survey <- readRDS('./Data./pcr_survey_combined.rds') %>%
    mutate(Gender = if_else(Gender %in% c('F','Female'),'F',Gender),
           Gender = if_else(Gender %in% c('M','Male'),'M',Gender),
           child_contact_often= if_else(is.na(child_contact_often),999,child_contact_often)) %>%
    filter(piab != 9999) %>%
    mutate(combined_ID = if_else(is.na(s1_match),ID, s1_match)) %>%
    mutate(piab_pos = if_else(piab < 40,1,0),
           lyta_pos = if_else(lyta < 40,1,0),
           season=substr(ID,1,2),
           Gender=if_else(ID=='S2_77' & is.na(Gender), 'F',Gender)) %>%
    group_by(combined_ID) %>%
    #fill missing values when info is on survey for another time point
    mutate( diabetes = if_else(diabetes==999 | is.na(diabetes),-999,diabetes),
            diabetes = max(diabetes, na.rm=T),
            diabetes = if_else(diabetes>=1,1,diabetes),
            diabetes= as.factor(if_else(diabetes==-999,999, diabetes)),
            
            asthma = if_else(asthma==999 |is.na(asthma),-999,asthma),
            asthma = max(asthma, na.rm=T),
            asthma = if_else(asthma>=1,1,asthma),
            asthma= as.factor(if_else(asthma==-999,999, asthma))
    ) %>%
    tidyr::fill(.,education, .direction='downup') %>%
    ungroup() 
  
  
  hh_match <- pcr_survey %>%
    mutate(season=substr(ID,1,2)) %>%
    filter(season=='S1') %>%
    dplyr::select(ID, Household ) %>%
    rename(s1_match_hh=Household)
  
  id_key <- pcr_survey %>%
    dplyr::select(ID, s1_match) %>%
    unique() %>%
    filter(!is.na(s1_match)) %>%
    left_join(hh_match, by=c('s1_match'='ID'))
  
ds1 <- clean_pcr$clean_pcr %>%
  mutate( lyta= if_else( lyta==9999,NA_real_, lyta),
          piab= if_else( piab==9999,NA_real_, piab),
          season = substr(ID,1,2),
          seasonid =as.numeric(substring(ID, 4)),
          seasonhh= as.numeric(substring(Household, 4)),
          serotype = if_else(serotype=='15 B/C', '15B/C', serotype),
          serotype = if_else(is.na(serotype), NA_character_, serotype),
          st=as.factor(serotype)
          ) %>%
  filter(ID !='S2_NA') %>%
  arrange(season, seasonid) %>%
  group_by(season, seasonid, seasonhh) %>% 
  mutate(sampleID=cur_group_id()) %>%
  ungroup() %>%
  group_by(season,seasonhh) %>%
  mutate(house=cur_group_id(),
         plot.index= house + (sampleID) ) %>%
  ungroup() %>% 
  arrange(season,seasonhh, seasonid)%>%
  left_join(id_key, by='ID') %>%
  mutate(combined_ID = if_else(is.na(s1_match),ID, s1_match),
         piab_pos= 1*piab<40) %>%
  group_by(combined_ID) %>%
  mutate(N_pos=sum(piab_pos)) %>%
  ungroup() %>%
  filter(N_pos>0)


#breakslist<-seq(cut1,cut2,by=1)

weeks<- paste0('Week ', c(0,2,4,6,8,10)) 


p1 <- ds1 %>%
  filter(time<=6) %>%
ggplot( aes(time, combined_ID)) + geom_tile(aes(fill = serotype),
                                                colour = "white")  +
  facet_wrap(~season) +
  theme_classic() 



}
