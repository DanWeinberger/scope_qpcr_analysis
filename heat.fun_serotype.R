
heat.fun_serotype <- function( target='piab', cut1=15, cut2=45, cut3 = 47, cut4=95,cut5=141){
#ds1 <- data.array[target,,]
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
  arrange(season,seasonhh, seasonid) 


#breakslist<-seq(cut1,cut2,by=1)

weeks<- paste0('Week ', c(0,2,4,6,8,10)) 


p1 <- ds1 %>%
  filter(time<=6) %>%
ggplot( aes(time, Household)) + geom_tile(aes(fill = serotype),
                                                colour = "white")  +
  facet_wrap(~HH_order) +
  theme_classic() 
 # scale_fill_distiller(direction = -1, type='qual')
  
  #scale_fill_manual(values=c("red", "blue", "black")) +
  



}