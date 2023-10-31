# What to do if we have borderline positive then negative test:
#   
#   1) If weak positive for piaB (e.g., 35 Ct or higher) AND at least one of the repeats is \<40, then it is positive
# 
# 2) If weak positive for piaB (e.g., 35 Ct or higher), AND (2 repeats negative) then NEGATIVE
# 
# 3) if weak positive for piaB and then negative on 1 repeat performed and negative, then negative
# 
# 4) If posivi, take ave of Ct among those \<40
clean_pcr_fun <- function(){
key1a <- read.csv('./Data/hh_id_key.csv')

master1 <- read_excel('./Data/confidential/SCOPE_Master_20210108.xlsx', sheet='COM SAL')


key2 <- read_excel('./Data/confidential/SCOPE Participant MRN and ID.xlsx', skip=1)
key3 <- cbind.data.frame('id'=c(100:101) , 'Household'=c(51,51) )

key4a <- bind_rows(key1a,key3)

key4a$id <- paste0('S1_', key4a$id)
key4a$Household <- paste0('S1_', key4a$Household) #SEASON 1 key


file_list <- list.files(path="./Data/Final Scope Results Drift Correction")

all.res.season1 <-
  lapply(file_list, function(x){
    print(x)
    ds <- read_excel(paste0("./Data/Final Scope Results Drift Correction/",x), skip=19)
    ds$expt <- substr(x,1,4)
    ds$Sample <- as.character(ds$Sample)
    ds$Target <- as.character(ds$Target)
    ds$Cq <- as.numeric(as.character(ds$Cq))
    cols.keep <- 
      c('Cq','Sample','Target','Content','expt','Biological Set Name')
    ds.names <- names(ds)
    ds.names <- intersect(ds.names,cols.keep)
    ds <- ds[,ds.names]
    ds$Sample <- paste0('S1_', ds$Sample)
    
    return(ds)
  })

file_list_s2 <- list.files(path="./Data/cleaned_season2")


all.res.season2 <-
  lapply(file_list_s2, function(x){
    print(x)
    ds <- read_excel(paste0("./Data/cleaned_season2/",x), skip=19)
    ds$expt <- substr(x,1,4)
    ds$Sample <- as.character(ds$Sample)
    ds$Target <- as.character(ds$Target)
    ds$Cq <- as.numeric(as.character(ds$Cq))
    cols.keep <- 
      c('Cq','Sample','Target','Content','expt','Biological Set Name')
    ds.names <- names(ds)
    ds.names <- intersect(ds.names,cols.keep)
    ds <- ds[,ds.names]
    ds$`Biological Set Name` <- NA
    ds$Sample <- paste0('S2_', ds$Sample)
    return(ds)
  })

all.res <- c( all.res.season1,all.res.season2)

all.res <- lapply(1:length(all.res), function(x){ all.res[[x]]$plate= x 
return(all.res[[x]])
}) 

test1 <- lapply(all.res, function(x)  is.character(x$Target))

b1 <- bind_rows(all.res) 

b1$Cq[is.na(b1$Cq)] <- 45

b1$Target[is.na(b1$Target)] <- 
  b1$`Biological Set Name`[is.na(b1$Target)]

b1 <- b1[b1$Content!='Std',]

b1$Target <- gsub(' ' ,'', b1$Target, fixed=T)

b1$Target <- tolower(b1$Target)


b1$Target[grep('lyta',b1$Target)] <- 'lyta'

b1$Target[grep('piab',b1$Target)] <- 'piab'


#b1$Sample[is.na(b1$Sample)] <- b1$`sample id`[is.na(b1$Sample)]

b1 <- b1[!is.na(b1$Sample),]

b1 <- b1[-grep('neg',b1$Sample),]
b1 <- b1[-grep('NA',b1$Sample),]
b1 <- b1[-grep('NTC',b1$Sample),]
b1 <- b1[-grep('NEG',b1$Sample),]
b1 <- b1[-grep('NEC',b1$Sample),]
b1 <- b1[-grep('nec',b1$Sample),]
b1 <- b1[-grep('ntc',b1$Sample),]
b1 <- b1[-grep('apex',b1$Sample),]

b1 <- b1[-grep('-1',b1$Sample),]
b1 <- b1[-grep('-2',b1$Sample),]
b1 <- b1[-grep('-3',b1$Sample),]
b1 <- b1[-grep('-4',b1$Sample),]
b1 <- b1[-grep('-5',b1$Sample),]
b1 <- b1[-grep('QC',b1$Sample),]

b1$Sample <- gsub('rep','',b1$Sample)

#b1$Sample <- round(as.numeric(b1$Sample),1)

c1 <- b1[, c('Sample','Cq','Target','plate')]

c1$Cq[is.na(c1$Cq)] <- 45

c1 <- c1 %>%
  group_by(Sample ,Target) %>%
  mutate(repN=row_number())

c1.rep <- reshape2::dcast(c1 ,  Target+Sample   ~ repN , value.var='Cq')
write.csv(c1.rep,'./Data/ct_by_sample_rep.csv')

c1.m <- reshape2::melt(c1, id.vars= c('Sample','Target','plate', 'repN'))

##If sample tested multiple times call pos if pos on multiple runs. Ct<40=pos

c1.m <- c1.m %>%
  group_by(Sample, Target) %>%
  mutate(N_tests= n(), n_pos=sum(value<40), ct_pos=mean(value[value<40], na.rm=T), ct_min=min(value, na.rm=T) ) %>%
  ungroup() 

#if negative, set ct to 45
c1.m$ct_pos[is.nan(c1.m$ct_pos)] <- 45

c1.m$ct_pos2 <- c1.m$ct_pos

#If min Ct value<35, use ave value of positive regardless of number of tests performed.
c1.m$ct_pos2[c1.m$ct_min <= 35] <- c1.m$ct_pos[c1.m$ct_min <= 35] #if ave is <35, call positive regardless of N tests

#If min Ct>35, and 3 or more tests performed, with only 1 positive, call it a negative
c1.m$ct_pos2[c1.m$N_tests>=3 & c1.m$n_pos<=1 & c1.m$ct_min > 35] <- 45 #if 3 or more tests, and 0 or 1 are weakly positive or neg, then call it negative

#If min >35, with at least 2 tests performed and at least 2 were positive, call it positive
c1.m$ct_pos2[c1.m$N_tests>=2 & c1.m$n_pos>= 2 & c1.m$ct_min > 35] <- 
  c1.m$ct_pos[c1.m$N_tests>=2 & c1.m$n_pos>= 2 & c1.m$ct_min > 35] #if 2 or more tests, and 0 or 1 are weakly positive or neg, then call it negative

#if min Ct>35, N tests is <=2 and only 1 positive, call it negative
c1.m$ct_pos2[c1.m$N_tests<=2 & c1.m$n_pos< c1.m$N_tests & c1.m$ct_min > 35] <- 45 #if 2 or fewer tests, and at least 1 is positive, call positive


c1.c <- reshape2::dcast(c1.m[,c('Sample','variable','Target','ct_pos2')], Sample +variable ~ Target, fun.aggregate = min, fill=9999, value.var='ct_pos2')

c1.c$pos <- c1.c$lyta<40 & c1.c$piab<40

write.csv(c1.c, './Data/cleaned_file_s2.csv')


all.ids <- unique(c1.c$Sample)
all.ids <- all.ids[substr(all.ids,1,2)=='S2']
all.ids <- gsub(".*_","",all.ids)
all.ids <- sub('\\..*', '', all.ids)
all.ids <- sort(as.numeric(as.character(unique(all.ids))))

key4b <- cbind.data.frame('id'=all.ids ,'Household'=rep(1:(length(all.ids)/2),each=2) )      
key4b$id <- paste0('S2_', key4b$id)
key4b$Household <- paste0('S2_', key4b$Household) #SEASON 1 key

key1ab <- bind_rows(key4a, key4b)

c1.m$ID <- sub("\\.[0-9]+$", "", c1.m$Sample)

c1.m$time_1a <- sub('.*\\.', '', c1.m$Sample)
c1.m$time_2a <- as.numeric(substr(c1.m$time_1a,1,1)) 
c1.m$time_2b <- as.numeric(substr(c1.m$time_1a,2,2))
c1.m$time_2b[is.na(c1.m$time_2b)] <- 0
c1.m$time <- round(c1.m$time_2a + c1.m$time_2b/10)

c1.m <- c1.m[order(c1.m$ID, c1.m$time),]

c1.m <- merge(c1.m, key1ab, by.x='ID',by.y='id', all.x=T)

c1.m <- c1.m[!(c1.m$ID %in% c('S1_38','S1_39', 'S1_48','S1_49', 'S1_58', 'S1_59')),]

d1 <- dcast(c1.m[,c('ID','Target','time','ct_pos2')],  ID +Target ~ time, fun.aggregate = min, na.rm=T, fill=9999, value.var = 'ct_pos2')


write.csv(d1, './Data/Result1_S2.csv')

d1.alt <- dcast(c1.m[,c('ID','Target','time','ct_pos2')],  ID +time~ Target, fun.aggregate = min, na.rm=T, fill=9999, value.var = 'ct_pos2')

d1.alt <- d1.alt[!(d1.alt$ID %in% c('S1_38','S1_39', 'S1_48','S1_49', 'S1_58', 'S1_59')),] #these IDs were not included in study

write.csv(d1.alt, './Data/Result2_S2.csv') #Anne's preferred format

c1.m <- c1.m[!(c1.m$ID %in% c('S1_38','S1_39', 'S1_48','S1_49', 'S1_58', 'S1_59')),] #these IDs were not included in study

d1.a <- acast(c1.m[,c('ID','Target','time','ct_pos2','Household')], Target ~ ID +Household ~ time, fun.aggregate=min, na.rm=T, fill=9999, value.var = 'ct_pos2')

d1.ds <- reshape2::dcast(c1.m[,c('ID','Target','time','ct_pos2','Household')],   Household +ID+ time ~ Target, fun.aggregate=min, na.rm=T, fill=9999, value.var = 'ct_pos2')

d1.ds <- d1.ds %>%
  group_by(Household ) %>%
  mutate(HH_order= as.numeric(as.factor(ID))) %>%
  ungroup()

##MANUALLY UPDATE S2 RESULTS BASED ON AUDIT BY ANNE WYLLIE (OCT 31, 2023):

d1.ds <- d1.ds %>%
  mutate(  piab = if_else(ID=='S2_2' & time==6, 38.34,
                  if_else(ID=='S2_24' & time==2, 38.62, 
                  if_else(ID=='S2_25' & time==1, 32.46,
                  if_else(ID=='S2_25' & time==2, 30.21,         
                  if_else(ID=='S2_25' & time==3, 31,       
                  if_else(ID=='S2_25' & time==4, 31.32,        
                  if_else(ID=='S2_25' & time==6, 32.45,         
                  if_else(ID=='S2_27' & time==2, 36.37,         
                  if_else(ID=='S2_28' & time==1, 28.18,         
                  if_else(ID=='S2_28' & time==2, 35.92,         
                  if_else(ID=='S2_28' & time==5, 32.09,         
                  if_else(ID=='S2_35' & time==5, 30.77,
                  if_else(ID=='S2_35' & time==6, 31.91,
                  if_else(ID=='S2_36' & time==5, 37.23,               
                  if_else(ID=='S2_40' & time==6, 38.58,       
                  if_else(ID=='S2_41' & time==6, 36.07,         
                  if_else(ID=='S2_48' & time==6, 38.6,         
                  if_else(ID=='S2_49' & time==5, 38.27,         
                  if_else(ID=='S2_50' & time==5, 38.9,         
                  if_else(ID=='S2_61' & time==3, 38.8,         
                  if_else(ID=='S2_70' & time==3, 23.09,                                                         
                  if_else(ID=='S2_89' & time==2, 45 ,
                          piab
                                  )))))))))))))))))))))),
                  lyta = if_else(ID=='S2_2' & time==6, 38.28,
                  if_else(ID=='S2_24' & time==2, 45, 
                  if_else(ID=='S2_25' & time==1, 30.26,
                  if_else(ID=='S2_25' & time==2, 29.3,         
                  if_else(ID=='S2_25' & time==3, 29.26,       
                  if_else(ID=='S2_25' & time==4, 29.98,        
                  if_else(ID=='S2_25' & time==6, 30.01,         
                  if_else(ID=='S2_27' & time==2, 36.53,         
                  if_else(ID=='S2_28' & time==1, 27.55,         
                  if_else(ID=='S2_28' & time==2, 34.63,         
                  if_else(ID=='S2_28' & time==5, 31.03,         
                  if_else(ID=='S2_35' & time==5, 28.45,
                  if_else(ID=='S2_35' & time==6, 29.36,
                  if_else(ID=='S2_36' & time==5, 45,               
                  if_else(ID=='S2_40' & time==6, 25.08,       
                  if_else(ID=='S2_41' & time==6, 36.15,         
                  if_else(ID=='S2_48' & time==6, 45,         
                  if_else(ID=='S2_49' & time==5, 37.33,         
                  if_else(ID=='S2_50' & time==5, 45,         
                  if_else(ID=='S2_61' & time==3, 37.33,         
                  if_else(ID=='S2_70' & time==3, 22.22,                                                         
                  if_else(ID=='S2_89' & time==2,  45,
                          lyta
                                  )))))))))))))))))))))),          

           )

# d1.ds %>%
#   filter(substr(ID,1,2)=='S2' &piab<=45) %>%
#   ggplot(aes(x=piab, y=pia_update)) +
#     geom_point() +
#   geom_hline(yintercept=35) +
#   geom_vline(xintercept=35) +
#   theme_classic()

saveRDS(d1.ds, './data/PCR_compiled.rds')

out.list <- list('clean_pcr'=d1.ds,  'pcr_summary'=c1.m, 'pcr_summary_long'=c1.c)
}
