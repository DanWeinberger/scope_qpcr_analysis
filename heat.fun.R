
heat.fun <- function( target='piab', cut1=15, cut2=45, cut3 = 47, cut4=94,cut5=141){
#ds1 <- data.array[target,,]
ds1 <- d1.ds
ds1$lyta[ds1$lyta==9999] <- NA
ds1$piab[ds1$piab==9999] <- NA

breakslist<-seq(cut1,cut2,by=1)

weeks<- paste0('Week ', 1:6) 

ds1$season <- substr(ds1$ID,1,2)
ds1$seasonid <- as.numeric(substring(ds1$ID, 4))
ds1$seasonhh <- as.numeric(substring(ds1$Household, 4))

ds1$sampleID <- ds1 %>% group_by(season, seasonid, seasonhh) %>% group_indices()

ds1 <- arrange(ds1, season,seasonhh, seasonid)

#ds1$house <- as.numeric(as.factor(ds1$Household))

ds.plot <- dcast(ds1, house+sampleID + season + seasonid + seasonhh~time, value.var = target)

ds.plot <- ds.plot %>%
  arrange(season,seasonhh) %>%
  mutate(house=group_indices()) %>%
  ungroup() %>%
  group_by(house,sampleID) %>%
  mutate()
  arrange(house, sampleID)

sub1 <- ds.plot[1:cut3,] 
gap1 <- which(sub1$hh_order==1) +1
gap1 <- gap1[-length(gap1)]

#First half
heat1 <- (pheatmap(sub1[,c('1','2','3','4','5','6')], 
                   cluster_cols = F, 
                   cluster_rows = F, 
                   gaps_row = gap1,
                   labels_row = sub1$sampleID,
                   cellwidth = 9,
                   cellheight = 9,
                   color=colorRampPalette(c('red4','red','darkorange','orange','gold','lightgoldenrod1','white','white'))(length(breakslist)),
                   breaks=breakslist,
                   legend=F,
                   labels_col = weeks))

sub2 <- ds.plot[(cut3+1):cut4,] 
gap2 <- which(sub2$hh_order==1) +1
gap2 <- gap2[-length(gap2)]

heat2<-pheatmap(sub2[,c('1','2','3','4','5','6')],
                cluster_cols = F, 
                cluster_rows = F, 
                gaps_row = gap2,
                labels_row = sub2$sampleID,
                cellwidth = 9,
                cellheight = 9,
                color=colorRampPalette(c('red4','red','darkorange','orange','gold','lightgoldenrod1','white','white'))(length(breakslist)),
                breaks=breakslist,
                legend=F,
                labels_col = weeks)

sub3 <- ds.plot[(cut4+1):cut5,] 
gap3 <- which(sub3$hh_order==1) +1
gap3 <- gap3[-length(gap3)]

heat3<-pheatmap(sub3[,c('1','2','3','4','5','6')],
                cluster_cols = F, 
                cluster_rows = F, 
                gaps_row = gap3,
                labels_row = sub3$sampleID,
                cellwidth = 9,
                cellheight = 9,
                color=colorRampPalette(c('red4','red','darkorange','orange','gold','lightgoldenrod1','white','white'))(length(breakslist)),
                breaks=breakslist,
                legend=F,
                labels_col = weeks)

sub4 <- ds.plot[(cut5+1):nrow(ds.plot),] 
gap4 <- which(sub4$hh_order==1) +1
gap4 <- gap4[-length(gap4)]

heat4<-pheatmap(sub4[,c('1','2','3','4','5','6')],
                cluster_cols = F, 
                cluster_rows = F, 
                gaps_row = gap4,
                labels_row = sub4$sampleID,
                cellwidth = 9,
                cellheight = 9,
                color=colorRampPalette(c('red4','red','darkorange','orange','gold','lightgoldenrod1','white','white'))(length(breakslist)),
                breaks=breakslist,
                legend=T,
                labels_col = weeks)


tiff(paste("Heat", target,".tiff"), width=4, height=8, unit='in', res=200)
grid.arrange(heat1[[4]],heat2[[4]],heat3[[4]],heat4[[4]],ncol=4)
dev.off()

out.graph <- grid.arrange(heat1[[4]],heat2[[4]],ncol=2)
return(out.graph)

}