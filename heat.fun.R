
heat.fun <- function( target='piab', cut1=15, cut2=45, cut3 = 47, cut4=95,cut5=141){
#ds1 <- data.array[target,,]
ds1 <- clean_pcr$clean_pcr
ds1$lyta[ds1$lyta==9999] <- NA
ds1$piab[ds1$piab==9999] <- NA

breakslist<-seq(cut1,cut2,by=1)

weeks<- paste0('Week ', c(0,2,4,6,8,10)) 

ds1$season <- substr(ds1$ID,1,2)
ds1$seasonid <- as.numeric(substring(ds1$ID, 4))
ds1$seasonhh <- as.numeric(substring(ds1$Household, 4))

ds1 <- ds1 %>%
  group_by(season, seasonid, seasonhh) %>% 
  mutate(sampleID=cur_group_id()) %>%
  ungroup() %>%
  group_by(season,seasonhh) %>%
  mutate(house=group_indices()) %>%
  ungroup() %>% 
 arrange(season,seasonhh, seasonid)


ds.plot <- dcast(ds1, house+sampleID + season + seasonid + seasonhh +HH_order~time, value.var = target)

ds.plot$labels <- paste(ds.plot$season, ds.plot$seasonid, sep='_')
sub1 <- ds.plot[1:cut3,] 
gap1 <- which(sub1$HH_order==1) +1
gap1 <- gap1[-length(gap1)]

#First half
heat1 <- (pheatmap(sub1[,c('1','2','3','4','5','6')], 
                   cluster_cols = F, 
                   cluster_rows = F, 
                   gaps_row = gap1,
                   labels_row = sub1$labels,
                   cellwidth = 9,
                   cellheight = 9,
                   color=colorRampPalette(c('red4','red','darkorange','orange','gold','lightgoldenrod1','white','white'))(length(breakslist)),
                   breaks=breakslist,
                   legend=F,
                   labels_col = weeks))

sub2 <- ds.plot[(cut3+1):cut4,] 
gap2 <- which(sub2$HH_order==1) +1
gap2 <- gap2[-length(gap2)]

heat2<-pheatmap(sub2[,c('1','2','3','4','5','6')],
                cluster_cols = F, 
                cluster_rows = F, 
                gaps_row = gap2,
                labels_row = sub2$labels,
                cellwidth = 9,
                cellheight = 9,
                color=colorRampPalette(c('red4','red','darkorange','orange','gold','lightgoldenrod1','white','white'))(length(breakslist)),
                breaks=breakslist,
                legend=F,
                labels_col = weeks)

sub3 <- ds.plot[(cut4+1):cut5,] 
gap3 <- which(sub3$HH_order==1) +1
gap3 <- gap3[-length(gap3)]

heat3<-pheatmap(sub3[,c('1','2','3','4','5','6')],
                cluster_cols = F, 
                cluster_rows = F, 
                gaps_row = gap3,
                labels_row = sub3$labels,
                cellwidth = 9,
                cellheight = 9,
                color=colorRampPalette(c('red4','red','darkorange','orange','gold','lightgoldenrod1','white','white'))(length(breakslist)),
                breaks=breakslist,
                legend=F,
                labels_col = weeks)

sub4 <- ds.plot[(cut5+1):nrow(ds.plot),] 
gap4 <- which(sub4$HH_order==1) +1
gap4 <- gap4[-length(gap4)]

heat4<-pheatmap(sub4[,c('1','2','3','4','5','6')],
                cluster_cols = F, 
                cluster_rows = F, 
                gaps_row = gap4,
                labels_row = sub4$labels,
                cellwidth = 9,
                cellheight = 9,
                color=colorRampPalette(c('red4','red','darkorange','orange','gold','lightgoldenrod1','white','white'))(length(breakslist)),
                breaks=breakslist,
                legend=T,
                labels_col = weeks)


tiff(paste("Heat", target,".tiff"), width=8, height=8, unit='in', res=200)
grid.arrange(heat1[[4]],heat2[[4]],heat3[[4]],heat4[[4]],ncol=4)
dev.off()

out.graph <- grid.arrange(heat1[[4]],heat2[[4]],heat3[[4]],heat4[[4]],ncol=4)

return(out.graph)

}