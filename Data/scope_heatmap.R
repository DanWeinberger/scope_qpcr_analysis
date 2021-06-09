all<-readxl::read_xlsx('./Data/cleaned_file_20210603.xlsx',sheet=2)


library(pheatmap)
library(RColorBrewer)
library(cowplot)
library(gridExtra)


piab<-all[,c(3,6,9,12,15,18)]
piab$piab4<-as.numeric(piab$piab4)
rownames(piab)<-all$Sample

#ensure you have household numbers in datafile - also currently manual
house<-all[,'Household']
rownames(house)<-all$Sample

breakslist<-seq(15,45,by=1)


weeks<-c('Week 1','Week 2','Week 3','Week 4','Week 5','Week 6')

#First half
heat1<-pheatmap(piab[1:47,], 
        cluster_cols = F, 
        cluster_rows = F, 
        gaps_row = c(head(as.numeric(cumsum(table(house$Household[1:47]))), -1)),
        labels_row = all$Sample[1:47],
        cellwidth = 9,
        cellheight = 9,
        color=colorRampPalette(c('red4','red','darkorange','orange','gold','lightgoldenrod1','white','white'))(length(breakslist)),
        breaks=breakslist,
        legend=F,
        labels_col = weeks)

#2,4,6,8,10,12,14,16,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47


#second half
heat2<-pheatmap(piab[48:length(piab$piab1),],
        cluster_cols = F, 
        cluster_rows = F, 
        gaps_row = c(head(as.numeric(cumsum(table(house$Household[48:length(house$Household)]))), -1)),
        labels_row = all$Sample[48:length(piab$piab1)],
        cellwidth = 9,
        cellheight = 9,
        color=colorRampPalette(c('red4','red','darkorange','orange','gold','lightgoldenrod1','white','white'))(length(breakslist)),
        breaks=breakslist,
        legend=T,
        labels_col = weeks)

#2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46

#group together - currently have to manually size
grid.arrange(heat1[[4]],heat2[[4]],ncol=2)





