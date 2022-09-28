
heat.fun.contact <- function(ds=q3.c, cut3=47){
ds1 <- ds[,c('1','2','3','4','5','6')]
ds1[ds1==9999] <- NA

weeks<- paste0('Week ', c(0,2,4,6,8,10)) 

house <- ds$Household
sampleID <- ds$ID

#First half
heat1 <- (pheatmap(ds1[1:cut3,], 
                   cluster_cols = F, 
                   cluster_rows = F, 
                   gaps_row = c(head(as.numeric(cumsum(table(house[1:cut3]))), -1)),
                   labels_row = sampleID[1:cut3],
                   cellwidth = 9,
                   cellheight = 9,
                   color=c('white','red4'),
                   breaks=c(-1,0,1),
                   legend=F,
                   labels_col = weeks))

heat2<-pheatmap(ds1[(cut3+1):nrow(ds1),],
                cluster_cols = F, 
                cluster_rows = F, 
                gaps_row = c(head(as.numeric(cumsum(table(house[(cut3+1):nrow(ds1)]))), -1)),
                labels_row = sampleID[(cut3+1):nrow(ds1)],
                cellwidth = 9,
                cellheight = 9,
                color=c('white','red4'),
                breaks=c(-1,0,1),
                legend=F,
                labels_col = weeks)

tiff(paste0("Heat_contact",".tiff"), width=4, height=8, unit='in', res=200)
grid.arrange(heat1[[4]],heat2[[4]],ncol=2)
dev.off()

out.graph <- grid.arrange(heat1[[4]],heat2[[4]],ncol=2)
return(out.graph)

}
