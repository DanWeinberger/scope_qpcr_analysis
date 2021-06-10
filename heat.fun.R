
heat.fun <- function(target='piab', cut1=15, cut2=45, cut3 = 47){
ds1 <- d1.a[target,,]
ds1[ds1==9999] <- NA

breakslist<-seq(cut1,cut2,by=1)

weeks<- paste0('Week ', 1:6) 

house <- word(dimnames(ds1)[[1]], 2, sep = "_")

#First half
heat1 <- (pheatmap(ds1[1:cut3,], 
                   cluster_cols = F, 
                   cluster_rows = F, 
                   gaps_row = c(head(as.numeric(cumsum(table(house[1:cut3]))), -1)),
                   labels_row = all$Sample[1:cut3],
                   cellwidth = 9,
                   cellheight = 9,
                   color=colorRampPalette(c('red4','red','darkorange','orange','gold','lightgoldenrod1','white','white'))(length(breakslist)),
                   breaks=breakslist,
                   legend=F,
                   labels_col = weeks))

heat2<-pheatmap(ds1[(cut3+1):nrow(ds1),],
                cluster_cols = F, 
                cluster_rows = F, 
                gaps_row = c(head(as.numeric(cumsum(table(house[(cut3+1):nrow(ds1)]))), -1)),
                labels_row = all$Sample[(cut3+1):nrow(ds1)],
                cellwidth = 9,
                cellheight = 9,
                color=colorRampPalette(c('red4','red','darkorange','orange','gold','lightgoldenrod1','white','white'))(length(breakslist)),
                breaks=breakslist,
                legend=T,
                labels_col = weeks)

tiff(paste("Heat", target,".tiff"), width=4, height=8, unit='in', res=400)
grid.arrange(heat1[[4]],heat2[[4]],ncol=2)
dev.off()

out.graph <- grid.arrange(heat1[[4]],heat2[[4]],ncol=2)
return(out.graph)

}