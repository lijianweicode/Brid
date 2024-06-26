
#PCA
library(vegan)
library(Rmisc)
library(ggsci)
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(ggplot2)
library(ggprism)

setwd("PCoA")

#data
bird <- read.csv("fd_all.csv",header = T,sep= ",",row.names = 1)
bird<- log(bird+1)
group <- read.csv("group.csv",header = T,sep= ",",row.names = 1)


distance <- vegdist(bird, method = 'bray')
pcoa <- cmdscale(distance, k = (nrow(bird) - 1), eig = TRUE)
## plot data
plot_data <- data.frame({pcoa$point})[1:2]


plot_data$ID <- rownames(plot_data)
names(plot_data)[1:2] <- c('PCoA1', 'PCoA2')

eig = pcoa$eig

plot_data <- merge(plot_data, group, by = 'ID', all.x = TRUE)

dune_dist <- vegdist(bird, method="bray", binary=F)
dune_pcoa <- cmdscale(dune_dist, k=(nrow(bird) - 1), eig=T)

dune_pcoa_points <- as.data.frame(dune_pcoa$points)
sum_eig <- sum(dune_pcoa$eig)
eig_percent <- round(dune_pcoa$eig/sum_eig*100,1)

colnames(dune_pcoa_points) <- paste0("PCoA", 1:3)

dune_pcoa_result <- cbind(dune_pcoa_points, group)

dune_pcoa_result$class <- factor(dune_pcoa_result$iucn)


#----------------------------------------------------------------
#PERMANOVA test
dune.div <- adonis2(bird ~ class, data = dune_pcoa_result, permutations = 999, method="bray")
#----------------------------------------------------------------
write.csv(dune_dist,"dune_dist.csv")
#----------------------------------------------------------------

#plot
library(ggplot2)
pn <- ggplot(dune_pcoa_result, aes(PCoA1, PCoA2)) +
    geom_point(aes(colour = class),size=4,alpha = 1)+ 
    
    labs(title="") + 
    xlab(paste("PCoA1 61.80%")) + 
    ylab(paste("PCoA2 22.53%"))+
    geom_vline(aes(xintercept = 0),linetype="dotted")+
    geom_hline(aes(yintercept = 0),linetype="dotted")+
    theme(plot.title = element_text(size=3,colour = "black",hjust = 0.5,face = "bold"))
endpn <- pn+theme_prism(border = TRUE) + coord_cartesian(clip = "off")+scale_color_manual(values = c("#ba6d6c","#ba6d6c","#999999","#ba6d6c","#ba6d6c"))+theme(text=element_text(family="serif"))+theme(legend.position="right")
endpn
