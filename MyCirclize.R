LobeID <- read.table("/Circle edges/LobeID.txt", header = TRUE, sep = "\t")
head(LobeID)
GeneID <- read.table("/Circle edges/GyrusID.txt", header = TRUE, sep = "\t")
head(GeneID)
GeneFe <- read.table("/Circle edges/ROIID.txt", header = TRUE, sep = "\t")
head(GeneFe)
Links <- read.table("/Circle edges/Links.txt", header = TRUE, sep = "\t")
head(Links)

LobeID$LobeID <- factor(LobeID$LobeID, levels = LobeID$LobeID)

library(stringi)
library(circlize)


pdf("circlize1.pdf",width = 17.5,height = 10)
par(mar=rep(0,4))
circos.clear()
circos.par(start.degree = 90, #�����￪ʼ����������ʱ��˳��
           gap.degree = 2, #ÿ����sectors��ļ����С���˴���ÿ�������ļ��
           track.margin = c(0,0.08), #gap.degree�涨�����ң�track.margin�涨����
           cell.padding = c(0,0,0,0) #��
)


#### ����"LobeID.txt"����

#ͼ�Ƕ����Χ�ɵ�һ��Ȧ������ÿ���鼰�����鶼����һ������������ϵ��
#����ķ�����Ϊx��
circos.initialize(factors = LobeID$LobeID,
                  xlim = cbind(LobeID$LobeStart, LobeID$LobeEnd))

circos.trackPlotRegion(ylim = c(0, 1), factors = LobeID$LobeID, track.height=0.1,
                       
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name <- get.cell.meta.data("sector.index")
                         i <- get.cell.meta.data("sector.numeric.index")
                         xlim <- get.cell.meta.data("xlim")
                         ylim <- get.cell.meta.data("ylim")
                         
                         #��ID���ֵ�����
                         circos.text(x = mean(xlim), 
                                     y = 1,
                                     labels = name,
                                     cex = 1.7,   #��ID���ִ�С
                                     #col = "darkgrey", #��ID����ɫ
                                     adj = c(0.5,-0.8)
                                     #���������ID�ʷ���״���У������������д�����������
                                     #niceFacing = TRUE, #��IDͷ����
                                     #facing = "reverse.clockwise",
                                     #adj = c(1.5, -0.8) #��ID��λ��
                         )
                         
                         #����
                         circos.rect(xleft = xlim[1], 
                                     ybottom = ylim[1],
                                     xright = xlim[2], 
                                     ytop = ylim[2],
                                     col = "blue",
                                     border = "black")
                       })




#### ����"GyrusID.txt"������

library(graphics)
#bgcol <- rainbow(sum(LobeID$LobeEnd), s = 1, v = 1, start = 0, end = max(1, sum(LobeID$LobeEnd) - 1)/sum(LobeID$LobeEnd), alpha = 1)

for (i in seq_len(nrow(GeneID))){
  ylim <- c(0, 1)
  circos.rect(sector.index = GeneID$LobeID[i],
              track.index = 1,
              xleft = GeneID$featureStart[i], # + 0.01, #��feature֮��������϶
              ybottom = ylim[1],
              xright = GeneID$featureEnd[i], # - 0.01, #��feature֮��������϶
              ytop = ylim[2],
              #�������ÿ��������ɫ��ͬ���òʺ�ɫ��ÿһ��Gene feature��������������
              #col = bgcol[i],
              #�������ÿ������ͬһ����ɫ�����������ļ���д�õ���ɫ����������������
              col = "white",
              border = paste("#", GeneID$barColor[i], sep = ""), lwd = 8)
}

# ���������
for (i in seq_len(nrow(GeneID))){
  ylim <- c(0, 1)
  circos.text((GeneID$featureStart[i] + GeneID$featureEnd[i])/2,
              ylim[1] + 0.5,
              GeneID$GyrusID[i],
              sector.index = GeneID$LobeID[i],
              facing = "inside",
              cex = 0.9)
}




circos.trackPlotRegion(ylim = c(0, 1), factors = LobeID$LobeID, track.height=0.1,

                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name <- get.cell.meta.data("sector.index")
                         i <- get.cell.meta.data("sector.numeric.index")
                         xlim <- get.cell.meta.data("xlim")
                         ylim <- get.cell.meta.data("ylim")

                         #����
                         circos.rect(xleft = xlim[1],
                                     ybottom = ylim[1],
                                     xright = xlim[2],
                                     ytop = ylim[2],
                                     col = "blue",
                                     border = "white",lwd = 6)
                       })


#�������С��黹����ԲȦͼ��ÿ��������ɫ��ͬ��������ʾ�ʺ�ɫ������circlize_rainbow.pdf������Ч�����Ͱ��������forѭ����ġ�col = ...���л��ɡ�col = bgcol[i], ��
#�����������У����òʺ�ɫ
library(graphics)
bgcol <- rainbow(sum(LobeID$LobeEnd), s = 1, v = 1, start = 0, end = max(1, sum(LobeID$LobeEnd) - 1)/sum(LobeID$LobeEnd), alpha = 1)

for (i in seq_len(nrow(GeneFe))){
  ylim <- c(0, 1)
  circos.rect(sector.index = GeneFe$LobeID[i], 
              track.index = 2,
              xleft = GeneFe$featureStart[i], # + 0.01, #��feature֮��������϶
              ybottom = ylim[1], 
              xright = GeneFe$featureEnd[i], # - 0.01, #��feature֮��������϶
              ytop = ylim[2],
              #�������ÿ��������ɫ��ͬ���òʺ�ɫ��ÿһ��Gene feature��������������
              #col = bgcol[i],
              #�������ÿ������ͬһ����ɫ�����������ļ���д�õ���ɫ����������������
              col = paste("#", GeneFe$barColor[i], sep = ""),
              border = "gray")
}

# ���������
for (i in seq_len(nrow(GeneFe))){
  ylim <- c(0, 1)
  circos.text(GeneFe$featureStart[i] + 0.5,
              ylim[1] + 0.5,
              GeneFe$ROIID[i],
              sector.index = GeneFe$LobeID[i], 
              facing = "inside",
              cex = 0.8)
}







#### ����"Links.txt"������
for(i in seq_len(nrow(Links))){
  circos.link(sector.index1 = Links$ROI_1[i], 
              point1 = Links[i, 2],
              sector.index2 = Links$ROI_2[i], 
              point2 = Links[i, 4],
              
              #�������ļ����Ѿ�д�������ߵ���ɫ
              col = paste("#", Links$link_color[i], sep = ""), 
              #��������������ļ���д��ɫ�������������������У�������ָ����ɫ
              #col = "red", 
              
              border = FALSE, 
              lwd = 2, #���ߵĴ�ϸ
              rou = 0.64, #������ʼ����ֹ���yֵ��Բ�뾶�İٷֱȣ�
              h.ratio = 0.7 #���ߵĹ��θ߶ȣ���ֵԽ�󣬹���Խ��
              #h.ratio = Links$link_height[i] #�������ļ���ָ�����θ߶�
  )
}


#Legend of gene
GeneCol<-data.frame(legendID=GeneID$GyrusID,legendCol=GeneID$barColor)
GeneCol_uniq<-base::unique(GeneCol) 
legend(1,0.2,
       bty = "n",#��Ҫ�߿�
       pch = 16,#���������Σ�Բ����16
       cex = 1.2,
       legend = c("SFG:Superior_Frontal_Gyrus","PrG:Precentral_Gyrus","PCL:Paracentral_Lobule",
                  "STG:Superior_Temporal_Gyrus","MTG:Middle_Temporal_Gyrus","FuG:Fusiform_Gyrus",
                  "SPL:Superior_Parietal_Lobule","IPL:Inferior_Parietal_Lobuleb","PoG:Postcentral_Gyrus",
                  "MVOcC:Medio_Ventral_Occipital_Cortex","LOcC:lateral_Occipital_Cortex"),
       col = paste0("#",GeneCol_uniq$legendCol),
       horiz = FALSE)

#Legend of links
LinksCol<-data.frame(Links$link_color)
LinksCol_uniq<-base::unique(LinksCol) 
legend(1,0.4,
       bty = "n",
       lty = 1,#����
       lwd = 5,#�ߵĴ�ϸ
       legend = c("Not Significantly correlated with HAMD","Significantly correlated with HAMD"),
       col = paste0("#",LinksCol_uniq$Links.link_col),
       horiz = FALSE)

dev.off()
