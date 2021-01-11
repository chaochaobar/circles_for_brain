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
circos.par(start.degree = 90, #从哪里开始画，沿着逆时针顺序
           gap.degree = 2, #每两个sectors间的间隔大小，此处是每两个组间的间隔
           track.margin = c(0,0.08), #gap.degree规定了左右，track.margin规定上下
           cell.padding = c(0,0,0,0) #？
)


#### 根据"LobeID.txt"画组

#图是多个组围成的一个圈，其中每个组及其亚组都画在一个单独的坐标系里
#基因的方向作为x轴
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
                         
                         #组ID文字的设置
                         circos.text(x = mean(xlim), 
                                     y = 1,
                                     labels = name,
                                     cex = 1.7,   #组ID文字大小
                                     #col = "darkgrey", #组ID的颜色
                                     adj = c(0.5,-0.8)
                                     #如果想让组ID呈放射状排列，就用下面三行代替上面那行
                                     #niceFacing = TRUE, #组ID头朝上
                                     #facing = "reverse.clockwise",
                                     #adj = c(1.5, -0.8) #组ID的位置
                         )
                         
                         #画组
                         circos.rect(xleft = xlim[1], 
                                     ybottom = ylim[1],
                                     xright = xlim[2], 
                                     ytop = ylim[2],
                                     col = "blue",
                                     border = "black")
                       })




#### 根据"GyrusID.txt"画亚组

library(graphics)
#bgcol <- rainbow(sum(LobeID$LobeEnd), s = 1, v = 1, start = 0, end = max(1, sum(LobeID$LobeEnd) - 1)/sum(LobeID$LobeEnd), alpha = 1)

for (i in seq_len(nrow(GeneID))){
  ylim <- c(0, 1)
  circos.rect(sector.index = GeneID$LobeID[i],
              track.index = 1,
              xleft = GeneID$featureStart[i], # + 0.01, #让feature之间流出空隙
              ybottom = ylim[1],
              xright = GeneID$featureEnd[i], # - 0.01, #让feature之间流出空隙
              ytop = ylim[2],
              #如果想让每个方块颜色不同，用彩虹色画每一个Gene feature就运行下面这行
              #col = bgcol[i],
              #如果想让每个组用同一个颜色，调用输入文件中写好的颜色，就运行下面这行
              col = "white",
              border = paste("#", GeneID$barColor[i], sep = ""), lwd = 8)
}

# 亚组的文字
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

                         #画组
                         circos.rect(xleft = xlim[1],
                                     ybottom = ylim[1],
                                     xright = xlim[2],
                                     ytop = ylim[2],
                                     col = "blue",
                                     border = "white",lwd = 6)
                       })


#提需求的小伙伴还想让圆圈图中每个方块颜色不同，整体显示彩虹色，就像circlize_rainbow.pdf那样的效果，就把下面这个for循环里的“col = ...”行换成“col = bgcol[i], ”
#运行下面两行，设置彩虹色
library(graphics)
bgcol <- rainbow(sum(LobeID$LobeEnd), s = 1, v = 1, start = 0, end = max(1, sum(LobeID$LobeEnd) - 1)/sum(LobeID$LobeEnd), alpha = 1)

for (i in seq_len(nrow(GeneFe))){
  ylim <- c(0, 1)
  circos.rect(sector.index = GeneFe$LobeID[i], 
              track.index = 2,
              xleft = GeneFe$featureStart[i], # + 0.01, #让feature之间流出空隙
              ybottom = ylim[1], 
              xright = GeneFe$featureEnd[i], # - 0.01, #让feature之间流出空隙
              ytop = ylim[2],
              #如果想让每个方块颜色不同，用彩虹色画每一个Gene feature就运行下面这行
              #col = bgcol[i],
              #如果想让每个组用同一个颜色，调用输入文件中写好的颜色，就运行下面这行
              col = paste("#", GeneFe$barColor[i], sep = ""),
              border = "gray")
}

# 亚组的文字
for (i in seq_len(nrow(GeneFe))){
  ylim <- c(0, 1)
  circos.text(GeneFe$featureStart[i] + 0.5,
              ylim[1] + 0.5,
              GeneFe$ROIID[i],
              sector.index = GeneFe$LobeID[i], 
              facing = "inside",
              cex = 0.8)
}







#### 根据"Links.txt"画连线
for(i in seq_len(nrow(Links))){
  circos.link(sector.index1 = Links$ROI_1[i], 
              point1 = Links[i, 2],
              sector.index2 = Links$ROI_2[i], 
              point2 = Links[i, 4],
              
              #在输入文件中已经写好了连线的颜色
              col = paste("#", Links$link_color[i], sep = ""), 
              #如果不想在输入文件中写颜色，还可以运行下面这行，在这里指定颜色
              #col = "red", 
              
              border = FALSE, 
              lwd = 2, #连线的粗细
              rou = 0.64, #连线起始和终止点的y值（圆半径的百分比）
              h.ratio = 0.7 #连线的拱形高度，数值越大，拱形越低
              #h.ratio = Links$link_height[i] #在输入文件中指定拱形高度
  )
}


#Legend of gene
GeneCol<-data.frame(legendID=GeneID$GyrusID,legendCol=GeneID$barColor)
GeneCol_uniq<-base::unique(GeneCol) 
legend(1,0.2,
       bty = "n",#不要边框
       pch = 16,#画成正方形，圆点是16
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
       lty = 1,#画线
       lwd = 5,#线的粗细
       legend = c("Not Significantly correlated with HAMD","Significantly correlated with HAMD"),
       col = paste0("#",LinksCol_uniq$Links.link_col),
       horiz = FALSE)

dev.off()

