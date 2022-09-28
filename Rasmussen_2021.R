#Rasmussen et al., 2022
#Full complement of R scripts used for data analysis in the manuscript:
# <ADD CITATION>

#Erin Sparks
#Februray 7, 2021

#R Version 4.2.0 for Mac
setwd("~/Dropbox/UD/Writing/2020_N15Paper/RInputFiles")

#Version 3.3.6
library("ggplot2")
#Version 1.0.9
library("dplyr")
#Version 1.4.4
library("reshape2")
#Version 0.4.0
library("ggpubr")
#Version 0.1.7
library("png")
#Version 1.1.1
library("cowplot")
#Version 0.9.22
library("rptR")
#Version 3.0.13
library("car")
#Version 2.4.15
library("rcompanion")
#Version 2.9
library("ggsci")  
#Version 0.92
library("corrplot")
#Version 1.3.5
library("agricolae")




#####
#input data
#3point bending data
data1 <-read.csv ("LE_processedData_03042021_BWR.csv")
head(data1)
#pixel areas
data2r <-read.csv("rootpixel.csv")
head(data2r)
data2s <-read.csv("shootpixel.csv")
head(data2s)
#dry weights
data3 <-read.csv ("dryweights.csv")
head(data3)
#N15 Uptake Data from mechanically different genotypes
data4 <-read.csv("AllGenoUptake_Soil.csv")
head(data4)
#Averages Comparison
data5 <-read.csv("PlantAverages.csv")
head(data5)
#Averages Comparison - only complete dataset
data5a <-read.csv("PlantAverages_complete.csv")
head(data5a)
#Uptake by Diameter
data6 <-read.csv("UptakeByDiameter2.csv")
head(data6)
#Soil Analysis
data7 <-read.csv("soilanalysis.csv")
head(data7)
#Soil by Uptake Analysis
data8 <-read.csv("SoilByUptake.csv")
head(data8)


#---Figure 1 & S2. Biomechanics---------------------------
head(data1)
data1$Geno = factor(data1$Geno, levels = unique(data1$Geno))
data1$Year = as.factor(data1$Year)


Fig1A <- ggplot (data1, aes(Geno, UL, group=Geno))+
  geom_violin(fill="grey")+
  geom_point(size = 1) +
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(hjust=1, vjust=0.5,size=12,angle = 90), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12, colour = "blue"),
        strip.text.y = element_text(size = 12, colour = "black")) +
  ylab("Ultimate Load (N)") + 
  xlab("")

#res.aov <- aov(UL ~ Geno, data = data1)
#summary(res.aov)
#tukey.test <- TukeyHSD(res.aov)
t#ukey.test

res_aov <- lm(UL ~ Geno, data = data1)
anova(res_aov)
tukey_aov=aov(res_aov)
HSD.test(tukey_aov, trt = c("Geno"), console = TRUE)


FigS1A <- ggplot (data1, aes(Geno, BL, group=Geno))+
  geom_violin(fill="grey")+
  geom_point(size = 1) +
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(hjust=1, vjust=0.5,size=12,angle = 90), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12, colour = "blue"),
        strip.text.y = element_text(size = 12, colour = "black")) +
  ylab("Break Load (N)") + 
  xlab("")

res.aov <- aov(BL ~ Geno, data = data1)
summary(res.aov)
tukey.test <- TukeyHSD(res.aov)
tukey.test

FigS1B <- ggplot (data1, aes(UL, BL, color=Geno))+
  geom_point(size = 1) +
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(hjust=1, vjust=0.5,size=12,angle = 90), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12, colour = "blue"),
        strip.text.y = element_text(size = 12, colour = "black")) +
  ylab("Break Load (N)") + 
  xlab("Ultimate Load (N)")

cor.test(data1$UL, data1$BL, method="pearson", use="everything")

#Structural Mechanics by Diameter
head(data1)

res.aov <- aov(MajorD_cm ~ Geno, data = data1)
summary(res.aov)
tukey.test <- TukeyHSD(res.aov)
tukey.test


Fig1B <- 
  ggplot (data1, aes(UL, MajorD_cm, color=Geno))+
  geom_point(size = 1) +
  #geom_smooth(method="glm",se=FALSE)+
  #facet_wrap(~Geno, scales="free", ncol=5)+
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12, colour = "blue"),
        strip.text.y = element_text(size = 12, colour = "blue"),
        legend.title = element_blank()) +
  ylab("Diameter (cm)") + 
  xlab("Ultimate Load (N)")




Fig1C <- 
ggplot (data1, aes(UL, MajorD_cm))+
  geom_point(size = 1) +
  geom_smooth(method="glm",se=FALSE)+
  facet_wrap(~Geno, ncol=5)+
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        plot.title=element_text(size=14, vjust=3), 
        axis.text=element_text(size=14), 
        axis.title = element_text(size=14), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 14, colour = "blue"),
        strip.text.y = element_text(size = 14, colour = "blue")) +
  ylab("Diameter (cm)") + 
  xlab("Ultimate Load (N)")

cor.test(data1$UL, data1$MajorD_cm, method="pearson", use="everything")
model <- lm(formula = UL ~ MajorD_cm, data = data1)
summary(model)

head(data1)
data1$Dbig <- data1$MajorD_cm * 100

B73 <- data1[ which(data1$Geno =='B73'),]
Oh43 <- data1[ which(data1$Geno =='Oh43'),]
A632 <- data1[ which(data1$Geno =='A632'),]
W64A <- data1[ which(data1$Geno =='W64A'),]
HP301 <- data1[ which(data1$Geno =='HP301'),]
HK <- data1[ which(data1$Geno =='Hickory King'),]
GT112 <- data1[ which(data1$Geno =='GT112'),]
Ky21 <- data1[ which(data1$Geno =='Ky21'),]
LH252 <- data1[ which(data1$Geno =='LH252'),]

model <- lm(formula = Dbig ~ UL, data = data1)
summary(model)
#Slope = 0.27432

model <- lm(formula = Dbig ~ UL, data = B73)
summary(model)
#Slope = 1.45
model <- lm(formula = Dbig ~ UL, data = Oh43)
summary(model)
#Slope = 0.8584
model <- lm(formula = Dbig ~ UL, data = A632)
summary(model)
#Slope = 0.5198
model <- lm(formula = Dbig ~ UL, data = W64A)
summary(model)
#Slope = 0.4551
model <- lm(formula = Dbig ~ UL, data = HP301)
summary(model)
#Slope = 0.4817
model <- lm(formula = Dbig ~ UL, data = HK)
summary(model)
#Slope = 0.7623
model <- lm(formula = Dbig ~ UL, data = GT112)
summary(model)
#Slope = 0.7367
model <- lm(formula = Dbig ~ UL, data = Ky21)
summary(model)
#Slope = 0.2527
model <- lm(formula = Dbig ~ UL, data = LH252)
summary(model)
#Slope = 0.16718



#Top Left = 0, 0.5. 
#Top Right = 0.5, 0.5
#Bottom Right = 0.5, 0
#Bottom Left = 0, 0


pdf("Figure1.pdf", width = 8, height = 7)

ggdraw() +
  draw_plot(Fig1A, x = 0, y = 0.45, width = .5, height = .5) +
  draw_plot(Fig1B, x = 0.55, y = 0.5, width = .4, height = .48) +
  draw_plot(Fig1C, x = 0, y = 0, width = .9, height = 0.48) +
  draw_plot_label(label = c("A", "B", "C"), 
                  size = 15,
                  x = c(0, 0.5, 0), 
                  y = c(1, 1, 0.5))
dev.off()

#ggsave("Figure1.jpg", plot=Fig1, height=6, width=8, units=c("in"), dpi=600)


pdf("FigureS2.pdf", width = 6, height = 7)

ggdraw() +
  draw_plot(FigS1A, x = 0.3, y = 0.5, width = .45, height = .45) +
  draw_plot(FigS1B, x = 0.3, y = 0.05, width = .6, height = 0.45) +
  draw_plot_label(label = c("A", "B"), 
                  size = 15,
                  x = c(0.35, 0.35), 
                  y = c(1, 0.55))
#ggsave("FigureS1.jpg", plot=FigS1, height=6, width=8, units=c("in"), dpi=600)

dev.off()


#---Figure 2. Architecture---------------------------

B73root <- readPNG("1311-1_B73_root45.png")
B73shoot <- readPNG("1311-1_B73_shoot45.png")

W64Aroot <- readPNG("1358-2_W64A_root330.png")
W64Ashoot <- readPNG("1358-2_W64A_shoot330.png")

Hp301root <- readPNG("1393-1_Hp301_root199.png")
Hp301shoot <-readPNG("1393-1_Hp301_shoot199.png")

HKroot <- readPNG("1350-2_HK_root85.png")
HKshoot <- readPNG("1350-2_HK_shoot85.png")

Ky21root <- readPNG("1371-2_Ky21_root85.png")
Ky21shoot <- readPNG("1371-2_Ky21_shoot85.png")
  
  
Fig2A <- ggplot() + 
  background_image(B73shoot) 
Fig2B <- ggplot() + 
  background_image(W64Ashoot) 
Fig2C <- ggplot() + 
  background_image(Hp301shoot)
Fig2D <- ggplot() + 
  background_image(HKshoot)
Fig2E <- ggplot() + 
  background_image(Ky21shoot)

Fig2F <- ggplot() + 
  background_image(B73root) 
Fig2G <- ggplot() + 
  background_image(W64Aroot) 
Fig2H <- ggplot() + 
  background_image(Hp301root)
Fig2I <- ggplot() + 
  background_image(HKroot)
Fig2J <- ggplot() + 
  background_image(Ky21root)

data2r$Geno = factor(data2r$Geno, levels = unique(data2r$Geno))
data2s$Geno = factor(data2s$Geno, levels = unique(data2s$Geno))

head(data2r)


Fig2K <- 
  ggplot (data2s, aes(Geno, cm.2, group=Geno))+
  geom_violin(fill="grey")+
  geom_point(size = 1) +
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(hjust=1, vjust=0.5,size=12,angle = 90), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12, colour = "blue"),
        strip.text.y = element_text(size = 12, colour = "black")) +
  ylab("Shoot Area (cm^2)") + 
  xlab("")

#res.aov <- aov(cm.2 ~ Geno, data = data2s)
#summary(res.aov)
#tukey.test <- TukeyHSD(res.aov)
#tukey.test

res_aov <- lm(cm.2 ~ Geno, data = data2s)
anova(res_aov)
tukey_aov=aov(res_aov)
HSD.test(tukey_aov, trt = c("Geno"), console = TRUE)




Fig2L <- 
  ggplot (data2r, aes(Geno, cm.2, group=Geno))+
  geom_violin(fill="grey")+
  geom_point(size = 1) +
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(hjust=1, vjust=0.5,size=12,angle = 90), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12, colour = "blue"),
        strip.text.y = element_text(size = 12, colour = "black")) +
  ylab("Root Area (cm^2)") + 
  xlab("")

#res.aov <- aov(cm.2 ~ Geno, data = data2r)
#summary(res.aov)
#tukey.test <- TukeyHSD(res.aov)
#tukey.test

res_aov <- lm(cm.2 ~ Geno, data = data2r)
anova(res_aov)
tukey_aov=aov(res_aov)
HSD.test(tukey_aov, trt = c("Geno"), console = TRUE)




pdf("Figure2.pdf", width = 8, height = 8)

ggdraw() +
  draw_plot(Fig2A, x = 0, y = 0.75, width = .2, height = 0.2) +
  draw_plot(Fig2B, x = 0.2, y = 0.75, width = .2, height = 0.2) +
  draw_plot(Fig2C, x = 0.4, y = 0.75, width = .2, height = 0.2) +
  draw_plot(Fig2D, x = 0.6, y = 0.75, width = .2, height = 0.2) +
  draw_plot(Fig2E, x = 0.8, y = 0.75, width = .2, height = 0.2) +
  draw_plot(Fig2F, x = 0, y = 0.5, width = .2, height = 0.2) +
  draw_plot(Fig2G, x = 0.2, y = 0.5, width = .2, height = 0.2) +
  draw_plot(Fig2H, x = 0.4, y = 0.5, width = .2, height = 0.2) +
  draw_plot(Fig2I, x = 0.6, y = 0.5, width = .2, height = 0.2) +
  draw_plot(Fig2J, x = 0.8, y = 0.5, width = .2, height = 0.2) +
  draw_plot(Fig2K, x = 0, y = 0, width = .5, height = 0.5) +
  draw_plot(Fig2L, x = 0.5, y = 0, width = .5, height = 0.5) +
  draw_plot_label(label = c("A", "B","C","D"), 
                  size = 15,
                  x = c(0, 0, 0,0.5), 
                  y = c(1, 0.75, 0.5, 0.5))

#ggsave("Figure2.jpg", plot=Fig2, height=8, width=8, units=c("in"), dpi=600)

dev.off()

#---Figure S3. Dry Weights---------------------------
head(data3)
data3$Geno = factor(data3$Geno, levels = unique(data3$Geno))

FigS3A <- ggplot (data3, aes(Geno, DryWeight.gm., group=Geno))+
  geom_violin(fill="grey")+
  geom_point(size = 1) +
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=12, hjust=1,vjust=0.5,angle = 90), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12, colour = "blue"),
        strip.text.y = element_text(size = 12, colour = "black")) +
  ylab("Shoot Dry Weight (g)") + 
  xlab("")+
  ylim(0,325)

#ggsave("FigureS2.jpg", plot=FigS2, height=4, width=4, units=c("in"), dpi=600)

res.aov <- aov(DryWeight.gm. ~ Geno, data = data3)
summary(res.aov)
tukey.test <- TukeyHSD(res.aov)
tukey.test

#Figure S3B.  High Correlation between biomass measurements.
colnames(data5)
df_cor = data5[,c(11:14)]
M <- cor(df_cor, use = "pairwise.complete.obs")

corrplot (M, 
          method = 'ellipse', 
          type = 'upper', 
          tl.cex = 1, 
          number.cex = 1,
          cl.cex = 1,
          addCoef.col = 'black', 
          tl.col = 'black',
          col.lim = c(0, 1),
          cl.ratio = 0.4)

Reread <- readPNG("S3B.png")
FigS3B <- ggplot() + 
  background_image(Reread) 


pdf("FigureS3.pdf", width = 6, height = 8)

ggdraw() +
  draw_plot(FigS3A, x = 0.3, y = 0.5, width = .5, height = .45) +
  draw_plot(FigS3B, x = 0.3, y = 0.05, width = 0.5, height = 0.45) +
  draw_plot_label(label = c("A", "B"), 
                  size = 15,
                  x = c(0.35, 0.35), 
                  y = c(1, 0.55))
#ggsave("FigureS1.jpg", plot=FigS1, height=6, width=8, units=c("in"), dpi=600)

dev.off()

#---Figure S4. Soil Analysis---------------------------

#maintain genotype order by biomechanics as in Figure 1A
data7$genotype = factor(data7$genotype, levels = unique(data7$genotype))

FigS4B <- ggplot (data7, aes(x=genotype, y=N...., fill=Location))+
  geom_violin()+
  scale_fill_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=12, vjust=0.5,hjust=1,angle = 90), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12, colour = "blue"),
        strip.text.y = element_text(size = 12, colour = "black")) +
  ylab("Soil Nitrogen (%)") + 
  xlab("")


FigS4C <- ggplot (data7, aes(x=genotype, y=C...., fill=Location))+
  geom_violin()+
  scale_fill_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=12, vjust=0.5,hjust=1,angle = 90), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12, colour = "blue"),
        strip.text.y = element_text(size = 12, colour = "black")) +
  ylab("Soil Carbon (%)") + 
  xlab("")

FigS4D <- ggplot (data7, aes(x=C...., y=N...., color=genotype, shape = Location))+
  geom_point(size = 3) +
  #geom_smooth(method="glm", se=FALSE)+
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) +
  ylab("Soil Nitrogen (%)") + 
  xlab("Soil Carbon (%)")

head(data7)
res.aov <- aov(N.... ~ Location + genotype + Location:genotype, data = data7)
summary(res.aov)
tukey.test <- TukeyHSD(res.aov)
tukey.test

res.aov <- aov(C.... ~ Location + genotype + Location:genotype, data = data7)
summary(res.aov)
tukey.test <- TukeyHSD(res.aov)
tukey.test

cor.test (data7$N...., data7$C...., use = "everything", method = c("pearson"))

Diagram <- readPNG("MaizeRows-01.png")

FigS4A <- ggplot() + 
  background_image(Diagram) 


pdf("FigureS4.pdf", width = 8, height = 8)

  ggdraw() +
  draw_plot(FigS4A, x = 0, y = 0.5, width = .5, height = .5) +
  draw_plot(FigS4B, x = 0.5, y = 0.5, width = .5, height = .45) +
  draw_plot(FigS4C, x = 0, y = 0, width = .5, height = 0.5)+
  draw_plot(FigS4D, x = 0.5, y = 0, width = .5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C","D"), 
                  size = 12,
                  x = c(0, 0.5, 0, 0.5), 
                  y = c(1, 1, 0.5, 0.5))

#ggsave("FigureS3.jpg", plot=FigS3, height=6, width=8, units=c("in"), dpi=600)
dev.off()

#---Figure S5. Soil Analysis by Uptake ---------------------------
head(data8)
data8$genotype = factor(data8$genotype, levels = unique(data8$genotype))

FigS5A <- ggplot (data8, aes(x=Gly, y=N...., color=genotype))+
  geom_point(size = 1) +
  geom_smooth(method="glm", se=FALSE)+
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.position = "none") +
  ylab("Soil Nitrogen (%)") + 
  xlab("Glycine (ug/mg/hr)")


FigS5B <- ggplot (data8, aes(x=NH4, y=N...., color=genotype))+
  geom_point(size = 1) +
  geom_smooth(method="glm", se=FALSE)+
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.position = "none") +
  ylab("Soil Nitrogen (%)") + 
  xlab("NH4 (ug/mg/hr)")

FigS5C <- ggplot (data8, aes(x=NO3, y=N...., color=genotype))+
  geom_point(size = 1) +
  geom_smooth(method="glm", se=FALSE)+
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) +
  ylab("Soil Nitrogen (%)") + 
  xlab("NO3 (ug/mg/hr)")

pdf("FigureS5.pdf", width = 8, height = 8)

ggdraw() +
  draw_plot(FigS5A, x = 0, y = 0.5, width = .5, height = .5) +
  draw_plot(FigS5B, x = 0.5, y = 0.5, width = .5, height = .5) +
  draw_plot(FigS5C, x = 0, y = 0, width = .67, height = 0.49) +
  draw_plot_label(label = c("A", "B", "C"), 
                  size = 15,
                  x = c(0, 0.5, 0), 
                  y = c(1, 1, 0.5))

#ggsave("FigureS4.jpg", plot=FigS4, height=6, width=8, units=c("in"), dpi=600)
dev.off()

#---Figure 3. N15 Uptake ---------------------------
head(data4)
data4$genotype = factor(data4$genotype, levels = unique(data4$genotype))

Fig3B <- 
  ggplot (data4, aes(genotype, N15.per.weight.per.hour..ug.mg.hr., group=genotype, color=N.treatment))+
  geom_violin(fill = "grey")+
  geom_point(size = 1) +
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=12, angle=45), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.title = element_blank()) +
  ylab("N15 (ug/mg/hr)") + 
  xlab("")+
  ylim(0,20)

Fig3A <- ggplot (data4, aes(N.treatment, N15.per.weight.per.hour..ug.mg.hr., group=N.treatment, color=genotype))+
  geom_violin(fill = "grey")+
  geom_point(size = 2) +
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=12, angle=90, hjust=1, vjust=0.5), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.title = element_blank()) +
  ylab("N15 (ug/mg/hr)") + 
  xlab("")+
  ylim(0,20)

res.aov <- aov(N15.per.weight.per.hour..ug.mg.hr. ~ N.treatment + genotype + N.treatment:genotype, data = data4)
summary(res.aov)
tukey.test <- TukeyHSD(res.aov)
tukey.test

res.aov <- aov(diameter.corrected..cm. ~ genotype, data = data6)
summary(res.aov)
tukey.test <- TukeyHSD(res.aov)
tukey.test

head(data6)
data6$genotype = factor(data6$genotype, levels = unique(data6$genotype))


Fig3C <-ggplot (data6, aes(N15.per.weight.per.hour..ug.mg.hr., diameter.corrected..cm., color=genotype))+
  geom_point(size = 2) +
  facet_wrap(~N.treatment, ncol=3)+
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        plot.title=element_text(size=14, vjust=3), 
        axis.text=element_text(size=14), 
        axis.title = element_text(size=14), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 14, colour = "blue"),
        strip.text.y = element_text(size = 14, colour = "blue"),
        legend.position = "none") +
  ylab("Diameter (cm)") + 
  xlab("N15 (ug/mg/hr)")

Fig3D <-ggplot (data6, aes(total.N.per.mg.DW..ug.mg.DW., diameter.corrected..cm., color=genotype))+
  geom_point(size = 2) +
  scale_color_d3()+
  theme(text=element_text(family="Times"),
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        plot.title=element_text(size=14, vjust=3), 
        axis.text=element_text(size=14), 
        axis.title = element_text(size=14), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 14, colour = "blue"),
        strip.text.y = element_text(size = 14, colour = "blue"),
        legend.title = element_blank()) +
  ylab("Diameter (cm)") + 
  xlab("Total N (ug/mg)")

res.aov <- aov(diameter.corrected..cm. ~ genotype, data = data6)
summary(res.aov)

cor.test (data6$total.N.per.mg.DW..ug.mg.DW., data6$diameter.corrected..cm., use = "everything", method = c("pearson"))
cor.test (data6$total.N.per.mg.DW..ug.mg.DW., data6$N15.per.weight.per.hour..ug.mg.hr., use = "everything", method = c("pearson"))


data6$Dbig <- data6$diameter.corrected..cm.* 100

Gly <- data6[ which(data6$N.treatment =='Gly'),]
NH4 <- data6[ which(data6$N.treatment =='NH4'),]
NO3 <- data6[ which(data6$N.treatment =='NO3'),]

cor.test(Gly$N15.per.weight.per.hour..ug.mg.hr., Gly$Dbig, use = "everything", method = c("pearson"))
cor.test(NH4$N15.per.weight.per.hour..ug.mg.hr., NH4$Dbig, use = "everything", method = c("pearson"))
cor.test(NO3$N15.per.weight.per.hour..ug.mg.hr., NO3$Dbig, use = "everything", method = c("pearson"))

##Data for Table S2
head(data6)

Gly <- data6[ which(data6$N.treatment =='Gly'),]
NH4 <- data6[ which(data6$N.treatment == 'NH4'),]
NO3 <- data6[ which(data6$N.treatment == 'NO3'),]

cor.test(Gly$Dbig, Gly$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = Gly)
summary(model)

cor.test(NH4$Dbig, NH4$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = NH4)
summary(model)

cor.test(NO3$Dbig, NO3$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = NO3)
summary(model)



colnames(Gly)
B73 <- Gly[ which(Gly$genotype =='B73'),]
W64A <- Gly[ which(Gly$genotype  =='W64A'),]
HP301 <- Gly[ which(Gly$genotype  =='Hp301'),]
Ky21 <- Gly[ which(Gly$genotype  =='Ky21'),]
HK <- Gly[ which(Gly$genotype  =='Hickory King'),]


cor.test(B73$Dbig, B73$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = B73)
summary(model)

cor.test(W64A$Dbig, W64A$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = W64A)
summary(model)

cor.test(HP301$Dbig, HP301$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = HP301)
summary(model)

cor.test(Ky21$Dbig, Ky21$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = Ky21)
summary(model)

cor.test(HK$Dbig, HK$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = HK)
summary(model)


colnames(NH4)
B73_2 <- NH4[ which(NH4$genotype =='B73'),]
W64A_2 <- NH4[ which(NH4$genotype =='W64A'),]
HP301_2 <- NH4[ which(NH4$genotype  =='Hp301'),]
Ky21_2 <- NH4[ which(NH4$genotype =='Ky21'),]
HK_2 <- Gly[ which(Gly$genotype  =='Hickory King'),]

cor.test(B73_2$Dbig, B73_2$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = B73_2)
summary(model)

cor.test(W64A_2$Dbig, W64A_2$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = W64A_2)
summary(model)

cor.test(HP301_2$Dbig, HP301_2$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = HP301_2)
summary(model)

cor.test(Ky21_2$Dbig, Ky21_2$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = Ky21_2)
summary(model)

cor.test(HK_2$Dbig, HK_2$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = HK_2)
summary(model)


colnames(NO3)
B73_3 <- NO3[ which(NO3$genotype =='B73'),]
W64A_3 <- NO3[ which(NO3$genotype   =='W64A'),]
HP301_3 <- NO3[ which(NO3$genotype   =='Hp301'),]
Ky21_3 <- NO3[ which(NO3$genotype  =='Ky21'),]
HK_3 <- Gly[ which(Gly$genotype  =='Hickory King'),]


cor.test(B73_3$Dbig, B73_3$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = B73_3)
summary(model)

cor.test(W64A_3$Dbig, W64A_3$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = W64A_3)
summary(model)

cor.test(HP301_3$Dbig, HP301_3$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = HP301_3)
summary(model)

cor.test(Ky21_3$Dbig, Ky21_3$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = Ky21_3)
summary(model)

cor.test(HK_3$Dbig, HK_3$N15.per.weight.per.hour..ug.mg.hr., method="pearson", use="everything")
model <- lm(formula = Dbig ~ N15.per.weight.per.hour..ug.mg.hr., data = HK_3)
summary(model)



colnames(data6)
B73_4 <- data6[ which(data6$genotype =='B73'),]
W64A_4 <- data6[ which(data6$genotype  =='W64A'),]
HP301_4 <- data6[ which(data6$genotype  =='Hp301'),]
Ky21_4 <- data6[ which(data6$genotype  =='Ky21'),]
HK_4 <- data6[ which(data6$genotype  =='Hickory King'),]

cor.test(data6$Dbig, data6$total.N.per.mg.DW..ug.mg.DW., method="pearson", use="everything")
model <- lm(formula = Dbig ~ total.N.per.mg.DW..ug.mg.DW., data = data6)
summary(model)



cor.test(B73_4$Dbig, B73_4$total.N.per.mg.DW..ug.mg.DW., method="pearson", use="everything")
model <- lm(formula = Dbig ~ total.N.per.mg.DW..ug.mg.DW., data = B73_4)
summary(model)

cor.test(W64A_4$Dbig, W64A_4$total.N.per.mg.DW..ug.mg.DW., method="pearson", use="everything")
model <- lm(formula = Dbig ~ total.N.per.mg.DW..ug.mg.DW., data = W64A_4)
summary(model)

cor.test(HP301_4$Dbig, HP301_4$total.N.per.mg.DW..ug.mg.DW., method="pearson", use="everything")
model <- lm(formula = Dbig ~ total.N.per.mg.DW..ug.mg.DW., data = HP301_4)
summary(model)

cor.test(Ky21_4$Dbig, Ky21_4$total.N.per.mg.DW..ug.mg.DW., method="pearson", use="everything")
model <- lm(formula = Dbig ~ total.N.per.mg.DW..ug.mg.DW., data = Ky21_4)
summary(model)

cor.test(HK_4$Dbig, HK_4$total.N.per.mg.DW..ug.mg.DW., method="pearson", use="everything")
model <- lm(formula = Dbig ~ total.N.per.mg.DW..ug.mg.DW., data = HK_4)
summary(model)



pdf("Figure3.pdf", width = 8, height = 7)

#Save as pdf:  8in Wide x 6in High
ggdraw() +
  draw_plot(Fig3A, x = 0, y = 0.5, width = .5, height = .5) +
  draw_plot(Fig3B, x = 0.5, y = 0.45, width = .5, height = .55) +
  draw_plot(Fig3D, x = 0, y = 0, width = .45, height = .5) +
  draw_plot(Fig3C, x = 0.45, y = 0, width = .5, height = .5) +
  draw_plot_label(label = c("A", "B","C","D"), 
                  size = 15,
                  x = c(0, 0.5, 0,0.45), 
                  y = c(1, 1, 0.5,0.5))

#ggsave("Figure3.jpg", plot=Fig3, height=6, width=8, units=c("in"), dpi=600)
dev.off()



#---Figure S5 Correlations---------------------------
head(data5)

#There is not effect of genotype on either uptake or transporter expression.
#So we ignore genotype & 

#Figure S5A.  High Correlation between TotalN and individual N.
colnames(data5)
df_cor = data5[,c(4:7)]
M <- cor(df_cor, use = "pairwise.complete.obs")

#corrplot.mixed (M, upper = 'ellipse', lower = 'number', tl.cex = 0.8, number.cex = 0.6, p.mat = testRes$p, insig = 'blank')
corrplot (M, 
          method = 'ellipse', 
          type = 'upper', 
          tl.cex = 1, 
          number.cex = 1,
          cl.cex = 1,
          addCoef.col = 'black', 
          tl.col = 'black',
          col.lim = c(0, 1),
          cl.ratio = 0.4)



#Figure S5B.  High Correlation between N and biomasses.
colnames(data5)
df_cor = data5[,c(4:11)]
M <- cor(df_cor, use = "pairwise.complete.obs")

#corrplot.mixed (M, upper = 'ellipse', lower = 'number', tl.cex = 0.8, number.cex = 0.6, p.mat = testRes$p, insig = 'blank')
corrplot (M, 
          method = 'ellipse', 
          type = 'upper', 
          tl.cex = 1, 
          number.cex = 1,
          cl.cex = 1,
          addCoef.col = 'black', 
          tl.col = 'black',
          cl.ratio = 0.4)














