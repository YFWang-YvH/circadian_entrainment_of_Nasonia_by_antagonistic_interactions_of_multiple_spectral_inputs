# This script is as part of the manuscript Circadian rhythm entrainment of the jewel wasp, Nasonia vitripennis, by antagonistic interactions of multiple spectral inputs
# In this script, we modelled and analyzed 14 different wavelengths and 4 different light intensities 

rm(list=ls()) # clear all objects
getwd()
setwd("/path/to/data/directory")

###################################
# load packages
windowsFonts(Times=windowsFont("Times New Roman"))
library(data.table)
library(readxl)
library(tidyr)
library(dplyr)
library(plyr)
library(reshape2)
library(reshape)
library(plotrix)
library(ggplot2)
library(ggpubr)
library(xlsx)
library(MASS)
library(MuMIn) 
library(jtools)
library(interactions)
library(officer)
library(ggpubr)
library(gg3D)
library(mgcv)
library(data.table)
library(tidymv)
library(rvg)
library(officer)

###################################
#figure 1 

#light setup
d <- fread("spectrometer spectrum for manuscript.txt")

#monochromatic filter light intensity
spectrometer <- fread("monochromatic filters spectrometer.csv")


spectrometer2 <- melt(data = spectrometer,  id.vars = c('wavelength'),variable.name = 'filters')
spectrometer2 <- spectrometer2[spectrometer2$value != 0, ]
spectrometer2$lightintensity <- log10(spectrometer2$value)
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="650" & spectrometer2$wavelength<580),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="631" & spectrometer2$wavelength<560),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="613" & spectrometer2$wavelength<530),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="477" & spectrometer2$wavelength<450),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="511" & spectrometer2$wavelength<475),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="552" & spectrometer2$wavelength<500),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="631" & spectrometer2$wavelength>699),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="613" & spectrometer2$wavelength>699),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="588" & spectrometer2$wavelength>699),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="566" & spectrometer2$wavelength>699),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="552" & spectrometer2$wavelength>699),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="528" & spectrometer2$wavelength>699),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="511" & spectrometer2$wavelength>699),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="494" & spectrometer2$wavelength>699),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="477" & spectrometer2$wavelength>699),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="456" & spectrometer2$wavelength>699),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="441" & spectrometer2$wavelength>699),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="419" & spectrometer2$wavelength>699),]
spectrometer2 <- spectrometer2[!(spectrometer2$filters=="390" & spectrometer2$wavelength>699),]

levels(spectrometer2$filters) <- c('390 nm', '419 nm', '441 nm', '456 nm', '477 nm', '494 nm', '511 nm', '528 nm', '552 nm', '566 nm', '588 nm', '613 nm', '631 nm','650 nm')

# The palette for each wavelength:
cbPalette <- c("#79008d",#390# 
               "#6d00fb",#419#
               "#000bff", #441# 
               "#0066ff",#456#
               "#00c8ff",#477# 
               "#00ffd5",#494# 
               "#09ff00",#511# 
               "#56ff00", #528# 
               "#a9ff00", #552#
               "#d5ff00" , #566#
               "#ffe600", #588#
               "#ff9100" , #613#
               "#ff4b00",#631#
               "#ff0000" )
p1 <-  ggplot()+
  geom_line(data = spectrometer2, mapping=aes(x = wavelength, y = lightintensity,  
                                              group=filters, 
                                              colour=filters),size=0.5,show.legend = FALSE)+
  scale_colour_manual(values=cbPalette)+
  geom_ribbon(data= spectrometer2,
              mapping=aes(x=wavelength,ymax=lightintensity, 
                          group=filters,fill=filters),ymin=0, color='black') +
  scale_fill_manual(name='', values= cbPalette ,guide = guide_legend(direction = "vertical"))+
  labs(x="wavelength (nm)", y="log (I) (photons cm-2 s-1 nm-1)") +
  xlim(380,700)+
  ylim(7.8,14)+
  geom_line(data=d,mapping=aes(x=d$`Wavelength [nm]`,y=d$`light box log`),colour="black")+
  geom_line(data=d,mapping=aes(x=d$`Wavelength [nm]`,y=d$`stimulator log`),colour="red")+
  theme_classic()+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14))+
  theme(axis.text=element_text(family="Times",size=12)) +
  guides(color = guide_legend(override.aes = list(size = 1))) + #change the size of legend
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(family="Times",size=10)) #change legend text font size
p1

##########################
#figure 2
# photoperiod and PRC
dmean1 <- fread("dmean.txt")
d1<- fread("AsymCx 1410LD PRC doubleplot.txt")
d1 <- d1[-c(153:278),]
dmean1 <- dmean1[-c(14:24),]


plot1 <- ggplot()+
  geom_point(data=d1, aes(x=`Light inpulse time`,y=`Phase Shift (h)`), shape=1)+
  geom_errorbar(data=dmean1, aes(x=`Lightinpulsetime`, ymin=`meanhour`-`cirSD`,ymax=`meanhour`+`cirSD`),width=0.2)+
  geom_point(data=dmean1, aes(x=`Lightinpulsetime`, y=`meanhour`))+
  geom_line(data=dmean1,aes(x=`Lightinpulsetime`, y=`meanhour`),size=0.5)+
  theme_classic()+
  geom_abline(slope=0,intercept = 0, lty=2, size =0.5)+
  scale_x_continuous(breaks = seq(0,24,2)) +
  scale_y_continuous(breaks = seq(-12,12,4)) +
  coord_cartesian(xlim = c(0, 24), ylim = c(-12, 12)) +
  labs( x="time of light pulse (ZT, h)", y="phase shift (h)") +
  theme_classic()+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 22))+
  theme(axis.text=element_text(family="Times",size=20)) +
  guides(color = guide_legend(override.aes = list(size = 1))) + #change the size of legend
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=20), #change legend title font size
        legend.text = element_text(family="Times",size=10)) #change legend text font size

plot1


d<- fread("Different photoperiod PRC.txt")
p1<-ggplot(d,aes(x=`Lightperiod`,y=`phaseshift`))+
  theme_classic()+
  geom_abline(slope=0,intercept = 0, lty=2, size =0.5)+
  geom_point(shape = 1)+
  geom_line(stat="summary",fun.y="mean", size=0.5)+
  geom_point(stat="summary",fun.y="mean")+
  geom_errorbar(stat="summary",
                fun.ymin = function(x) {mean(x)-sd(x)/sqrt(length(x))},
                fun.ymax = function(x) {mean(x)+sd(x)/sqrt(length(x))},width=0.2)+
  scale_x_continuous(breaks = seq(4,18,2), labels=c("4:20","6:18","8:16","10:14","12:12","14:10","16:8","18:6"))+
  scale_y_continuous(breaks = seq(-12,12,4)) +
  coord_cartesian(xlim = c(4, 18), ylim = c(-12, 12)) +
  labs(x="photoperiod", y="phase shift (hr)") +
  theme_classic()+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 22))+
  theme(axis.text=element_text(family="Times",size=20)) +
  guides(color = guide_legend(override.aes = list(size = 1))) + #change the size of legend
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=20), #change legend title font size
        legend.text = element_text(family="Times",size=10)) #change legend text font size
p1

p<-ggarrange(p1,plot1,nrow = 2,ncol =1, labels = c("(a)", "(b)"),align = "hv", common.legend = F, legend = NULL)
p

#############################
#figure 3
#boxplot of action spectrum raw data
#load all raw data
d1 <- fread("d1_actionspectrumdata2.csv")
str(d1)

#creating DRCdata which is the summarised data set
d1$count <-1
d1aggregate <- setDT(d1)[,.(meanphaseshift=mean(phaseshift), n = sum(count), sd = sd(phaseshift)), by = c('wavelength', 'photons',"irradiance")]
d1aggregate$SEM <- d1aggregate$sd/sqrt(d1aggregate$n)
DRCdata <- d1aggregate

#initial inspection of summarised dataset and all raw data
p1<- ggplot(DRCdata,aes(x=log(photons),y=meanphaseshift))+
  geom_point()+
  facet_wrap(DRCdata$wavelength~.)+
  theme_classic()+
  geom_errorbar(aes(ymin=meanphaseshift-SEM, ymax=meanphaseshift+SEM))
p1
p2 <- ggplot(d1,aes(x=lightintensity,y=phaseshift))+
  geom_point(mapping=aes(x=lightintensity, y=phaseshift),shape=1)+
  facet_wrap(wavelength~.)+
  theme_classic()
p2

#prepare boxplot for the manuscript to show variations in the data
#in order to manuiplate the boxplot better, plotting boxplot for each wavelength separately
#subset rawdata into each wavelength
wls <- c(390,419,441,456,477,494,511,528,552,566,588,613,631,650)
for (i in 1:14) {
  di <- subset(d1,  wavelength == wls[i])
  pi <- ggplot(di, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
    geom_boxplot()+
    geom_jitter(shape=16, position=position_jitter(0.2))+
    theme_classic()+
    labs(x="log10 of light intensity", y="delayed phase shift (h)")  +
    ylim(-12,12)+
    theme(plot.title = element_text(hjust = 0.5,size=12))+
    theme(axis.title = element_text(hjust = 0.5, size = 14, face = "bold"))+
    theme(axis.text=element_text(size=12)) +
    geom_hline(yintercept = 0, linetype="dashed")
  assign(paste("di", i, sep=""), di)
  assign(paste("pi", i, sep=""), pi)
  
}

p1 <- ggplot(di1, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title="390 nm ",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p1
p2 <- ggplot(di2, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+
  theme_classic()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title="419 nm",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p3 <- ggplot(di3, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title="441 nm",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p4 <- ggplot(di4, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title="456 nm",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p5 <- ggplot(di5, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title="477 nm",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p6 <- ggplot(di6, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title="494 nm",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p7 <- ggplot(di7, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title="511 nm",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p8 <- ggplot(di8, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title="528 nm",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p9 <- ggplot(di9, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title="552 nm",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p10 <- ggplot(di10, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title="566 nm",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p11 <- ggplot(di11, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title="588 nm",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p12 <- ggplot(di12, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title ="613 nm",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p13 <- ggplot(di13, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title = "631 nm",x="log10 of light intensity", y="delayed phase shift (h)")  +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")
p14 <- ggplot(di14, aes(x=lightintensity, y=phaseshift,group=lightintensity)) +
  geom_boxplot()+geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_classic()+
  labs(title="650 nm",x="log10 of light intensity", y="delayed phase shift (h)") +
  ylim(-12,12)+
  theme(plot.title = element_text(family="Times",hjust = 0.5,size=12))+
  theme(axis.title = element_text(family="Times",hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text=element_text(family="Times",size=12)) +
  geom_hline(yintercept = 0, linetype="dashed")

p <- ggarrange(p1+rremove("x.title")+rremove("y.title"),
               p2+rremove("x.title")+rremove("y.title"),
               p3+rremove("x.title")+rremove("y.title"),
               p4+rremove("x.title")+rremove("y.title"),
               p5+rremove("x.title")+rremove("y.title"),
               p6+rremove("x.title")+rremove("y.title"),
               p7+rremove("x.title")+rremove("y.title"),
               p8+rremove("x.title")+rremove("y.title"),
               p9+rremove("x.title")+rremove("y.title"),
               p10+rremove("x.title")+rremove("y.title"),
               p11+rremove("x.title")+rremove("y.title"),
               p12+rremove("x.title")+rremove("y.title"),
               p13+rremove("x.title")+rremove("y.title"),
               p14+rremove("x.title")+rremove("y.title"),
               nrow = 4,ncol =4, 
               label.x = "log10 of light intensity", label.y = "delayed phase shift (h)",
               align = "hv", common.legend = F, legend = NULL)

# Annotate the figure by adding a common labels
#figure 3 in the manuscript
p1 <- annotate_figure(p,
                      left = text_grob("delayed phase shift (hr)", rot = 90),
                      bottom = text_grob("log10 of light intensity",
                                         hjust = 0.5))
p1

####################################
#figure 3
# 3D plot and GAM model
d <- fread("raw_data.csv")

## Look at some values
table(d$wavelength)
table(d$lightintensity)
table(d$weight)

## Does light intensity need log transform?
summary(d$lightintensity) # yes
summary(log10(d$lightintensity)) # better

d <- d %>% mutate(light_intensity = log10(lightintensity))

## Plot 
ggplot(d,aes(x=light_intensity)) +
  geom_histogram(bins = 30)

ggplot(d,aes(x=wavelength,y=phaseshift,color=light_intensity)) +
  geom_point() +
  geom_smooth()

ggplot(d,aes(x=wavelength,y=light_intensity,z=phaseshift)) +
  theme_void() +
  axes_3D() +
  stat_3D()  

## Let's fit a 2D smoother
m1 <- gam(phaseshift ~ s(wavelength) + s(light_intensity),
          data = d)
m2 <- gam(phaseshift ~ s(wavelength, k=7) + s(light_intensity),
          data = d)
summary(m2) #m1 R squared is better 0.15 vs m2 0.13
summary(m2)$r.sq
summary(m2)$s.table

layout(matrix(1:4, nrow = 2))
gam.check(m1)

layout(matrix(1:2, nrow = 1))
p0 <- gratia::draw(m1)
p0
plot(m1)

#3D plot
dev.off() # reset everything
vis.gam(m1,n.grid=50, theta=35,phi=32, zlab = "", ticktype="detailed", color="topo")
vis.gam(m2,n.grid=50, theta=35,phi=32, zlab = "", ticktype="detailed", color="topo")


#fitted values vs residuals of two models
datas <- data.table(Fitted_values = c(m1$fitted.values,
                                      m2$fitted.values),
                    Residuals = c(m1$residuals,
                                  m2$residuals),
                    Model = rep(c("m1", "m2"), each = nrow(d)))

p <- ggplot(data = datas,
            aes(Fitted_values, Residuals)) +
  facet_grid(Model~., switch = "y") +
  geom_point(size = 1.7) +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 13, face = "bold"),
        strip.background = element_rect(color = "black")) +
  labs(title = "Fitted values vs Residuals of two models")
p

#model prediction based on m1 or m2
model_p1<- predict_gam(m1)
model_p2<- predict_gam(m2)

# model_p1<- as.data.frame(model_p1)
# write.csv(model_p1, "GAMmodel_p1output.csv")
m1aggregate <- setDT(model_p1)[,.(meanfit=mean(fit), meansefit=mean(se.fit)), by = c("wavelength")]

##################################################
#create a new powerpoint document for export figures
fig <- dml(ggobj = p)

doc <- read_pptx("GAM-model.pptx")
doc <- add_slide(doc, 'Title and Content', 'Office Theme')

#add the plot
#doc <- ph_with(doc, plot, location = ph_location_fullsize())
doc <- ph_with(doc, fig, location = ph_location(width=5,height=10))
#write the document to a file
print(doc, target = "GAM-model.pptx")


