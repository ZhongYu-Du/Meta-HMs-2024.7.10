# Meta-HMs
## The code for plant responses to heavy metals

### Author: Zhongyu Du

### Time: 2024-07-10

# Code for meta-analysis

### Read me
TB, Total biomass

AB, Aboveground biomass

LB, Leaf biomass

RB, Root biomass


```
###A global meta-analysis of ability on remediation of 
###heavy metal contaminated soil by woody plants

#Author: Zhongyu Du

#Time: 2023-01-12
###A global meta-analysis of ability on remediation of 
###heavy metal contaminated soil by woody plants

#Author: Zhongyu Du

#Time: 2024-03-03
rm(list = ls())
setwd("E:\\博士研究生\\Meta分析论文\\B重金属与木本植物meta")
setwd("F:\\博士研究生\\Meta分析论文\\B重金属与木本植物meta")
{
  library(orchaRd)
  library(readxl)
  library(metafor)
  library(meta)
  library(lme4)
  library(lmerTest)
  library(ggplot2)
  library(ggpubr)
  library(ggthemes)
  library(dplyr)
  library(matrixcalc)
  library(plotbiomes)
  library(raster)
  library(maptools)
  library(geodata)
  library(sp)
  library(maps)
  library(ggplot2)
  library(sp)
  library(maptools)
  library(tidyverse)
  library(ggthemes)
  library(openxlsx)
  library(ggbreak)
  library(cowplot)
}
# install.packages("devtools")
#devtools::install_github("valentinitnelav/plotbiomes")
options(scipen=200)



############################# Fig.1 Map  #######################################

###########################   Fig.1a Global map   ##############################

package_list = c("maps","ggplot2","sp","maptools","tidyverse","ggthemes")

for(package in package_list){
  if (!require(package, character.only = T, quietly = T)){
    install.packages(package)
    library(package, character.only =  T)
  }
}

### read data
d_map <- openxlsx::read.xlsx("data-24-0227.xlsx", sheet = "final-data")
head(d_map)

#判断是否为异常点
library(CoordinateCleaner)
problem_records<- clean_coordinates(d_map, lon = "longitude", lat = "latitude", species = "Species",tests = c("capitals", "centroids", "equal","institutions","zeros", "seas"))
problem_records$.sea
#write.csv(problem_records, "problem_records.csv")
summary(problem_records)


#information
table(d_map$HMs_type)
table(d_map$Experimental_method)
aa <- table(d_map$Study_ID)
#write.csv(aa, "aa.csv")

#factor
d_map$Experimental_method<-factor(d_map$Experimental_method)
d_map$HMs_type<-factor(d_map$HMs_type, levels = c("Cd","Pb","Cu","Zn","As", "Sb","Ni","Cr","Ti"))

str(d_map)


#world
mapworld<-borders("world",regions = ".", colour ="black",fill="gray50", alpha=0.3, size=0.01) # baseic map
mp <- ggplot(data = d_map)+ mapworld

Fig.1a <- mp+ geom_point(aes(x=longitude, y=latitude, shape=Experimental_method, color=HMs_type), size = 4, alpha=0.8)+
  scale_shape_manual(values = c(15,16,17,18))+ labs(x="Longitude",y="Latitude")+ theme_void()+ theme(text = element_text(size = 14,face = "bold"))+
  theme(legend.position = c(0.15, 0.42))
Fig.1a


######################   Fig.1b Whittaker figure   #############################
library(plotbiomes)
library(raster)
library(maptools)
library(geodata)
library(sp)

path <- system.file("extdata", "temp_pp.tif", package = "plotbiomes")
temp_pp <- raster::stack(path)
names(temp_pp) <- c("temperature", "precipitation")

d_map <- openxlsx::read.xlsx("data-24-0227.xlsx", sheet = "final-data")
coordinates(d_map)=c("longitude", "latitude")

extractions <- raster::extract(temp_pp, d_map, df = TRUE)

extractions$temperature <- extractions$temperature/10
extractions$precipitation <- extractions$precipitation/10

extractions$HM_type <- factor(d_map$HMs_type, levels = c("Cd","Pb","Cu","Zn","As","Sb","Ni","Cr","Ti"))

#my themes
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#whittaker map
Fig.1b <- whittaker_base_plot() + geom_point(data = extractions,aes(x = temperature, y = precipitation, color = HM_type), size=5,stroke = 0) +
  theme_classic((base_size=16))+
  theme(legend.justification = c(0, 1),
        legend.position = c(0,1),
        legend.background = element_rect(fill = NA),
        legend.box = "horizontal",
        legend.spacing.x = unit(0.5, units = "cm"))+
  theme(axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 16))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.0, linetype="solid"))
Fig.1b



###################    Fig.1c  Publish papers figure   #########################

d_map <- openxlsx::read.xlsx("data-24-0227.xlsx", sheet = "final-data")

Fig.1c <- ggplot(d_map,aes(x = Published_year))+
  geom_density(fill = "gray60",alpha=0.5)+
  xlab("Published year")+
  ylab("Density")+
  theme_classic()+
  theme(panel.grid=element_blank())+
  theme(axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 16))+
  theme(strip.text.x = element_text(
    size = 16, face = "bold"))+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  scale_x_continuous(limits = c(1995,2023), breaks=seq(1995,2023, by=5)) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.0, linetype="solid"))


##merge figures
Fig.1bc <- cowplot::plot_grid(Fig.1b, Fig.1c, labels = c("(b)","(c)"), align = "hv", label_size = 20)

Fig.1 <- cowplot::plot_grid(Fig.1a, Fig.1bc, ncol = 1, rel_heights = c(1, 1.2), labels = c("(a)"), label_size = 20)


#ggsave("Figure 1 0625.pdf", Fig.1, width = 14, height = 12)



########################  Table S2 Basic information in the meta-analysis ######

d_basic <- read_xlsx("data-24-0227.xlsx", sheet = "final-data")
str(d_basic)
art <- table(d_basic$Study_ID)
length(art)
#write.csv(art, "art.csv")
#HMs type
hms <- table(d_basic $HMs_type)
length(hms)
#write.csv(hms,"HMS.csv")

#Exp
expp <- table(d_basic $Experimental_method)
length(expp)
#write.csv(expp,"expp.csv")

#Species
spec <- table(d_basic$Species)
#write.csv(spec,"Species.csv")

#Genus
genu <- table(d_basic$Genus)

table(d_basic$Genus)

d_group <- d_basic %>% group_by(Climate_area_A, Climate_area_A1, HMs_type, HMs_level, Functional_group, Family, Genus) %>% summarise(N = n())


dd <- table(d_group$Genus)
length(dd)

#climate area/HMs levels/HMs type
#write.csv(d_group, "d_group.csv")


d_group_shrub <- subset(d_group, Functional_group == "Shrub")

length(d_basic$Genus)
#write.csv(genu,"Genus.csv")

#Family
fam <- table(d_basic $Family)
#write.csv(fam,"Family.csv")

###Country
con <- table(d_basic $Country)
con
#write.csv(con,"Country.csv")
table(d_basic$Published_year)


###Functional group
tree <- subset(d_basic, Functional_group == "Tree")
tree_genus <- table(tree$Genus)
length(tree_genus)
tree_country <- table(tree$Country)
#write.csv(tree_genus, "Tree_genus.csv")
#write.csv(tree_country, "tree_country.csv")

shrub <- subset(d_basic, Functional_group == "Shrub")

shrub_genus <- table(shrub$Genus)
length(shrub_genus)
#write.csv(shrub_genus, "Shrub_genus.csv")


shrub_country <- table(shrub$Country)
#write.csv(shrub_country, "shrub_country.csv")

##### Density plot
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))


# 1 time
#[,c(1:3,13, 18:19, 23)]
stress_time <- d_basic %>% as.data.frame()
stress_time$group <- ifelse(stress_time$stress_time == "Unknow", "Unknow", "data")

stress_time$EMS <- if_else(stress_time$Experimental_method == "Hydroponics", "Hydroponics", "Soil/Steril_Steril_medium")
stress_time_data <- subset(stress_time, group == "data")
stress_time_data$stress_time <- as.numeric(stress_time_data$stress_time)
table(stress_time_data$group)
stress_time_data_mean <- stress_time_data %>% group_by(Functional_group, EMS) %>% summarise(Mean = mean(stress_time), N = n(), Max = max(stress_time), Min = min(stress_time))
stress_time_data_mean
stress_time_data$Functional_group <- factor(stress_time_data$Functional_group, levels = c("Tree", "Shrub"))


######################  Figure  S3         #####################################
stress_time_data_mean %>% as.data.frame()

timetwo <- ggplot(stress_time_data, aes(x = stress_time, color = EMS, fill = EMS)) +
  geom_histogram() +
  facet_wrap(.~ Functional_group, scales = "free") +
  theme_bw() + font +
  #theme(legend.position = "none") +
  xlab("Stress time (d)") + ylab("Frequency") +
  theme(strip.text.x = element_text(size = 18)) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12))
  
timetwo 
#ggsave("Figure S3 0625.pdf",timetwo, width = 8, height = 3.5)


# 2 HMs Concentration：[, c(1:3,13, 18, 30, 35)]
hms_con <- d_basic %>% as.data.frame()
str(hms_con)
library(dplyr)
head(hms_con, 20)
hms_con$EMS <- if_else(hms_con$Experimental_method == "Hydroponics", "Hydroponics", "Soil")

HMs_type_EMS <- hms_con %>% group_by(Functional_group, EMS, HMs_type) %>%  summarise(Mean = mean(Pollutant_difference), MAX = max(Pollutant_difference), Min = min(Pollutant_difference), N = n())
print(HMs_type_EMS, n = 23)
#write.csv(HMs_type_EMS, "HMs_type_EMS.csv")


# Cd
Cd_con <- subset(hms_con, HMs_type == "Cd")
Cd_con_mean <- Cd_con %>% group_by(EMS) %>% summarise(Mean = mean(Pollutant_difference), N = n())


pCd <- ggplot(data = Cd_con, aes(x = Pollutant_difference, fill = EMS)) +
  geom_density(color = "black", size = 1.2) +
  theme_few() +
  scale_x_continuous(limits = c(-100, 300), breaks=seq(-100, 300, by=100)) +
  xlab("Cd Concentration") + ylab("Density") + font +
  geom_vline(data = Cd_con_mean, aes(xintercept = Mean, color = EMS),size = 1.2,linetype="dashed")+
  annotate("text", x = 170, y = 0.02, label = "Mean = 55.63", color = "black", size = 5) +
  annotate("text", x = 170, y = 0.017, label = "n = 931", color = "black", size = 5) +
  theme(legend.position = c(0.75, 0.4)) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))
  
pCd


# Cu
Cu_con <- subset(hms_con, HMs_type == "Cu")
Cu_con_mean <- Cu_con %>% group_by(EMS) %>% summarise(Mean = mean(Pollutant_difference), N = n())

pCu <- ggplot(data = Cu_con, aes(x = Pollutant_difference,fill = EMS)) +
  geom_density(color = "black", size = 1.2) +
  theme_few() +
  scale_x_continuous(limits = c(-100, 700), breaks=seq(-100, 700, by=200)) +
  xlab("Cu Concentration") + ylab("Density") + font +
  geom_vline(data = Cu_con_mean, aes(xintercept = Mean, color = EMS), size = 1.2,linetype="dashed")+
  annotate("text", x = 500, y = 0.010, label = "Mean = 224.95", color = "black", size = 5) +
  annotate("text", x = 500, y = 0.007, label = "n = 257", color = "black", size = 5) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

pCu


# Pb
Pb_con <- subset(hms_con, HMs_type == "Pb")
Pb_con_mean <- Pb_con %>% group_by(EMS) %>% summarise(Mean = mean(Pollutant_difference), N = n())

pPb <- ggplot(data = Pb_con, aes(x = Pollutant_difference,fill = EMS)) +
  geom_density(color = "black", size = 1.2) +
  theme_few() +
  scale_x_continuous(limits = c(-1000, 5000), breaks=seq(-1000, 5000, by=2000)) +
  xlab("Pb Concentration") + ylab("Density") + font +
  geom_vline(data = Pb_con_mean, aes(xintercept = Mean, color = EMS), size = 1.2,linetype="dashed")+
  annotate("text", x = 2600, y = 0.001, label = "Mean = 701.53", color = "black", size = 5) +
  annotate("text", x = 2600, y = 0.0007, label = "n = 342", color = "black", size = 5)+
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

pPb

# Zn
Zn_con <- subset(hms_con, HMs_type == "Zn")
Zn_con_mean <- Zn_con %>% group_by(EMS) %>% summarise(Mean = mean(Pollutant_difference), N = n())
pZn <- ggplot(data = Zn_con, aes(x = Pollutant_difference,fill = EMS)) +
  geom_density(color = "black", size = 1.2) +
  theme_few() +
  scale_x_continuous(limits = c(-1000, 3000), breaks=seq(-1000, 3000, by=1000)) +
  xlab("Zn Concentration") + ylab("Density") + font +
  geom_vline(data = Zn_con_mean, aes(xintercept = Mean, color = EMS), size = 1.2,linetype="dashed")+
  annotate("text", x = 1800, y = 0.0012, label = "Mean = 537.20", color = "black", size = 5)+
  annotate("text", x = 1800, y = 0.0009, label = "n = 156", color = "black", size = 5) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

pZn


# As
As_con <- subset(hms_con, HMs_type == "As")
As_con_mean <- As_con %>% group_by(EMS) %>% summarise(Mean = mean(Pollutant_difference), N = n())
pAs <- ggplot(data = As_con, aes(x = Pollutant_difference,fill = EMS)) +
  geom_density(color = "black", size = 1.2) +
  theme_few() +
  scale_x_continuous(limits = c(-2000, 8000), breaks=seq(-2000, 8000, by=2000)) +
  xlab("As Concentration") + ylab("Density") + font +
  geom_vline(data = As_con_mean, aes(xintercept = Mean, color = EMS), size = 1.2,linetype="dashed")+
  annotate("text", x = 1700, y = 0.006, label = "Mean = 2770.96", color = "black", size = 5) +
  annotate("text", x = 1700, y = 0.003, label = "n = 40", color = "black", size = 5) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

pAs

# Sb
Sb_con <- subset(hms_con, HMs_type == "Sb")
Sb_con_mean <- Sb_con %>% group_by(EMS) %>% summarise(Mean = mean(Pollutant_difference), N = n())
pSb <- ggplot(data = Sb_con, aes(x = Pollutant_difference,fill = EMS)) +
  geom_density(fill = "#00BFC4", color = "black", size = 1.2) +
  theme_few() +
  scale_x_continuous(limits = c(-8000, 18000), breaks=seq(-8000, 18000, by=10000)) +
  xlab("Sb Concentration") + ylab("Density") + font +
  geom_vline(data = Sb_con_mean, aes(xintercept = Mean), color = "#00BFC4", size = 1.2,linetype="dashed")+
  annotate("text", x = 11000, y = 0.0002, label = "Mean = 3299.67", color = "#00BFC4", size = 5)+
  annotate("text", x = 11000, y = 0.00017, label = "n = 30", color = "#00BFC4", size = 5) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

pSb

# Ni
Ni_con <- subset(hms_con, HMs_type == "Ni")
Ni_con_mean <- Ni_con %>% group_by(EMS) %>% summarise(Mean = mean(Pollutant_difference), N = n())
pNi <- ggplot(data = Ni_con, aes(x = Pollutant_difference,fill = EMS)) +
  geom_density(fill = "#00BFC4", color = "black", size = 1.2) +
  theme_few() +
  scale_x_continuous(limits = c(-1000, 1000), breaks=seq(-1000, 1000, by=500)) +
  xlab("Ni Concentration") + ylab("Density") + font +
  geom_vline(data = Ni_con_mean, aes(xintercept = Mean), color = "#00BFC4", size = 1.2,linetype="dashed")+
  annotate("text", x = -500, y = 0.0020, label = "Mean = 191.97", color = "#00BFC4", size = 5)+
  annotate("text", x = -500, y = 0.0017, label = "n = 9", color = "#00BFC4", size = 5) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

pNi

# Cr
Cr_con <- subset(hms_con, HMs_type == "Cr")
Cr_con_mean <- Cr_con %>% group_by(EMS) %>% summarise(Mean = mean(Pollutant_difference), N = n())
pCr <- ggplot(data = Cr_con, aes(x = Pollutant_difference,fill = EMS)) +
  geom_density(fill = "#00BFC4", color = "black", size = 1.2) +
  theme_few() +
  scale_x_continuous(limits = c(-200, 600), breaks=seq(-200, 600, by=200)) +
  xlab("Cr Concentration") + ylab("Density") + font +
  geom_vline(data = Cr_con_mean, aes(xintercept = Mean), color = "#00BFC4", size = 1.2,linetype="dashed")+
  annotate("text", x = 300, y = 0.004, label = "Mean = 140", color = "#00BFC4", size = 5)+
  annotate("text", x = 300, y = 0.0037, label = "n = 6", color = "#00BFC4", size = 5) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))
pCr

# Ti
Ti_con <- subset(hms_con, HMs_type == "Ti")
Ti_con_mean <- Ti_con %>% group_by(EMS) %>% summarise(Mean = mean(Pollutant_difference), N = n())
pTi <- ggplot(data = Ti_con, aes(x = Pollutant_difference,fill = EMS)) +
  geom_density(fill = "#00BFC4", color = "black", size = 1.2) +
  theme_few() +
  scale_x_continuous(limits = c(-400, 600), breaks=seq(-400, 600, by=300)) +
  xlab("Ti Concentration") + ylab("Density") + font +
  geom_vline(data = Ti_con_mean, aes(xintercept = Mean), color = "#00BFC4", size = 1.2,linetype="dashed")+
  annotate("text", x = 380, y = 0.005, label = "Mean = 143", color = "#00BFC4", size = 5)+
  annotate("text", x = 380, y = 0.0047, label = "n = 6", color = "#00BFC4", size = 5) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

pTi

HMSCON <- plot_grid(pCd, pCu, pPb, pZn, pAs, pSb, pNi, pCr, pTi, ncol = 3, labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)"), label_size = 18, label_x = -0.03)
#ggsave("Figure S2 0625.pdf",HMSCON, width = 15, height = 14)




####################################  R/S  #####################################
d_tb_rs <- read_xlsx("data-24-0227.xlsx", sheet = "RS")
str(d_tb_rs)
d_tb_rs$EMS <- ifelse(d_tb_rs$Experimental_method == "Hydroponics", "Hydroponics", "Soil/Steril_Steril_medium")
d_tb_rs <- d_tb_rs %>% drop_na(RS_CK)

d_tb_rs_1 <- d_tb_rs[, c(3,15:16, 30,158, 78:79)]

d_tb_rs_long <- pivot_longer(d_tb_rs_1, cols = -c(1:5), names_to = "Treatment", values_to = "RS")


d_tb_RS_hyd <- subset(d_tb_rs_long, EMS == "Hydroponics")
d_tb_RS_soil <- subset(d_tb_rs_long, EMS == "Soil/Steril_Steril_medium")

# Figure
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
###  Hydroponics ###
fig_hyd <- ggplot(d_tb_RS_hyd, aes(x = reorder(Genus, -RS), y = RS, fill = factor(Treatment))) +
  geom_boxplot(outlier.size = 0.5, size = 0.5) +
  stat_summary(fun.y ="mean", geom="point", shape=20, size=2.5, color="red",alpha=1, position = position_dodge2(width = 0.75, preserve = "single"))+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'R/S', fill = '', color = '') +
  scale_y_continuous(limits = c(0, 1.3), breaks=seq(0, 1.3, by=0.4))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = c(0.85, 0.6))+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 8 , y = 1.09,label = "R/S = 1.0", colour="blue", size = 6)+
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

fig_hyd

###  Soil ###
fig_soil <- ggplot(d_tb_RS_soil, aes(x = reorder(Genus, -RS), y = RS, fill = factor(Treatment))) +
  geom_boxplot(outlier.size = 0.5, size = 0.5) +
  stat_summary(fun.y ="mean", geom="point", shape=20, size=2.5, color="red",alpha=1, position = position_dodge2(width = 0.75, preserve = "single"))+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'R/S', fill = '', color = '') +
  scale_y_continuous(limits = c(0, 1.3), breaks=seq(0, 1.3, by=0.4))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = c(0.85, 0.88))+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 15 , y = 1.09,label = "R/S = 1.0", colour="blue", size = 6)+
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))
fig_soil


ems_fig <- plot_grid(fig_hyd, fig_soil, labels = c("(a)", "(b)"), label_size = 20, ncol = 1)
#ggsave("Figure 4 0625.pdf", ems_fig, width = 10, height = 9)



#### HMs
###  Hydroponics ###
fig_hyd_hms <- ggplot(d_tb_RS_hyd, aes(x = reorder(Genus, -RS), y = RS, fill = factor(Treatment))) +
  geom_boxplot(outlier.size = 0.5, size = 0.5) +
  stat_summary(fun.y ="mean", geom="point", shape=20, size=2.5, color="red",alpha=1, position = position_dodge2(width = 0.75, preserve = "single"))+
  theme_bw()+
  facet_grid(.~HMs_type) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'R/S', fill = '', color = '') +
  scale_y_continuous(limits = c(0, 1.3), breaks=seq(0, 1.3, by=0.4))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  #theme(legend.position = c(0.85, 0.6))+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 8 , y = 1.09,label = "R/S = 1.0", colour="blue", size = 6)+
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

fig_hyd_hms
d_tb_RS_soil <- subset(d_tb_RS_soil, HMs_type != "Ni")
###  Soil ###
fig_soil_hms <- ggplot(d_tb_RS_soil, aes(x = reorder(Genus, -RS), y = RS, fill = factor(Treatment))) +
  geom_boxplot(outlier.size = 0.5, size = 0.5) +
  stat_summary(fun.y ="mean", geom="point", shape=20, size=2.5, color="red",alpha=1, position = position_dodge2(width = 0.75, preserve = "single"))+
  theme_bw()+
  facet_grid(.~HMs_type) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'R/S', fill = '', color = '') +
  scale_y_continuous(limits = c(0, 1.3), breaks=seq(0, 1.3, by=0.4))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  #theme(legend.position = c(0.85, 0.88))+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 15 , y = 1.09,label = "R/S = 1.0", colour="blue", size = 6)+
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))
fig_soil_hms

library(ggpubr)
fig_rs_hms <- ggarrange(fig_hyd_hms, fig_soil_hms, ncol = 1, common.legend = T, legend = "top", labels = c("(a)", "(b)"), font.label = list(size = 30))

#ggsave("Figure S7 0625.pdf",fig_rs_hms, width = 20, height = 18)





#############################    Meta-analysis mainly  #########################

#############################     Fig.1a Biomass    ############################
######Fig.1a-1 Total biomass##########
d_tb <- read_xlsx("data-24-0227.xlsx", sheet = "total_biomass")
#effect size
es_total_biomass<-escalc(measure = "ROM", 
                         m1i = TB_Xt,
                         sd1i = TB_SDt,
                         n1i = TB_Nt,
                         m2i = TB_Xc,
                         sd2i = TB_SDc,
                         n2i = TB_Nc,
                         data = d_tb)

#removed the NA in yi and vi
es_total_biomass <- drop_na(es_total_biomass, yi)

#Factor
es_total_biomass$Study_ID <- factor(es_total_biomass$Study_ID)
es_total_biomass$HMs_type <- factor(es_total_biomass$HMs_type)
#How many HMs
table(es_total_biomass$HMs_type)

#Get the HMs data
da_Cd <- subset(es_total_biomass, HMs_type == "Cd") #243
da_Cu <- subset(es_total_biomass, HMs_type == "Cu") #62
da_Pb <- subset(es_total_biomass, HMs_type == "Pb") #161
da_Zn <- subset(es_total_biomass, HMs_type == "Zn") #29
da_Sb <- subset(es_total_biomass, HMs_type == "Sb") #16
da_As <- subset(es_total_biomass, HMs_type == "As") #8
##################effects of Cd on Total biomass#########################
Cd_tb_overall <- rma.mv(yi, vi, data = da_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_tb_overall.n <- da_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_tb_overall.R <- coef(summary(Cd_tb_overall)) %>% mutate(HMs_type="Cd", size=(Cd_tb_overall.n$n)) 


##################effects of Cu on Total biomass#########################
Cu_tb_overall <- rma.mv(yi, vi, data = da_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_tb_overall.n <- da_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_tb_overall.R <- coef(summary(Cu_tb_overall)) %>% mutate(HMs_type="Cu", size=(Cu_tb_overall.n$n)) 


##################effects of Pb on Total biomass#########################
Pb_tb_overall <- rma.mv(yi, vi, data = da_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_tb_overall.n <- da_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_tb_overall.R <- coef(summary(Pb_tb_overall)) %>% mutate(HMs_type="Pb", size=(Pb_tb_overall.n$n)) 


##################effects of Zn on Total biomass#########################
Zn_tb_overall <- rma.mv(yi, vi, data = da_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_tb_overall.n <- da_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_tb_overall.R <- coef(summary(Zn_tb_overall)) %>% mutate(HMs_type="Zn", size=(Zn_tb_overall.n$n)) 


##################effects of Sb on Total biomass#########################
Sb_tb_overall <- rma.mv(yi, vi, data = da_Sb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Sb_tb_overall.n <- da_Sb %>%  group_by(HMs_type) %>% summarise(n = n())
Sb_tb_overall.R <- coef(summary(Sb_tb_overall)) %>% mutate(HMs_type="Sb", size=(Sb_tb_overall.n$n)) 


##################effects of As on Total biomass#########################
As_tb_overall <- rma.mv(yi, vi, data = da_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_tb_overall.n <- da_As %>%  group_by(HMs_type) %>% summarise(n = n())
As_tb_overall.R <- coef(summary(As_tb_overall)) %>% mutate(HMs_type="As", size=(As_tb_overall.n$n)) 



############bind results for Total biomass############
dtb <- bind_rows(Cd_tb_overall.R, Cu_tb_overall.R, Pb_tb_overall.R, Zn_tb_overall.R, Sb_tb_overall.R, As_tb_overall.R)
dtb$Parameter <- c(rep("Total biomass", 6))


#################    Fig.1a-2 Above biomass   ##################################
d_ab <- read_xlsx("data-24-0227.xlsx", sheet = "aboveground_biomass")

#effect size
es_above_biomass<-escalc(measure = "ROM", 
                         m1i = AB_Xt,
                         sd1i = AB_SDt,
                         n1i = AB_Nt,
                         m2i = AB_Xc,
                         sd2i = AB_SDc,
                         n2i = AB_Nc,
                         data = d_ab)

#removed the NA in yi and vi
es_above_biomass <- drop_na(es_above_biomass, yi)

es_above_biomass$Study_ID <- factor(es_above_biomass$Study_ID)
es_above_biomass$HMs_type <- factor(es_above_biomass$HMs_type)
table(es_above_biomass$HMs_type)


da_ab_Cd <- subset(es_above_biomass, HMs_type == "Cd") #173
da_ab_Cu <- subset(es_above_biomass, HMs_type == "Cu") #46
da_ab_Pb <- subset(es_above_biomass, HMs_type == "Pb") #122
da_ab_Zn <- subset(es_above_biomass, HMs_type == "Zn") #53
da_ab_As <- subset(es_above_biomass, HMs_type == "As") #3
da_ab_Ni <- subset(es_above_biomass, HMs_type == "Ni") #3

##################effects of Cd on Above biomass#########################
Cd_ab_overall <- rma.mv(yi, vi, data = da_ab_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_ab_overall.n <- da_ab_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_ab_overall.R <- coef(summary(Cd_ab_overall)) %>% mutate(HMs_type="Cd", size=(Cd_ab_overall.n$n)) 


##################effects of Cu on Above biomass#########################
Cu_ab_overall <- rma.mv(yi, vi, data = da_ab_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_ab_overall.n <- da_ab_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_ab_overall.R <- coef(summary(Cu_ab_overall)) %>% mutate(HMs_type="Cu", size=(Cu_ab_overall.n$n)) 


##################effects of Pb on Above biomass#########################
Pb_ab_overall <- rma.mv(yi, vi, data = da_ab_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_ab_overall.n <- da_ab_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_ab_overall.R <- coef(summary(Pb_ab_overall)) %>% mutate(HMs_type="Pb", size=(Pb_ab_overall.n$n)) 


##################effects of Zn on Above biomass#########################
Zn_ab_overall <- rma.mv(yi, vi, data = da_ab_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_ab_overall.n <- da_ab_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_ab_overall.R <- coef(summary(Zn_ab_overall)) %>% mutate(HMs_type="Zn", size=(Zn_ab_overall.n$n)) 


##################effects of As on Above biomass#########################
As_ab_overall <- rma.mv(yi, vi, data = da_ab_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_ab_overall.n <- da_ab_As %>%  group_by(HMs_type) %>% summarise(n = n())
As_ab_overall.R <- coef(summary(As_ab_overall)) %>% mutate(HMs_type="As", size=(As_ab_overall.n$n)) 

##################effects of As on Above biomass#########################
Ni_ab_overall <- rma.mv(yi, vi, data = da_ab_Ni, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Ni_ab_overall.n <- da_ab_Ni %>%  group_by(HMs_type) %>% summarise(n = n())
Ni_ab_overall.R <- coef(summary(Ni_ab_overall)) %>% mutate(HMs_type="Ni", size=(Ni_ab_overall.n$n)) 



############bind results for Above biomass############
dab <- bind_rows(Cd_ab_overall.R, Cu_ab_overall.R, Pb_ab_overall.R, Zn_ab_overall.R, As_ab_overall.R, Ni_ab_overall.R)
dab$Parameter <- c(rep("Aboveground biomass", 6))


###################       Fig.1a-3 Leaf biomass    #############################
d_lb <- read_xlsx("data-24-0227.xlsx", sheet = "leaf_biomass")
str(d_lb)
#effect size
es_leaf_biomass<-escalc(measure = "ROM", 
                         m1i = LB_Xt,
                         sd1i = LB_SDt,
                         n1i = LB_Nt,
                         m2i = LB_Xc,
                         sd2i = LB_SDc,
                         n2i = LB_Nc,
                         data = d_lb)

#removed the NA in yi and vi
es_leaf_biomass <- drop_na(es_leaf_biomass, yi)

es_leaf_biomass$Study_ID <- factor(es_leaf_biomass$Study_ID)
es_leaf_biomass$HMs_type <- factor(es_leaf_biomass$HMs_type)
table(es_leaf_biomass$HMs_type)


da_lb_Cd <- subset(es_leaf_biomass, HMs_type == "Cd") #294
da_lb_Cu <- subset(es_leaf_biomass, HMs_type == "Cu") #68
da_lb_Pb <- subset(es_leaf_biomass, HMs_type == "Pb") #99
da_lb_Zn <- subset(es_leaf_biomass, HMs_type == "Zn") #61
da_lb_As <- subset(es_leaf_biomass, HMs_type == "As") #7
da_lb_Sb <- subset(es_leaf_biomass, HMs_type == "Sb") #2

##################effects of Cd on Leaf biomass#########################
Cd_lb_overall <- rma.mv(yi, vi, data = da_lb_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_lb_overall.n <- da_lb_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_lb_overall.R <- coef(summary(Cd_lb_overall)) %>% mutate(HMs_type="Cd", size=(Cd_lb_overall.n$n)) 


##################effects of Cu on Leaf biomass#########################
Cu_lb_overall <- rma.mv(yi, vi, data = da_lb_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_lb_overall.n <- da_lb_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_lb_overall.R <- coef(summary(Cu_lb_overall)) %>% mutate(HMs_type="Cu", size=(Cu_lb_overall.n$n)) 


##################effects of Pb on Leaf biomass#########################
Pb_lb_overall <- rma.mv(yi, vi, data = da_lb_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_lb_overall.n <- da_lb_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_lb_overall.R <- coef(summary(Pb_lb_overall)) %>% mutate(HMs_type="Pb", size=(Pb_lb_overall.n$n)) 


##################effects of Zn on Leaf biomass#########################
Zn_lb_overall <- rma.mv(yi, vi, data = da_lb_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_lb_overall.n <- da_lb_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_lb_overall.R <- coef(summary(Zn_lb_overall)) %>% mutate(HMs_type="Zn", size=(Zn_lb_overall.n$n)) 


##################effects of As on Leaf biomass#########################
As_lb_overall <- rma.mv(yi, vi, data = da_lb_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_lb_overall.n <- da_lb_As %>%  group_by(HMs_type) %>% summarise(n = n())
As_lb_overall.R <- coef(summary(As_lb_overall)) %>% mutate(HMs_type="As", size=(As_lb_overall.n$n)) 

##################effects of As on Leaf biomass#########################
Sb_lb_overall <- rma.mv(yi, vi, data = da_lb_Sb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Sb_lb_overall.n <- da_lb_Sb %>%  group_by(HMs_type) %>% summarise(n = n())
Sb_lb_overall.R <- coef(summary(Sb_lb_overall)) %>% mutate(HMs_type="Sb", size=(Sb_lb_overall.n$n)) 


############bind results for Leaf biomass############
dlb <- bind_rows(Cd_lb_overall.R, Cu_lb_overall.R, Pb_lb_overall.R, Zn_lb_overall.R, As_lb_overall.R, Sb_lb_overall.R)
dlb$Parameter <- c(rep("Leaf biomass", 6))




##############################    Fig.1a-4 Stem biomass  #######################
d_sb <- read_xlsx("data-24-0227.xlsx", sheet = "stem_biomass")
str(d_sb)
#effect size
es_stem_biomass<-escalc(measure = "ROM", 
                        m1i = SB_Xt,
                        sd1i = SB_SDt,
                        n1i = SB_Nt,
                        m2i = SB_Xc,
                        sd2i = SB_SDc,
                        n2i = SB_Nc,
                        data = d_sb)

#removed the NA in yi and vi
es_stem_biomass <- drop_na(es_stem_biomass, yi)

es_stem_biomass$Study_ID <- factor(es_stem_biomass$Study_ID)
es_stem_biomass$HMs_type <- factor(es_stem_biomass$HMs_type)
table(es_stem_biomass$HMs_type)


da_sb_Cd <- subset(es_stem_biomass, HMs_type == "Cd") #311
da_sb_Cu <- subset(es_stem_biomass, HMs_type == "Cu") #50
da_sb_Pb <- subset(es_stem_biomass, HMs_type == "Pb") #86
da_sb_Zn <- subset(es_stem_biomass, HMs_type == "Zn") #60
da_sb_Sb <- subset(es_stem_biomass, HMs_type == "Sb") #2

##################effects of Cd on stem biomass#########################
Cd_sb_overall <- rma.mv(yi, vi, data = da_sb_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_sb_overall.n <- da_sb_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_sb_overall.R <- coef(summary(Cd_sb_overall)) %>% mutate(HMs_type="Cd", size=(Cd_sb_overall.n$n)) 


##################effects of Cu on stem biomass#########################
Cu_sb_overall <- rma.mv(yi, vi, data = da_sb_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_sb_overall.n <- da_sb_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_sb_overall.R <- coef(summary(Cu_sb_overall)) %>% mutate(HMs_type="Cu", size=(Cu_sb_overall.n$n)) 


##################effects of Pb on stem biomass#########################
Pb_sb_overall <- rma.mv(yi, vi, data = da_sb_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_sb_overall.n <- da_sb_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_sb_overall.R <- coef(summary(Pb_sb_overall)) %>% mutate(HMs_type="Pb", size=(Pb_sb_overall.n$n)) 


##################effects of Zn on stem biomass#########################
Zn_sb_overall <- rma.mv(yi, vi, data = da_sb_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_sb_overall.n <- da_sb_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_sb_overall.R <- coef(summary(Zn_sb_overall)) %>% mutate(HMs_type="Zn", size=(Zn_sb_overall.n$n)) 


##################effects of As on stem biomass#########################
Sb_sb_overall <- rma.mv(yi, vi, data = da_sb_Sb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Sb_sb_overall.n <- da_sb_Sb %>%  group_by(HMs_type) %>% summarise(n = n())
Sb_sb_overall.R <- coef(summary(Sb_sb_overall)) %>% mutate(HMs_type="Sb", size=(Sb_sb_overall.n$n)) 


############bind results for stem biomass############
dsb <- bind_rows(Cd_sb_overall.R, Cu_sb_overall.R, Pb_sb_overall.R, Zn_sb_overall.R, Sb_sb_overall.R)
dsb$Parameter <- c(rep("Stem biomass", 5))



#################         Fig.1a-5 Underground biomass     #####################
d_rb <- read_xlsx("data-24-0227.xlsx", sheet = "root_biomass")

#effect size
es_root_biomass<-escalc(measure = "ROM", 
                        m1i = RB_Xt,
                        sd1i = RB_SDt,
                        n1i = RB_Nt,
                        m2i = RB_Xc,
                        sd2i = RB_SDc,
                        n2i = RB_Nc,
                        data = d_rb)

#removed the NA in yi and vi
es_root_biomass <- drop_na(es_root_biomass, yi)


es_root_biomass$Study_ID <- factor(es_root_biomass$Study_ID)
es_root_biomass$HMs_type <- factor(es_root_biomass$HMs_type)
table(es_root_biomass$HMs_type)


da_ub_Cd <- subset(es_root_biomass, HMs_type == "Cd") #455
da_ub_Cu <- subset(es_root_biomass, HMs_type == "Cu") #109
da_ub_Pb <- subset(es_root_biomass, HMs_type == "Pb") #206
da_ub_Zn <- subset(es_root_biomass, HMs_type == "Zn") #118
da_ub_As <- subset(es_root_biomass, HMs_type == "As") #10
da_ub_Sb <- subset(es_root_biomass, HMs_type == "Sb") #2
da_ub_Ni <- subset(es_root_biomass, HMs_type == "Ni") #3

##################effects of Cd on root biomass#########################
Cd_ub_overall <- rma.mv(yi, vi, data = da_ub_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_ub_overall.n <- da_ub_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_ub_overall.R <- coef(summary(Cd_ub_overall)) %>% mutate(HMs_type="Cd", size=(Cd_ub_overall.n$n)) 


##################effects of Cu on root biomass#########################
Cu_ub_overall <- rma.mv(yi, vi, data = da_ub_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_ub_overall.n <- da_ub_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_ub_overall.R <- coef(summary(Cu_ub_overall)) %>% mutate(HMs_type="Cu", size=(Cu_ub_overall.n$n)) 


##################effects of Pb on root biomass#########################
Pb_ub_overall <- rma.mv(yi, vi, data = da_ub_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_ub_overall.n <- da_ub_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_ub_overall.R <- coef(summary(Pb_ub_overall)) %>% mutate(HMs_type="Pb", size=(Pb_ub_overall.n$n)) 


##################effects of Zn on root biomass#########################
Zn_ub_overall <- rma.mv(yi, vi, data = da_ub_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_ub_overall.n <- da_ub_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_ub_overall.R <- coef(summary(Zn_ub_overall)) %>% mutate(HMs_type="Zn", size=(Zn_ub_overall.n$n)) 


##################effects of As on root biomass#########################
As_ub_overall <- rma.mv(yi, vi, data = da_ub_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_ub_overall.n <- da_ub_As %>%  group_by(HMs_type) %>% summarise(n = n())
As_ub_overall.R <- coef(summary(As_ub_overall)) %>% mutate(HMs_type="As", size=(As_ub_overall.n$n)) 


##################effects of Sb on root biomass#########################
Sb_ub_overall <- rma.mv(yi, vi, data = da_ub_Sb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Sb_ub_overall.n <- da_ub_Sb %>%  group_by(HMs_type) %>% summarise(n = n())
Sb_ub_overall.R <- coef(summary(Sb_ub_overall)) %>% mutate(HMs_type="Sb", size=(Sb_ub_overall.n$n)) 


##################effects of Ni on root biomass#########################
Ni_ub_overall <- rma.mv(yi, vi, data = da_ub_Ni, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Ni_ub_overall.n <- da_ub_Ni %>%  group_by(HMs_type) %>% summarise(n = n())
Ni_ub_overall.R <- coef(summary(Ni_ub_overall)) %>% mutate(HMs_type="Ni", size=(Ni_ub_overall.n$n)) 


############bind results for Underground biomass############
dub <- bind_rows(Cd_ub_overall.R, Cu_ab_overall.R, Pb_ub_overall.R, Zn_ub_overall.R, As_ub_overall.R, Sb_ub_overall.R, Ni_ub_overall.R)
dub$Parameter <- c(rep("Underground biomass", 7))


###bind results for Fig.1 total above under biomass##
d_biomass <- bind_rows(dtb, dab, dub, dlb, dsb)


make_pct <- function(x) (exp(x) - 1) * 100
d_biomass$estimate <- make_pct(d_biomass$estimate)
d_biomass$ci.lb <- make_pct(d_biomass$ci.lb)
d_biomass$ci.ub <- make_pct(d_biomass$ci.ub)

d_biomass$HMs_type <- factor(d_biomass$HMs_type, levels = c("Cd", "Cu", "Pb", "Zn", "Sb", "As", "Ni"))
d_biomass$Parameter <- factor(d_biomass$Parameter, levels = c("Total biomass", "Aboveground biomass", "Underground biomass", "Leaf biomass", "Stem biomass"))
d_biomass$Sig <- ifelse(d_biomass$pval < 0.001, "***", ifelse(d_biomass$pval < 0.01, "**", ifelse(d_biomass$pval < 0.05, "**", "")))

Fig.1 <- ggplot(d_biomass,aes(x=estimate, y=HMs_type, color = Parameter))+
  geom_point(aes(),shape=16, size=5,  position = position_dodge(0.9))+
  geom_errorbar(aes(xmin = ci.lb, xmax =ci.ub),size=1,  width=0.2, position = position_dodge(0.9))+
  facet_wrap(~Parameter, scales = "free",ncol = 3, nrow = 2) + 
  xlab('Estimate (%)')+ ylab("Heavy metals type") +
  #scale_x_continuous(expand = c(0, 0),limits = c(-80, 350),breaks=seq(-80,350,50))+
  theme_bw()+
  geom_vline(aes(xintercept=0), linetype="dashed",color = "grey", linewidth=1.5)+
  theme(legend.position = "none")+
  theme(text = element_text(size =23), 
        axis.title.y=element_text(size=25)) +
  theme(axis.ticks.x=element_line(color="black",size=1,lineend = 10))+
  theme(axis.ticks.y=element_line(color="black",size=1,lineend = 10))+
  theme(axis.ticks.length=unit(.15, "cm"))+
  theme(axis.ticks.y=element_line(color="black",size=1,lineend=10))+
  geom_text(aes(y=HMs_type, x=ci.ub+10,label=paste0("(",size,")", Sig)),size=5, fontface="bold",position = position_dodge(0.9))
Fig.1

#ggsave("Figure S5 0625.pdf", Fig.1, height = 10, width = 10)



#########################Fig.2 Heavy metals concentration#######################

######Fig.2b-1 Total concentration##########

d_t_hms <- read_xlsx("data-24-0227.xlsx", sheet = "total_HMs_concentration")

#effect size
es_hms_total <- escalc(measure = "ROM", 
                       m1i = THMs_Xt,
                       sd1i = THMs_SDt,
                       n1i = THMs_Nt,
                       m2i = THMs_Xc,
                       sd2i = THMs_SDc,
                       n2i = THMs_Nc,
                       data = d_t_hms)

#removed the NA in yi and vi
es_hms_total <- drop_na(es_hms_total, yi)


es_hms_total$Study_ID <- factor(es_hms_total$Study_ID)
es_hms_total$HMs_type <- factor(es_hms_total$HMs_type)
table(es_hms_total$HMs_type)

da_hms_total_Cd <- subset(es_hms_total, HMs_type == "Cd") #30
da_hms_total_Cu <- subset(es_hms_total, HMs_type == "Cu") #6
da_hms_total_Pb <- subset(es_hms_total, HMs_type == "Pb") #30
da_hms_total_Zn <- subset(es_hms_total, HMs_type == "Zn") #8

##################effects of Cd on Total heavy metals concentration#########################
Cd_total_hms_overall <- rma.mv(yi, vi, data = da_hms_total_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_total_hms_overall.n <- da_hms_total_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_total_hms_overall.R <- coef(summary(Cd_total_hms_overall)) %>% mutate(HMs_type="Cd", size=(Cd_total_hms_overall.n$n)) 


##################effects of Cu on Total heavy metals concentration#########################
Cu_total_hms_overall <- rma.mv(yi, vi, data = da_hms_total_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_total_hms_overall.n <- da_hms_total_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_total_hms_overall.R <- coef(summary(Cu_total_hms_overall)) %>% mutate(HMs_type="Cu", size=(Cu_total_hms_overall.n$n)) 


##################effects of Pb on Total heavy metals concentration#########################
Pb_total_hms_overall <- rma.mv(yi, vi, data = da_hms_total_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_total_hms_overall.n <- da_hms_total_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_total_hms_overall.R <- coef(summary(Pb_total_hms_overall)) %>% mutate(HMs_type="Pb", size=(Pb_total_hms_overall.n$n)) 


##################effects of Pb on Total heavy metals concentration#########################
Zn_total_hms_overall <- rma.mv(yi, vi, data = da_hms_total_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_total_hms_overall.n <- da_hms_total_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_total_hms_overall.R <- coef(summary(Zn_total_hms_overall)) %>% mutate(HMs_type="Zn", size=(Zn_total_hms_overall.n$n)) 





############bind results for Total biomass############
dthms <- bind_rows(Cd_total_hms_overall.R, Cu_total_hms_overall.R, Pb_total_hms_overall.R, Zn_total_hms_overall.R)
dthms$Parameter <- c(rep("Total HMs concentration", 4))


######Fig.1b-2 Aboveground concentration##########

d_a_hms <- read_xlsx("data-24-0227.xlsx", sheet = "aboveground_HMs_concentration")

#effect size
es_hms_above <- escalc(measure = "ROM", 
                       m1i = AHMs_Xt,
                       sd1i = AHMs_SDt,
                       n1i = AHMs_Nt,
                       m2i = AHMs_Xc,
                       sd2i = AHMs_SDc,
                       n2i = AHMs_Nc,
                       data = d_a_hms)

#removed the NA in yi and vi
es_hms_above <- drop_na(es_hms_above, yi)



es_hms_above$Study_ID <- factor(es_hms_above$Study_ID)
es_hms_above$HMs_type <- factor(es_hms_above$HMs_type)
table(es_hms_above$HMs_type)

da_hms_above_Cd <- subset(es_hms_above, HMs_type == "Cd") #90
da_hms_above_Cu <- subset(es_hms_above, HMs_type == "Cu") #24
da_hms_above_Pb <- subset(es_hms_above, HMs_type == "Pb") #50
da_hms_above_Zn <- subset(es_hms_above, HMs_type == "Zn") #23
da_hms_above_As <- subset(es_hms_above, HMs_type == "As") #3
da_hms_above_Ni <- subset(es_hms_above, HMs_type == "Ni") #3


##################effects of Cd on Above heavy metals concentration#########################
Cd_above_hms_overall <- rma.mv(yi, vi, data = da_hms_above_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_above_hms_overall.n <- da_hms_above_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_above_hms_overall.R <- coef(summary(Cd_above_hms_overall)) %>% mutate(HMs_type="Cd", size=(Cd_above_hms_overall.n$n)) 


##################effects of Cu on above heavy metals concentration#########################
Cu_above_hms_overall <- rma.mv(yi, vi, data = da_hms_above_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_above_hms_overall.n <- da_hms_above_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_above_hms_overall.R <- coef(summary(Cu_above_hms_overall)) %>% mutate(HMs_type="Cu", size=(Cu_above_hms_overall.n$n)) 


##################effects of Pb on above heavy metals concentration#########################
Pb_above_hms_overall <- rma.mv(yi, vi, data = da_hms_above_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_above_hms_overall.n <- da_hms_above_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_above_hms_overall.R <- coef(summary(Pb_above_hms_overall)) %>% mutate(HMs_type="Pb", size=(Pb_above_hms_overall.n$n)) 

##################effects of Zn on above heavy metals concentration#########################
Zn_above_hms_overall <- rma.mv(yi, vi, data = da_hms_above_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_above_hms_overall.n <- da_hms_above_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_above_hms_overall.R <- coef(summary(Zn_above_hms_overall)) %>% mutate(HMs_type="Zn", size=(Zn_above_hms_overall.n$n)) 

##################effects of As on above heavy metals concentration#########################
As_above_hms_overall <- rma.mv(yi, vi, data = da_hms_above_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_above_hms_overall.n <- da_hms_above_As %>%  group_by(HMs_type) %>% summarise(n = n())
As_above_hms_overall.R <- coef(summary(As_above_hms_overall)) %>% mutate(HMs_type="As", size=(As_above_hms_overall.n$n)) 


##################effects of Ni on above heavy metals concentration#########################
Ni_above_hms_overall <- rma.mv(yi, vi, data = da_hms_above_Ni, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Ni_above_hms_overall.n <- da_hms_above_Ni %>%  group_by(HMs_type) %>% summarise(n = n())
Ni_above_hms_overall.R <- coef(summary(Ni_above_hms_overall)) %>% mutate(HMs_type="Ni", size=(Ni_above_hms_overall.n$n)) 


############bind results for above biomass############
dahms <- bind_rows(Cd_above_hms_overall.R, Cu_above_hms_overall.R, Pb_above_hms_overall.R, Zn_above_hms_overall.R, As_above_hms_overall.R, Ni_above_hms_overall.R)
dahms$Parameter <- c(rep("Aboveground HMs concentration", 6))




######Fig.2b-3 Leaf concentration##########

d_l_hms <- read_xlsx("data-24-0227.xlsx", sheet = "leaf_HMs_concentration")

#effect size
es_hms_leaf <- escalc(measure = "ROM", 
                       m1i = LHMs_Xt,
                       sd1i = LHMs_SDt,
                       n1i = LHMs_Nt,
                       m2i = LHMs_Xc,
                       sd2i = LHMs_SDc,
                       n2i = LHMs_Nc,
                       data = d_l_hms)

#removed the NA in yi and vi
es_hms_leaf <- drop_na(es_hms_leaf, yi)



es_hms_leaf$Study_ID <- factor(es_hms_leaf$Study_ID)
es_hms_leaf$HMs_type <- factor(es_hms_leaf$HMs_type)
table(es_hms_leaf$HMs_type)

da_hms_leaf_Cd <- subset(es_hms_leaf, HMs_type == "Cd") #353
da_hms_leaf_Cu <- subset(es_hms_leaf, HMs_type == "Cu") #96
da_hms_leaf_Pb <- subset(es_hms_leaf, HMs_type == "Pb") #172
da_hms_leaf_Zn <- subset(es_hms_leaf, HMs_type == "Zn") #95
da_hms_leaf_As <- subset(es_hms_leaf, HMs_type == "As") #34
da_hms_leaf_Ni <- subset(es_hms_leaf, HMs_type == "Ni") #6
da_hms_leaf_Sb <- subset(es_hms_leaf, HMs_type == "Sb") #14
da_hms_leaf_Ti <- subset(es_hms_leaf, HMs_type == "Ti") #6

##################effects of Cd on leaf heavy metals concentration#########################
Cd_leaf_hms_overall <- rma.mv(yi, vi, data = da_hms_leaf_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_leaf_hms_overall.n <- da_hms_leaf_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_leaf_hms_overall.R <- coef(summary(Cd_leaf_hms_overall)) %>% mutate(HMs_type="Cd", size=(Cd_leaf_hms_overall.n$n)) 


##################effects of Cu on leaf heavy metals concentration#########################
Cu_leaf_hms_overall <- rma.mv(yi, vi, data = da_hms_leaf_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_leaf_hms_overall.n <- da_hms_leaf_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_leaf_hms_overall.R <- coef(summary(Cu_leaf_hms_overall)) %>% mutate(HMs_type="Cu", size=(Cu_leaf_hms_overall.n$n)) 


##################effects of Pb on leaf heavy metals concentration#########################
Pb_leaf_hms_overall <- rma.mv(yi, vi, data = da_hms_leaf_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_leaf_hms_overall.n <- da_hms_leaf_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_leaf_hms_overall.R <- coef(summary(Pb_leaf_hms_overall)) %>% mutate(HMs_type="Pb", size=(Pb_leaf_hms_overall.n$n)) 

##################effects of Zn on leaf heavy metals concentration#########################
Zn_leaf_hms_overall <- rma.mv(yi, vi, data = da_hms_leaf_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_leaf_hms_overall.n <- da_hms_leaf_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_leaf_hms_overall.R <- coef(summary(Zn_leaf_hms_overall)) %>% mutate(HMs_type="Zn", size=(Zn_leaf_hms_overall.n$n)) 

##################effects of As on leaf heavy metals concentration#########################
As_leaf_hms_overall <- rma.mv(yi, vi, data = da_hms_leaf_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_leaf_hms_overall.n <- da_hms_leaf_As %>%  group_by(HMs_type) %>% summarise(n = n())
As_leaf_hms_overall.R <- coef(summary(As_leaf_hms_overall)) %>% mutate(HMs_type="As", size=(As_leaf_hms_overall.n$n)) 


##################effects of Ni on leaf heavy metals concentration#########################
Ni_leaf_hms_overall <- rma.mv(yi, vi, data = da_hms_leaf_Ni, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Ni_leaf_hms_overall.n <- da_hms_leaf_Ni %>%  group_by(HMs_type) %>% summarise(n = n())
Ni_leaf_hms_overall.R <- coef(summary(Ni_leaf_hms_overall)) %>% mutate(HMs_type="Ni", size=(Ni_leaf_hms_overall.n$n)) 


##################effects of Sb on leaf heavy metals concentration#########################
Sb_leaf_hms_overall <- rma.mv(yi, vi, data = da_hms_leaf_Sb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Sb_leaf_hms_overall.n <- da_hms_leaf_Sb %>%  group_by(HMs_type) %>% summarise(n = n())
Sb_leaf_hms_overall.R <- coef(summary(Sb_leaf_hms_overall)) %>% mutate(HMs_type="Sb", size=(Sb_leaf_hms_overall.n$n)) 


##################effects of Ti on leaf heavy metals concentration#########################
Ti_leaf_hms_overall <- rma.mv(yi, vi, data = da_hms_leaf_Ti, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Ti_leaf_hms_overall.n <- da_hms_leaf_Ti %>%  group_by(HMs_type) %>% summarise(n = n())
Ti_leaf_hms_overall.R <- coef(summary(Ti_leaf_hms_overall)) %>% mutate(HMs_type="Ti", size=(Ti_leaf_hms_overall.n$n)) 





############bind results for leaf biomass############
dlhms <- bind_rows(Cd_leaf_hms_overall.R, Cu_leaf_hms_overall.R, Pb_leaf_hms_overall.R, Zn_leaf_hms_overall.R, As_leaf_hms_overall.R, Ni_leaf_hms_overall.R, Sb_leaf_hms_overall.R, Ti_leaf_hms_overall.R )
dlhms$Parameter <- c(rep("Leaf HMs concentration", 8))






######Fig.2b-4 Stem concentration##########

d_s_hms <- read_xlsx("data-24-0227.xlsx", sheet = "stem_HMs_concentration")

#effect size
es_hms_stem <- escalc(measure = "ROM", 
                      m1i = SHMs_Xt,
                      sd1i = SHMs_SDt,
                      n1i = SHMs_Nt,
                      m2i = SHMs_Xc,
                      sd2i = SHMs_SDc,
                      n2i = SHMs_Nc,
                      data = d_s_hms)

#removed the NA in yi and vi
es_hms_stem <- drop_na(es_hms_stem, yi)



es_hms_stem$Study_ID <- factor(es_hms_stem$Study_ID)
es_hms_stem$HMs_type <- factor(es_hms_stem$HMs_type)
table(es_hms_stem$HMs_type)

da_hms_stem_Cd <- subset(es_hms_stem, HMs_type == "Cd") #349
da_hms_stem_Cu <- subset(es_hms_stem, HMs_type == "Cu") #57
da_hms_stem_Pb <- subset(es_hms_stem, HMs_type == "Pb") #175
da_hms_stem_Zn <- subset(es_hms_stem, HMs_type == "Zn") #60
da_hms_stem_As <- subset(es_hms_stem, HMs_type == "As") #22
da_hms_stem_Ni <- subset(es_hms_stem, HMs_type == "Ni") #6
da_hms_stem_Sb <- subset(es_hms_stem, HMs_type == "Sb") #14
da_hms_stem_Ti <- subset(es_hms_stem, HMs_type == "Ti") #6
da_hms_stem_Cr <- subset(es_hms_stem, HMs_type == "Cr") #6

##################effects of Cd on stem heavy metals concentration#########################
Cd_stem_hms_overall <- rma.mv(yi, vi, data = da_hms_stem_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_stem_hms_overall.n <- da_hms_stem_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_stem_hms_overall.R <- coef(summary(Cd_stem_hms_overall)) %>% mutate(HMs_type="Cd", size=(Cd_stem_hms_overall.n$n)) 


##################effects of Cu on stem heavy metals concentration#########################
Cu_stem_hms_overall <- rma.mv(yi, vi, data = da_hms_stem_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_stem_hms_overall.n <- da_hms_stem_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_stem_hms_overall.R <- coef(summary(Cu_stem_hms_overall)) %>% mutate(HMs_type="Cu", size=(Cu_stem_hms_overall.n$n)) 


##################effects of Pb on stem heavy metals concentration#########################
Pb_stem_hms_overall <- rma.mv(yi, vi, data = da_hms_stem_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_stem_hms_overall.n <- da_hms_stem_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_stem_hms_overall.R <- coef(summary(Pb_stem_hms_overall)) %>% mutate(HMs_type="Pb", size=(Pb_stem_hms_overall.n$n)) 

##################effects of Zn on stem heavy metals concentration#########################
Zn_stem_hms_overall <- rma.mv(yi, vi, data = da_hms_stem_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_stem_hms_overall.n <- da_hms_stem_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_stem_hms_overall.R <- coef(summary(Zn_stem_hms_overall)) %>% mutate(HMs_type="Zn", size=(Zn_stem_hms_overall.n$n)) 

##################effects of As on stem heavy metals concentration#########################
As_stem_hms_overall <- rma.mv(yi, vi, data = da_hms_stem_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_stem_hms_overall.n <- da_hms_stem_As %>%  group_by(HMs_type) %>% summarise(n = n())
As_stem_hms_overall.R <- coef(summary(As_stem_hms_overall)) %>% mutate(HMs_type="As", size=(As_stem_hms_overall.n$n)) 


##################effects of Ni on stem heavy metals concentration#########################
Ni_stem_hms_overall <- rma.mv(yi, vi, data = da_hms_stem_Ni, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Ni_stem_hms_overall.n <- da_hms_stem_Ni %>%  group_by(HMs_type) %>% summarise(n = n())
Ni_stem_hms_overall.R <- coef(summary(Ni_stem_hms_overall)) %>% mutate(HMs_type="Ni", size=(Ni_stem_hms_overall.n$n)) 


##################effects of Sb on stem heavy metals concentration#########################
Sb_stem_hms_overall <- rma.mv(yi, vi, data = da_hms_stem_Sb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Sb_stem_hms_overall.n <- da_hms_stem_Sb %>%  group_by(HMs_type) %>% summarise(n = n())
Sb_stem_hms_overall.R <- coef(summary(Sb_stem_hms_overall)) %>% mutate(HMs_type="Sb", size=(Sb_stem_hms_overall.n$n)) 


##################effects of Ti on stem heavy metals concentration#########################
Ti_stem_hms_overall <- rma.mv(yi, vi, data = da_hms_stem_Ti, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Ti_stem_hms_overall.n <- da_hms_stem_Ti %>%  group_by(HMs_type) %>% summarise(n = n())
Ti_stem_hms_overall.R <- coef(summary(Ti_stem_hms_overall)) %>% mutate(HMs_type="Ti", size=(Ti_stem_hms_overall.n$n)) 


##################effects of Cr on stem heavy metals concentration#########################
Cr_stem_hms_overall <- rma.mv(yi, vi, data = da_hms_stem_Cr, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cr_stem_hms_overall.n <- da_hms_stem_Cr %>%  group_by(HMs_type) %>% summarise(n = n())
Cr_stem_hms_overall.R <- coef(summary(Cr_stem_hms_overall)) %>% mutate(HMs_type="Cr", size=(Cr_stem_hms_overall.n$n)) 




############bind results for stem biomass############
dshms <- bind_rows(Cd_stem_hms_overall.R, Cu_stem_hms_overall.R, Pb_stem_hms_overall.R, Zn_stem_hms_overall.R, As_stem_hms_overall.R, Ni_stem_hms_overall.R, Sb_stem_hms_overall.R, Ti_stem_hms_overall.R, Cr_stem_hms_overall.R)
dshms$Parameter <- c(rep("Stem HMs concentration", 9))




######Fig.2b-5 Underground concentration##########
d_u_hms <- read_xlsx("data-24-0227.xlsx", sheet = "root_HMs_concentration")

#effect size
es_hms_under <- escalc(measure = "ROM", 
                      m1i = RHMs_Xt,
                      sd1i = RHMs_SDt,
                      n1i = RHMs_Nt,
                      m2i = RHMs_Xc,
                      sd2i = RHMs_SDc,
                      n2i = RHMs_Nc,
                      data = d_u_hms)


#removed the NA in yi and vi
es_hms_under <- drop_na(es_hms_under, yi)


es_hms_under$Study_ID <- factor(es_hms_under$Study_ID)
es_hms_under$HMs_type <- factor(es_hms_under$HMs_type)
table(es_hms_under$HMs_type)

da_hms_under_Cd <- subset(es_hms_under, HMs_type == "Cd") #468
da_hms_under_Cu <- subset(es_hms_under, HMs_type == "Cu") #114
da_hms_under_Pb <- subset(es_hms_under, HMs_type == "Pb") #213
da_hms_under_Zn <- subset(es_hms_under, HMs_type == "Zn") #124
da_hms_under_As <- subset(es_hms_under, HMs_type == "As") #32
da_hms_under_Sb <- subset(es_hms_under, HMs_type == "Sb") #14
da_hms_under_Ni <- subset(es_hms_under, HMs_type == "Ni") #9
da_hms_under_Cr <- subset(es_hms_under, HMs_type == "Cr") #6

##################effects of Cd on under heavy metals concentration#########################
Cd_under_hms_overall <- rma.mv(yi, vi, data = da_hms_under_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_under_hms_overall.n <- da_hms_under_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_under_hms_overall.R <- coef(summary(Cd_under_hms_overall)) %>% mutate(HMs_type="Cd", size=(Cd_under_hms_overall.n$n)) 


##################effects of Cu on under heavy metals concentration#########################
Cu_under_hms_overall <- rma.mv(yi, vi, data = da_hms_under_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_under_hms_overall.n <- da_hms_under_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_under_hms_overall.R <- coef(summary(Cu_under_hms_overall)) %>% mutate(HMs_type="Cu", size=(Cu_under_hms_overall.n$n)) 


##################effects of Pb on under heavy metals concentration#########################
Pb_under_hms_overall <- rma.mv(yi, vi, data = da_hms_under_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_under_hms_overall.n <- da_hms_under_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_under_hms_overall.R <- coef(summary(Pb_under_hms_overall)) %>% mutate(HMs_type="Pb", size=(Pb_under_hms_overall.n$n)) 

##################effects of Zn on under heavy metals concentration#########################
Zn_under_hms_overall <- rma.mv(yi, vi, data = da_hms_under_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_under_hms_overall.n <- da_hms_under_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_under_hms_overall.R <- coef(summary(Zn_under_hms_overall)) %>% mutate(HMs_type="Zn", size=(Zn_under_hms_overall.n$n)) 



##################effects of Sb on under heavy metals concentration#########################
Sb_under_hms_overall <- rma.mv(yi, vi, data = da_hms_under_Sb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Sb_under_hms_overall.n <- da_hms_under_Sb %>%  group_by(HMs_type) %>% summarise(n = n())
Sb_under_hms_overall.R <- coef(summary(Sb_under_hms_overall)) %>% mutate(HMs_type="Sb", size=(Sb_under_hms_overall.n$n)) 

##################effects of As on under heavy metals concentration#########################
As_under_hms_overall <- rma.mv(yi, vi, data = da_hms_under_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_under_hms_overall.n <- da_hms_under_As %>%  group_by(HMs_type) %>% summarise(n = n())
As_under_hms_overall.R <- coef(summary(As_under_hms_overall)) %>% mutate(HMs_type="As", size=(As_under_hms_overall.n$n)) 

##################effects of Ni on under heavy metals concentration#########################
Ni_under_hms_overall <- rma.mv(yi, vi, data = da_hms_under_Ni, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Ni_under_hms_overall.n <- da_hms_under_Ni %>%  group_by(HMs_type) %>% summarise(n = n())
Ni_under_hms_overall.R <- coef(summary(Ni_under_hms_overall)) %>% mutate(HMs_type="Ni", size=(Ni_under_hms_overall.n$n)) 


##################effects of Cr on under heavy metals concentration#########################
Cr_under_hms_overall <- rma.mv(yi, vi, data = da_hms_under_Cr, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cr_under_hms_overall.n <- da_hms_under_Cr %>%  group_by(HMs_type) %>% summarise(n = n())
Cr_under_hms_overall.R <- coef(summary(Cr_under_hms_overall)) %>% mutate(HMs_type="Cr", size=(Cr_under_hms_overall.n$n)) 




############bind results for heavy metals concentration############
duhms <- bind_rows(Cd_under_hms_overall.R, Cu_under_hms_overall.R, Pb_under_hms_overall.R, Zn_under_hms_overall.R, Sb_under_hms_overall.R,As_under_hms_overall.R,Ni_under_hms_overall.R, Cr_under_hms_overall.R)
duhms$Parameter <- c(rep("Underground HMs concentration", 8))


###bind results for Fig.1 total above under biomass##
d_hms <- bind_rows(dthms, dahms, duhms, dlhms, dshms)
options(scipen = 0) 

make_pct <- function(x) (exp(x) - 1) * 100

d_hms$estimate <- make_pct(d_hms$estimate)
d_hms$ci.lb <- make_pct(d_hms$ci.lb)
d_hms$ci.ub <- make_pct(d_hms$ci.ub)

d_hms$HMs_type <- factor(d_hms$HMs_type, levels = c("Cd", "Cu", "Pb", "Zn", "Sb", "As", "Ni", "Cr", "Ti"))
d_hms$Parameter <- factor(d_hms$Parameter, levels = c("Total HMs concentration", "Aboveground HMs concentration", "Underground HMs concentration", "Leaf HMs concentration", "Stem HMs concentration"))
d_hms$Sig <- ifelse(d_hms$pval < 0.001, "***", ifelse(d_hms$pval < 0.01, "**", ifelse(d_hms$pval < 0.05, "**", "")))


Fig.2 <- ggplot(d_hms,aes(x=estimate, y=HMs_type, color = Parameter))+
  geom_point(aes(),shape=16, size=5,  position = position_dodge(0.9))+
  geom_errorbar(aes(xmin = ci.lb, xmax =ci.ub),size=1,  width=0.2, position = position_dodge(0.9))+
  facet_wrap(~Parameter, scales = "free",ncol = 3, nrow = 2) + 
  xlab('Estimate (%)')+ ylab("Heavy metals type") +
  #scale_x_continuous(expand = c(0, 0),limits = c(-80, 350),breaks=seq(-80,350,50))+
  theme_bw()+
  geom_vline(aes(xintercept=0), linetype="dashed",color = "grey", linewidth=1.5)+
  theme(legend.position = "none")+
  theme(text = element_text(size =23), 
        axis.title.y=element_text(size=25)) +
  theme(axis.ticks.x=element_line(color="black",size=1,lineend = 10))+
  theme(axis.ticks.y=element_line(color="black",size=1,lineend = 10))+
  theme(axis.ticks.length=unit(.15, "cm"))+
  theme(axis.ticks.y=element_line(color="black",size=1,lineend=10))+
  geom_text(aes(y=HMs_type, x=ci.ub+10,label=paste0("(",size,")", Sig)),size=5, fontface="bold",position = position_dodge(0.9))
Fig.2

#ggsave("Figure S6 0625.pdf", Fig.2, height = 12, width = 14)



#########################Fig.1c Enzyme#######################

###############Fig.1c-1 SOD in leaf###########
d_l_sod <- read_xlsx("data-24-0227.xlsx", sheet = "SOD")

#effect size
es_sod_total <- escalc(measure = "ROM", 
                       m1i = SOD_Xt,
                       sd1i = SOD_SDt,
                       n1i = SOD_Nt,
                       m2i = SOD_Xc,
                       sd2i = SOD_SDc,
                       n2i = SOD_Nc,
                       data = d_l_sod)

#removed the NA in yi and vi
es_sod_total <- drop_na(es_sod_total, yi)



es_sod_total$Study_ID <- factor(es_sod_total$Study_ID)
es_sod_total$HMs_type <- factor(es_sod_total$HMs_type)
table(es_sod_total$HMs_type)

da_sod_leaf_Cd <- subset(es_sod_total, HMs_type == "Cd") #179
da_sod_leaf_Cu <- subset(es_sod_total, HMs_type == "Cu") #95
da_sod_leaf_Pb <- subset(es_sod_total, HMs_type == "Pb") #68
da_sod_leaf_Zn <- subset(es_sod_total, HMs_type == "Zn") #11
da_sod_leaf_As <- subset(es_sod_total, HMs_type == "As") #5

##################effects of Cd on leaf SOD#########################
Cd_leaf_sod_overall <- rma.mv(yi, vi, data = da_sod_leaf_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_leaf_sod_overall.n <- da_sod_leaf_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_leaf_sod_overall.R <- coef(summary(Cd_leaf_sod_overall)) %>% mutate(HMs_type="Cd", size=(Cd_leaf_sod_overall.n$n)) 


##################effects of Cu on leaf SOD#########################
Cu_leaf_sod_overall <- rma.mv(yi, vi, data = da_sod_leaf_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_leaf_sod_overall.n <- da_sod_leaf_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_leaf_sod_overall.R <- coef(summary(Cu_leaf_sod_overall)) %>% mutate(HMs_type="Cu", size=(Cu_leaf_sod_overall.n$n)) 


##################effects of Pb on leaf SOD#########################
Pb_leaf_sod_overall <- rma.mv(yi, vi, data = da_sod_leaf_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_leaf_sod_overall.n <- da_sod_leaf_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_leaf_sod_overall.R <- coef(summary(Pb_leaf_sod_overall)) %>% mutate(HMs_type="Pb", size=(Pb_leaf_sod_overall.n$n)) 

##################effects of Zn on leaf SOD#########################
Zn_leaf_sod_overall <- rma.mv(yi, vi, data = da_sod_leaf_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_leaf_sod_overall.n <- da_sod_leaf_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_leaf_sod_overall.R <- coef(summary(Zn_leaf_sod_overall)) %>% mutate(HMs_type="Zn", size=(Zn_leaf_sod_overall.n$n)) 


##################effects of Zn on leaf SOD#########################
As_leaf_sod_overall <- rma.mv(yi, vi, data = da_sod_leaf_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_leaf_sod_overall.n <- da_sod_leaf_As %>%  group_by(HMs_type) %>% summarise(n = n())
As_leaf_sod_overall.R <- coef(summary(As_leaf_sod_overall)) %>% mutate(HMs_type="As", size=(As_leaf_sod_overall.n$n)) 


############bind results for SOD############
dlsod <- bind_rows(Cd_leaf_sod_overall.R, Cu_leaf_sod_overall.R, Pb_leaf_sod_overall.R, Zn_leaf_sod_overall.R, As_leaf_sod_overall.R)
dlsod$Parameter <- c(rep("SOD", 5))



###############Fig.1c-2 POD in leaf###########
d_l_pod <- read_xlsx("data-24-0227.xlsx", sheet = "POD")


#effect size
es_pod_leaf <- escalc(measure = "ROM", 
                       m1i = POD_Xt,
                       sd1i = POD_SDt,
                       n1i = POD_Nt,
                       m2i = POD_Xc,
                       sd2i = POD_SDc,
                       n2i = POD_Nc,
                       data = d_l_pod)

#removed the NA in yi and vi
es_pod_leaf <- drop_na(es_pod_leaf, yi)


es_pod_leaf$Study_ID <- factor(es_pod_leaf$Study_ID)
es_pod_leaf$HMs_type <- factor(es_pod_leaf$HMs_type)
table(es_pod_leaf$HMs_type)

da_pod_leaf_Cd <- subset(es_pod_leaf, HMs_type == "Cd") #156
da_pod_leaf_Cu <- subset(es_pod_leaf, HMs_type == "Cu") #79 
da_pod_leaf_Pb <- subset(es_pod_leaf, HMs_type == "Pb") #48
da_pod_leaf_Zn <- subset(es_pod_leaf, HMs_type == "Zn") #11
da_pod_leaf_As <- subset(es_pod_leaf, HMs_type == "As") #5

##################effects of Cd on leaf pod#########################
Cd_leaf_pod_overall <- rma.mv(yi, vi, data = da_pod_leaf_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_leaf_pod_overall.n <- da_pod_leaf_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_leaf_pod_overall.R <- coef(summary(Cd_leaf_pod_overall)) %>% mutate(HMs_type="Cd", size=(Cd_leaf_pod_overall.n$n)) 


##################effects of Cu on leaf pod#########################
Cu_leaf_pod_overall <- rma.mv(yi, vi, data = da_pod_leaf_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_leaf_pod_overall.n <- da_pod_leaf_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_leaf_pod_overall.R <- coef(summary(Cu_leaf_pod_overall)) %>% mutate(HMs_type="Cu", size=(Cu_leaf_pod_overall.n$n)) 


##################effects of Pb on leaf pod#########################
Pb_leaf_pod_overall <- rma.mv(yi, vi, data = da_pod_leaf_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_leaf_pod_overall.n <- da_pod_leaf_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_leaf_pod_overall.R <- coef(summary(Pb_leaf_pod_overall)) %>% mutate(HMs_type="Pb", size=(Pb_leaf_pod_overall.n$n)) 

##################effects of Zn on leaf pod#########################
Zn_leaf_pod_overall <- rma.mv(yi, vi, data = da_pod_leaf_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_leaf_pod_overall.n <- da_pod_leaf_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_leaf_pod_overall.R <- coef(summary(Zn_leaf_pod_overall)) %>% mutate(HMs_type="Zn", size=(Zn_leaf_pod_overall.n$n)) 

##################effects of As on leaf pod#########################
As_leaf_pod_overall <- rma.mv(yi, vi, data = da_pod_leaf_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_leaf_pod_overall.n <- da_pod_leaf_As %>%  group_by(HMs_type) %>% summarise(n = n())
As_leaf_pod_overall.R <- coef(summary(As_leaf_pod_overall)) %>% mutate(HMs_type="As", size=(As_leaf_pod_overall.n$n)) 



############bind results for POD############
dlpod <- bind_rows(Cd_leaf_pod_overall.R, Cu_leaf_pod_overall.R, Pb_leaf_pod_overall.R, Zn_leaf_pod_overall.R, As_leaf_pod_overall.R)
dlpod$Parameter <- c(rep("POD", 5))



###############Fig.1c-3 CAT in leaf###########
d_l_cat <- read_xlsx("data-24-0227.xlsx", sheet = "CAT")


#effect size
es_cat_leaf <- escalc(measure = "ROM", 
                       m1i = CAT_Xt,
                       sd1i = CAT_SDt,
                       n1i = CAT_Nt,
                       m2i = CAT_Xc,
                       sd2i = CAT_SDc,
                       n2i = CAT_Nc,
                       data = d_l_cat)

#removed the NA in yi and vi
es_cat_leaf <- drop_na(es_cat_leaf, yi)


es_cat_leaf$Study_ID <- factor(es_cat_leaf$Study_ID)
es_cat_leaf$HMs_type <- factor(es_cat_leaf$HMs_type)
table(es_cat_leaf$HMs_type)

da_cat_leaf_Cd <- subset(es_cat_leaf, HMs_type == "Cd") #162
da_cat_leaf_Cu <- subset(es_cat_leaf, HMs_type == "Cu") #89
da_cat_leaf_Pb <- subset(es_cat_leaf, HMs_type == "Pb") #25
da_cat_leaf_Zn <- subset(es_cat_leaf, HMs_type == "Zn") #11
da_cat_leaf_As <- subset(es_cat_leaf, HMs_type == "As") #5

##################effects of Cd on leaf cat#########################
Cd_leaf_cat_overall <- rma.mv(yi, vi, data = da_cat_leaf_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_leaf_cat_overall.n <- da_cat_leaf_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_leaf_cat_overall.R <- coef(summary(Cd_leaf_cat_overall)) %>% mutate(HMs_type="Cd", size=(Cd_leaf_cat_overall.n$n)) 


##################effects of Cu on leaf cat#########################
Cu_leaf_cat_overall <- rma.mv(yi, vi, data = da_cat_leaf_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_leaf_cat_overall.n <- da_cat_leaf_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_leaf_cat_overall.R <- coef(summary(Cu_leaf_cat_overall)) %>% mutate(HMs_type="Cu", size=(Cu_leaf_cat_overall.n$n)) 


##################effects of Pb on leaf cat#########################
Pb_leaf_cat_overall <- rma.mv(yi, vi, data = da_cat_leaf_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_leaf_cat_overall.n <- da_cat_leaf_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_leaf_cat_overall.R <- coef(summary(Pb_leaf_cat_overall)) %>% mutate(HMs_type="Pb", size=(Pb_leaf_cat_overall.n$n)) 

##################effects of Zn on leaf cat#########################
Zn_leaf_cat_overall <- rma.mv(yi, vi, data = da_cat_leaf_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_leaf_cat_overall.n <- da_cat_leaf_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_leaf_cat_overall.R <- coef(summary(Zn_leaf_cat_overall)) %>% mutate(HMs_type="Zn", size=(Zn_leaf_cat_overall.n$n)) 

##################effects of As on leaf cat#########################
As_leaf_cat_overall <- rma.mv(yi, vi, data = da_cat_leaf_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_leaf_cat_overall.n <- da_cat_leaf_As %>%  group_by(HMs_type) %>% summarise(n = n())
As_leaf_cat_overall.R <- coef(summary(As_leaf_cat_overall)) %>% mutate(HMs_type="As", size=(As_leaf_cat_overall.n$n)) 




############bind results for CAT############
dlcat <- bind_rows(Cd_leaf_cat_overall.R, Cu_leaf_cat_overall.R, Pb_leaf_cat_overall.R, Zn_leaf_cat_overall.R, As_leaf_cat_overall.R)
dlcat$Parameter <- c(rep("CAT", 5))




###############Fig.1c-4 MDA in leaf###########
d_l_mda <- read_xlsx("data-24-0227.xlsx", sheet = "MDA")

#effect size
es_mda_leaf <- escalc(measure = "ROM", 
                       m1i = MDA_Xt,
                       sd1i = MDA_SDt,
                       n1i = MDA_Nt,
                       m2i = MDA_Xc,
                       sd2i = MDA_SDc,
                       n2i = MDA_Nc,
                       data = d_l_mda)

#removed the NA in yi and vi
es_mda_leaf <- drop_na(es_mda_leaf, yi)


es_mda_leaf$Study_ID <- factor(es_mda_leaf$Study_ID)
es_mda_leaf$HMs_type <- factor(es_mda_leaf$HMs_type)
table(es_mda_leaf$HMs_type)

da_mda_leaf_Cd <- subset(es_mda_leaf, HMs_type == "Cd") #173
da_mda_leaf_Cu <- subset(es_mda_leaf, HMs_type == "Cu") #59
da_mda_leaf_Pb <- subset(es_mda_leaf, HMs_type == "Pb") #58
da_mda_leaf_Zn <- subset(es_mda_leaf, HMs_type == "Zn") #11
da_mda_leaf_As <- subset(es_mda_leaf, HMs_type == "As") #4

##################effects of Cd on leaf mda#########################
Cd_leaf_mda_overall <- rma.mv(yi, vi, data = da_mda_leaf_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_leaf_mda_overall.n <- da_mda_leaf_Cd %>%  group_by(HMs_type) %>% summarise(n = n())
Cd_leaf_mda_overall.R <- coef(summary(Cd_leaf_mda_overall)) %>% mutate(HMs_type="Cd", size=(Cd_leaf_mda_overall.n$n)) 


##################effects of Cu on leaf mda#########################
Cu_leaf_mda_overall <- rma.mv(yi, vi, data = da_mda_leaf_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cu_leaf_mda_overall.n <- da_mda_leaf_Cu %>%  group_by(HMs_type) %>% summarise(n = n())
Cu_leaf_mda_overall.R <- coef(summary(Cu_leaf_mda_overall)) %>% mutate(HMs_type="Cu", size=(Cu_leaf_mda_overall.n$n)) 


##################effects of Pb on leaf mda#########################
Pb_leaf_mda_overall <- rma.mv(yi, vi, data = da_mda_leaf_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_leaf_mda_overall.n <- da_mda_leaf_Pb %>%  group_by(HMs_type) %>% summarise(n = n())
Pb_leaf_mda_overall.R <- coef(summary(Pb_leaf_mda_overall)) %>% mutate(HMs_type="Pb", size=(Pb_leaf_mda_overall.n$n)) 

##################effects of Zn on leaf mda#########################
Zn_leaf_mda_overall <- rma.mv(yi, vi, data = da_mda_leaf_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_leaf_mda_overall.n <- da_mda_leaf_Zn %>%  group_by(HMs_type) %>% summarise(n = n())
Zn_leaf_mda_overall.R <- coef(summary(Zn_leaf_mda_overall)) %>% mutate(HMs_type="Zn", size=(Zn_leaf_mda_overall.n$n)) 

##################effects of As on leaf mda#########################
As_leaf_mda_overall <- rma.mv(yi, vi, data = da_mda_leaf_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_leaf_mda_overall.n <- da_mda_leaf_As %>%  group_by(HMs_type) %>% summarise(n = n())
As_leaf_mda_overall.R <- coef(summary(As_leaf_mda_overall)) %>% mutate(HMs_type="As", size=(As_leaf_mda_overall.n$n)) 


############bind results for MDA############
dlmda <- bind_rows(Cd_leaf_mda_overall.R, Cu_leaf_mda_overall.R, Pb_leaf_mda_overall.R, Zn_leaf_mda_overall.R, As_leaf_mda_overall.R )
dlmda$Parameter <- c(rep("MDA", 5))


###bind results for Fig.1 total above under biomass##
d_enz <- bind_rows(dlsod, dlpod, dlcat, dlmda)


make_pct <- function(x) (exp(x) - 1) * 100

d_enz$estimate <- make_pct(d_enz$estimate)
d_enz$ci.lb <- make_pct(d_enz$ci.lb)
d_enz$ci.ub <- make_pct(d_enz$ci.ub)

d_enz$HMs_type <- factor(d_enz$HMs_type, levels = c("Cd", "Cu", "Pb", "Zn", "As"))
d_enz$Sig <- ifelse(d_enz$pval < 0.001, "***", ifelse(d_enz$pval < 0.01, "**", ifelse(d_enz$pval < 0.05, "**", "")))


d_enz3 <- subset(d_enz, Parameter!= "MDA")
d_mda <- subset(d_enz, Parameter == "MDA")


d_enz3$Parameter <- factor(d_enz3$Parameter, levels = c("SOD", "POD", "CAT"))


Fig.S11MDA <- ggplot(d_mda,aes(x=estimate, y=HMs_type, color = Parameter))+
  geom_point(aes(),shape=16, size=5,  position = position_dodge(0.9))+
  geom_errorbar(aes(xmin = ci.lb, xmax =ci.ub),size=1,  width=0.2, position = position_dodge(0.9))+
  facet_wrap(~Parameter, scales = "free",ncol = 2, nrow = 2) + 
  xlab('Estimate (%)')+ ylab("Heavy metals type") +
  #scale_x_continuous(expand = c(0, 0),limits = c(-80, 350),breaks=seq(-80,350,50))+
  theme_bw()+
  geom_vline(aes(xintercept=0), linetype="dashed",color = "grey", linewidth=1.5)+
  theme(legend.position = "none")+
  theme(text = element_text(size =23), 
        axis.title.y=element_text(size=25)) +
  theme(axis.ticks.x=element_line(color="black",size=1,lineend = 10))+
  theme(axis.ticks.y=element_line(color="black",size=1,lineend = 10))+
  theme(axis.ticks.length=unit(.15, "cm"))+
  theme(axis.ticks.y=element_line(color="black",size=1,lineend=10))+
  geom_text(aes(y=HMs_type, x=ci.ub+25,label=paste0("(",size,")", Sig)),size=5, fontface="bold",position = position_dodge(0.9))
Fig.S11MDA

#ggsave("Figure S11 0625.pdf", Fig.S11MDA, height = 6, width = 5)


Fig.S12Enz <- ggplot(d_enz3,aes(x=estimate, y=HMs_type, color = Parameter))+
  geom_point(aes(),shape=16, size=5,  position = position_dodge(0.9))+
  geom_errorbar(aes(xmin = ci.lb, xmax =ci.ub),size=1,  width=0.2, position = position_dodge(0.9))+
  facet_wrap(~Parameter, scales = "free",ncol = 2, nrow = 2) + 
  xlab('Estimate (%)')+ ylab("Heavy metals type") +
  #scale_x_continuous(expand = c(0, 0),limits = c(-80, 350),breaks=seq(-80,350,50))+
  theme_bw()+
  geom_vline(aes(xintercept=0), linetype="dashed",color = "grey", linewidth=1.5)+
  theme(legend.position = "none")+
  theme(text = element_text(size =23), 
        axis.title.y=element_text(size=25)) +
  theme(axis.ticks.x=element_line(color="black",size=1,lineend = 10))+
  theme(axis.ticks.y=element_line(color="black",size=1,lineend = 10))+
  theme(axis.ticks.length=unit(.15, "cm"))+
  theme(axis.ticks.y=element_line(color="black",size=1,lineend=10))+
  geom_text(aes(y=HMs_type, x=ci.ub+25,label=paste0("(",size,")", Sig)),size=5, fontface="bold",position = position_dodge(0.9))
Fig.S12Enz

#ggsave("Figure S12 0625.pdf", Fig.S12Enz, height = 10, width = 8)




#########################Fig.4 Genus Biomass (subgroup)#######################
#####Fig.2a genus Total biomass 
es_total_biomass$Study_ID <- factor(es_total_biomass$Study_ID)
es_total_biomass$HMs_type <- factor(es_total_biomass$HMs_type)
es_total_biomass$Genus <- factor(es_total_biomass$Genus)
table(es_total_biomass$HMs_type)
table(es_total_biomass$Genus)

genus_counts.tb <- table(es_total_biomass$Genus)
selected_genus.tb <- names(genus_counts.tb[genus_counts.tb >= 3])
selected.tb <- es_total_biomass[es_total_biomass$Genus %in% selected_genus.tb, ]
table(selected.tb$HMs_type)

da_selected_Cd <- subset(selected.tb, HMs_type == "Cd") #228
da_selected_Cu <- subset(selected.tb, HMs_type == "Cu") #60
da_selected_Pb <- subset(selected.tb, HMs_type == "Pb") #161
da_selected_Zn <- subset(selected.tb, HMs_type == "Zn") #29
da_selected_Sb <- subset(selected.tb, HMs_type == "Sb") #16
da_selected_As <- subset(selected.tb, HMs_type == "As") #8

#Cd
counts_Cd_tb <- table(da_selected_Cd$Genus)
da_selected_Cd.tb <- names(counts_Cd_tb[counts_Cd_tb >= 3])
da_selected_Cd <- da_selected_Cd[da_selected_Cd$Genus %in% da_selected_Cd.tb, ]
#Cu
counts_Cu_tb <- table(da_selected_Cu$Genus)
da_selected_Cu.tb <- names(counts_Cu_tb[counts_Cu_tb >= 3])
da_selected_Cu <- da_selected_Cu[da_selected_Cu$Genus %in% da_selected_Cu.tb, ]

#Pb
counts_Pb_tb <- table(da_selected_Pb$Genus)
da_selected_Pb.tb <- names(counts_Pb_tb[counts_Pb_tb >= 3])
da_selected_Pb <- da_selected_Pb[da_selected_Pb$Genus %in% da_selected_Pb.tb, ]

#Zn
counts_Zn_tb <- table(da_selected_Zn$Genus)
da_selected_Zn.tb <- names(counts_Zn_tb[counts_Zn_tb >= 3])
da_selected_Zn <- da_selected_Zn[da_selected_Zn$Genus %in% da_selected_Zn.tb, ]

#Sb
counts_Sb_tb <- table(da_selected_Sb$Genus)
da_selected_Sb.tb <- names(counts_Sb_tb[counts_Sb_tb >= 3])
da_selected_Sb <- da_selected_Sb[da_selected_Sb$Genus %in% da_selected_Sb.tb, ]

#As
counts_As_tb <- table(da_selected_As$Genus)
da_selected_As.tb <- names(counts_As_tb[counts_As_tb >= 2])
da_selected_As <- da_selected_As[da_selected_As$Genus %in% da_selected_As.tb, ]



##################effects of Cd on Total biomass#########################
Cd_tb_overall_selected <- rma.mv(yi, vi, data = da_selected_Cd, mods= ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Cd_tb_overall_selected.n <- da_selected_Cd %>%  group_by(Genus) %>% summarise(n = n())
Cd_tb_overall_selected.R <- coef(summary(Cd_tb_overall_selected)) %>% mutate(HMs_type="Cd", Genus = row.names(coef(summary(Cd_tb_overall_selected))), size=(Cd_tb_overall_selected.n$n)) 

##Qm and p value
Cd_tb_overall_Qm_selected <- rma.mv(yi, vi, data = da_selected_Cd, mods= ~ Genus, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
summary(Cd_tb_overall_Qm_selected)


##################effects of Cu on Total biomass#########################
Cu_tb_overall_selected <- rma.mv(yi, vi, data = da_selected_Cu, mods= ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
table(da_selected_Cu$Genus)

Cu_tb_overall_selected.n <- da_selected_Cu %>%  group_by(Genus) %>% summarise(n = n())
Cu_tb_overall_selected.n <- Cu_tb_overall_selected.n[-c(3),]
Cu_tb_overall_selected.R <- coef(summary(Cu_tb_overall_selected)) %>% mutate(HMs_type="Cu", Genus = row.names(coef(summary(Cu_tb_overall_selected))), size=(Cu_tb_overall_selected.n$n)) 

##Qm and p value
Cu_tb_overall_Qm_selected <- rma.mv(yi, vi, data = da_selected_Cu, mods= ~ Genus, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
summary(Cu_tb_overall_Qm_selected)


##################effects of Pb on Total biomass#########################
Pb_tb_overall_selected <- rma.mv(yi, vi, data = da_selected_Pb, mods= ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Pb_tb_overall_selected.n <- da_selected_Pb %>%  group_by(Genus) %>% summarise(n = n())
Pb_tb_overall_selected.R <- coef(summary(Pb_tb_overall_selected)) %>% mutate(HMs_type="Pb", Genus = row.names(coef(summary(Pb_tb_overall_selected))), size=(Pb_tb_overall_selected.n$n)) 

##Qm and p value
Pb_tb_overall_Qm_selected <- rma.mv(yi, vi, data = da_selected_Pb, mods= ~ Genus, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
summary(Pb_tb_overall_Qm_selected)



##################effects of Zn on Total biomass#########################
Zn_tb_overall_selected <- rma.mv(yi, vi, data = da_selected_Zn, mods= ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Zn_tb_overall_selected.n <- da_selected_Zn %>%  group_by(Genus) %>% summarise(n = n())
Zn_tb_overall_selected.n <- Zn_tb_overall_selected.n[-c(4),]
Zn_tb_overall_selected.R <- coef(summary(Zn_tb_overall_selected)) %>% mutate(HMs_type="Zn",Genus = row.names(coef(summary(Zn_tb_overall_selected))), size=(Zn_tb_overall_selected.n$n)) 

##Qm and p value
Zn_tb_overall_Qm_selected <- rma.mv(yi, vi, data = da_selected_Zn, mods= ~ Genus, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
summary(Zn_tb_overall_Qm_selected)



##################effects of Sb on Total biomass#########################
Sb_tb_overall_selected <- rma.mv(yi, vi, data = da_selected_Sb, mods= ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


Sb_tb_overall_selected.n <- da_selected_Sb %>%  group_by(Genus) %>% summarise(n = n())
Sb_tb_overall_selected.R <- coef(summary(Sb_tb_overall_selected)) %>% mutate(HMs_type="Sb",Genus = row.names(coef(summary(Sb_tb_overall_selected))), size=(Sb_tb_overall_selected.n$n)) 

##Qm and p value
Sb_tb_overall_Qm_selected <- rma.mv(yi, vi, data = da_selected_Sb, mods= ~ Genus, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
summary(Sb_tb_overall_Qm_selected)


##################effects of As on Total biomass#########################
As_tb_overall_selected <- rma.mv(yi, vi, data = da_selected_As, mods= ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


As_tb_overall_selected.n <- da_selected_As %>%  group_by(Genus) %>% summarise(n = n())
As_tb_overall_selected.R <- coef(summary(As_tb_overall_selected)) %>% mutate(HMs_type="As",Genus = row.names(coef(summary(As_tb_overall_selected))), size=(As_tb_overall_selected.n$n)) 

##Qm and p value
As_tb_overall_Qm_selected <- rma.mv(yi, vi, data = da_selected_As, mods= ~ Genus, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
summary(As_tb_overall_Qm_selected)


############bind results for Total biomass############
dtb_selected <- bind_rows(Cd_tb_overall_selected.R, Cu_tb_overall_selected.R, Pb_tb_overall_selected.R, Zn_tb_overall_selected.R, Sb_tb_overall_selected.R, As_tb_overall_selected.R)

make_pct <- function(x) (exp(x) - 1) * 100

dtb_selected$estimate <- make_pct(dtb_selected$estimate)
dtb_selected$ci.lb <- make_pct(dtb_selected$ci.lb)
dtb_selected$ci.ub <- make_pct(dtb_selected$ci.ub)

dtb_selected$HMs_type <- factor(dtb_selected$HMs_type, levels = c("Cd", "Cu", "Pb", "Zn", "Sb", "As"))
dtb_selected$Genus = gsub("Genus","",dtb_selected$Genus)
dtb_selected$Sig <- ifelse(dtb_selected$pval < 0.001, "***", ifelse(dtb_selected$pval < 0.01, "**", ifelse(dtb_selected$pval < 0.05, "*", "")))

Fig.4 <- ggplot(dtb_selected,aes(x=estimate,y=Genus, color = HMs_type))+
  geom_point(aes(),shape=16, size=5,  position = position_dodge(0.9))+
  geom_errorbar(aes(xmin = ci.lb, xmax =ci.ub),size=1,  width=0.2, position = position_dodge(0.9))+
  theme_bw()+
  facet_wrap(~HMs_type, scales = "free",ncol = 3, nrow = 2) +
  xlab('Total biomass (%)')+ ylab("Genus")+ 
  scale_x_continuous(expand = c(0, 0),limits = c(-80, 130),breaks=seq(-80,130,40))+
  geom_vline(aes(xintercept=0), linetype="dashed",color = "grey", linewidth=1.5)+
  theme(legend.position = "none")+
  theme(text = element_text(size =23), 
        axis.title.y=element_text(size=25)) +
  theme(axis.ticks.x=element_line(color="black",size=0.8,lineend = 10))+
  theme(axis.ticks.y=element_line(color="black",size=0.8,lineend = 10))+
  theme(axis.ticks.length=unit(.15, "cm"))+
  theme(axis.ticks.y=element_line(color="black",size=0.8,lineend=10),
        axis.text.y = element_text(angle = 0,vjust = 0.85,hjust = 0.75, face = "italic"))+
  geom_text(aes(y=Genus, x=ci.ub+16,label=paste0("(",size,")", Sig)),size=5, fontface="bold",position = position_dodge(0.9))
Fig.4

#ggsave("Figure 3 0625.pdf", Fig.4, width = 18, height = 15)






#########################Fig.5 Genus species#######################

########################### TI (tolerance index ) ####################################
d_ti <- read_xlsx("data-24-0227.xlsx", sheet = "Index")
str(d_ti)
d_ti <- drop_na(d_ti, TI)
d_ti$EMS <- ifelse(d_ti$Experimental_method == "Hydroponics", "Hydroponics", "Soil/Steril_medium")

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))


### EMS
Ti <- ggplot(d_ti, aes(x = reorder(Genus, -TI), y = TI, fill = EMS)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5) +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun.y ="mean", geom="point", shape=20, size=2.5, color="red",alpha=1, position = position_dodge2(width = 0.75, preserve = "single"))+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Tolerance index', fill = 'Type', color = '') +
  scale_y_continuous(limits = c(0,2), breaks=seq(0,2, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = c(0.85, 0.8))+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.6, linetype = 'dashed', col = 'red', size = 0.8)+
  geom_hline(yintercept=0.35, linetype = 'dashed', col = 'black', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 42 , y = 1.07,label = "TI = 1.0", colour="blue", size = 6)+
  annotate("text", x = 4 , y = 0.67,label = "TI = 0.60", colour="red", size = 6)+
  annotate("text", x = 4 , y = 0.42,label = "TI = 0.35",colour="black", size = 6) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

Ti


### HMs_type
Ti_hms <- ggplot(d_ti, aes(x = reorder(Genus, -TI), y = TI, fill = HMs_type)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5) +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun.y ="mean", geom="point", shape=20, size=2.5, color="red",alpha=1, position = position_dodge2(width = 0.85, preserve = "single"))+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Tolerance index', fill = 'HMs type', color = '') +
  scale_y_continuous(limits = c(0,2), breaks=seq(0,2, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = c(0.70, 0.75))+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.6, linetype = 'dashed', col = 'red', size = 0.8)+
  geom_hline(yintercept=0.35, linetype = 'dashed', col = 'black', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 42 , y = 1.07,label = "TI = 1.0", colour="blue", size = 6)+
  annotate("text", x = 4 , y = 0.67,label = "TI = 0.60", colour="red", size = 6)+
  annotate("text", x = 4 , y = 0.42,label = "TI = 0.35",colour="black", size = 6) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

Ti_hms



#####################################   TF   ###################################
d_tf <- read_xlsx("data-24-0227.xlsx", sheet = "Index")
d_tf$EMS <- ifelse(d_tf$Experimental_method == "Hydroponics", "Hydroponics", "Soil/Steril_medium")
d_tf <- drop_na(d_tf, TF)


font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

### EMS
TF <- ggplot(d_tf, aes(x = reorder(Genus, -TF), y = TF, fill = EMS)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5) +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun.y ="mean", geom="point", shape=20, size=2.5, color="red",alpha=1, position = position_dodge2(width = 0.75, preserve = "single"))+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Transportion factor', fill = 'Type', color = '') +
  scale_y_continuous(limits = c(0, 1.8), breaks=seq(0, 1.8, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = c(0.85, 0.8))+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.5, linetype = 'dashed', col = 'gray', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 10 , y = 1.09,label = "TF = 1.0", colour="blue", size = 6)+
  annotate("text", x = 10 , y = 0.6,label = "TF = 0.5", colour="black", size = 6) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

TF


### HMs
TF_hms <- ggplot(d_tf, aes(x = reorder(Genus, -TF), y = TF, fill = HMs_type)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5) +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun.y ="mean", geom="point", shape=20, size=2.5, color="red",alpha=1, position = position_dodge2(width = 0.75, preserve = "single"))+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Transportion factor', fill = 'HMs type', color = '') +
  scale_y_continuous(limits = c(0, 1.8), breaks=seq(0, 1.8, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = c(0.85, 0.8))+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.5, linetype = 'dashed', col = 'gray', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 15 , y = 1.09,label = "TF = 1.0", colour="blue", size = 6)+
  annotate("text", x = 15 , y = 0.6,label = "TF = 0.5", colour="black", size = 6) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

TF_hms

ti_tf <- plot_grid(Ti_hms, TF_hms, ncol = 1, labels = c("(a)", "(b)"), label_size = 20, label_x = -0.02)

#ggsave("Figure S8 0625.pdf", ti_tf, width = 12, height = 13)

########################### TBCF ####################################
d_tbcf <- read_xlsx("data-24-0227.xlsx", sheet = "Index")
d_tbcf$EMS <- ifelse(d_tbcf$Experimental_method == "Hydroponics", "Hydroponics", "Soil/Steril_medium")
d_tbcf <- drop_na(d_tbcf, TBCF)
d_tbcf_soil <- subset(d_tbcf, EMS == "Soil/Steril_medium")

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#### HMs_type
TBCF_hms <- ggplot(d_tbcf_soil, aes(x = reorder(Genus, -TBCF), y = TBCF, fill = HMs_type)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5) +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun.y ="mean", geom="point", shape=20, size=2.5, color="red",alpha=1, position = position_dodge2(width = 0.75, preserve = "single"))+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Bioconcentration factor', fill = 'HMs type', color = '') +
  scale_y_continuous(limits = c(0, 1.8), breaks=seq(0, 1.8, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = c(0.8, 0.8))+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.5, linetype = 'dashed', col = 'gray', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 6 , y = 1.09,label = "TBCF = 1.0", colour="blue", size = 6)+
  annotate("text", x = 6 , y = 0.6,label = "TBCF = 0.5", colour="gray10", size = 6) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

TBCF_hms


fac1 <- plot_grid(TBCF_hms, TF, ncol = 2,rel_widths = c(1,2), labels = c("(b)", "(c)"), label_x = -0.033, label_size = 20)

fac2 <- plot_grid(Ti, fac1, ncol = 1, labels = c("(a)"), label_size = 20)
fac2

#ggsave("Figure 5 0625.pdf", fac2, width = 12, height = 10)





########################### ABCF ####################################
d_abcf <- read_xlsx("data-24-0227.xlsx", sheet = "Index")
str(d_abcf)
d_abcf <- drop_na(d_abcf, ABCF)
d_abcf$EMS <- ifelse(d_abcf$Experimental_method == "Hydroponics", "Hydroponics", "Soil/Steril_medium")
d_abcf_soil <- subset(d_abcf, EMS == "Soil/Steril_medium")


font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

ABCF <- ggplot(d_abcf_soil, aes(x = reorder(Genus, -ABCF), y = ABCF, fill = HMs_type)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5) +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun.y ="mean", geom="point", shape=20, size=2.5, color="red",alpha=1, position = position_dodge2(width = 0.75, preserve = "single"))+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Bioconcentration factor', fill = 'HMs type', color = '') +
  scale_y_continuous(limits = c(0, 1.8), breaks=seq(0, 1.8, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = c(0.8, 0.8))+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.5, linetype = 'dashed', col = 'gray', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 7 , y = 1.09,label = "ABCF = 1.0", colour="blue", size = 6)+
  annotate("text", x = 7 , y = 0.6,label = "ABCF = 0.5", colour="black", size = 6) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

ABCF
#ggsave("Figure S9 0625.pdf", ABCF, width = 8, height = 7)


########################### LBCF ####################################
d_lbcf <- read_xlsx("data-24-0227.xlsx", sheet = "Index")
str(d_lbcf)
d_lbcf <- drop_na(d_lbcf, LBCF)
d_lbcf$EMS <- ifelse(d_lbcf$Experimental_method == "Hydroponics", "Hydroponics", "Soil/Steril_medium")
d_lbcf_soil <- subset(d_lbcf, EMS == "Soil/Steril_medium")


font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

LBCF <- ggplot(d_lbcf_soil, aes(x = reorder(Genus, -LBCF), y = LBCF, fill = HMs_type)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5) +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun.y ="mean", geom="point", shape=20, size=2.5, color="red",alpha=1, position = position_dodge2(width = 0.75, preserve = "single"))+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Bioconcentration factor', fill = 'HMs type', color = '') +
  scale_y_continuous(limits = c(0, 18), breaks=seq(0, 18, by=5))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = c(0.85, 0.7))+
  geom_hline(yintercept=5, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'gray', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 45 , y = 6,label = "LBCF = 5", colour="blue", size = 6)+
  annotate("text", x = 45 , y = 2,label = "LBCF = 1", colour="black", size = 6) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

LBCF



########################### SBCF ####################################
d_sbcf <- read_xlsx("data-24-0227.xlsx", sheet = "Index")
str(d_sbcf)
d_sbcf <- drop_na(d_sbcf, SBCF)
d_sbcf$EMS <- ifelse(d_sbcf$Experimental_method == "Hydroponics", "Hydroponics", "Soil/Steril_medium")
d_sbcf_soil <- subset(d_sbcf, EMS == "Soil/Steril_medium")


font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

SBCF <- ggplot(d_sbcf_soil, aes(x = reorder(Genus, -SBCF), y = SBCF, fill = HMs_type)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5) +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun.y ="mean", geom="point", shape=20, size=2.5, color="red",alpha=1, position = position_dodge2(width = 0.75, preserve = "single"))+
  theme_bw()+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Bioconcentration factor', fill = 'HMs type', color = '') +
  scale_y_continuous(limits = c(0, 1.8), breaks=seq(0, 1.8, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = c(0.85, 0.78))+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.5, linetype = 'dashed', col = 'gray', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 45 , y = 1.09,label = "SBCF = 1.0", colour="blue", size = 6)+
  annotate("text", x = 45 , y = 0.6,label = "SBCF = 0.5", colour="black", size = 6) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

SBCF

index <- plot_grid(LBCF, SBCF, ncol = 1, labels = c("(a)", "(b)"), label_size = 20)
index
#ggsave("Figure S10 0625.pdf", index, width = 15, height = 15)



################## Fig.7  Subgroup analysis #########################


# 1、Experimental_method 
# 2、Functional_group
# 3、Stress_time
# 4、Tree_age
# 5、pH
# 6、HMs_level
# 7、Plant_method



####   Total biomass  ####

#removed the NA in yi and vi
es_total_biomass <- drop_na(es_total_biomass, yi)


# 1、Experimental_method
res_meta_tb_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_em_n <- es_total_biomass %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_tb_em_reml <- mod_results(res_meta_tb_em, mod = "Experimental_method", group = "Study_ID", data = es_total_biomass)
res_meta_tb_em_reml <- coef(summary(res_meta_tb_em)) %>% mutate(factor = levels(as.factor(es_total_biomass$Experimental_method)), Index = "Total biomass", size = (res_meta_tb_em_n$n))

# 2、Functional group
res_meta_tb_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_fg_n <- es_total_biomass %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_tb_fg_reml <- mod_results(res_meta_tb_fg, mod = "Functional_group", group = "Study_ID", data = es_total_biomass)
res_meta_tb_fg_reml <- coef(summary(res_meta_tb_fg)) %>% mutate(factor = levels(as.factor(es_total_biomass$Functional_group)), Index = "Total biomass", size = (res_meta_tb_fg_n$n))

# 3、stress_time
res_meta_tb_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_st_n <- es_total_biomass %>% group_by(Stress_time) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_tb_st_reml <- mod_results(res_meta_tb_st, mod = "Stress_time", group = "Study_ID", data = es_total_biomass)
res_meta_tb_st_reml <- coef(summary(res_meta_tb_st)) %>% mutate(factor = levels(as.factor(es_total_biomass$Stress_time)), Index = "Total biomass", size = (res_meta_tb_st_n$n))


# 4、Tree_age
res_meta_tb_ta <- rma.mv(yi, vi, mod = ~ Tree_age, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_ta <- rma.mv(yi, vi, mod = ~ Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_ta_n <- es_total_biomass %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_tb_ta_reml <- mod_results(res_meta_tb_ta, mod = "Tree_age", group = "Study_ID", data = es_total_biomass)
res_meta_tb_ta_reml <- coef(summary(res_meta_tb_ta)) %>% mutate(factor = levels(as.factor(es_total_biomass$Tree_age)), Index = "Total biomass", size = (res_meta_tb_ta_n$n))


# 5、pH
res_meta_tb_ph <- rma.mv(yi, vi, mod = ~ pH, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_ph <- rma.mv(yi, vi, mod = ~ pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_ph_n <- es_total_biomass %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_tb_ph_reml <- mod_results(res_meta_tb_ph, mod = "pH", group = "Study_ID", data = es_total_biomass)
res_meta_tb_ph_reml <- coef(summary(res_meta_tb_ph)) %>% mutate(factor = levels(as.factor(es_total_biomass$pH)), Index = "Total biomass", size = (res_meta_tb_ph_n$n))


# 6、HMs_level
res_meta_tb_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_hms_n <- es_total_biomass %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_tb_hms_reml <- mod_results(res_meta_tb_hms, mod = "HMs_level", group = "Study_ID", data = es_total_biomass)
res_meta_tb_hms_reml <- coef(summary(res_meta_tb_hms)) %>% mutate(factor = levels(as.factor(es_total_biomass$HMs_level)), Index = "Total biomass", size = (res_meta_tb_hms_n$n))


# 7、Plant_method
res_meta_tb_pm <- rma.mv(yi, vi, mod = ~ Plant_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_pm <- rma.mv(yi, vi, mod = ~ Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_total_biomass)
res_meta_tb_pm_n <- es_total_biomass %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_tb_pm_reml <- mod_results(res_meta_tb_pm, mod = "Plant_method", group = "Study_ID", data = es_total_biomass)
res_meta_tb_pm_reml <- coef(summary(res_meta_tb_pm)) %>% mutate(factor = levels(as.factor(es_total_biomass$Plant_method)), Index = "Total biomass", size = (res_meta_tb_pm_n$n))


###merge data
d_tb_fig <- rbind(res_meta_tb_em_reml,
                  res_meta_tb_fg_reml,
                  res_meta_tb_st_reml,
                  res_meta_tb_ta_reml,
                  res_meta_tb_ph_reml,
                  res_meta_tb_hms_reml,
                  res_meta_tb_pm_reml)

# Added the group, label and significantly levels

d_tb_fig$group = dplyr::recode_factor(d_tb_fig$factor,
                                      Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                      Shrub = "b",Tree = "b",
                                      More_six_month = "c", Three_month = "c", Six_month = "c",
                                      More_two_year = "d", Two_year = "d", One_year = "d",
                                      Acidic = "e", Neutral = "e", Alkaline = "e",
                                      High = "f", Middle = "f", Low = "f", Extreme = "f",
                                      Cutting = "g", Seedling = "g", Plant = "g")


d_tb_fig$factor = factor(d_tb_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))

d_tb_fig$Sig <- ifelse(d_tb_fig$pval < 0.001, "***", ifelse(d_tb_fig$pval < 0.01, "**", ifelse(d_tb_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_tb_fig$estimate <- make_pct(d_tb_fig$estimate)
d_tb_fig$ci.lb <- make_pct(d_tb_fig$ci.lb)
d_tb_fig$ci.ub <- make_pct(d_tb_fig$ci.ub)
#write.csv(d_tb_fig, "d_tb_fig.csv")
#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.7a = 
  ggplot(data=d_tb_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= -80,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='Total biomass')+
  theme_classic()+
  scale_y_continuous(limits = c(-100,100), breaks=seq(-100,100, by=50))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
#ggsave("Fig.2a TB.pdf",Fig.7a, width = 6, height = 8)



####   Aboveground biomass  ####

#removed the NA in yi and vi
es_above_biomass <- drop_na(es_above_biomass, yi)


# 1、Experimental_method
res_meta_ab_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_em_n <- es_above_biomass %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_ab_em_reml <- mod_results(res_meta_ab_em, mod = "Experimental_method", group = "Study_ID", data = es_above_biomass)
res_meta_ab_em_reml <- coef(summary(res_meta_ab_em)) %>% mutate(factor = levels(as.factor(es_above_biomass$Experimental_method)), Index = "Above biomass", size = (res_meta_ab_em_n$n))

# 2、Functional group
res_meta_ab_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_fg_n <- es_above_biomass %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_ab_fg_reml <- mod_results(res_meta_ab_fg, mod = "Functional_group", group = "Study_ID", data = es_above_biomass)
res_meta_ab_fg_reml <- coef(summary(res_meta_ab_fg)) %>% mutate(factor = levels(as.factor(es_above_biomass$Functional_group)), Index = "Above biomass", size = (res_meta_ab_fg_n$n))

# 3、stress_time
res_meta_ab_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_st_n <- es_above_biomass %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_ab_st_reml <- mod_results(res_meta_ab_st, mod = "Stress_time", group = "Study_ID", data = es_above_biomass)
res_meta_ab_st_reml <- coef(summary(res_meta_ab_st)) %>% mutate(factor = levels(as.factor(es_above_biomass$Stress_time)), Index = "Above biomass", size = (res_meta_ab_st_n$n))


# 4、Tree_age
res_meta_ab_ta <- rma.mv(yi, vi, mod = ~ Tree_age, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_ta <- rma.mv(yi, vi, mod = ~ Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_ta_n <- es_above_biomass %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_ab_ta_reml <- mod_results(res_meta_ab_ta, mod = "Tree_age", group = "Study_ID", data = es_above_biomass)
res_meta_ab_ta_reml <- coef(summary(res_meta_ab_ta)) %>% mutate(factor = levels(as.factor(es_above_biomass$Tree_age)), Index = "Above biomass", size = (res_meta_ab_ta_n$n))


# 5、pH
res_meta_ab_ph <- rma.mv(yi, vi, mod = ~ pH, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_ph <- rma.mv(yi, vi, mod = ~ pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_ph_n <- es_above_biomass %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_ab_ph_reml <- mod_results(res_meta_ab_ph, mod = "pH", group = "Study_ID", data = es_above_biomass)
res_meta_ab_ph_reml <- coef(summary(res_meta_ab_ph)) %>% mutate(factor = levels(as.factor(es_above_biomass$pH)), Index = "Above biomass", size = (res_meta_ab_ph_n$n))


# 6、HMs_level
res_meta_ab_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_hms_n <- es_above_biomass %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_ab_hms_reml <- mod_results(res_meta_ab_hms, mod = "HMs_level", group = "Study_ID", data = es_above_biomass)
res_meta_ab_hms_reml <- coef(summary(res_meta_ab_hms)) %>% mutate(factor = levels(as.factor(es_above_biomass$HMs_level)), Index = "Above biomass", size = (res_meta_ab_hms_n$n))


# 7、Plant_method
res_meta_ab_pm <- rma.mv(yi, vi, mod = ~ Plant_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_pm <- rma.mv(yi, vi, mod = ~ Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_above_biomass)
res_meta_ab_pm_n <- es_above_biomass %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_ab_pm_reml <- mod_results(res_meta_ab_pm, mod = "Plant_method", group = "Study_ID", data = es_above_biomass)
res_meta_ab_pm_reml <- coef(summary(res_meta_ab_pm)) %>% mutate(factor = levels(as.factor(es_above_biomass$Plant_method)), Index = "Above biomass", size = (res_meta_ab_pm_n$n))


###merge data
d_ab_fig <- rbind(res_meta_ab_em_reml,
                  res_meta_ab_fg_reml,
                  res_meta_ab_st_reml,
                  res_meta_ab_ta_reml,
                  res_meta_ab_ph_reml,
                  res_meta_ab_hms_reml,
                  res_meta_ab_pm_reml)

# Added the group, label and significantly levels

d_ab_fig$group = dplyr::recode_factor(d_ab_fig$factor,
                                      Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                      Shrub = "b",Tree = "b",
                                      More_six_month = "c", Three_month = "c", Six_month = "c",
                                      More_two_year = "d", Two_year = "d", One_year = "d",
                                      Acidic = "e", Neutral = "e", Alkaline = "e",
                                      High = "f", Middle = "f", Low = "f", Extreme = "f",
                                      Cutting = "g", Seedling = "g", Plant = "g")


d_ab_fig$factor = factor(d_ab_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))

d_ab_fig$Sig <- ifelse(d_ab_fig$pval < 0.001, "***", ifelse(d_ab_fig$pval < 0.01, "**", ifelse(d_ab_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_ab_fig$estimate <- make_pct(d_ab_fig$estimate)
d_ab_fig$ci.lb <- make_pct(d_ab_fig$ci.lb)
d_ab_fig$ci.ub <- make_pct(d_ab_fig$ci.ub)
#write.csv(d_ab_fig, "d_ab_fig.csv")
#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.7b = 
  ggplot(data=d_ab_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= -80,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='Above biomass')+
  theme_classic()+
  scale_y_continuous(limits = c(-100,100), breaks=seq(-100,100, by=50))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
#ggsave("Fig.2bAB.pdf",Fig.7b, width = 6, height = 8)




####   Leaf biomass  ####

#removed the NA in yi and vi
es_leaf_biomass <- drop_na(es_leaf_biomass, yi)


# 1、Experimental_method
res_meta_leaf_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_em_n <- es_leaf_biomass %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_leaf_em_reml <- mod_results(res_meta_leaf_em, mod = "Experimental_method", group = "Study_ID", data = es_leaf_biomass)
res_meta_leaf_em_reml <- coef(summary(res_meta_leaf_em)) %>% mutate(factor = levels(as.factor(es_leaf_biomass$Experimental_method)), Index = "leaf biomass", size = (res_meta_leaf_em_n$n))

# 2、Functional group
res_meta_leaf_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_fg_n <- es_leaf_biomass %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_leaf_fg_reml <- mod_results(res_meta_leaf_fg, mod = "Functional_group", group = "Study_ID", data = es_leaf_biomass)
res_meta_leaf_fg_reml <- coef(summary(res_meta_leaf_fg)) %>% mutate(factor = levels(as.factor(es_leaf_biomass$Functional_group)), Index = "leaf biomass", size = (res_meta_leaf_fg_n$n))

# 3、stress_time
res_meta_leaf_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_st_n <- es_leaf_biomass %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_leaf_st_reml <- mod_results(res_meta_leaf_st, mod = "Stress_time", group = "Study_ID", data = es_leaf_biomass)
res_meta_leaf_st_reml <- coef(summary(res_meta_leaf_st)) %>% mutate(factor = levels(as.factor(es_leaf_biomass$Stress_time)), Index = "leaf biomass", size = (res_meta_leaf_st_n$n))


# 4、Tree_age
res_meta_leaf_ta <- rma.mv(yi, vi, mod = ~ Tree_age, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_ta <- rma.mv(yi, vi, mod = ~ Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_ta_n <- es_leaf_biomass %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_leaf_ta_n <- res_meta_leaf_ta_n[-1, ]
#res_meta_leaf_ta_reml <- mod_results(res_meta_leaf_ta, mod = "Tree_age", group = "Study_ID", data = es_leaf_biomass)
res_meta_leaf_ta_reml <- coef(summary(res_meta_leaf_ta)) %>% mutate(factor = c("One_year","Two_year"), Index = "leaf biomass", size = (res_meta_leaf_ta_n$n))


# 5、pH
res_meta_leaf_ph <- rma.mv(yi, vi, mod = ~ pH, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_ph <- rma.mv(yi, vi, mod = ~ pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_ph_n <- es_leaf_biomass %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_leaf_ph_reml <- mod_results(res_meta_leaf_ph, mod = "pH", group = "Study_ID", data = es_leaf_biomass)
res_meta_leaf_ph_reml <- coef(summary(res_meta_leaf_ph)) %>% mutate(factor = levels(as.factor(es_leaf_biomass$pH)), Index = "leaf biomass", size = (res_meta_leaf_ph_n$n))


# 6、HMs_level
res_meta_leaf_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_hms_n <- es_leaf_biomass %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_leaf_hms_reml <- mod_results(res_meta_leaf_hms, mod = "HMs_level", group = "Study_ID", data = es_leaf_biomass)
res_meta_leaf_hms_reml <- coef(summary(res_meta_leaf_hms)) %>% mutate(factor = levels(as.factor(es_leaf_biomass$HMs_level)), Index = "leaf biomass", size = (res_meta_leaf_hms_n$n))


# 7、Plant_method
res_meta_leaf_pm <- rma.mv(yi, vi, mod = ~ Plant_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_pm <- rma.mv(yi, vi, mod = ~ Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_leaf_biomass)
res_meta_leaf_pm_n <- es_leaf_biomass %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_leaf_pm_reml <- mod_results(res_meta_leaf_pm, mod = "Plant_method", group = "Study_ID", data = es_leaf_biomass)
res_meta_leaf_pm_reml <- coef(summary(res_meta_leaf_pm)) %>% mutate(factor = levels(as.factor(es_leaf_biomass$Plant_method)), Index = "leaf biomass", size = (res_meta_leaf_pm_n$n))


###merge data
d_leaf_fig <- rbind(res_meta_leaf_em_reml,
                  res_meta_leaf_fg_reml,
                  res_meta_leaf_st_reml,
                  res_meta_leaf_ta_reml,
                  res_meta_leaf_ph_reml,
                  res_meta_leaf_hms_reml,
                  res_meta_leaf_pm_reml)

# Added the group, label and significantly levels

d_leaf_fig$group = dplyr::recode_factor(d_leaf_fig$factor,
                                      Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                      Shrub = "b",Tree = "b",
                                      More_six_month = "c", Three_month = "c", Six_month = "c",
                                      More_two_year = "d", Two_year = "d", One_year = "d",
                                      Acidic = "e", Neutral = "e", Alkaline = "e",
                                      High = "f", Middle = "f", Low = "f", Extreme = "f",
                                      Cutting = "g", Seedling = "g", Plant = "g")


d_leaf_fig$factor = factor(d_leaf_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))

d_leaf_fig$Sig <- ifelse(d_leaf_fig$pval < 0.001, "***", ifelse(d_leaf_fig$pval < 0.01, "**", ifelse(d_leaf_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_leaf_fig$estimate <- make_pct(d_leaf_fig$estimate)
d_leaf_fig$ci.lb <- make_pct(d_leaf_fig$ci.lb)
d_leaf_fig$ci.ub <- make_pct(d_leaf_fig$ci.ub)
#write.csv(d_leaf_fig, "d_leaf_fig.csv")
#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.7c = 
  ggplot(data=d_leaf_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= -80,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='Leaf biomass')+
  theme_classic()+
  scale_y_continuous(limits = c(-100,100), breaks=seq(-100,100, by=50))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
#ggsave("Fig.2cLB.pdf",Fig.7c, width = 6, height = 8)


####   Stem biomass  ####

#removed the NA in yi and vi
es_stem_biomass <- drop_na(es_stem_biomass, yi)


# 1、Experimental_method
res_meta_stem_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_stem_biomass)
res_meta_stem_em_n <- es_stem_biomass %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_stem_em_reml <- mod_results(res_meta_stem_em, mod = "Experimental_method", group = "Study_ID", data = es_stem_biomass)
res_meta_stem_em_reml <- coef(summary(res_meta_stem_em)) %>% mutate(factor = levels(as.factor(es_stem_biomass$Experimental_method)), Index = "stem biomass", size = (res_meta_stem_em_n$n))

# 2、Functional group
res_meta_stem_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_stem_biomass)
res_meta_stem_fg_n <- es_stem_biomass %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_stem_fg_reml <- mod_results(res_meta_stem_fg, mod = "Functional_group", group = "Study_ID", data = es_stem_biomass)
res_meta_stem_fg_reml <- coef(summary(res_meta_stem_fg)) %>% mutate(factor = levels(as.factor(es_stem_biomass$Functional_group)), Index = "stem biomass", size = (res_meta_stem_fg_n$n))

# 3、stress_time
res_meta_stem_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_stem_biomass)
res_meta_stem_st_n <- es_stem_biomass %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_stem_st_reml <- mod_results(res_meta_stem_st, mod = "Stress_time", group = "Study_ID", data = es_stem_biomass)
res_meta_stem_st_reml <- coef(summary(res_meta_stem_st)) %>% mutate(factor = levels(as.factor(es_stem_biomass$Stress_time)), Index = "stem biomass", size = (res_meta_stem_st_n$n))


# 4、Tree_age
res_meta_stem_ta <- rma.mv(yi, vi, mod = ~ Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_stem_biomass)
res_meta_stem_ta_n <- es_stem_biomass %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_stem_ta_reml <- mod_results(res_meta_stem_ta, mod = "Tree_age", group = "Study_ID", data = es_stem_biomass)
res_meta_stem_ta_reml <- coef(summary(res_meta_stem_ta)) %>% mutate(factor = levels(as.factor(es_stem_biomass$Tree_age)), Index = "stem biomass", size = (res_meta_stem_ta_n$n))


# 5、pH
res_meta_stem_ph <- rma.mv(yi, vi, mod = ~ pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_stem_biomass)
res_meta_stem_ph_n <- es_stem_biomass %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_stem_ph_reml <- mod_results(res_meta_stem_ph, mod = "pH", group = "Study_ID", data = es_stem_biomass)
res_meta_stem_ph_reml <- coef(summary(res_meta_stem_ph)) %>% mutate(factor = levels(as.factor(es_stem_biomass$pH)), Index = "stem biomass", size = (res_meta_stem_ph_n$n))


# 6、HMs_level
res_meta_stem_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_stem_biomass)
res_meta_stem_hms_n <- es_stem_biomass %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_stem_hms_reml <- mod_results(res_meta_stem_hms, mod = "HMs_level", group = "Study_ID", data = es_stem_biomass)
res_meta_stem_hms_reml <- coef(summary(res_meta_stem_hms)) %>% mutate(factor = levels(as.factor(es_stem_biomass$HMs_level)), Index = "stem biomass", size = (res_meta_stem_hms_n$n))


# 7、Plant_method
res_meta_stem_pm <- rma.mv(yi, vi, mod = ~ Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_stem_biomass)
res_meta_stem_pm_n <- es_stem_biomass %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_stem_pm_reml <- mod_results(res_meta_stem_pm, mod = "Plant_method", group = "Study_ID", data = es_stem_biomass)
res_meta_stem_pm_reml <- coef(summary(res_meta_stem_pm)) %>% mutate(factor = levels(as.factor(es_stem_biomass$Plant_method)), Index = "stem biomass", size = (res_meta_stem_pm_n$n))


###merge data
d_stem_fig <- rbind(res_meta_stem_em_reml,
                    res_meta_stem_fg_reml,
                    res_meta_stem_st_reml,
                    res_meta_stem_ta_reml,
                    res_meta_stem_ph_reml,
                    res_meta_stem_hms_reml,
                    res_meta_stem_pm_reml)

# Added the group, label and significantly levels

d_stem_fig$group = dplyr::recode_factor(d_stem_fig$factor,
                                        Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                        Shrub = "b",Tree = "b",
                                        More_six_month = "c", Three_month = "c", Six_month = "c",
                                        More_two_year = "d", Two_year = "d", One_year = "d",
                                        Acidic = "e", Neutral = "e", Alkaline = "e",
                                        High = "f", Middle = "f", Low = "f", Extreme = "f",
                                        Cutting = "g", Seedling = "g", Plant = "g")


d_stem_fig$factor = factor(d_stem_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted","Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))

d_stem_fig$Sig <- ifelse(d_stem_fig$pval < 0.001, "***", ifelse(d_stem_fig$pval < 0.01, "**", ifelse(d_stem_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_stem_fig$estimate <- make_pct(d_stem_fig$estimate)
d_stem_fig$ci.lb <- make_pct(d_stem_fig$ci.lb)
d_stem_fig$ci.ub <- make_pct(d_stem_fig$ci.ub)

#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.7d = 
  ggplot(data=d_stem_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= -80,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='Stem biomass')+
  theme_classic()+
  scale_y_continuous(limits = c(-100,100), breaks=seq(-100,100, by=50))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))


####   Root biomass  ####

#removed the NA in yi and vi
es_root_biomass <- drop_na(es_root_biomass, yi)


# 1、Experimental_method
res_meta_root_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_em_n <- es_root_biomass %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_root_em_reml <- mod_results(res_meta_root_em, mod = "Experimental_method", group = "Study_ID", data = es_root_biomass)
res_meta_root_em_reml <- coef(summary(res_meta_root_em)) %>% mutate(factor = levels(as.factor(es_root_biomass$Experimental_method)), Index = "root biomass", size = (res_meta_root_em_n$n))

# 2、Functional group
res_meta_root_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_fg_n <- es_root_biomass %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_root_fg_reml <- mod_results(res_meta_root_fg, mod = "Functional_group", group = "Study_ID", data = es_root_biomass)
res_meta_root_fg_reml <- coef(summary(res_meta_root_fg)) %>% mutate(factor = levels(as.factor(es_root_biomass$Functional_group)), Index = "root biomass", size = (res_meta_root_fg_n$n))

# 3、stress_time
res_meta_root_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_st_n <- es_root_biomass %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_root_st_reml <- mod_results(res_meta_root_st, mod = "Stress_time", group = "Study_ID", data = es_root_biomass)
res_meta_root_st_reml <- coef(summary(res_meta_root_st)) %>% mutate(factor = levels(as.factor(es_root_biomass$Stress_time)), Index = "root biomass", size = (res_meta_root_st_n$n))


# 4、Tree_age
res_meta_root_ta <- rma.mv(yi, vi, mod = ~ Tree_age, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_ta <- rma.mv(yi, vi, mod = ~ Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_ta_n <- es_root_biomass %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_root_ta_reml <- mod_results(res_meta_root_ta, mod = "Tree_age", group = "Study_ID", data = es_root_biomass)
res_meta_root_ta_reml <- coef(summary(res_meta_root_ta)) %>% mutate(factor = levels(as.factor(es_root_biomass$Tree_age)), Index = "root biomass", size = (res_meta_root_ta_n$n))


# 5、pH
res_meta_root_ph <- rma.mv(yi, vi, mod = ~ pH, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_ph <- rma.mv(yi, vi, mod = ~ pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_ph_n <- es_root_biomass %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_root_ph_reml <- mod_results(res_meta_root_ph, mod = "pH", group = "Study_ID", data = es_root_biomass)
res_meta_root_ph_reml <- coef(summary(res_meta_root_ph)) %>% mutate(factor = levels(as.factor(es_root_biomass$pH)), Index = "root biomass", size = (res_meta_root_ph_n$n))


# 6、HMs_level
res_meta_root_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_hms_n <- es_root_biomass %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_root_hms_reml <- mod_results(res_meta_root_hms, mod = "HMs_level", group = "Study_ID", data = es_root_biomass)
res_meta_root_hms_reml <- coef(summary(res_meta_root_hms)) %>% mutate(factor = levels(as.factor(es_root_biomass$HMs_level)), Index = "root biomass", size = (res_meta_root_hms_n$n))


# 7、Plant_method
res_meta_root_pm <- rma.mv(yi, vi, mod = ~ Plant_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_pm <- rma.mv(yi, vi, mod = ~ Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_root_biomass)
res_meta_root_pm_n <- es_root_biomass %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_root_pm_reml <- mod_results(res_meta_root_pm, mod = "Plant_method", group = "Study_ID", data = es_root_biomass)
res_meta_root_pm_reml <- coef(summary(res_meta_root_pm)) %>% mutate(factor = levels(as.factor(es_root_biomass$Plant_method)), Index = "root biomass", size = (res_meta_root_pm_n$n))


###merge data
d_root_fig <- rbind(res_meta_root_em_reml,
                    res_meta_root_fg_reml,
                    res_meta_root_st_reml,
                    res_meta_root_ta_reml,
                    res_meta_root_ph_reml,
                    res_meta_root_hms_reml,
                    res_meta_root_pm_reml)

# Added the group, label and significantly levels

d_root_fig$group = dplyr::recode_factor(d_root_fig$factor,
                                        Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                        Shrub = "b",Tree = "b",
                                        More_six_month = "c", Three_month = "c", Six_month = "c",
                                        More_two_year = "d", Two_year = "d", One_year = "d",
                                        Acidic = "e", Neutral = "e", Alkaline = "e",
                                        High = "f", Middle = "f", Low = "f", Extreme = "f",
                                        Cutting = "g", Seedling = "g", Plant = "g")


d_root_fig$factor = factor(d_root_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))

d_root_fig$Sig <- ifelse(d_root_fig$pval < 0.001, "***", ifelse(d_root_fig$pval < 0.01, "**", ifelse(d_root_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_root_fig$estimate <- make_pct(d_root_fig$estimate)
d_root_fig$ci.lb <- make_pct(d_root_fig$ci.lb)
d_root_fig$ci.ub <- make_pct(d_root_fig$ci.ub)
#write.csv(d_root_fig, "d_root_fig.csv")
#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.7ee = 
  ggplot(data=d_root_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= -80,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='Underground biomass')+
  theme_classic()+
  scale_y_continuous(limits = c(-100,100), breaks=seq(-100,100, by=50))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))

#ggsave("Fig.2d UB.pdf", Fig.7ee, width = 6, height = 8)



################  Fig.8  Heavy metals concentration  ##########

####   Total HMs concentration  ####

#removed the NA in yi and vi
es_hms_total <- drop_na(es_hms_total, yi)


# 1、Experimental_method
res_meta_total_hms_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_total)
res_meta_total_hms_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_total)
res_meta_total_hms_em_n <- es_hms_total %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_total_hms_em_reml <- mod_results(res_meta_total_hms_em, mod = "Experimental_method", group = "Study_ID", data = es_hms_total)
res_meta_total_hms_em_reml <- coef(summary(res_meta_total_hms_em)) %>% mutate(factor = levels(as.factor(es_hms_total$Experimental_method)), Index = "total_hms biomass", size = (res_meta_total_hms_em_n$n))

# 2、Functional group
res_meta_total_hms_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_total)
res_meta_total_hms_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_total)
res_meta_total_hms_fg_n <- es_hms_total %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_total_hms_fg_reml <- mod_results(res_meta_total_hms_fg, mod = "Functional_group", group = "Study_ID", data = es_hms_total)
res_meta_total_hms_fg_reml <- coef(summary(res_meta_total_hms_fg)) %>% mutate(factor = levels(as.factor(es_hms_total$Functional_group)), Index = "total_hms biomass", size = (res_meta_total_hms_fg_n$n))

# 3、stress_time
res_meta_total_hms_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_total)
res_meta_total_hms_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_total)
res_meta_total_hms_st_n <- es_hms_total %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_total_hms_st_reml <- mod_results(res_meta_total_hms_st, mod = "Stress_time", group = "Study_ID", data = es_hms_total)
res_meta_total_hms_st_reml <- coef(summary(res_meta_total_hms_st)) %>% mutate(factor = levels(as.factor(es_hms_total$Stress_time)), Index = "total_hms biomass", size = (res_meta_total_hms_st_n$n))


# 4、Tree_age (1)
res_meta_total_hms_ta <- rma.mv(yi, vi, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_total)
res_meta_total_hms_ta_n <- es_hms_total %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
#res_meta_total_hms_ta_reml <- mod_results(res_meta_total_hms_ta, mod = "Tree_age", group = "Study_ID")
res_meta_total_hms_ta_reml <- coef(summary(res_meta_total_hms_ta)) %>% mutate(factor = levels(as.factor(es_hms_total$Tree_age)), Index = "total_hms biomass", size = (res_meta_total_hms_ta_n$n))


# 5、pH(1)
res_meta_total_hms_ph <- rma.mv(yi, vi, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_total)
res_meta_total_hms_ph_n <- es_hms_total %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
#res_meta_total_hms_ph_reml <- mod_results(res_meta_total_hms_ph, mod = "pH", group = "Study_ID", data = es_hms_total)
res_meta_total_hms_ph_reml <- coef(summary(res_meta_total_hms_ph)) %>% mutate(factor = levels(as.factor(es_hms_total$pH)), Index = "total_hms biomass", size = (res_meta_total_hms_ph_n$n))


# 6、HMs_level
res_meta_total_hms_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_total)
res_meta_total_hms_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_total)
res_meta_total_hms_hms_n <- es_hms_total %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_total_hms_hms_reml <- mod_results(res_meta_total_hms_hms, mod = "HMs_level", group = "Study_ID", data = es_hms_total)
res_meta_total_hms_hms_reml <- coef(summary(res_meta_total_hms_hms)) %>% mutate(factor = levels(as.factor(es_hms_total$HMs_level)), Index = "total_hms biomass", size = (res_meta_total_hms_hms_n$n))


# 7、Plant_method
res_meta_total_hms_pm <- rma.mv(yi, vi, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_total)
res_meta_total_hms_pm_n <- es_hms_total %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
#res_meta_total_hms_pm_reml <- mod_results(res_meta_total_hms_pm, mod = "Plant_method", group = "Study_ID", data = es_hms_total)
res_meta_total_hms_pm_reml <- coef(summary(res_meta_total_hms_pm)) %>% mutate(factor = levels(as.factor(es_hms_total$Plant_method)), Index = "total_hms biomass", size = (res_meta_total_hms_pm_n$n))


###merge data
d_total_hms_fig <- rbind(res_meta_total_hms_em_reml,
                    res_meta_total_hms_fg_reml,
                    res_meta_total_hms_st_reml,
                    res_meta_total_hms_ta_reml,
                    res_meta_total_hms_ph_reml,
                    res_meta_total_hms_hms_reml,
                    res_meta_total_hms_pm_reml)

# Added the group, label and significantly levels

d_total_hms_fig$group = dplyr::recode_factor(d_total_hms_fig$factor,
                                        Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                        Shrub = "b",Tree = "b",
                                        More_six_month = "c", Three_month = "c", Six_month = "c",
                                        More_two_year = "d", Two_year = "d", One_year = "d",
                                        Acidic = "e", Neutral = "e", Alkaline = "e",
                                        High = "f", Middle = "f", Low = "f", Extreme = "f",
                                        Cutting = "g", Seedling = "g", Plant = "g")


d_total_hms_fig$factor = factor(d_total_hms_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))

d_total_hms_fig$Sig <- ifelse(d_total_hms_fig$pval < 0.001, "***", ifelse(d_total_hms_fig$pval < 0.01, "**", ifelse(d_total_hms_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_total_hms_fig$estimate <- make_pct(d_total_hms_fig$estimate)
d_total_hms_fig$ci.lb <- make_pct(d_total_hms_fig$ci.lb)
d_total_hms_fig$ci.ub <- make_pct(d_total_hms_fig$ci.ub)
#write.csv(d_total_hms_fig, "d_total_hms_fig.csv")
#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.8a = 
  ggplot(data=d_total_hms_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= - 10000,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='Total HMs concentration')+
  theme_classic()+
  scale_y_continuous(limits = c(-20000,40000), breaks=seq(-20000,40000, by=20000))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
#save pdf
#ggsave("Fig.2e THMs.pdf", Fig.8a, width = 6, height = 8)



####   Aboveground HMs concentration  ####

#removed the NA in yi and vi
es_hms_above <- drop_na(es_hms_above, yi)


# 1、Experimental_method
res_meta_above_hms_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_em_n <- es_hms_above %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_above_hms_em_reml <- mod_results(res_meta_above_hms_em, mod = "Experimental_method", group = "Study_ID", data = es_hms_above)
res_meta_above_hms_em_reml <- coef(summary(res_meta_above_hms_em)) %>% mutate(factor = levels(as.factor(es_hms_above$Experimental_method)), Index = "above_hms biomass", size = (res_meta_above_hms_em_n$n))

# 2、Functional group
res_meta_above_hms_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_fg_n <- es_hms_above %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_above_hms_fg_reml <- mod_results(res_meta_above_hms_fg, mod = "Functional_group", group = "Study_ID", data = es_hms_above)
res_meta_above_hms_fg_reml <- coef(summary(res_meta_above_hms_fg)) %>% mutate(factor = levels(as.factor(es_hms_above$Functional_group)), Index = "above_hms biomass", size = (res_meta_above_hms_fg_n$n))

# 3、stress_time
res_meta_above_hms_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_st_n <- es_hms_above %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_above_hms_st_reml <- mod_results(res_meta_above_hms_st, mod = "Stress_time", group = "Study_ID", data = es_hms_above)
res_meta_above_hms_st_reml <- coef(summary(res_meta_above_hms_st)) %>% mutate(factor = levels(as.factor(es_hms_above$Stress_time)), Index = "above_hms biomass", size = (res_meta_above_hms_st_n$n))


# 4、Tree_age
res_meta_above_hms_ta <- rma.mv(yi, vi, mods = ~Tree_age, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_ta <- rma.mv(yi, vi, mods = ~Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_ta_n <- es_hms_above %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_above_hms_ta_reml <- mod_results(res_meta_above_hms_ta, mod = "Tree_age", group = "Study_ID", data = es_hms_above)
res_meta_above_hms_ta_reml <- coef(summary(res_meta_above_hms_ta)) %>% mutate(factor = levels(as.factor(es_hms_above$Tree_age)), Index = "above_hms biomass", size = (res_meta_above_hms_ta_n$n))


# 5、pH
res_meta_above_hms_ph <- rma.mv(yi, vi, mods = ~pH, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_ph <- rma.mv(yi, vi, mods = ~pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_ph_n <- es_hms_above %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_above_hms_ph_reml <- mod_results(res_meta_above_hms_ph, mod = "pH", group = "Study_ID", data = es_hms_above)
res_meta_above_hms_ph_reml <- coef(summary(res_meta_above_hms_ph)) %>% mutate(factor = levels(as.factor(es_hms_above$pH)), Index = "above_hms biomass", size = (res_meta_above_hms_ph_n$n))


# 6、HMs_level
res_meta_above_hms_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_hms_n <- es_hms_above %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_above_hms_hms_reml <- mod_results(res_meta_above_hms_hms, mod = "HMs_level", group = "Study_ID", data = es_hms_above)
res_meta_above_hms_hms_reml <- coef(summary(res_meta_above_hms_hms)) %>% mutate(factor = levels(as.factor(es_hms_above$HMs_level)), Index = "above_hms biomass", size = (res_meta_above_hms_hms_n$n))


# 7、Plant_method
res_meta_above_hms_pm <- rma.mv(yi, vi, mod = ~Plant_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_pm <- rma.mv(yi, vi, mod = ~Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_above)
res_meta_above_hms_pm_n <- es_hms_above %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_above_hms_pm_reml <- mod_results(res_meta_above_hms_pm, mod = "Plant_method", group = "Study_ID", data = es_hms_above)
res_meta_above_hms_pm_reml <- coef(summary(res_meta_above_hms_pm)) %>% mutate(factor = levels(as.factor(es_hms_above$Plant_method)), Index = "above_hms biomass", size = (res_meta_above_hms_pm_n$n))


###merge data
d_above_hms_fig <- rbind(res_meta_above_hms_em_reml,
                         res_meta_above_hms_fg_reml,
                         res_meta_above_hms_st_reml,
                         res_meta_above_hms_ta_reml,
                         res_meta_above_hms_ph_reml,
                         res_meta_above_hms_hms_reml,
                         res_meta_above_hms_pm_reml)

# Added the group, label and significantly levels

d_above_hms_fig$group = dplyr::recode_factor(d_above_hms_fig$factor,
                                             Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                             Shrub = "b",Tree = "b",
                                             More_six_month = "c", Three_month = "c", Six_month = "c",
                                             More_two_year = "d", Two_year = "d", One_year = "d",
                                             Acidic = "e", Neutral = "e", Alkaline = "e",
                                             High = "f", Middle = "f", Low = "f", Extreme = "f",
                                             Cutting = "g", Seedling = "g", Plant = "g")


d_above_hms_fig$factor = factor(d_above_hms_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))

d_above_hms_fig$Sig <- ifelse(d_above_hms_fig$pval < 0.001, "***", ifelse(d_above_hms_fig$pval < 0.01, "**", ifelse(d_above_hms_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_above_hms_fig$estimate <- make_pct(d_above_hms_fig$estimate)
d_above_hms_fig$ci.lb <- make_pct(d_above_hms_fig$ci.lb)
d_above_hms_fig$ci.ub <- make_pct(d_above_hms_fig$ci.ub)
#write.csv(d_above_hms_fig, "d_above_hms_fig.csv")
#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.8b = 
  ggplot(data=d_above_hms_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= - 4000,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='Above HMs concentration')+
  theme_classic()+
  scale_y_continuous(limits = c(-5000,5000), breaks=seq(-5000,5000, by=2500))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
#save pdf
#ggsave("Fig.2f ATHs.pdf", Fig.8b, width = 6, height = 8)





####   Leaf HMs concentration  ####

#removed the NA in yi and vi
es_hms_leaf <- drop_na(es_hms_leaf, yi)


# 1、Experimental_method
res_meta_leaf_hms_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_em_n <- es_hms_leaf %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_leaf_hms_em_reml <- mod_results(res_meta_leaf_hms_em, mod = "Experimental_method", group = "Study_ID", data = es_hms_leaf)
res_meta_leaf_hms_em_reml <- coef(summary(res_meta_leaf_hms_em)) %>% mutate(factor = levels(as.factor(es_hms_leaf$Experimental_method)), Index = "leaf_hms biomass", size = (res_meta_leaf_hms_em_n$n))

# 2、Functional group
res_meta_leaf_hms_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_fg_n <- es_hms_leaf %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_leaf_hms_fg_reml <- mod_results(res_meta_leaf_hms_fg, mod = "Functional_group", group = "Study_ID", data = es_hms_leaf)
res_meta_leaf_hms_fg_reml <- coef(summary(res_meta_leaf_hms_fg)) %>% mutate(factor = levels(as.factor(es_hms_leaf$Functional_group)), Index = "leaf_hms biomass", size = (res_meta_leaf_hms_fg_n$n))

# 3、stress_time
res_meta_leaf_hms_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_st_n <- es_hms_leaf %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_leaf_hms_st_reml <- mod_results(res_meta_leaf_hms_st, mod = "Stress_time", group = "Study_ID", data = es_hms_leaf)
res_meta_leaf_hms_st_reml <- coef(summary(res_meta_leaf_hms_st)) %>% mutate(factor = levels(as.factor(es_hms_leaf$Stress_time)), Index = "leaf_hms biomass", size = (res_meta_leaf_hms_st_n$n))


# 4、Tree_age
res_meta_leaf_hms_ta <- rma.mv(yi, vi, mods = ~Tree_age, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_ta <- rma.mv(yi, vi, mods = ~Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_ta_n <- es_hms_leaf %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_leaf_hms_ta_reml <- mod_results(res_meta_leaf_hms_ta, mod = "Tree_age", group = "Study_ID", data = es_hms_leaf)
res_meta_leaf_hms_ta_reml <- coef(summary(res_meta_leaf_hms_ta)) %>% mutate(factor = levels(as.factor(es_hms_leaf$Tree_age)), Index = "leaf_hms biomass", size = (res_meta_leaf_hms_ta_n$n))


# 5、pH
res_meta_leaf_hms_ph <- rma.mv(yi, vi, mods = ~pH, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_ph <- rma.mv(yi, vi, mods = ~pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_ph_n <- es_hms_leaf %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_leaf_hms_ph_reml <- mod_results(res_meta_leaf_hms_ph, mod = "pH", group = "Study_ID", data = es_hms_leaf)
res_meta_leaf_hms_ph_reml <- coef(summary(res_meta_leaf_hms_ph)) %>% mutate(factor = levels(as.factor(es_hms_leaf$pH)), Index = "leaf_hms biomass", size = (res_meta_leaf_hms_ph_n$n))


# 6、HMs_level
res_meta_leaf_hms_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_hms_n <- es_hms_leaf %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_leaf_hms_hms_reml <- mod_results(res_meta_leaf_hms_hms, mod = "HMs_level", group = "Study_ID", data = es_hms_leaf)
res_meta_leaf_hms_hms_reml <- coef(summary(res_meta_leaf_hms_hms)) %>% mutate(factor = levels(as.factor(es_hms_leaf$HMs_level)), Index = "leaf_hms biomass", size = (res_meta_leaf_hms_hms_n$n))


# 7、Plant_method
res_meta_leaf_hms_pm <- rma.mv(yi, vi, mod = ~Plant_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_pm <- rma.mv(yi, vi, mod = ~Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_leaf)
res_meta_leaf_hms_pm_n <- es_hms_leaf %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_leaf_hms_pm_reml <- mod_results(res_meta_leaf_hms_pm, mod = "Plant_method", group = "Study_ID", data = es_hms_leaf)
res_meta_leaf_hms_pm_reml <- coef(summary(res_meta_leaf_hms_pm)) %>% mutate(factor = levels(as.factor(es_hms_leaf$Plant_method)), Index = "leaf_hms biomass", size = (res_meta_leaf_hms_pm_n$n))


###merge data
d_leaf_hms_fig <- rbind(res_meta_leaf_hms_em_reml,
                         res_meta_leaf_hms_fg_reml,
                         res_meta_leaf_hms_st_reml,
                         res_meta_leaf_hms_ta_reml,
                         res_meta_leaf_hms_ph_reml,
                         res_meta_leaf_hms_hms_reml,
                         res_meta_leaf_hms_pm_reml)

# Added the group, label and significantly levels

d_leaf_hms_fig$group = dplyr::recode_factor(d_leaf_hms_fig$factor,
                                             Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                             Shrub = "b",Tree = "b",
                                             More_six_month = "c", Three_month = "c", Six_month = "c",
                                             More_two_year = "d", Two_year = "d", One_year = "d",
                                             Acidic = "e", Neutral = "e", Alkaline = "e",
                                             High = "f", Middle = "f", Low = "f", Extreme = "f",
                                             Cutting = "g", Seedling = "g", Plant = "g")


d_leaf_hms_fig$factor = factor(d_leaf_hms_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))

d_leaf_hms_fig$Sig <- ifelse(d_leaf_hms_fig$pval < 0.001, "***", ifelse(d_leaf_hms_fig$pval < 0.01, "**", ifelse(d_leaf_hms_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_leaf_hms_fig$estimate <- make_pct(d_leaf_hms_fig$estimate)
d_leaf_hms_fig$ci.lb <- make_pct(d_leaf_hms_fig$ci.lb)
d_leaf_hms_fig$ci.ub <- make_pct(d_leaf_hms_fig$ci.ub)
#write.csv(d_leaf_hms_fig, "d_leaf_hms_fig.csv")
#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.8c = 
  ggplot(data=d_leaf_hms_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= - 900,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='Leaf HMs concentration')+
  theme_classic()+
  scale_y_continuous(limits = c(-1250,2500), breaks=seq(-1250,2500, by=1250))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
#save pdf
#ggsave("Fig.2g LTHs.pdf", Fig.8c, width = 6, height = 8)






####   Stem HMs concentration  ####

#removed the NA in yi and vi
es_hms_stem <- drop_na(es_hms_stem, yi)


# 1、Experimental_method
res_meta_stem_hms_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_em_n <- es_hms_stem %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_stem_hms_em_reml <- mod_results(res_meta_stem_hms_em, mod = "Experimental_method", group = "Study_ID", data = es_hms_stem)
res_meta_stem_hms_em_reml <- coef(summary(res_meta_stem_hms_em)) %>% mutate(factor = levels(as.factor(es_hms_stem$Experimental_method)), Index = "stem_hms biomass", size = (res_meta_stem_hms_em_n$n))

# 2、Functional group
res_meta_stem_hms_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_fg_n <- es_hms_stem %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_stem_hms_fg_reml <- mod_results(res_meta_stem_hms_fg, mod = "Functional_group", group = "Study_ID", data = es_hms_stem)
res_meta_stem_hms_fg_reml <- coef(summary(res_meta_stem_hms_fg)) %>% mutate(factor = levels(as.factor(es_hms_stem$Functional_group)), Index = "stem_hms biomass", size = (res_meta_stem_hms_fg_n$n))

# 3、stress_time
res_meta_stem_hms_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_st_n <- es_hms_stem %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_stem_hms_st_reml <- mod_results(res_meta_stem_hms_st, mod = "Stress_time", group = "Study_ID", data = es_hms_stem)
res_meta_stem_hms_st_reml <- coef(summary(res_meta_stem_hms_st)) %>% mutate(factor = levels(as.factor(es_hms_stem$Stress_time)), Index = "stem_hms biomass", size = (res_meta_stem_hms_st_n$n))


# 4、Tree_age
res_meta_stem_hms_ta <- rma.mv(yi, vi, mods = ~Tree_age, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_ta <- rma.mv(yi, vi, mods = ~Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_ta_n <- es_hms_stem %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_stem_hms_ta_reml <- mod_results(res_meta_stem_hms_ta, mod = "Tree_age", group = "Study_ID", data = es_hms_stem)
res_meta_stem_hms_ta_reml <- coef(summary(res_meta_stem_hms_ta)) %>% mutate(factor = levels(as.factor(es_hms_stem$Tree_age)), Index = "stem_hms biomass", size = (res_meta_stem_hms_ta_n$n))


# 5、pH
res_meta_stem_hms_ph <- rma.mv(yi, vi, mods = ~pH, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_ph <- rma.mv(yi, vi, mods = ~pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_ph_n <- es_hms_stem %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_stem_hms_ph_reml <- mod_results(res_meta_stem_hms_ph, mod = "pH", group = "Study_ID", data = es_hms_stem)
res_meta_stem_hms_ph_reml <- coef(summary(res_meta_stem_hms_ph)) %>% mutate(factor = levels(as.factor(es_hms_stem$pH)), Index = "stem_hms biomass", size = (res_meta_stem_hms_ph_n$n))


# 6、HMs_level
res_meta_stem_hms_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_hms_n <- es_hms_stem %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_stem_hms_hms_reml <- mod_results(res_meta_stem_hms_hms, mod = "HMs_level", group = "Study_ID", data = es_hms_stem)
res_meta_stem_hms_hms_reml <- coef(summary(res_meta_stem_hms_hms)) %>% mutate(factor = levels(as.factor(es_hms_stem$HMs_level)), Index = "stem_hms biomass", size = (res_meta_stem_hms_hms_n$n))


# 7、Plant_method
res_meta_stem_hms_pm <- rma.mv(yi, vi, mod = ~Plant_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_pm <- rma.mv(yi, vi, mod = ~Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_stem)
res_meta_stem_hms_pm_n <- es_hms_stem %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_stem_hms_pm_reml <- mod_results(res_meta_stem_hms_pm, mod = "Plant_method", group = "Study_ID", data = es_hms_stem)
res_meta_stem_hms_pm_reml <- coef(summary(res_meta_stem_hms_pm)) %>% mutate(factor = levels(as.factor(es_hms_stem$Plant_method)), Index = "stem_hms biomass", size = (res_meta_stem_hms_pm_n$n))


###merge data
d_stem_hms_fig <- rbind(res_meta_stem_hms_em_reml,
                        res_meta_stem_hms_fg_reml,
                        res_meta_stem_hms_st_reml,
                        res_meta_stem_hms_ta_reml,
                        res_meta_stem_hms_ph_reml,
                        res_meta_stem_hms_hms_reml,
                        res_meta_stem_hms_pm_reml)

# Added the group, label and significantly levels

d_stem_hms_fig$group = dplyr::recode_factor(d_stem_hms_fig$factor,
                                            Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                            Shrub = "b",Tree = "b",
                                            More_six_month = "c", Three_month = "c", Six_month = "c",
                                            More_two_year = "d", Two_year = "d", One_year = "d",
                                            Acidic = "e", Neutral = "e", Alkaline = "e",
                                            High = "f", Middle = "f", Low = "f", Extreme = "f",
                                            Cutting = "g", Seedling = "g", Plant = "g")

d_stem_hms_fig$factor = factor(d_stem_hms_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))
d_stem_hms_fig$Sig <- ifelse(d_stem_hms_fig$pval < 0.001, "***", ifelse(d_stem_hms_fig$pval < 0.01, "**", ifelse(d_stem_hms_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_stem_hms_fig$estimate <- make_pct(d_stem_hms_fig$estimate)
d_stem_hms_fig$ci.lb <- make_pct(d_stem_hms_fig$ci.lb)
d_stem_hms_fig$ci.ub <- make_pct(d_stem_hms_fig$ci.ub)

#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.8d = 
  ggplot(data=d_stem_hms_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= - 900,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='Stem HMs concentration')+
  theme_classic()+
  scale_y_continuous(limits = c(-1250,2500), breaks=seq(-1250,2500, by=1250))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
#save pdf






####   Root HMs concentration  ####

#removed the NA in yi and vi
es_hms_root <- drop_na(es_hms_under, yi)


# 1、Experimental_method
res_meta_root_hms_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_em_n <- es_hms_root %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_root_hms_em_reml <- mod_results(res_meta_root_hms_em, mod = "Experimental_method", group = "Study_ID", data = es_hms_root)
res_meta_root_hms_em_reml <- coef(summary(res_meta_root_hms_em)) %>% mutate(factor = levels(as.factor(es_hms_root$Experimental_method)), Index = "root_hms biomass", size = (res_meta_root_hms_em_n$n))

# 2、Functional group
res_meta_root_hms_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_fg_n <- es_hms_root %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_root_hms_fg_reml <- mod_results(res_meta_root_hms_fg, mod = "Functional_group", group = "Study_ID", data = es_hms_root)
res_meta_root_hms_fg_reml <- coef(summary(res_meta_root_hms_fg)) %>% mutate(factor = levels(as.factor(es_hms_root$Functional_group)), Index = "root_hms biomass", size = (res_meta_root_hms_fg_n$n))

# 3、stress_time
res_meta_root_hms_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_st_n <- es_hms_root %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_root_hms_st_reml <- mod_results(res_meta_root_hms_st, mod = "Stress_time", group = "Study_ID", data = es_hms_root)
res_meta_root_hms_st_reml <- coef(summary(res_meta_root_hms_st)) %>% mutate(factor = levels(as.factor(es_hms_root$Stress_time)), Index = "root_hms biomass", size = (res_meta_root_hms_st_n$n))


# 4、Tree_age
res_meta_root_hms_ta <- rma.mv(yi, vi, mods = ~Tree_age, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_ta <- rma.mv(yi, vi, mods = ~Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_ta_n <- es_hms_root %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_root_hms_ta_reml <- mod_results(res_meta_root_hms_ta, mod = "Tree_age", group = "Study_ID", data = es_hms_root)
res_meta_root_hms_ta_reml <- coef(summary(res_meta_root_hms_ta)) %>% mutate(factor = levels(as.factor(es_hms_root$Tree_age)), Index = "root_hms biomass", size = (res_meta_root_hms_ta_n$n))


# 5、pH
res_meta_root_hms_ph <- rma.mv(yi, vi, mods = ~pH, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_ph <- rma.mv(yi, vi, mods = ~pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_ph_n <- es_hms_root %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_root_hms_ph_reml <- mod_results(res_meta_root_hms_ph, mod = "pH", group = "Study_ID", data = es_hms_root)
res_meta_root_hms_ph_reml <- coef(summary(res_meta_root_hms_ph)) %>% mutate(factor = levels(as.factor(es_hms_root$pH)), Index = "root_hms biomass", size = (res_meta_root_hms_ph_n$n))


# 6、HMs_level
res_meta_root_hms_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_hms_n <- es_hms_root %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_root_hms_hms_reml <- mod_results(res_meta_root_hms_hms, mod = "HMs_level", group = "Study_ID", data = es_hms_root)
res_meta_root_hms_hms_reml <- coef(summary(res_meta_root_hms_hms)) %>% mutate(factor = levels(as.factor(es_hms_root$HMs_level)), Index = "root_hms biomass", size = (res_meta_root_hms_hms_n$n))


# 7、Plant_method
res_meta_root_hms_pm <- rma.mv(yi, vi, mod = ~Plant_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_pm <- rma.mv(yi, vi, mod = ~Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_hms_root)
res_meta_root_hms_pm_n <- es_hms_root %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_root_hms_pm_reml <- mod_results(res_meta_root_hms_pm, mod = "Plant_method", group = "Study_ID", data = es_hms_root)
res_meta_root_hms_pm_reml <- coef(summary(res_meta_root_hms_pm)) %>% mutate(factor = levels(as.factor(es_hms_root$Plant_method)), Index = "root_hms biomass", size = (res_meta_root_hms_pm_n$n))


###merge data
d_root_hms_fig <- rbind(res_meta_root_hms_em_reml,
                        res_meta_root_hms_fg_reml,
                        res_meta_root_hms_st_reml,
                        res_meta_root_hms_ta_reml,
                        res_meta_root_hms_ph_reml,
                        res_meta_root_hms_hms_reml,
                        res_meta_root_hms_pm_reml)

# Added the group, label and significantly levels

d_root_hms_fig$group = dplyr::recode_factor(d_root_hms_fig$factor,
                                            Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                            Shrub = "b",Tree = "b",
                                            More_six_month = "c", Three_month = "c", Six_month = "c",
                                            More_two_year = "d", Two_year = "d", One_year = "d",
                                            Acidic = "e", Neutral = "e", Alkaline = "e",
                                            High = "f", Middle = "f", Low = "f", Extreme = "f",
                                            Cutting = "g", Seedling = "g", Plant = "g")

d_root_hms_fig$factor = factor(d_root_hms_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))
d_root_hms_fig$Sig <- ifelse(d_root_hms_fig$pval < 0.001, "***", ifelse(d_root_hms_fig$pval < 0.01, "**", ifelse(d_root_hms_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_root_hms_fig$estimate <- make_pct(d_root_hms_fig$estimate)
d_root_hms_fig$ci.lb <- make_pct(d_root_hms_fig$ci.lb)
d_root_hms_fig$ci.ub <- make_pct(d_root_hms_fig$ci.ub)
#write.csv(d_root_hms_fig, "d_root_hms_fig.csv")
#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.8e = 
  ggplot(data=d_root_hms_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= - 2000,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='Underground HMs concentration')+
  theme_classic()+
  scale_y_continuous(limits = c(-2500,5000), breaks=seq(-2500,5000, by=2500))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
#save pdf
#ggsave("Fig.2h UTHs.pdf", Fig.8e, width = 6, height = 8)



####################   Fig.9   SOD/POD/CAT/MDA   ############

############   SOD  ##################
#Removed NA in yi
es_sod_total <- drop_na(es_sod_total, yi)

# 1、Experimental_method
res_meta_sod_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_em_n <- es_sod_total %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_sod_em_reml <- mod_results(res_meta_sod_em, mod = "Experimental_method", group = "Study_ID", data = es_sod_total)
res_meta_sod_em_reml <- coef(summary(res_meta_sod_em)) %>% mutate(factor = levels(as.factor(es_sod_total$Experimental_method)), Index = "sod", size = (res_meta_sod_em_n$n))

# 2、Functional group
res_meta_sod_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_fg_n <- es_sod_total %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_sod_fg_reml <- mod_results(res_meta_sod_fg, mod = "Functional_group", group = "Study_ID", data = es_sod_total)
res_meta_sod_fg_reml <- coef(summary(res_meta_sod_fg)) %>% mutate(factor = levels(as.factor(es_sod_total$Functional_group)), Index = "sod", size = (res_meta_sod_fg_n$n))

# 3、stress_time
res_meta_sod_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_st_n <- es_sod_total %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_sod_st_reml <- mod_results(res_meta_sod_st, mod = "Stress_time", group = "Study_ID", data = es_sod_total)
res_meta_sod_st_reml <- coef(summary(res_meta_sod_st)) %>% mutate(factor = levels(as.factor(es_sod_total$Stress_time)), Index = "sod", size = (res_meta_sod_st_n$n))


# 4、Tree_age
res_meta_sod_ta <- rma.mv(yi, vi, mods = ~Tree_age, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_ta <- rma.mv(yi, vi, mods = ~Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_ta_n <- es_sod_total %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_sod_ta_reml <- mod_results(res_meta_sod_ta, mod = "Tree_age", group = "Study_ID", data = es_sod_total)
res_meta_sod_ta_reml <- coef(summary(res_meta_sod_ta)) %>% mutate(factor = levels(as.factor(es_sod_total$Tree_age)), Index = "sod", size = (res_meta_sod_ta_n$n))


# 5、pH
res_meta_sod_ph <- rma.mv(yi, vi, mods = ~pH, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_ph <- rma.mv(yi, vi, mods = ~pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_ph_n <- es_sod_total %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_sod_ph_reml <- mod_results(res_meta_sod_ph, mod = "pH", group = "Study_ID", data = es_sod_total)
res_meta_sod_ph_reml <- coef(summary(res_meta_sod_ph)) %>% mutate(factor = levels(as.factor(es_sod_total$pH)), Index = "sod", size = (res_meta_sod_ph_n$n))


# 6、HMs_level
res_meta_sod_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_hms_n <- es_sod_total %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_sod_hms_reml <- mod_results(res_meta_sod_hms, mod = "HMs_level", group = "Study_ID", data = es_sod_total)
res_meta_sod_hms_reml <- coef(summary(res_meta_sod_hms)) %>% mutate(factor = levels(as.factor(es_sod_total$HMs_level)), Index = "sod", size = (res_meta_sod_hms_n$n))


# 7、Plant_method
res_meta_sod_pm <- rma.mv(yi, vi, mod = ~Plant_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_pm <- rma.mv(yi, vi, mod = ~Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_sod_total)
res_meta_sod_pm_n <- es_sod_total %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_sod_pm_reml <- mod_results(res_meta_sod_pm, mod = "Plant_method", group = "Study_ID", data = es_sod_total)
res_meta_sod_pm_reml <- coef(summary(res_meta_sod_pm)) %>% mutate(factor = levels(as.factor(es_sod_total$Plant_method)), Index = "sod", size = (res_meta_sod_pm_n$n))


###merge data
d_sod_fig <- rbind(res_meta_sod_em_reml,
                        res_meta_sod_fg_reml,
                        res_meta_sod_st_reml,
                        res_meta_sod_ta_reml,
                        res_meta_sod_ph_reml,
                        res_meta_sod_hms_reml,
                        res_meta_sod_pm_reml)

# Added the group, label and significantly levels

d_sod_fig$group = dplyr::recode_factor(d_sod_fig$factor,
                                            Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                            Shrub = "b",Tree = "b",
                                            More_six_month = "c", Three_month = "c", Six_month = "c",
                                            More_two_year = "d", Two_year = "d", One_year = "d",
                                            Acidic = "e", Neutral = "e", Alkaline = "e",
                                            High = "f", Middle = "f", Low = "f", Extreme = "f",
                                            Cutting = "g", Seedling = "g", Plant = "g")

d_sod_fig$factor = factor(d_sod_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))
d_sod_fig$Sig <- ifelse(d_sod_fig$pval < 0.001, "***", ifelse(d_sod_fig$pval < 0.01, "**", ifelse(d_sod_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_sod_fig$estimate <- make_pct(d_sod_fig$estimate)
d_sod_fig$ci.lb <- make_pct(d_sod_fig$ci.lb)
d_sod_fig$ci.ub <- make_pct(d_sod_fig$ci.ub)
#write.csv(d_sod_fig, "d_sod_fig.csv")
#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.9a = 
  ggplot(data=d_sod_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= - 150,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='SOD')+
  theme_classic()+
  scale_y_continuous(limits = c(-200,300), breaks=seq(-200,300, by=100))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
#save pdf
#ggsave("Fig.7a 0625.pdf", Fig.9a, width = 6, height = 8)




############   POD  ##################
#Removed NA in yi
es_pod_total <- drop_na(es_pod_leaf, yi)

# 1、Experimental_method
res_meta_pod_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_em_n <- es_pod_total %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_pod_em_reml <- mod_results(res_meta_pod_em, mod = "Experimental_method", group = "Study_ID", data = es_pod_total)
res_meta_pod_em_reml <- coef(summary(res_meta_pod_em)) %>% mutate(factor = levels(as.factor(es_pod_total$Experimental_method)), Index = "pod", size = (res_meta_pod_em_n$n))

# 2、Functional group
res_meta_pod_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_fg_n <- es_pod_total %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_pod_fg_reml <- mod_results(res_meta_pod_fg, mod = "Functional_group", group = "Study_ID", data = es_pod_total)
res_meta_pod_fg_reml <- coef(summary(res_meta_pod_fg)) %>% mutate(factor = levels(as.factor(es_pod_total$Functional_group)), Index = "pod", size = (res_meta_pod_fg_n$n))

# 3、stress_time
res_meta_pod_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_st_n <- es_pod_total %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_pod_st_reml <- mod_results(res_meta_pod_st, mod = "Stress_time", group = "Study_ID", data = es_pod_total)
res_meta_pod_st_reml <- coef(summary(res_meta_pod_st)) %>% mutate(factor = levels(as.factor(es_pod_total$Stress_time)), Index = "pod", size = (res_meta_pod_st_n$n))


# 4、Tree_age
res_meta_pod_ta <- rma.mv(yi, vi, mods = ~Tree_age, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_ta <- rma.mv(yi, vi, mods = ~Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_ta_n <- es_pod_total %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_pod_ta_reml <- mod_results(res_meta_pod_ta, mod = "Tree_age", group = "Study_ID", data = es_pod_total)
res_meta_pod_ta_reml <- coef(summary(res_meta_pod_ta)) %>% mutate(factor = levels(as.factor(es_pod_total$Tree_age)), Index = "pod", size = (res_meta_pod_ta_n$n))


# 5、pH
res_meta_pod_ph <- rma.mv(yi, vi, mods = ~pH, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_ph <- rma.mv(yi, vi, mods = ~pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_ph_n <- es_pod_total %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_pod_ph_reml <- mod_results(res_meta_pod_ph, mod = "pH", group = "Study_ID", data = es_pod_total)
res_meta_pod_ph_reml <- coef(summary(res_meta_pod_ph)) %>% mutate(factor = levels(as.factor(es_pod_total$pH)), Index = "pod", size = (res_meta_pod_ph_n$n))


# 6、HMs_level
res_meta_pod_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_hms_n <- es_pod_total %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_pod_hms_reml <- mod_results(res_meta_pod_hms, mod = "HMs_level", group = "Study_ID", data = es_pod_total)
res_meta_pod_hms_reml <- coef(summary(res_meta_pod_hms)) %>% mutate(factor = levels(as.factor(es_pod_total$HMs_level)), Index = "pod", size = (res_meta_pod_hms_n$n))


# 7、Plant_method
res_meta_pod_pm <- rma.mv(yi, vi, mod = ~Plant_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_pm <- rma.mv(yi, vi, mod = ~Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_pod_total)
res_meta_pod_pm_n <- es_pod_total %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_pod_pm_reml <- mod_results(res_meta_pod_pm, mod = "Plant_method", group = "Study_ID", data = es_pod_total)
res_meta_pod_pm_reml <- coef(summary(res_meta_pod_pm)) %>% mutate(factor = levels(as.factor(es_pod_total$Plant_method)), Index = "pod", size = (res_meta_pod_pm_n$n))


###merge data
d_pod_fig <- rbind(res_meta_pod_em_reml,
                   res_meta_pod_fg_reml,
                   res_meta_pod_st_reml,
                   res_meta_pod_ta_reml,
                   res_meta_pod_ph_reml,
                   res_meta_pod_hms_reml,
                   res_meta_pod_pm_reml)

# Added the group, label and significantly levels

d_pod_fig$group = dplyr::recode_factor(d_pod_fig$factor,
                                       Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                       Shrub = "b",Tree = "b",
                                       More_six_month = "c", Three_month = "c", Six_month = "c",
                                       More_two_year = "d", Two_year = "d", One_year = "d",
                                       Acidic = "e", Neutral = "e", Alkaline = "e",
                                       High = "f", Middle = "f", Low = "f", Extreme = "f",
                                       Cutting = "g", Seedling = "g", Plant = "g")

d_pod_fig$factor = factor(d_pod_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))
d_pod_fig$Sig <- ifelse(d_pod_fig$pval < 0.001, "***", ifelse(d_pod_fig$pval < 0.01, "**", ifelse(d_pod_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_pod_fig$estimate <- make_pct(d_pod_fig$estimate)
d_pod_fig$ci.lb <- make_pct(d_pod_fig$ci.lb)
d_pod_fig$ci.ub <- make_pct(d_pod_fig$ci.ub)
#write.csv(d_pod_fig, "d_pod_fig.csv")
#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.9b = 
  ggplot(data=d_pod_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= - 250,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='POD')+
  theme_classic()+
  scale_y_continuous(limits = c(-300,300), breaks=seq(-200,200, by=100))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
#save pdf
#ggsave("Fig.7b POD.pdf", Fig.9b, width = 6, height = 8)





############   CAT  ##################
#Removed NA in yi
es_cat_total <- drop_na(es_cat_leaf, yi)

# 1、Experimental_method
res_meta_cat_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_em_n <- es_cat_total %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_cat_em_reml <- mod_results(res_meta_cat_em, mod = "Experimental_method", group = "Study_ID", data = es_cat_total)
res_meta_cat_em_reml <- coef(summary(res_meta_cat_em)) %>% mutate(factor = levels(as.factor(es_cat_total$Experimental_method)), Index = "cat", size = (res_meta_cat_em_n$n))

# 2、Functional group
res_meta_cat_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_fg_n <- es_cat_total %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_cat_fg_reml <- mod_results(res_meta_cat_fg, mod = "Functional_group", group = "Study_ID", data = es_cat_total)
res_meta_cat_fg_reml <- coef(summary(res_meta_cat_fg)) %>% mutate(factor = levels(as.factor(es_cat_total$Functional_group)), Index = "cat", size = (res_meta_cat_fg_n$n))

# 3、stress_time
res_meta_cat_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_st_n <- es_cat_total %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_cat_st_reml <- mod_results(res_meta_cat_st, mod = "Stress_time", group = "Study_ID", data = es_cat_total)
res_meta_cat_st_reml <- coef(summary(res_meta_cat_st)) %>% mutate(factor = levels(as.factor(es_cat_total$Stress_time)), Index = "cat", size = (res_meta_cat_st_n$n))


# 4、Tree_age
res_meta_cat_ta <- rma.mv(yi, vi, mods = ~Tree_age, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_ta <- rma.mv(yi, vi, mods = ~Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_ta_n <- es_cat_total %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_cat_ta_reml <- mod_results(res_meta_cat_ta, mod = "Tree_age", group = "Study_ID", data = es_cat_total)
res_meta_cat_ta_reml <- coef(summary(res_meta_cat_ta)) %>% mutate(factor = levels(as.factor(es_cat_total$Tree_age)), Index = "cat", size = (res_meta_cat_ta_n$n))


# 5、pH
res_meta_cat_ph <- rma.mv(yi, vi, mods = ~pH, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_ph <- rma.mv(yi, vi, mods = ~pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_ph_n <- es_cat_total %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_cat_ph_reml <- mod_results(res_meta_cat_ph, mod = "pH", group = "Study_ID", data = es_cat_total)
res_meta_cat_ph_reml <- coef(summary(res_meta_cat_ph)) %>% mutate(factor = levels(as.factor(es_cat_total$pH)), Index = "cat", size = (res_meta_cat_ph_n$n))


# 6、HMs_level
res_meta_cat_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_hms_n <- es_cat_total %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_cat_hms_reml <- mod_results(res_meta_cat_hms, mod = "HMs_level", group = "Study_ID", data = es_cat_total)
res_meta_cat_hms_reml <- coef(summary(res_meta_cat_hms)) %>% mutate(factor = levels(as.factor(es_cat_total$HMs_level)), Index = "cat", size = (res_meta_cat_hms_n$n))


# 7、Plant_method
res_meta_cat_pm <- rma.mv(yi, vi, mod = ~Plant_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_pm <- rma.mv(yi, vi, mod = ~Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_cat_total)
res_meta_cat_pm_n <- es_cat_total %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_cat_pm_reml <- mod_results(res_meta_cat_pm, mod = "Plant_method", group = "Study_ID", data = es_cat_total)
res_meta_cat_pm_reml <- coef(summary(res_meta_cat_pm)) %>% mutate(factor = levels(as.factor(es_cat_total$Plant_method)), Index = "cat", size = (res_meta_cat_pm_n$n))


###merge data
d_cat_fig <- rbind(res_meta_cat_em_reml,
                   res_meta_cat_fg_reml,
                   res_meta_cat_st_reml,
                   res_meta_cat_ta_reml,
                   res_meta_cat_ph_reml,
                   res_meta_cat_hms_reml,
                   res_meta_cat_pm_reml)

# Added the group, label and significantly levels

d_cat_fig$group = dplyr::recode_factor(d_cat_fig$factor,
                                       Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                       Shrub = "b",Tree = "b",
                                       More_six_month = "c", Three_month = "c", Six_month = "c",
                                       More_two_year = "d", Two_year = "d", One_year = "d",
                                       Acidic = "e", Neutral = "e", Alkaline = "e",
                                       High = "f", Middle = "f", Low = "f", Extreme = "f",
                                       Cutting = "g", Seedling = "g", Plant = "g")

d_cat_fig$factor = factor(d_cat_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))
d_cat_fig$Sig <- ifelse(d_cat_fig$pval < 0.001, "***", ifelse(d_cat_fig$pval < 0.01, "**", ifelse(d_cat_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_cat_fig$estimate <- make_pct(d_cat_fig$estimate)
d_cat_fig$ci.lb <- make_pct(d_cat_fig$ci.lb)
d_cat_fig$ci.ub <- make_pct(d_cat_fig$ci.ub)
#write.csv(d_cat_fig, "d_cat_fig.csv")
#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.9c = 
  ggplot(data=d_cat_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= - 230,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='CAT')+
  theme_classic()+
  scale_y_continuous(limits = c(-300,600), breaks=seq(-300,600, by=300))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
#save pdf
#ggsave("Fig.7c CAT.pdf", Fig.9c, width = 6, height = 8)




############   MDA  ##################
#Removed NA in yi
es_mda_total <- drop_na(es_mda_leaf, yi)

# 1、Experimental_method
res_meta_mda_em <- rma.mv(yi, vi, mod = ~ Experimental_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_em <- rma.mv(yi, vi, mod = ~ Experimental_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_em_n <- es_mda_total %>% group_by(Experimental_method) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_mda_em_reml <- mod_results(res_meta_mda_em, mod = "Experimental_method", group = "Study_ID", data = es_mda_total)
res_meta_mda_em_reml <- coef(summary(res_meta_mda_em)) %>% mutate(factor = levels(as.factor(es_mda_total$Experimental_method)), Index = "mda", size = (res_meta_mda_em_n$n))

# 2、Functional group
res_meta_mda_fg <- rma.mv(yi, vi, mod = ~ Functional_group, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_fg <- rma.mv(yi, vi, mod = ~ Functional_group-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_fg_n <- es_mda_total %>% group_by(Functional_group) %>% summarise(n = n())  %>% na.omit() #removed NA
res_meta_mda_fg_reml <- mod_results(res_meta_mda_fg, mod = "Functional_group", group = "Study_ID", data = es_mda_total)
res_meta_mda_fg_reml <- coef(summary(res_meta_mda_fg)) %>% mutate(factor = levels(as.factor(es_mda_total$Functional_group)), Index = "mda", size = (res_meta_mda_fg_n$n))

# 3、stress_time
res_meta_mda_st <- rma.mv(yi, vi, mod = ~ Stress_time, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_st <- rma.mv(yi, vi, mod = ~ Stress_time-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_st_n <- es_mda_total %>% group_by(Stress_time) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_mda_st_reml <- mod_results(res_meta_mda_st, mod = "Stress_time", group = "Study_ID", data = es_mda_total)
res_meta_mda_st_reml <- coef(summary(res_meta_mda_st)) %>% mutate(factor = levels(as.factor(es_mda_total$Stress_time)), Index = "mda", size = (res_meta_mda_st_n$n))


# 4、Tree_age
res_meta_mda_ta <- rma.mv(yi, vi, mods = ~Tree_age, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_ta <- rma.mv(yi, vi, mods = ~Tree_age-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_ta_n <- es_mda_total %>% group_by(Tree_age) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_mda_ta_reml <- mod_results(res_meta_mda_ta, mod = "Tree_age", group = "Study_ID", data = es_mda_total)
res_meta_mda_ta_reml <- coef(summary(res_meta_mda_ta)) %>% mutate(factor = levels(as.factor(es_mda_total$Tree_age)), Index = "mda", size = (res_meta_mda_ta_n$n))


# 5、pH
res_meta_mda_ph <- rma.mv(yi, vi, mods = ~pH, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_ph <- rma.mv(yi, vi, mods = ~pH-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_ph_n <- es_mda_total %>% group_by(pH) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_mda_ph_reml <- mod_results(res_meta_mda_ph, mod = "pH", group = "Study_ID", data = es_mda_total)
res_meta_mda_ph_reml <- coef(summary(res_meta_mda_ph)) %>% mutate(factor = levels(as.factor(es_mda_total$pH)), Index = "mda", size = (res_meta_mda_ph_n$n))


# 6、HMs_level
res_meta_mda_hms <- rma.mv(yi, vi, mod = ~ HMs_level, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_hms <- rma.mv(yi, vi, mod = ~ HMs_level-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_hms_n <- es_mda_total %>% group_by(HMs_level) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_mda_hms_reml <- mod_results(res_meta_mda_hms, mod = "HMs_level", group = "Study_ID", data = es_mda_total)
res_meta_mda_hms_reml <- coef(summary(res_meta_mda_hms)) %>% mutate(factor = levels(as.factor(es_mda_total$HMs_level)), Index = "mda", size = (res_meta_mda_hms_n$n))


# 7、Plant_method
res_meta_mda_pm <- rma.mv(yi, vi, mod = ~Plant_method, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_pm <- rma.mv(yi, vi, mod = ~Plant_method-1, random = list(~1|Study_ID/Co_ID), method = "REML", data = es_mda_total)
res_meta_mda_pm_n <- es_mda_total %>% group_by(Plant_method) %>% summarise(n = n()) %>% na.omit() #removed NA
res_meta_mda_pm_reml <- mod_results(res_meta_mda_pm, mod = "Plant_method", group = "Study_ID", data = es_mda_total)
res_meta_mda_pm_reml <- coef(summary(res_meta_mda_pm)) %>% mutate(factor = levels(as.factor(es_mda_total$Plant_method)), Index = "mda", size = (res_meta_mda_pm_n$n))


###merge data
d_mda_fig <- rbind(res_meta_mda_em_reml,
                   res_meta_mda_fg_reml,
                   res_meta_mda_st_reml,
                   res_meta_mda_ta_reml,
                   res_meta_mda_ph_reml,
                   res_meta_mda_hms_reml,
                   res_meta_mda_pm_reml)

# Added the group, label and significantly levels

d_mda_fig$group = dplyr::recode_factor(d_mda_fig$factor,
                                       Field = "a", Hydroponics = "a", Steril_medium = "a", Potted = "a",
                                       Shrub = "b",Tree = "b",
                                       More_six_month = "c", Three_month = "c", Six_month = "c",
                                       More_two_year = "d", Two_year = "d", One_year = "d",
                                       Acidic = "e", Neutral = "e", Alkaline = "e",
                                       High = "f", Middle = "f", Low = "f", Extreme = "f",
                                       Cutting = "g", Seedling = "g", Plant = "g")

d_mda_fig$factor = factor(d_mda_fig$factor, levels = c("Hydroponics", "Steril_medium", "Potted", "Field", "Tree", "Shrub", "Three_month", "Six_month", "More_six_month", "One_year", "Two_year", "More_two_year", "Acidic", "Neutral","Alkaline","Low","Middle","High","Extreme","Seedling","Plant","Cutting"))
d_mda_fig$Sig <- ifelse(d_mda_fig$pval < 0.001, "***", ifelse(d_mda_fig$pval < 0.01, "**", ifelse(d_mda_fig$pval < 0.05, "*", "")))
mods_names <- as_labeller(c('a'="Experimental\nmethod",
                            'b'="Functional\ngroup",
                            'c'="Stress period",
                            'd'="Tree age",
                            'e'="pH",
                            'f'="HMs level",
                            'g'="Plant material"))

## Chenage (%)
make_pct <- function(x) (exp(x) - 1) * 100


d_mda_fig$estimate <- make_pct(d_mda_fig$estimate)
d_mda_fig$ci.lb <- make_pct(d_mda_fig$ci.lb)
d_mda_fig$ci.ub <- make_pct(d_mda_fig$ci.ub)
#write.csv(d_mda_fig, "d_mda_fig.csv")
#Experiment method、Functional group、Stress period、Tree age、pH、HMs level、Plant material
scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))


Fig.9d = 
  ggplot(data=d_mda_fig, aes(x=factor, y=estimate, ymin=ci.lb, ymax=ci.ub, color = group))+  
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#6BB492","#6495ED","#FF4500","#313695", "#3366CC"))+
  geom_pointrange(shape=20, size=1.5, position=position_dodge(width=c(0.1)))+
  geom_text(aes(x=factor, y= - 150,label=paste0("(",size,")", Sig)),size=4,fontface="bold",position = position_dodge(0.8))+
  coord_flip() +
  labs(x='Moderators', y='Change (%)', title='MDA')+
  theme_classic()+
  scale_y_continuous(limits = c(-200,300), breaks=seq(-200,300, by=100))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=14),
        strip.background = element_blank(),
        plot.title= element_text(size=14, hjust = 0.5),
        axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 14))
#save pdf
#ggsave("Fig.6 MDA.pdf", Fig.9d, width = 6, height = 8)






#####################    Supporting Information       ##########################



############################Fig.S3 Top country/genus/family#####################
############Fig.S3a country###############
#读入数据
con_data <- readxl::read_xlsx("data-24-0227.xlsx", sheet = "final-data")

#country
dat_con <- data.frame(table(con_data$Country))
dat_con$Rank = ifelse(dat_con$Freq >= 10, "Top","Low")


font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

c1 <-ggplot(dat_con, aes(x = reorder(Var1, -Freq),y = Freq))+
  geom_col(width = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  font +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))+
  labs(x = "Country", y = "Publish number")+
  geom_vline(xintercept = 10.5)

c2<-c1+scale_y_break(c(80, 1250),
                     space = 0.2,
                     scales = 0.4)
#ggsave("Country.pdf", width = 25, height = 5)
dat_con_top = subset(dat_con, Rank == "Top")
c11 <-ggplot(dat_con_top, aes(x = reorder(Var1, -Freq),y = Freq))+
  geom_col(width = 0.5)+
  theme_classic()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  font +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))+
  labs(x = "Country", y = "Publish numbers")+
  ggtitle("Top 14")+ theme(plot.title = element_text(hjust = 0.5, size = 18))


c22<-c11+scale_y_break(c(80,1250),#截断位置及范围
                       space = 0.2,#间距大小
                       scales = 0.4,
                       ticklabels=seq (1250,1270,10))#上下显示比例，大于1上面比例大，小于1下面比例大
pdf('Fig.S3a.pdf',height = 4, width =7, onefile = F)
c22
dev.off()


############Fig.S3b Family###############
#Family
dat_Family <- data.frame(table(con_data$Family))

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

f5 <-ggplot(dat_Family, aes(x = reorder(Var1, -Freq),y = Freq))+
  geom_col()+
  theme_classic()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  font +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
  labs(x = "Family", y = "Publish number")

f6<-f5+scale_y_break(c(150,580),#截断位置及范围
                     space = 0.2,#间距大小
                     scales = 0.4)#上下显示比例，大于1上面比例大，小于1下面比例大



dat_Family$Rank = ifelse(dat_Family$Freq >= 25, "Top", "Low")
dat_Family_TOP = subset(dat_Family, Rank == "Top")

c55 <-ggplot(dat_Family_TOP, aes(x = reorder(Var1, -Freq),y = Freq))+
  geom_col()+
  theme_classic()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  font +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
  labs(x = "Family", y = "Publish number")+
  ggtitle("Top 13")+ theme(plot.title = element_text(hjust = 0.5, size = 18))

ggsave("Fig.S3b.pdf",c55, width = 7, height = 4)

############Fig.S3c Genus###############
#Genus
dat_Genus <- data.frame(table(con_data$Genus))

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

g7 <-ggplot(dat_Genus, aes(x = reorder(Var1, -Freq),y = Freq))+
  geom_col()+
  theme_classic()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  font +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, face = "italic", hjust = 1, vjust = 1))+
  labs(x = "Genus", y = "Publish number")

g8<-g7+scale_y_break(c(210,410),#截断位置及范围
                     space = 0.2,#间距大小
                     scales = 0.4)#上下显示比例，大于1上面比例大，小于1下面比例大



dat_Genus$Rank = ifelse(dat_Genus$Freq >= 25, "Top", "Low")
dat_Genus_top = subset(dat_Genus, Rank == "Top")

g77 <-ggplot(dat_Genus_top, aes(x = reorder(Var1, -Freq),y = Freq))+
  geom_col()+
  theme_classic()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  font +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  theme(axis.text.x = element_text(angle = 30, face = "italic", hjust = 1, vjust = 1))+
  labs(x = "Genus", y = "Publish numbers")+
  ggtitle("Top 14")+ theme(plot.title = element_text(hjust = 0.5, size = 18))


#ggsave("Fig.S3c.pdf",g77, width = 7, height = 4)



##################################  Relationship  ##############################
library(ggpmisc)
library(readxl)
library(ggplot2)
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

library(ggpmisc)
library(readxl)
library(ggplot2)
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
###### Biomass #########
d_lnrr <- read_xlsx("data-24-0227.xlsx", sheet = "all_lnRR")
d_lnrr <- subset(d_lnrr, pHs != "Unknow")
d_lnrr$EMS <- ifelse(d_lnrr$Experimental_method == "Hydroponics", "Hydroponics", "Soil/Steril_medium")

str(d_lnrr)


p81 <- ggplot(d_lnrr, aes(pHs,TB))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, TB,label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Total biomass)")+
  scale_y_continuous(limits = c(-1.5,1.0), breaks=seq(-1.5,1.0,by=1)) +
  scale_x_continuous(limits = c(4,9), breaks=seq(4,9,by=1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


p82 = ggplot(d_lnrr, aes(pHs,AB))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, AB,label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Aboveground biomass)")+
  scale_y_continuous(limits = c(-2.5,1.0), breaks=seq(-2.5,1.0,by=1)) +
  scale_x_continuous(limits = c(3,9), breaks=seq(3,9,by=1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))



p83 = ggplot(d_lnrr, aes(pHs,LB))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, LB, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Leaf biomass)")+
  scale_y_continuous(limits = c(-1.5,1.5), breaks=seq(-1.5,1.5,by=1)) +
  scale_x_continuous(limits = c(3,9), breaks=seq(3,9,by=1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


p84 = ggplot(d_lnrr, aes(pHs,SB))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, SB, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Stem biomass)")+
  scale_y_continuous(limits = c(-2,1.5), breaks=seq(-2,1.5,by=1)) +
  scale_x_continuous(limits = c(3,9), breaks=seq(3,9,by=1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


p85 = ggplot(d_lnrr, aes(pHs,RB))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, RB, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Underground biomass)")+
  scale_y_continuous(limits = c(-2,1.2), breaks=seq(-2,1.2,by=1)) +
  scale_x_continuous(limits = c(3,9), breaks=seq(3,9,by=1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

##relationship between pH and biomass
#p15 <- plot_grid(p1, p2, p3,  p5, ncol = 2, align = "hv", labels = c("(a)", "(b)", "(c)", "(d)"))
#p15

#ggsave("Fig.9.pdf", p15, width = 13, height = 9)

###### THMs #########

p86 = ggplot(d_lnrr, aes(pHs,THMs))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, THMs, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Total HMs concentration)")+
  scale_y_continuous(limits = c(2, 6), breaks=seq(2, 6, by = 2)) +
  scale_x_continuous(limits = c(4.5, 6), breaks=seq(4.5, 6,by = 0.5)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


p87 = ggplot(d_lnrr, aes(pHs,AHMs))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, AHMs, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Above HMs concentration)")+
  scale_y_continuous(limits = c(-0.2, 6.7), breaks=seq(-0.2, 6.7, by = 2)) +
  scale_x_continuous(limits = c(4.5, 9), breaks=seq(4.5, 9,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


p88 = ggplot(d_lnrr, aes(pHs,LHMs))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, LHMs, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Leaf HMs concentration)")+
  scale_y_continuous(limits = c(-2.5, 6), breaks=seq(-2.5, 6, by = 2)) +
  scale_x_continuous(limits = c(4.5, 9), breaks=seq(4.5, 9,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


p89 = ggplot(d_lnrr, aes(pHs,SHMs))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, SHMs, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "top", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Stem HMs concentration)")+
  scale_y_continuous(limits = c(-0.5, 6.5), breaks=seq(-0.5, 6.5, by = 2)) +
  scale_x_continuous(limits = c(4, 9), breaks=seq(4, 9,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))



p810 = ggplot(d_lnrr, aes(pHs,RHMs))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, RHMs, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "top", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Under HMs concentration)")+
  scale_y_continuous(limits = c(-0.5, 10), breaks=seq(-0.5, 10, by = 2.5)) +
  scale_x_continuous(limits = c(4, 9), breaks=seq(4, 9,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


##relationship between pH and HMs
#p610 <- plot_grid(p6, p7, p8, p10, ncol = 2, align = "hv", labels = c("(a)", "(b)", "(c)", "(d)"))
#p610

#ggsave("Fig.10.pdf", p610, width = 13, height = 9)

p811 = ggplot(d_lnrr, aes(pHs,SOD))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, SOD, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(SOD)")+
  scale_y_continuous(limits = c(-1.5, 1.5), breaks=seq(-1.5, 1.5, by = 1)) +
  scale_x_continuous(limits = c(5, 8.5), breaks=seq(5, 8.5,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))



p812 = ggplot(d_lnrr, aes(pHs,POD))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, POD, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(POD)")+
  scale_y_continuous(limits = c(-2, 1.5), breaks=seq(-2, 1.5, by = 1)) +
  scale_x_continuous(limits = c(5, 8), breaks=seq(5, 8,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


p813 = ggplot(d_lnrr, aes(pHs,CAT))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, CAT, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(POD)")+
  scale_y_continuous(limits = c(-3, 2), breaks=seq(-3, 2, by = 1)) +
  scale_x_continuous(limits = c(5.5, 8), breaks=seq(5.5, 8,by = 0.5)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


p814 = ggplot(d_lnrr, aes(pHs,MDA))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, MDA, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(MDA)")+
  scale_y_continuous(limits = c(-1, 2), breaks=seq(-1, 2, by = 1)) +
  scale_x_continuous(limits = c(5.5, 8), breaks=seq(5.5, 8,by = 0.5)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))



p815 = ggplot(d_lnrr, aes(LHMs, SOD))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(LHMs, SOD, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(LHMs)", y = "LnRR(SOD)")+
  scale_y_continuous(limits = c(-1.5, 1.5), breaks=seq(-1.5, 1.5, by = 1)) +
  scale_x_continuous(limits = c(-0.5, 4.5), breaks=seq(-0.5, 4.5, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))



p816 = ggplot(d_lnrr, aes(LHMs, POD))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(LHMs, POD, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(LHMs)", y = "LnRR(POD)")+
  scale_y_continuous(limits = c(-2, 1.5), breaks=seq(-2, 1.5, by = 1)) +
  scale_x_continuous(limits = c(-0.5, 4), breaks=seq(-0.5, 4, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

p817 = ggplot(d_lnrr, aes(LHMs, CAT))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(LHMs, CAT,  label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(LHMs)", y = "LnRR(CAT)")+
  scale_y_continuous(limits = c(-1, 2), breaks=seq(-1, 2, by = 1)) +
  scale_x_continuous(limits = c(-0.5, 4), breaks=seq(-0.5, 4, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


p818 = ggplot(d_lnrr, aes(LHMs, MDA))+
  geom_point(size=6,pch=1, col="red")+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(LHMs, MDA, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(LHMs)", y = "LnRR(MDA)")+
  scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  scale_x_continuous(limits = c(-0.5, 4), breaks=seq(-0.5, 4, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

library(cowplot)
##relationship between pH and HMs
p81118 <- plot_grid(p81, p82, p83, p85, 
                    p86, p87,p88,p810,
                    p811, p812, p813, p814,
                    p815, p816, p817, p818, ncol = 4, align = "hv",
                    labels = c("(a)", "(b)", "(c)", "(d)",
                               "(e)", "(f)", "(g)", "(h)",
                               "(i)", "(j)", "(k)", "(l)",
                               "(m)", "(n)", "(o)", "(p)"),
                    label_size = 24)
p81118
#ggsave("Figure 8.pdf", p81118, width = 20, height = 18)



#################################  Figure S   ##################################
###### Biomass #########
d_lnrr <- read_xlsx("data-24-0227.xlsx", sheet = "all_lnRR")
d_lnrr <- subset(d_lnrr, pHs != "Unknow")
d_lnrr$EMS <- ifelse(d_lnrr$Experimental_method == "Hydroponics", "Hydroponics", "Soil/Steril_medium")

str(d_lnrr)

m1 <- lm(TB ~ pHs, data = d_lnrr)
summary(m1)
p1 <- ggplot(d_lnrr, aes(pHs,TB,color = EMS))+
  geom_point(size=6,pch=1, aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, TB,color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Total biomass)")+
  scale_y_continuous(limits = c(-1.5,1.0), breaks=seq(-1.5,1.0,by=1)) +
  scale_x_continuous(limits = c(4,9), breaks=seq(4,9,by=1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))



m2 <- lm(AB ~ pHs, data = d_lnrr)
summary(m2)
p2 = ggplot(d_lnrr, aes(pHs,AB, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, AB,color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Aboveground biomass)")+
  scale_y_continuous(limits = c(-2.5,1.0), breaks=seq(-2.5,1.0,by=1)) +
  scale_x_continuous(limits = c(3,9), breaks=seq(3,9,by=1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))



m3 <- lm(LB ~ pHs, data = d_lnrr)
summary(m3)
p3 = ggplot(d_lnrr, aes(pHs,LB, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, LB, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Leaf biomass)")+
  scale_y_continuous(limits = c(-1.5,1.5), breaks=seq(-1.5,1.5,by=1)) +
  scale_x_continuous(limits = c(3,9), breaks=seq(3,9,by=1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


m4 <- lm(SB ~ pHs, data = d_lnrr)
summary(m4)
p4 = ggplot(d_lnrr, aes(pHs,SB, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, SB, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Stem biomass)")+
  scale_y_continuous(limits = c(-2,1.5), breaks=seq(-2,1.5,by=1)) +
  scale_x_continuous(limits = c(3,9), breaks=seq(3,9,by=1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


m5 <- lm(RB ~ pHs, data = d_lnrr)
summary(m5)
p5 = ggplot(d_lnrr, aes(pHs,RB, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, RB,color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Underground biomass)")+
  scale_y_continuous(limits = c(-2,1.2), breaks=seq(-2,1.2,by=1)) +
  scale_x_continuous(limits = c(3,9), breaks=seq(3,9,by=1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

##relationship between pH and biomass
#p15 <- plot_grid(p1, p2, p3,  p5, ncol = 2, align = "hv", labels = c("(a)", "(b)", "(c)", "(d)"))
#p15

#ggsave("Fig.9.pdf", p15, width = 13, height = 9)

###### THMs #########

m6 <- lm(THMs ~ pHs, data = d_lnrr)
summary(m6)
p6 = ggplot(d_lnrr, aes(pHs,THMs, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, THMs, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Total HMs concentration)")+
  scale_y_continuous(limits = c(2, 6), breaks=seq(2, 6, by = 2)) +
  scale_x_continuous(limits = c(4.5, 6), breaks=seq(4.5, 6,by = 0.5)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

m7 <- lm(AHMs ~ pHs, data = d_lnrr)
summary(m7)
p7 = ggplot(d_lnrr, aes(pHs,AHMs, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, AHMs, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Above HMs concentration)")+
  scale_y_continuous(limits = c(-0.2, 6.7), breaks=seq(-0.2, 6.7, by = 2)) +
  scale_x_continuous(limits = c(4.5, 9), breaks=seq(4.5, 9,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


m8 <- lm(LHMs ~ pHs, data = d_lnrr)
summary(m8)
p8 = ggplot(d_lnrr, aes(pHs,LHMs, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, LHMs, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Leaf HMs concentration)")+
  scale_y_continuous(limits = c(-2.5, 6), breaks=seq(-2.5, 6, by = 2)) +
  scale_x_continuous(limits = c(4.5, 9), breaks=seq(4.5, 9,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


m9 <- lm(SHMs ~ pHs, data = d_lnrr)
summary(m9)
p9 = ggplot(d_lnrr, aes(pHs,SHMs, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, SHMs, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "top", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Stem HMs concentration)")+
  scale_y_continuous(limits = c(-0.5, 6.5), breaks=seq(-0.5, 6.5, by = 2)) +
  scale_x_continuous(limits = c(4, 9), breaks=seq(4, 9,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


m10 <- lm(RHMs ~ pHs, data = d_lnrr)
summary(m10)
p10 = ggplot(d_lnrr, aes(pHs,RHMs, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, RHMs, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "top", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(Under HMs concentration)")+
  scale_y_continuous(limits = c(-0.5, 10), breaks=seq(-0.5, 10, by = 2.5)) +
  scale_x_continuous(limits = c(4, 9), breaks=seq(4, 9,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


##relationship between pH and HMs
#p610 <- plot_grid(p6, p7, p8, p10, ncol = 2, align = "hv", labels = c("(a)", "(b)", "(c)", "(d)"))
#p610

#ggsave("Fig.10.pdf", p610, width = 13, height = 9)

m11 <- lm(SOD ~ pHs, data = d_lnrr)
summary(m11)
p11 = ggplot(d_lnrr, aes(pHs,SOD, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, SOD, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(SOD)")+
  scale_y_continuous(limits = c(-1.5, 1.5), breaks=seq(-1.5, 1.5, by = 1)) +
  scale_x_continuous(limits = c(5, 8.5), breaks=seq(5, 8.5,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


m12 <- lm(POD ~ pHs, data = d_lnrr)
summary(m12)
p12 = ggplot(d_lnrr, aes(pHs,POD, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, POD,color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(POD)")+
  scale_y_continuous(limits = c(-2, 1.5), breaks=seq(-2, 1.5, by = 1)) +
  scale_x_continuous(limits = c(5, 8), breaks=seq(5, 8,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))




m13 <- lm(CAT ~ pHs, data = d_lnrr)
summary(m13)
p13 = ggplot(d_lnrr, aes(pHs,CAT, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, CAT, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(POD)")+
  scale_y_continuous(limits = c(-3, 2), breaks=seq(-3, 2, by = 1)) +
  scale_x_continuous(limits = c(5.5, 8), breaks=seq(5.5, 8,by = 0.5)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


m14 <- lm(MDA ~ pHs, data = d_lnrr)
summary(m14)
p14 = ggplot(d_lnrr, aes(pHs,MDA, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, MDA, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(MDA)")+
  scale_y_continuous(limits = c(-1, 2), breaks=seq(-1, 2, by = 1)) +
  scale_x_continuous(limits = c(5.5, 8), breaks=seq(5.5, 8,by = 0.5)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


m15 <- lm(LHMs ~ SOD, data = d_lnrr)
summary(m15)
p15 = ggplot(d_lnrr, aes(LHMs, SOD,color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(LHMs, SOD,color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(LHMs)", y = "LnRR(SOD)")+
  scale_y_continuous(limits = c(-1.5, 1.5), breaks=seq(-1.5, 1.5, by = 1)) +
  scale_x_continuous(limits = c(-0.5, 4.5), breaks=seq(-0.5, 4.5, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


m16 <- lm(LHMs ~ POD, data = d_lnrr)
summary(m16)
p16 = ggplot(d_lnrr, aes(LHMs, POD, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(LHMs, POD, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(LHMs)", y = "LnRR(POD)")+
  scale_y_continuous(limits = c(-2, 1.5), breaks=seq(-2, 1.5, by = 1)) +
  scale_x_continuous(limits = c(-0.5, 4), breaks=seq(-0.5, 4, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

m17 <- lm(LHMs ~ CAT, data = d_lnrr)
summary(m17)

p17 = ggplot(d_lnrr, aes(LHMs, CAT, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(LHMs, CAT, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(LHMs)", y = "LnRR(CAT)")+
  scale_y_continuous(limits = c(-1, 2), breaks=seq(-1, 2, by = 1)) +
  scale_x_continuous(limits = c(-0.5, 4), breaks=seq(-0.5, 4, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


m18 <- lm(LHMs ~ MDA, data = d_lnrr)
summary(m18)
p18 = ggplot(d_lnrr, aes(LHMs, MDA, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(LHMs, MDA, color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(LHMs)", y = "LnRR(MDA)")+
  scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  scale_x_continuous(limits = c(-0.5, 4), breaks=seq(-0.5, 4, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

library(cowplot)
##relationship between pH and HMs
p1118 <- plot_grid(p1, p2, p3, p5, 
                   p6, p7,p8,p10,
                   p11, p12, p13, p14,
                   p15, p16, p17, p18, ncol = 4, align = "hv",
                   labels = c("(a)", "(b)", "(c)", "(d)",
                             "(e)", "(f)", "(g)", "(h)",
                             "(i)", "(j)", "(k)", "(l)",
                             "(m)", "(n)", "(o)", "(p)"),
                   label_size = 24)
p1118
p1118 <- ggpubr::ggarrange(p1, p2, p3, p5, 
                  p6, p7,p8,p10,
                  p11, p12, p13, p14,
                  p15, p16, p17, p18,
                  labels = c("(a)", "(b)", "(c)", "(d)",
                             "(e)", "(f)", "(g)", "(h)",
                             "(i)", "(j)", "(k)", "(l)",
                             "(m)", "(n)", "(o)", "(p)"),
                  label_size = 24, common.legend = T, legend = "bottom", ncol = 4, nrow = 4)
p1118


ggarrange(p1, p2, p3, p5, p6, p7, p8, p10, p11, p12, p13, p14, p15, p16, p17, p18, ncol = 4, nrow = 4, common.legend = T, legend = "bottom", labels = "auto", align = "hv")
#ggsave("Fig.10.pdf", p1118, width = 25, height = 18)




##########################  TI/TF/BCF  ########################################
str(d_lnrr)
a1 <- ggplot(d_lnrr, aes(MDA, TI))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(MDA, TI, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(MDA)", y = "Tolerance Index")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


a2 <- ggplot(d_lnrr, aes(SOD, TI))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(SOD, TI, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(SOD)", y = "Tolerance Index")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


a3 <- ggplot(d_lnrr, aes(POD, TI))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(POD, TI, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(POD)", y = "Tolerance Index")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


a4 <- ggplot(d_lnrr, aes(CAT, TI))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(CAT, TI, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(CAT)", y = "Tolerance Index")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


a5 <- ggplot(d_lnrr, aes(MDA, TF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(MDA, TF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(MDA)", y = "Translocation factor")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


a6 <- ggplot(d_lnrr, aes(SOD, TF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(SOD, TF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(SOD)", y = "Translocation factor")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


a7 <- ggplot(d_lnrr, aes(POD, TF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(POD, TF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(POD)", y = "Translocation factor")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


a8 <- ggplot(d_lnrr, aes(CAT, TF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(CAT, TF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(CAT)", y = "Translocation factor")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))



a9 <- ggplot(d_lnrr, aes(MDA, ABCF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(MDA, ABCF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(MDA)", y = "Bioconcentration factor")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


a10 <- ggplot(d_lnrr, aes(SOD, ABCF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(SOD, ABCF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(SOD)", y = "Bioconcentration factor")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


a11 <- ggplot(d_lnrr, aes(POD, ABCF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(POD, ABCF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(POD)", y = "Bioconcentration factor")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

a12 <- ggplot(d_lnrr, aes(CAT, ABCF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(CAT, ABCF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(CAT)", y = "Bioconcentration factor")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


re_index <- ggarrange(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, common.legend = T, legend = "bottom", align = "hv", labels = c("(a)", "(b)", "(c)", "(d)","(e)", "(f)", "(g)", "(h)", "(i)", "(j)", "(k)", "(l)"), font.label = list(size = 24, color = "black"), ncol = 3, nrow = 4)
ggsave("Figure S30 relationship index.pdf", re_index, width = 11, height = 14)



###########################  Relationship between TI and pH   ##################
b1 <- ggplot(d_lnrr, aes(pHs, TI))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, TI, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "top", label.x = "left")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "TI")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


b2 <- ggplot(d_lnrr, aes(pHs, TF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, TF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "top", label.x = "left")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "TF")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


b3 <- ggplot(d_lnrr, aes(pHs, LBCF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, LBCF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "top", label.x = "left")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LBCF")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))



b4 <- ggplot(d_lnrr, aes(pHs, SBCF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, SBCF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "top", label.x = "left")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "SBCF")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


b5 <- ggplot(d_lnrr, aes(pHs, RBCF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, RBCF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "top", label.x = "left")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "RBCF")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

b6 <- ggplot(d_lnrr, aes(pHs, TBCF))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(pHs, TBCF, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "top", label.x = "left")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "BCF")+
  #scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  #scale_x_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


rel_index <- ggarrange(b1, b2, b3, b4, b5, b6, ncol = 3, nrow = 2, common.legend = T, legend = "bottom", align = "hv", labels = c("(a)", "(b)", "(c)", "(d)","(e)", "(f)"), font.label = list(size = 22, color = "black"))
ggsave("Figure S17 pH index.pdf", rel_index, width = 9, height = 6)




##########################  HMs type   #########################################
table(d_lnrr$HMs_type)

d_lnrr_As <- subset(d_lnrr, HMs_type == "As")
d_lnrr_Cd <- subset(d_lnrr, HMs_type == "Cd")
d_lnrr_Cr <- subset(d_lnrr, HMs_type == "Cr")
d_lnrr_Cu <- subset(d_lnrr, HMs_type == "Cu")
d_lnrr_Ni <- subset(d_lnrr, HMs_type == "Ni")
d_lnrr_Pb <- subset(d_lnrr, HMs_type == "Pb")
d_lnrr_Sb <- subset(d_lnrr, HMs_type == "Sb")
d_lnrr_Ti <- subset(d_lnrr, HMs_type == "Ti")
d_lnrr_Zn <- subset(d_lnrr, HMs_type == "Zn")


#Cd
fCd <- ggplot(d_lnrr_Cd, aes(pHs,POD, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr_Cd,
               aes(pHs, POD,color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(POD)")+
  scale_y_continuous(limits = c(-2, 1.5), breaks=seq(-2, 1.5, by = 1)) +
  scale_x_continuous(limits = c(5, 8), breaks=seq(5, 8,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid")) +
  theme(legend.position = c(0.2, 0.2))

#Cu
fCu <- ggplot(d_lnrr_Cu, aes(pHs,POD, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr_Cu,
               aes(pHs, POD,color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(POD)")+
  scale_y_continuous(limits = c(-2, 1.5), breaks=seq(-2, 1.5, by = 1)) +
  scale_x_continuous(limits = c(5, 8), breaks=seq(5, 8,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid")) +
  theme(legend.position = "none")


#Pb
fPb <- ggplot(d_lnrr_Pb, aes(pHs,POD, color = EMS))+
  geom_point(size=6,pch=1,aes(color = EMS))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr_Pb,
               aes(pHs, POD,color = EMS, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "pH", y = "LnRR(POD)")+
  scale_y_continuous(limits = c(-2, 1.5), breaks=seq(-2, 1.5, by = 1)) +
  scale_x_continuous(limits = c(5, 8), breaks=seq(5, 8,by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid")) +
  theme(legend.position = "none")
 

hms_fig_ln <- plot_grid(fCd, fCu, fPb, ncol = 2, labels = c("(a)", "(b)", "(c)"), label_size = 20)

hms_fig_ln

#ggsave("hms_fig_lnRR.pdf", hms_fig_ln, width = 8, height = 8)


##### LHMs-SOD

sod_fig <- ggplot(d_lnrr, aes(LHMs, SOD, color = HMs_type))+
  geom_point(size=6,pch=1,aes(color = HMs_type))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(LHMs, SOD, color = HMs_type, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(LHMs)", y = "LnRR(SOD)")+
  scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  scale_x_continuous(limits = c(-0.5, 4), breaks=seq(-0.5, 4, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


pod_fig <-  ggplot(d_lnrr, aes(LHMs, POD, color = HMs_type))+
  geom_point(size=6,pch=1,aes(color = HMs_type))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(LHMs, POD, color = HMs_type, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(LHMs)", y = "LnRR(POD)")+
  scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  scale_x_continuous(limits = c(-0.5, 4), breaks=seq(-0.5, 4, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

cat_fig <- ggplot(d_lnrr, aes(LHMs, CAT, color = HMs_type))+
  geom_point(size=6,pch=1,aes(color = HMs_type))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(LHMs, CAT, color = HMs_type, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(LHMs)", y = "LnRR(CAT)")+
  scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  scale_x_continuous(limits = c(-0.5, 4), breaks=seq(-0.5, 4, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))


mda_fig <- ggplot(d_lnrr, aes(LHMs, MDA, color = HMs_type))+
  geom_point(size=6,pch=1,aes(color = HMs_type))+
  geom_smooth(method = "lm", se = T)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=14),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=d_lnrr,
               aes(LHMs, MDA, color = HMs_type, label=paste(..p.value.label..,sep = "~~~~")),
               formula = y~x, parse=T, size = 5,
               label.y = "bottom", label.x = "right")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Ln(LHMs)", y = "LnRR(MDA)")+
  scale_y_continuous(limits = c(-0.5, 2), breaks=seq(-0.5, 2, by = 1)) +
  scale_x_continuous(limits = c(-0.5, 4), breaks=seq(-0.5, 4, by = 1)) +
  theme(legend.title = element_text(size = 16), legend.text = element_text(size = 15)) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"))

hms_fig_ln_mei <- plot_grid(sod_fig, pod_fig, cat_fig, mda_fig, labels = c("(a)", "(b)", "(c)", "(d)"), label_size = 20, ncol = 2)
hms_fig_ln_mei

#ggsave("hms_fig_ln_mei.pdf", hms_fig_ln_mei, width = 12, height = 9)





############################Fig.S5 funnel plot#################

###### Fig.S5 and Table S4 #########  Egger's regression(若p<0.05，则进一步分析FSN)


##Total/Aboveground/Underground biomass
#Total biomas
fun.Cd.tb <- rma(yi, vi, data = da_Cd, method = "REML")
fun.Cu.tb <- rma(yi, vi, data = da_Cu, method = "REML")
fun.Pb.tb <- rma(yi, vi, data = da_Pb, method = "REML")
fun.Zn.tb <- rma(yi, vi, data = da_Zn, method = "REML")
fun.Sb.tb <- rma(yi, vi, data = da_Sb, method = "REML")
fun.As.tb <- rma(yi, vi, data = da_As, method = "REML")

#funnel
par(mfrow=c(4,4))
funnel(fun.Cd.tb, main="(a) Cd Total biomas")
funnel(fun.Cu.tb, main="(b) Cu Total biomas")
funnel(fun.Pb.tb, main="(c) Pb Total biomas")
funnel(fun.Zn.tb, main="(d) Zn Total biomas")
funnel(fun.Sb.tb, main="(e) Sb Total biomas")
funnel(fun.As.tb, main="(f) As Total biomas")

#Egger's regression
regtest(fun.Cd.tb, model="rma") 
regtest(fun.Cu.tb, model="rma") 
regtest(fun.Pb.tb, model="rma") 
regtest(fun.Zn.tb, model="rma") 
regtest(fun.Sb.tb, model="rma") 
regtest(fun.As.tb, model="rma") 

##Rosenthal fail safe number
fsn(yi,vi, data = da_Cu) #62
fsn(yi,vi, data = da_Sb) #16

5*62+10



#Aboveground biomas
fun.Cd.ab <- rma(yi, vi, data = da_ab_Cd, method = "REML")
fun.Cu.ab <- rma(yi, vi, data = da_ab_Cu, method = "REML")
fun.Pb.ab <- rma(yi, vi, data = da_ab_Pb, method = "REML")
fun.Zn.ab <- rma(yi, vi, data = da_ab_Zn, method = "REML")
fun.As.ab <- rma(yi, vi, data = da_ab_As, method = "REML")
fun.Ni.ab <- rma(yi, vi, data = da_ab_Ni, method = "REML")


#funnel
funnel(fun.Cd.ab, main="(g) Cd Aboveground biomas")
funnel(fun.Cu.ab, main="(h) Cu Aboveground biomas")
funnel(fun.Pb.ab, main="(i) Pb Aboveground biomas")
funnel(fun.Zn.ab, main="(j) Zn Aboveground biomas")
#funnel(fun.As.ab, main="(k) As Aboveground biomas")
#funnel(fun.Ni.ab, main="(l) Ni Aboveground biomas")

#Egger's regression
regtest(fun.Cd.ab, model="rma") 
regtest(fun.Cu.ab, model="rma") 
regtest(fun.Pb.ab, model="rma") 
regtest(fun.Zn.ab, model="rma") 
regtest(fun.As.ab, model="rma") 
regtest(fun.Ni.ab, model="rma") 

##Rosenthal fail safe number
fsn(yi,vi, data = da_ab_Cd) #173
fsn(yi,vi, data = da_ab_Cu) #46
fsn(yi,vi, data = da_ab_Zn) #34
fsn(yi,vi, data = da_ab_As) #7

table(da_ab_Zn$HMs_type)

5*53+10

#Underground biomas
fun.Cd.ub <- rma(yi, vi, data = da_ub_Cd, method = "REML")
fun.Cu.ub <- rma(yi, vi, data = da_ub_Cu, method = "REML")
fun.Pb.ub <- rma(yi, vi, data = da_ub_Pb, method = "REML")
fun.Zn.ub <- rma(yi, vi, data = da_ub_Zn, method = "REML")
fun.As.ub <- rma(yi, vi, data = da_ub_As, method = "REML")
fun.Sb.ub <- rma(yi, vi, data = da_ub_Sb, method = "REML")
fun.Ni.ub <- rma(yi, vi, data = da_ub_Ni, method = "REML")

#funnel
funnel(fun.Cd.ub, main="(k) Cd Underground biomas")
funnel(fun.Cu.ub, main="(l) Cu Underground biomas")
funnel(fun.Pb.ub, main="(m) Pb Underground biomas")
funnel(fun.Zn.ub, main="(n) Zn Underground biomas")
funnel(fun.As.ub, main="(o) As Underground biomas")
#funnel(fun.Sb.ub, main="(p) Sb Underground biomas")
funnel(fun.Ni.ub, main="(p) Ni Underground biomas")




#Egger's regression
regtest(fun.Cd.ub, model="rma") 
regtest(fun.Cu.ub, model="rma") 
regtest(fun.Pb.ub, model="rma") 
regtest(fun.Zn.ub, model="rma") 
regtest(fun.As.ub, model="rma") 
regtest(fun.Sb.ub, model="rma")
regtest(fun.Ni.ub, model="rma") 

##Rosenthal fail safe number
fsn(yi,vi, data = da_ub_Cd) #375
fsn(yi,vi, data = da_ub_Pb) #167

table(da_ub_Pb$HMs_type)

5*206+10



##Total/Aboveground/Leaf/Underground HMs concentration
#Total HMs concentration
fun.Cd.thms <- rma(yi, vi, data = da_hms_total_Cd, method = "REML")
fun.Cu.thms <- rma(yi, vi, data = da_hms_total_Cu, method = "REML")
fun.Pb.thms <- rma(yi, vi, data = da_hms_total_Pb, method = "REML")
fun.Zn.thms <- rma(yi, vi, data = da_hms_total_Zn, method = "REML")

#funnel
par(mfrow=c(6,4))
funnel(fun.Cd.thms, main="(a) Cd Total metal concentration")
#funnel(fun.Cu.thms, main="(b) Cu Total metal concentration")
funnel(fun.Pb.thms, main="(b) Pb Total metal concentration")
funnel(fun.Zn.thms, main="(c) Zn Total metal concentration")

#Egger's regression
regtest(fun.Cd.thms, model="rma") 
regtest(fun.Cu.thms, model="rma") 
regtest(fun.Pb.thms, model="rma") 
regtest(fun.Zn.thms, model="rma") 

##Rosenthal fail safe number
fsn(yi,vi, data = da_hms_total_Cd) #15


table(da_hms_total_Cd$HMs_type)

5*30+10


#Aboveground HMs concentration
fun.Cd.ahms <- rma(yi, vi, data = da_hms_above_Cd, method = "REML")
fun.Cu.ahms <- rma(yi, vi, data = da_hms_above_Cu, method = "REML")
fun.Pb.ahms <- rma(yi, vi, data = da_hms_above_Pb, method = "REML")
fun.Zn.ahms <- rma(yi, vi, data = da_hms_above_Zn, method = "REML")
fun.As.ahms <- rma(yi, vi, data = da_hms_above_As, method = "REML")
fun.Ni.ahms <- rma(yi, vi, data = da_hms_above_Ni, method = "REML")

#funnel
funnel(fun.Cd.ahms, main="(d) Cd Aboveground metal concentration")
funnel(fun.Cu.ahms, main="(e) Cu Aboveground metal concentration")
funnel(fun.Pb.ahms, main="(f) Pb Aboveground metal concentration")
funnel(fun.Zn.ahms, main="(g) Zn Aboveground metal concentration")
funnel(fun.As.ahms, main="(h) As Aboveground metal concentration")
funnel(fun.Ni.ahms, main="(i) Ni Aboveground metal concentration")

#Egger's regression
regtest(fun.Cd.ahms, model="rma") 
regtest(fun.Cu.ahms, model="rma") 
regtest(fun.Pb.ahms, model="rma") 
regtest(fun.Zn.ahms, model="rma")
regtest(fun.As.ahms, model="rma")
regtest(fun.Ni.ahms, model="rma")


##Rosenthal fail safe number
fsn(yi,vi, data = da_hms_above_Cd) #14
fsn(yi,vi, data = da_hms_above_Cu) #31
fsn(yi,vi, data = da_hms_above_Pb) #31
fsn(yi,vi, data = da_hms_above_Zn) #31
fsn(yi,vi, data = da_hms_above_As) #31
fsn(yi,vi, data = da_hms_above_Ni) #31


table(da_hms_above_Ni$HMs_type)

5*3+10


#Leaf HMs concentration
fun.Cd.lhums <- rma(yi, vi, data = da_hms_leaf_Cd, method = "REML")
fun.Cu.lhums <- rma(yi, vi, data = da_hms_leaf_Cu, method = "REML")
fun.Pb.lhums <- rma(yi, vi, data = da_hms_leaf_Pb, method = "REML")
fun.Zn.lhums <- rma(yi, vi, data = da_hms_leaf_Zn, method = "REML")
fun.Sb.lhums <- rma(yi, vi, data = da_hms_leaf_Sb, method = "REML")
fun.As.lhums <- rma(yi, vi, data = da_hms_leaf_As, method = "REML")
fun.Ni.lhums <- rma(yi, vi, data = da_hms_leaf_Ni, method = "REML")
fun.Ti.lhums <- rma(yi, vi, data = da_hms_leaf_Ti, method = "REML")

#funnel
funnel(fun.Cd.lhums, main="(j) Cd Leaf metal concentration")
funnel(fun.Cu.lhums, main="(k) Cu Leaf metal concentration")
funnel(fun.Pb.lhums, main="(l) Pb Leaf metal concentration")
funnel(fun.Zn.lhums, main="(m) Zn Leaf metal concentration")
funnel(fun.Sb.lhums, main="(n) Sb Leaf metal concentration")
funnel(fun.As.lhums, main="(o) As Leaf metal concentration")
funnel(fun.Ni.lhums, main="(p) Ni Leaf metal concentration")
funnel(fun.Ti.lhums, main="(q) Ti Leaf metal concentration")





#Egger's regression
regtest(fun.Cd.lhums, model="rma") 
regtest(fun.Cu.lhums, model="rma") 
regtest(fun.Pb.lhums, model="rma") 
regtest(fun.Zn.lhums, model="rma")
regtest(fun.Sb.lhums, model="rma")
regtest(fun.As.lhums, model="rma")
regtest(fun.Ni.lhums, model="rma")
regtest(fun.Ti.lhums, model="rma")

##Rosenthal fail safe number
fsn(yi,vi, data = da_hms_leaf_Cd) #353
fsn(yi,vi, data = da_hms_leaf_Cu) #96
fsn(yi,vi, data = da_hms_leaf_Pb) #172
fsn(yi,vi, data = da_hms_leaf_Zn) #95
fsn(yi,vi, data = da_hms_leaf_Sb) #14
fsn(yi,vi, data = da_hms_leaf_As) #34
fsn(yi,vi, data = da_hms_leaf_Ni) #6
fsn(yi,vi, data = da_hms_leaf_Ti) #6

table(da_hms_leaf_Ti$HMs_type)
5*353 +10
5*96 +10
5*172 +10
5*95 +10
5*14 +10
5*34 +10
5*6 +10




#Underground HMs concentration
fun.Cd.uhms <- rma(yi, vi, data = da_hms_under_Cd, method = "REML")
fun.Cu.uhms <- rma(yi, vi, data = da_hms_under_Cu, method = "REML")
fun.Pb.uhms <- rma(yi, vi, data = da_hms_under_Pb, method = "REML")
fun.Zn.uhms <- rma(yi, vi, data = da_hms_under_Zn, method = "REML")
fun.Sb.uhms <- rma(yi, vi, data = da_hms_under_Sb, method = "REML")
fun.As.uhms <- rma(yi, vi, data = da_hms_under_As, method = "REML")
fun.Ni.uhms <- rma(yi, vi, data = da_hms_under_Ni, method = "REML")

#funnel
funnel(fun.Cd.uhms, main="(r) Cd Underground metal concentration")
funnel(fun.Cu.uhms, main="(s) Cu Underground metal concentration")
funnel(fun.Pb.uhms, main="(t) Pb Underground metal concentration")
funnel(fun.Zn.uhms, main="(u) Zn Underground metal concentration")
funnel(fun.Sb.uhms, main="(v) Sb Underground metal concentration")
funnel(fun.As.uhms, main="(w) As Underground metal concentration")
funnel(fun.Ni.uhms, main="(x) Ni Underground metal concentration")





#Egger's regression
regtest(fun.Cd.uhms, model="rma") 
regtest(fun.Cu.uhms, model="rma") 
regtest(fun.Pb.uhms, model="rma") 
regtest(fun.Zn.uhms, model="rma")
regtest(fun.Sb.uhms, model="rma")
regtest(fun.As.uhms, model="rma")
regtest(fun.Ni.uhms, model="rma")

##Rosenthal fail safe number
fsn(yi,vi, data = da_hms_under_Cd) #323





##Enzyme
par(mfrow=c(4,4))
#SOD
fun.Cd.sod <- rma(yi, vi, data = da_sod_leaf_Cd, method = "REML")
fun.Cu.sod <- rma(yi, vi, data = da_sod_leaf_Cu, method = "REML")
fun.Pb.sod <- rma(yi, vi, data = da_sod_leaf_Pb, method = "REML")
fun.Zn.sod <- rma(yi, vi, data = da_sod_leaf_Zn, method = "REML")
#funnel
funnel(fun.Cd.sod, main="(a) Cd SOD")
funnel(fun.Cu.sod, main="(b) Cu SOD")
funnel(fun.Pb.sod, main="(c) Pb SOD")
funnel(fun.Zn.sod, main="(d) Zn SOD")

#Egger's regression
regtest(fun.Cd.sod, model="rma") 
regtest(fun.Cu.sod, model="rma") 
regtest(fun.Pb.sod, model="rma") 
regtest(fun.Zn.sod, model="rma")

##Rosenthal fail safe number
fsn(yi,vi, data = da_sod_leaf_Cu) #87
fsn(yi,vi, data = da_sod_leaf_Pb) #51
fsn(yi,vi, data = da_sod_leaf_Zn) #17

table(da_sod_leaf_Zn$HMs_type)
5*11+10


#POD
fun.Cd.pod <- rma(yi, vi, data = da_pod_leaf_Cd, method = "REML")
fun.Cu.pod <- rma(yi, vi, data = da_pod_leaf_Cu, method = "REML")
fun.Pb.pod <- rma(yi, vi, data = da_pod_leaf_Pb, method = "REML")
fun.Zn.pod <- rma(yi, vi, data = da_pod_leaf_Zn, method = "REML")
#funnel
funnel(fun.Cd.pod, main="(e) Cd POD")
funnel(fun.Cu.pod, main="(f) Cu POD")
funnel(fun.Pb.pod, main="(g) Pb POD")
funnel(fun.Zn.pod, main="(h) Zn POD")

#Egger's regression
regtest(fun.Cd.pod, model="rma") 
regtest(fun.Cu.pod, model="rma") 
regtest(fun.Pb.pod, model="rma") 
regtest(fun.Zn.pod, model="rma")


##Rosenthal fail safe number
fsn(yi,vi, data = da_pod_leaf_Cd) #137
fsn(yi,vi, data = da_pod_leaf_Zn) #17

table(da_pod_leaf_Zn$HMs_type)
5*11+10


#CAT
fun.Cd.cat <- rma(yi, vi, data = da_cat_leaf_Cd, method = "REML")
fun.Cu.cat <- rma(yi, vi, data = da_cat_leaf_Cu, method = "REML")
fun.Pb.cat <- rma(yi, vi, data = da_cat_leaf_Pb, method = "REML")
fun.Zn.cat <- rma(yi, vi, data = da_cat_leaf_Zn, method = "REML")
#funnel
funnel(fun.Cd.cat, main="(i) Cd CAT")
funnel(fun.Cu.cat, main="(j) Cu CAT")
funnel(fun.Pb.cat, main="(k) Pb CAT")
funnel(fun.Zn.cat, main="(l) Zn CAT")

#Egger's regression
regtest(fun.Cd.cat, model="rma") 
regtest(fun.Cu.cat, model="rma") 
regtest(fun.Pb.cat, model="rma") 
regtest(fun.Zn.cat, model="rma")


##Rosenthal fail safe number
fsn(yi,vi, data = da_cat_leaf_Cd) #143
fsn(yi,vi, data = da_cat_leaf_Cu) #80
fsn(yi,vi, data = da_cat_leaf_Zn) #17

table(da_cat_leaf_Zn$HMs_type)
5*11+10




#MDA
fun.Cd.mda <- rma(yi, vi, data = da_mda_leaf_Cd, method = "REML")
fun.Cu.mda <- rma(yi, vi, data = da_mda_leaf_Cu, method = "REML")
fun.Pb.mda <- rma(yi, vi, data = da_mda_leaf_Pb, method = "REML")
fun.Zn.mda <- rma(yi, vi, data = da_mda_leaf_Zn, method = "REML")
#funnel
funnel(fun.Cd.mda, main="(m) Cd MDA")
funnel(fun.Cu.mda, main="(n) Cu MDA")
funnel(fun.Pb.mda, main="(o) Pb MDA")
funnel(fun.Zn.mda, main="(p) Zn MDA")

#Egger's regression
regtest(fun.Cd.mda, model="rma") 
regtest(fun.Cu.mda, model="rma") 
regtest(fun.Pb.mda, model="rma") 
regtest(fun.Zn.mda, model="rma")


##Rosenthal fail safe number
fsn(yi,vi, data = da_mda_leaf_Cd) #141
fsn(yi,vi, data = da_mda_leaf_Cu) #51


table(da_mda_leaf_Cu$HMs_type)

5*59+10

