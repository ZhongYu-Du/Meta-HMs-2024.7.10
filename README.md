# Meta-HMs
## The code for plant responses to heavy metals

### Author: Zhongyu Du

### Time: 2023-10-25

# Code
```
###A global meta-analysis of ability on remediation of 
###heavy metal contaminated soil by woody plants

#Author: Zhongyu Du

#Time: 2023-01-12
#install.packages("pacman")
#pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, emmeans)
#devtools::install_github("daniel1noble/orchaRd", force = TRUE)
#devtools::install_local("C:\\Users\\Administrator\\Desktop\\orchaRd-main.zip")
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
}
#effect size and variance
setwd("E:\\博士研究生\\Meta分析论文\\B重金属与木本植物meta")
rm(list = ls())

#1 Global map figure####
#install.packages("openxlsx")

package_list = c("maps","ggplot2","sp","maptools","tidyverse","ggthemes")

for(package in package_list){
  if (!require(package, character.only = T, quietly = T)){
    install.packages(package)
    library(package, character.only =  T)
  }
}


data <- openxlsx::read.xlsx("data-all.xlsx", sheet = "Global_map")
head(data,50)
#information

table(data$HMs_type)
table(data$Experimental_method)


data$Experimental_method<-factor(data$Experimental_method)
data$HMs_type<-factor(data$HMs_type, levels = c("Cd","Pb","Cu","Zn","As",
                                                "Sb","Ni","Cr","Ti"))

data <- filter(data, data$longitude!="NA")
data <- filter(data, data$latitude!="NA")

data$longitude <- as.numeric(data$longitude)
data$latitude <- as.numeric(data$latitude)
str(data)
#world
mapworld<-borders("world",regions = ".",
                  colour ="black",fill="gray50",
                  alpha=0.3, size=0.01)# 绘制基本地图
mp <- ggplot(data = data)+
  mapworld

mp2<- mp+
  geom_point(aes(x=longitude,#经度
                 y=latitude,#纬度
                 shape=Experimental_method,
                 color=HMs_type),
             size = 4,
             alpha=0.8)+
  scale_shape_manual(values = c(15,16,17,18))+
  labs(x="Longitude",y="Latitude")+
  theme_void()+
  theme(text = element_text(size = 14,face = "bold"))+
  theme(legend.position = c(0.15,0.42))

mp2



#绘制年份文献数图
den <- ggplot(data,aes(x = Published_year))+
  geom_density(fill = "gray60",alpha=0.5)+
  xlab("Published year")+
  ylab("Density")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.text=element_text(size=14, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 16))+
  theme(strip.text.x = element_text(
    size = 16, face = "bold"))+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), 
        legend.text=element_text(size=14))#图例字体
den

##Whittaker生物群系图
library(plotbiomes)
#绘制数据分布点
library(raster)
library(maptools)
library(geodata)
library(sp)

path <- system.file("extdata", "temp_pp.tif", package = "plotbiomes")
temp_pp <- raster::stack(path)
names(temp_pp) <- c("temperature", "precipitation")


##读入数据
con_data <- readxl::read_xlsx("data-all.xlsx", sheet = "NDS")
#转换为空间数据
coordinates(con_data)=c("longitude", "latitude")

extractions <- raster::extract(temp_pp, con_data, df = TRUE)

extractions$temperature <- extractions$temperature/10
extractions$precipitation <- extractions$precipitation/10
extractions$HM_type <- factor(con_data$HMs_type, levels = c("Cd","Pb","Cu","Zn","As",
                                                            "Sb","Ni","Cr","Ti"))


plot(temp_pp[[1]]/10)
points(con_data)
plot(temp_pp[[2]])
points(con_data)



font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))



whittaker <- whittaker_base_plot() +
  geom_point(data = extractions,aes(x = temperature, y = precipitation, color = HM_type), 
             size   = 5,
             stroke = 0) +
  theme_few(base_size=16)+
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
        legend.text=element_text(size=14))#图例字体

whittaker


#判断各个点的区系
# 判断数据点在不在某个生物群系内
in_which_biome <- sapply(
  X = unique(Whittaker_biomes$biome),
  FUN = function(i) {
    dt <- subset(Whittaker_biomes, biome == i)
    vl <- point.in.polygon(
      
      con_data$longitude,
      con_data$latitude,
      dt$temp_c,
      dt$precp_cm,
      mode.checked = FALSE
    )
    ifelse(vl == 1, paste(i), NA)
  }
) |> 
  apply(MARGIN = 1, FUN = na.omit) # 1 indicates rows

# 生成一个新的列，记录每个数据点对应的生物群系
con_data$biome <- in_which_biome

head(con_data$biome)
write.csv(con_data, "con_data.csv")

#合并图
fig_s1_bc <- cowplot::plot_grid(whittaker, den, labels = c("(b)","(c)"), align = "hv", label_size = 20)

fig_S1 <- cowplot::plot_grid(mp2, fig_s1_bc, ncol = 1, rel_heights = c(1, 1.2), labels = c("(a)"), label_size = 20)
fig_S1

ggsave("fig_S1.pdf",fig_S1, width = 14, height = 12)

```


### Figure S1
![image](https://github.com/Byonone/Meta-HMs/blob/main/Figure%20S1.jpg)

```
######Biomass#####
#1 Total biomass####
rm(list = ls())
d_tb <- read_xlsx("data-all.xlsx", sheet = "Total_biomass")

#remove NA
d_tb_1 <- filter(d_tb, d_tb$Xc!="NA")
d_tb_1$Study_ID <- factor(d_tb_1$Study_ID)
table(d_tb_1$HMs_type)


#effect size
es_total_biomass<-escalc(measure = "ROM", 
                         m1i = Xt,
                         sd1i = SDt,
                         n1i = Nt,
                         m2i = Xc,
                         sd2i = SDc,
                         n2i = Nc,
                         data = d_tb_1)

#一步到位版
#HMs_type
res_total_overall <- rma.mv(yi, vi, data = es_total_biomass, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_total_HMs <- rma.mv(yi, vi, data = es_total_biomass, mod = ~HMs_type-1, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#提取结果
res_total_overall_reml <- mod_results(res_total_overall, mod = "1", group = "Study_ID") 
res_total_HMs_reml <- mod_results(res_total_HMs, mod = "HMs_type", group = "Study_ID")
#汇总数据
fig_data_total_biomass_HMs <-rbind(res_total_overall_reml$mod_table,
                                   res_total_HMs_reml$mod_table)

table(d_tb_1$HMs_type)
fig_data_total_biomass_HMs$HMs <- c("Overall (409)",
                                    "Cd (184)",
                                    "Cu (53)",
                                    "Pb (123)",
                                    "Sb (16)",
                                    "Zn (26)")

fig_data_total_biomass_HMs$col <- ifelse((exp(fig_data_total_biomass_HMs$upperCL)-1)*100>=0,"No","Yes")
fig_data_total_biomass_HMs$Index <- c(rep("Total biomass",6))

fig_data_total_biomass_HMs$HMs <- factor(fig_data_total_biomass_HMs$HMs, levels = c("Sb (16)", "Zn (26)", "Cu (53)","Pb (123)", "Cd (184)", "Overall (409)"))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
Fig_1_a <-
  ggplot(data=fig_data_total_biomass_HMs, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50,50), breaks=seq(-50,50, by=25))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 5.5, linetype = 'dashed', col = 'gray70')

Fig_1_a

#2 Aboveground biomass####
d_ab <- read_xlsx("data-all.xlsx", sheet = "Aboveground_biomass")

#remove NA
d_ab_1 <- filter(d_ab, d_ab$Xc!="NA")
d_ab_1$Study_ID <- factor(d_ab_1$Study_ID)
table(d_ab_1$HMs_type)

#effect size
es_above_biomass<-escalc(measure = "ROM", 
                         m1i = Xt,
                         sd1i = SDt,
                         n1i = Nt,
                         m2i = Xc,
                         sd2i = SDc,
                         n2i = Nc,
                         data = d_ab_1)

#一步到位版
#HMs_type
res_above_overall <- rma.mv(yi, vi, data = es_above_biomass, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_above_HMs <- rma.mv(yi, vi, data = es_above_biomass, mod = ~HMs_type-1, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#提取结果
res_above_overall_reml <- mod_results(res_above_overall, mod = "1", group = "Study_ID") 
res_above_HMs_reml <- mod_results(res_above_HMs, mod = "HMs_type", group = "Study_ID")
#汇总数据
fig_data_above_biomass_HMs <-rbind(res_above_overall_reml$mod_table,
                                   res_above_HMs_reml$mod_table)

table(d_ab_1$HMs_type)
fig_data_above_biomass_HMs$HMs <- c("Overall (286)",
                                    "As (7)",
                                    "Cd (118)",
                                    "Cu (24)",
                                    "Pb (103)",
                                    "Zn (34)")

fig_data_above_biomass_HMs$col <- ifelse((exp(fig_data_above_biomass_HMs$upperCL)-1)*100>=0,"No","Yes")
fig_data_above_biomass_HMs$Index <- c(rep("Aboveground biomass",6))

fig_data_above_biomass_HMs$HMs <- factor(fig_data_above_biomass_HMs$HMs, levels = c("As (7)", "Cu (24)", "Zn (34)","Pb (103)", "Cd (118)", "Overall (286)"))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
Fig_1_b <-
  ggplot(data=fig_data_above_biomass_HMs, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,100), breaks=seq(-100,100, by=50))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 5.5, linetype = 'dashed', col = 'gray70')

Fig_1_b

#3 Root biomass####
d_rb <- read_xlsx("data-all.xlsx", sheet = "Root_biomass")

#remove NA
d_rb_1 <- filter(d_rb, d_rb$Xc!="NA")
d_rb_1$Study_ID <- factor(d_rb_1$Study_ID)
table(d_rb_1$HMs_type)

#effect size
es_root_biomass<-escalc(measure = "ROM", 
                        m1i = Xt,
                        sd1i = SDt,
                        n1i = Nt,
                        m2i = Xc,
                        sd2i = SDc,
                        n2i = Nc,
                        data = d_rb_1)
es_root_biomass <- filter(es_root_biomass, es_root_biomass$yi!="0")
es_root_biomass <- filter(es_root_biomass, es_root_biomass$vi!="NA")



#Overall filed (VCV were used here)
V.root.bio<-metaAidR::make_VCV_matrix(data = es_root_biomass, V = "vi", 
                                      cluster = "Co_ID", 
                                      obs = "Inf_ID", type = "vcv", rho = 0.5)

V.root.bio[is.na(V.root.bio)] <- 0###去除表内含有NA的数值

# 使用nearPD来获得近似的正定矩阵
result <- nearPD(V.root.bio, corr = FALSE, do2eigen = TRUE, ensureSymmetry = TRUE)
# 检查result$mat是不是正定的
V.root.bio.posdef <- result$mat
V.root.bio.posdef <- as.matrix(V.root.bio.posdef)
is.positive.definite(V.root.bio.posdef, tol=1e-8)
###确保为TRUE
V.root.bio <- V.root.bio.posdef

# Random effects structure
#final model/best model
control <- list(optimizer="optim", optCtrl=list(maxit=1e8))##优化控制参数

#一步到位版
#HMs_type
res_root_overall <- rma.mv(yi, V.root.bio, data = es_root_biomass, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4, control = control)
res_root_HMs <- rma.mv(yi, V.root.bio, data = es_root_biomass, mod = ~HMs_type-1, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4, control = control)

#提取结果
res_root_overall_reml <- mod_results(res_root_overall, mod = "1", group = "Study_ID") 
res_root_HMs_reml <- mod_results(res_root_HMs, mod = "HMs_type", group = "Study_ID")
#汇总数据
fig_data_root_biomass_HMs <-rbind(res_root_overall_reml$mod_table,
                                  res_root_HMs_reml$mod_table)

table(d_rb_1$HMs_type)
fig_data_root_biomass_HMs$HMs <- c("Overall (721)",
                                   "As (10)",
                                   "Cd (375)",
                                   "Cu (95)",
                                   "Pb (167)",
                                   "Sb (2)",
                                   "Zn (87)")

fig_data_root_biomass_HMs$col <- ifelse((exp(fig_data_root_biomass_HMs$upperCL)-1)*100>=0,"No","Yes")
fig_data_root_biomass_HMs$Index <- c(rep("Underground biomass",7))

fig_data_root_biomass_HMs$HMs <- factor(fig_data_root_biomass_HMs$HMs, levels = c("Sb (2)","As (10)", "Zn (87)","Cu (95)", "Pb (167)", "Cd (375)", "Overall (721)"))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
Fig_1_c <-
  ggplot(data=fig_data_root_biomass_HMs, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,100), breaks=seq(-100,100, by=50))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 6.5, linetype = 'dashed', col = 'gray70')

Fig_1_c


biomass_er <- cowplot::plot_grid(Fig_1_a,Fig_1_b,Fig_1_c, ncol = 3,align = "hv", labels = c("(a)","(b)","(c)"), label_size = 24)
ggsave("biomass_v2.pdf",biomass_er, width = 14, height = 6,dpi = 600)

```
![image](https://github.com/Byonone/Meta-HMs/blob/main/biomass_v2.png)


#### Fiting
```
####model fit assessment
par(mfrow = c(1,3))
#Total biomass
##Rosenthal's Fail-safe N Calculation: N > 5k+10
fsn(es_total_biomass$yi, es_total_biomass$vi, data = es_total_biomass, type = "Rosenthal")

#funnel plot
funnel(res_total_overall,ylim=c(0,0.8), main = "(a) Total biomass")

#Aboveground biomass
##Rosenthal's Fail-safe N Calculation: N > 5k+10
fsn(es_above_biomass$yi, es_above_biomass$vi, data = es_above_biomass, type = "Rosenthal")

#funnel plot
funnel(res_above_overall,ylim=c(0,0.8), main = "(b) Aboveground biomass")

#Underground biomass
##Rosenthal's Fail-safe N Calculation: N > 5k+10
#fsn(es_root_biomass$yi, es_root_biomass$vi, data = es_root_biomass, type = "Rosenthal")

#funnel plot
funnel(res_root_overall,ylim=c(0,1), main = "(c) Underground biomass")

#9 * 3.5
```
![image](https://github.com/Byonone/Meta-HMs/blob/main/funnel-biomass.jpg)
