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


