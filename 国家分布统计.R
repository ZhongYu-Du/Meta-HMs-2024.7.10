###国家分布统计###
library(ggplot2)
library(ggbreak)
library(ggthemes)
rm(list = ls())
setwd("E:\\博士研究生\\Meta分析论文\\B重金属与木本植物meta")
#读入数据
con_data <- readxl::read_xlsx("data-all.xlsx", sheet = "NDS")

#country
dat_con <- data.frame(table(con_data$Country))

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

p1 <-ggplot(dat_con, aes(x = reorder(Var1, -Freq),y = Freq))+
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
  labs(x = "Country", y = "Publish number")

p2<-p1+scale_y_break(c(80,1250),#截断位置及范围
                     space = 0.2,#间距大小
                     scales = 0.4)#上下显示比例，大于1上面比例大，小于1下面比例大
p2
ggsave("Country.pdf", width = 25, height = 5)

#Area
dat_area <- data.frame(table(con_data$Area))

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

p3 <-ggplot(dat_area, aes(x = reorder(Var1, -Freq),y = Freq))+
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
  theme(axis.text.x = element_text(angle = 30, hjust = 0.4, vjust = .5))+
  labs(x = "Country", y = "Publish number")

p4<-p3+scale_y_break(c(80,1250),#截断位置及范围
                     space = 0.2,#间距大小
                     scales = 0.4)#上下显示比例，大于1上面比例大，小于1下面比例大
p4
#ggsave("Area.pdf", width = 25, height = 5)


#Family
dat_Family <- data.frame(table(con_data$Family))

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

p5 <-ggplot(dat_Family, aes(x = reorder(Var1, -Freq),y = Freq))+
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

p6<-p5+scale_y_break(c(150,580),#截断位置及范围
                     space = 0.2,#间距大小
                     scales = 0.4)#上下显示比例，大于1上面比例大，小于1下面比例大
p6
ggsave("Family.pdf", width = 25, height = 5)

#Genus
dat_Genus <- data.frame(table(con_data$Genus))

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

p7 <-ggplot(dat_Genus, aes(x = reorder(Var1, -Freq),y = Freq))+
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
  labs(x = "Genus", y = "Publish number")

p8<-p7+scale_y_break(c(210,410),#截断位置及范围
                     space = 0.2,#间距大小
                     scales = 0.4)#上下显示比例，大于1上面比例大，小于1下面比例大
p8
ggsave("Genus.pdf", width = 25, height = 5)  

#Published_year
dat_year <- data.frame(table(con_data$Published_year))

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

p9 <-ggplot(dat_year, aes(x = Var1, y = Freq))+
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
  labs(x = "Publish year", y = "Publish number")
p9
ggsave("Published_year.pdf", width = 10, height = 5)  



