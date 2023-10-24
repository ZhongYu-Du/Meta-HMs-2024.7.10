setwd("E:\\博士研究生\\Meta分析论文\\B重金属与木本植物meta")


#1 Total HMs concentration####
rm(list = ls())
d_t_hms <- read_xlsx("data-all.xlsx", sheet = "Total_HMs_concentration")

table(d_t_hms$HMs_type)


#effect size
es_hms_total <- escalc(measure = "ROM", 
                       m1i = Xt,
                       sd1i = SDt,
                       n1i = Nt,
                       m2i = Xc,
                       sd2i = SDc,
                       n2i = Nc,
                       data = d_t_hms)

#HMs(Pb/Cd/Cu)
data_hms_Cd <- subset(es_hms_total, HMs_type == "Cd")
data_hms_Pb <- subset(es_hms_total, HMs_type == "Pb")
data_hms_Cu <- subset(es_hms_total, HMs_type == "Cu")

#查看一共有多少个属
aggregate(es_hms_total$Genus, by = list(es_hms_total$Genus), FUN = length)
aggregate(data_hms_Cd$Genus, by = list(data_hms_Cd$Genus), FUN = length)
aggregate(data_hms_Pb$Genus, by = list(data_hms_Pb$Genus), FUN = length)
aggregate(data_hms_Cu$Genus, by = list(data_hms_Cu$Genus), FUN = length)


#Meta-analysis
#科属作为调节变量
res_hms_total <- rma.mv(yi, vi, mods = ~ Genus-1, data = es_hms_total, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Cd <- rma(yi, vi, mods = ~ Genus-1, data = data_hms_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Pb <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Cu <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


#extracting table of results
res_hms_total_reml <- mod_results(res_hms_total, mod = "Genus", group = "Study_ID") 
res_hms_Cd_reml <- mod_results(res_hms_Cd, mod = "Genus", group = "Study_ID") 
res_hms_Pb_reml <- mod_results(res_hms_Pb, mod = "Genus", group = "Study_ID") 
res_hms_Cu_reml <- mod_results(res_hms_Cu, mod = "Genus", group = "Study_ID") 

###Overall
res_hms_overall_reml <- res_hms_total_reml$mod_table
res_hms_overall_reml$col <- ifelse((exp(res_hms_overall_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_overall_reml$Index <- c(rep("Overall",7))
res_hms_overall_reml$numb <- table(es_hms_total$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_overall <- (exp(res_hms_overall_reml$upperCL)-1)*100 + 4000
hms_total <-
  ggplot(data=res_hms_overall_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-5000,70000), breaks=seq(-5000,70000, by=35000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_overall), size=5, color = "black")

hms_total

ggsave("Genus_hms_total_overall.pdf",hms_total, width = 9, height = 4.5, dpi = 600)

###Cd
res_hms_Cd_reml <- res_hms_Cd_reml$mod_table
res_hms_Cd_reml$col <- ifelse((exp(res_hms_Cd_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Cd_reml$Index <- c(rep("Cd",5))
res_hms_Cd_reml$numb <- table(data_hms_Cd$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cd <- (exp(res_hms_Cd_reml$upperCL)-1)*100 + 3000
hms_Cd <-
  ggplot(data=res_hms_Cd_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-5000,55000), breaks=seq(-5000,55000, by=25000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cd), size=5, color = "black")

hms_Cd
ggsave("Genus_hms_total_Cd.pdf",hms_Cd, width = 3.5, height = 4.5, dpi = 600)


###Pb
res_hms_Pb_reml <- res_hms_Pb_reml$mod_table
res_hms_Pb_reml$col <- ifelse((exp(res_hms_Pb_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Pb_reml$Index <- c(rep("Pb",5))
res_hms_Pb_reml$numb <- table(data_hms_Pb$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Pb <- (exp(res_hms_Pb_reml$upperCL)-1)*100 + 4000
hms_Pb <-
  ggplot(data=res_hms_Pb_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,40000), breaks=seq(-100,40000, by=20000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Pb), size=5, color = "black")

hms_Pb
ggsave("Genus_hms_total_Pb.pdf",hms_Pb, width = 3, height = 4.5, dpi = 600)


###Cu
res_hms_Cu_reml <- res_hms_Cu_reml$mod_table
res_hms_Cu_reml$col <- ifelse((exp(res_hms_Cu_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Cu_reml$Index <- c(rep("Cu",2))
res_hms_Cu_reml$numb <- table(data_hms_Cu$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cu <- (exp(res_hms_Cu_reml$upperCL)-1)*100 + 1000
hms_Cu <-
  ggplot(data=res_hms_Cu_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,8000), breaks=seq(-100,8000, by=4000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cu), size=5, color = "black")

hms_Cu
ggsave("Genus_hms_total_Cu.pdf",hms_Cu, width = 2.5, height = 4.5, dpi = 600)




#2 Aboveground HMs concentration####
rm(list = ls())
d_ab_hms <- read_xlsx("data-all.xlsx", sheet = "Aboveground_HMs_concentration")

table(d_ab_hms$HMs_type)
#effect size
es_hms_above <- escalc(measure = "ROM", 
                       m1i = Xt,
                       sd1i = SDt,
                       n1i = Nt,
                       m2i = Xc,
                       sd2i = SDc,
                       n2i = Nc,
                       data = d_ab_hms)

#HMs(Pb/Cd/Cu)
data_hms_Cd <- subset(es_hms_above, HMs_type == "Cd")
data_hms_Pb <- subset(es_hms_above, HMs_type == "Pb")
data_hms_Cu <- subset(es_hms_above, HMs_type == "Cu")
data_hms_Zn <- subset(es_hms_above, HMs_type == "Zn")
data_hms_As <- subset(es_hms_above, HMs_type == "As")




#查看一共有多少个属
aggregate(es_hms_above$Genus, by = list(es_hms_above$Genus), FUN = length)
aggregate(data_hms_Cd$Genus, by = list(data_hms_Cd$Genus), FUN = length)
aggregate(data_hms_Pb$Genus, by = list(data_hms_Pb$Genus), FUN = length)
aggregate(data_hms_Cu$Genus, by = list(data_hms_Cu$Genus), FUN = length)

aggregate(data_hms_Zn$Genus, by = list(data_hms_Zn$Genus), FUN = length)
aggregate(data_hms_As$Genus, by = list(data_hms_As$Genus), FUN = length)


#Meta-analysis
#科属作为调节变量(As删掉)
res_hms_above <- rma.mv(yi, vi, mods = ~ Genus-1, data = es_hms_above, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Cd <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Pb <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Cu <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Zn <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


#extracting table of results
res_hms_above_reml <- mod_results(res_hms_above, mod = "Genus", group = "Study_ID") 
res_hms_Cd_reml <- mod_results(res_hms_Cd, mod = "Genus", group = "Study_ID") 
res_hms_Pb_reml <- mod_results(res_hms_Pb, mod = "Genus", group = "Study_ID") 
res_hms_Cu_reml <- mod_results(res_hms_Cu, mod = "Genus", group = "Study_ID") 
res_hms_Zn_reml <- mod_results(res_hms_Zn, mod = "Genus", group = "Study_ID") 



###Overall
res_hms_overall_reml <- res_hms_above_reml$mod_table
res_hms_overall_reml$col <- ifelse((exp(res_hms_overall_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_overall_reml$Index <- c(rep("Overall",17))
res_hms_overall_reml$numb <- table(es_hms_above$Genus)

#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_overall <- (exp(res_hms_overall_reml$upperCL)-1)*100 + 4000
hms_above <-
  ggplot(data=res_hms_overall_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50000,450000), breaks=seq(-50000,450000, by=200000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_overall), size=5, color = "black")

hms_above

ggsave("Genus_hms_above_overall.pdf",hms_above, width = 10, height = 4.5, dpi = 600)


###Cd
res_hms_Cd_reml <- res_hms_Cd_reml$mod_table
res_hms_Cd_reml$col <- ifelse((exp(res_hms_Cd_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Cd_reml$Index <- c(rep("Cd",9))
res_hms_Cd_reml$numb <- table(data_hms_Cd$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cd <- (exp(res_hms_Cd_reml$upperCL)-1)*100 + 20000
hms_Cd <-
  ggplot(data=res_hms_Cd_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50000,250000), breaks=seq(-50000,250000, by=100000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cd), size=5, color = "black")

hms_Cd
ggsave("Genus_hms_above_Cd.pdf",hms_Cd, width = 5.5, height = 4.5, dpi = 600)


###Pb
res_hms_Pb_reml <- res_hms_Pb_reml$mod_table
res_hms_Pb_reml$col <- ifelse((exp(res_hms_Pb_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Pb_reml$Index <- c(rep("Pb",6))
res_hms_Pb_reml$numb <- table(data_hms_Pb$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Pb <- (exp(res_hms_Pb_reml$upperCL)-1)*100 + 12000
hms_Pb <-
  ggplot(data=res_hms_Pb_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,150000), breaks=seq(-100,150000, by=50000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Pb), size=5, color = "black")

hms_Pb
ggsave("Genus_hms_above_Pb.pdf",hms_Pb, width = 4, height = 4.5, dpi = 600)


###Cu
res_hms_Cu_reml <- res_hms_Cu_reml$mod_table
res_hms_Cu_reml$col <- ifelse((exp(res_hms_Cu_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Cu_reml$Index <- c(rep("Cu",4))
res_hms_Cu_reml$numb <- table(data_hms_Cu$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cu <- (exp(res_hms_Cu_reml$upperCL)-1)*100 + 30
hms_Cu <-
  ggplot(data=res_hms_Cu_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,550), breaks=seq(-100,550, by=200))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cu), size=5, color = "black")

hms_Cu
ggsave("Genus_hms_above_Cu.pdf",hms_Cu, width = 3.5, height = 4.5, dpi = 600)


###Zn
res_hms_Zn_reml <- res_hms_Zn_reml$mod_table
res_hms_Zn_reml$col <- ifelse((exp(res_hms_Zn_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Zn_reml$Index <- c(rep("Zn",2))
res_hms_Zn_reml$numb <- table(data_hms_Zn$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Zn <- (exp(res_hms_Zn_reml$upperCL)-1)*100 + 800
hms_Zn <-
  ggplot(data=res_hms_Zn_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,10000), breaks=seq(-100,10000, by=5000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Zn), size=5, color = "black")

hms_Zn
ggsave("Genus_hms_above_Zn.pdf",hms_Zn, width = 2, height = 4.5, dpi = 600)



#3 Underground HMs concentration####
rm(list = ls())
d_un_hms <- read_xlsx("data-all.xlsx", sheet = "Root_HMs_concentration")

table(d_un_hms$HMs_type)
#effect size
es_hms_under <- escalc(measure = "ROM", 
                       m1i = Xt,
                       sd1i = SDt,
                       n1i = Nt,
                       m2i = Xc,
                       sd2i = SDc,
                       n2i = Nc,
                       data = d_un_hms)


es_hms_under <- filter(es_hms_under, es_hms_under$yi!="NA")

table(es_hms_under$HMs_type)

#HMs(Pb/Cd/Cu)
data_hms_Cd <- subset(es_hms_under, HMs_type == "Cd")
data_hms_Pb <- subset(es_hms_under, HMs_type == "Pb")
data_hms_Cu <- subset(es_hms_under, HMs_type == "Cu")
data_hms_Zn <- subset(es_hms_under, HMs_type == "Zn")
data_hms_As <- subset(es_hms_under, HMs_type == "As")
data_hms_Sb <- subset(es_hms_under, HMs_type == "Sb")
data_hms_Ni <- subset(es_hms_under, HMs_type == "Ni")
data_hms_Cr <- subset(es_hms_under, HMs_type == "Cr")


#查看一共有多少个属
aggregate(es_hms_under$Genus, by = list(es_hms_under$Genus), FUN = length)
aggregate(data_hms_Cd$Genus, by = list(data_hms_Cd$Genus), FUN = length)
aggregate(data_hms_Pb$Genus, by = list(data_hms_Pb$Genus), FUN = length)
aggregate(data_hms_Cu$Genus, by = list(data_hms_Cu$Genus), FUN = length)
aggregate(data_hms_Zn$Genus, by = list(data_hms_Zn$Genus), FUN = length)
aggregate(data_hms_As$Genus, by = list(data_hms_As$Genus), FUN = length)

aggregate(data_hms_Sb$Genus, by = list(data_hms_Sb$Genus), FUN = length)
aggregate(data_hms_Ni$Genus, by = list(data_hms_Ni$Genus), FUN = length)
aggregate(data_hms_Cr$Genus, by = list(data_hms_Cr$Genus), FUN = length)

#Meta-analysis
#科属作为调节变量(Cr删掉)
res_hms_under <- rma.mv(yi, vi, mods = ~ Genus-1, data = es_hms_under, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Cd <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Pb <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Cu <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Zn <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_As <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Sb <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Sb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Ni <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Ni, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)



#extracting table of results
res_hms_under_reml <- mod_results(res_hms_under, mod = "Genus", group = "Study_ID") 
res_hms_Cd_reml <- mod_results(res_hms_Cd, mod = "Genus", group = "Study_ID") 
res_hms_Pb_reml <- mod_results(res_hms_Pb, mod = "Genus", group = "Study_ID") 
res_hms_Cu_reml <- mod_results(res_hms_Cu, mod = "Genus", group = "Study_ID") 
res_hms_Zn_reml <- mod_results(res_hms_Zn, mod = "Genus", group = "Study_ID") 

res_hms_As_reml <- mod_results(res_hms_As, mod = "Genus", group = "Study_ID") 
res_hms_Sb_reml <- mod_results(res_hms_Sb, mod = "Genus", group = "Study_ID") 
res_hms_Ni_reml <- mod_results(res_hms_Ni, mod = "Genus", group = "Study_ID") 



###Overall
res_hms_overall_reml <- res_hms_under_reml$mod_table
res_hms_overall_reml$col <- ifelse((exp(res_hms_overall_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_overall_reml$Index <- c(rep("Overall",62))
res_hms_overall_reml$numb <- table(es_hms_under$Genus)

#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_overall <- (exp(res_hms_overall_reml$upperCL)-1)*100 + 4000
hms_under <-
  ggplot(data=res_hms_overall_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50000,500000), breaks=seq(-50000,500000, by=250000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_overall), size=5, color = "black")

hms_under

ggsave("Genus_hms_under_overall.pdf",hms_under, width = 18, height = 6, dpi = 600)


###Cd
res_hms_Cd_reml <- res_hms_Cd_reml$mod_table
res_hms_Cd_reml$col <- ifelse((exp(res_hms_Cd_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Cd_reml$Index <- c(rep("Cd",42))
res_hms_Cd_reml$numb <- table(data_hms_Cd$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cd <- (exp(res_hms_Cd_reml$upperCL)-1)*100 + 20000
hms_Cd <-
  ggplot(data=res_hms_Cd_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50000,3800000), breaks=seq(-50000,3800000, by=1000000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cd), size=5, color = "black")

hms_Cd
ggsave("Genus_hms_under_Cd.pdf",hms_Cd, width = 18, height = 4.5, dpi = 600)


###Pb
res_hms_Pb_reml <- res_hms_Pb_reml$mod_table
res_hms_Pb_reml$col <- ifelse((exp(res_hms_Pb_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Pb_reml$Index <- c(rep("Pb",33))
res_hms_Pb_reml$numb <- table(data_hms_Pb$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Pb <- (exp(res_hms_Pb_reml$upperCL)-1)*100 + 12000
hms_Pb <-
  ggplot(data=res_hms_Pb_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,4000000), breaks=seq(-100,4000000, by=1000000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Pb), size=5, color = "black")

hms_Pb
ggsave("Genus_hms_under_Pb.pdf",hms_Pb, width = 12, height = 4.5, dpi = 600)


###Cu
res_hms_Cu_reml <- res_hms_Cu_reml$mod_table
res_hms_Cu_reml$col <- ifelse((exp(res_hms_Cu_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Cu_reml$Index <- c(rep("Cu",9))
res_hms_Cu_reml$numb <- table(data_hms_Cu$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cu <- (exp(res_hms_Cu_reml$upperCL)-1)*100 + 30
hms_Cu <-
  ggplot(data=res_hms_Cu_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-1000,30000), breaks=seq(-1000,30000, by=10000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cu), size=5, color = "black")

hms_Cu
ggsave("Genus_hms_under_Cu.pdf",hms_Cu, width = 6, height = 4.5, dpi = 600)


###Zn
res_hms_Zn_reml <- res_hms_Zn_reml$mod_table
res_hms_Zn_reml$col <- ifelse((exp(res_hms_Zn_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Zn_reml$Index <- c(rep("Zn",22))
res_hms_Zn_reml$numb <- table(data_hms_Zn$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Zn <- (exp(res_hms_Zn_reml$upperCL)-1)*100 + 800
hms_Zn <-
  ggplot(data=res_hms_Zn_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,100000), breaks=seq(-100,100000, by=25000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Zn), size=5, color = "black")

hms_Zn
ggsave("Genus_hms_under_Zn.pdf",hms_Zn, width = 8, height = 4.5, dpi = 600)


###As
res_hms_As_reml <- res_hms_As_reml$mod_table
res_hms_As_reml$col <- ifelse((exp(res_hms_As_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_As_reml$Index <- c(rep("As",6))
res_hms_As_reml$numb <- table(data_hms_As$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_As <- (exp(res_hms_As_reml$upperCL)-1)*100 + 800
hms_As <-
  ggplot(data=res_hms_As_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,500000), breaks=seq(-100,500000, by=150000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_As), size=5, color = "black")

hms_As
ggsave("Genus_hms_under_As.pdf",hms_As, width = 4, height = 4.5, dpi = 600)



###Sb
res_hms_Sb_reml <- res_hms_Sb_reml$mod_table
res_hms_Sb_reml$col <- ifelse((exp(res_hms_Sb_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Sb_reml$Index <- c(rep("Sb",2))
res_hms_Sb_reml$numb <- table(data_hms_Sb$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Sb <- (exp(res_hms_Sb_reml$upperCL)-1)*100 + 800
hms_Sb <-
  ggplot(data=res_hms_Sb_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(0,12000), breaks=seq(0,12000, by=3000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Sb), size=5, color = "black")

hms_Sb
ggsave("Genus_hms_under_Sb.pdf",hms_Sb, width = 2.5, height = 4.5, dpi = 600)


###Ni
res_hms_Ni_reml <- res_hms_Ni_reml$mod_table
res_hms_Ni_reml$col <- ifelse((exp(res_hms_Ni_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Ni_reml$Index <- c(rep("Ni",2))
res_hms_Ni_reml$numb <- table(data_hms_Ni$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Ni <- (exp(res_hms_Ni_reml$upperCL)-1)*100 + 800
hms_Ni <-
  ggplot(data=res_hms_Ni_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,800), breaks=seq(-100,800, by=300))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Ni), size=5, color = "black")

hms_Ni
ggsave("Genus_hms_under_Ni.pdf",hms_Ni, width = 2.5, height = 4.5, dpi = 600)



#4 Leaf HMs concentration####
rm(list = ls())
d_leaf_hms <- read_xlsx("data-all.xlsx", sheet = "Leaf_HMs_concentration")

table(d_leaf_hms$HMs_type)
#effect size
es_hms_leaf <- escalc(measure = "ROM", 
                       m1i = Xt,
                       sd1i = SDt,
                       n1i = Nt,
                       m2i = Xc,
                       sd2i = SDc,
                       n2i = Nc,
                       data = d_leaf_hms)


es_hms_leaf <- filter(es_hms_leaf, es_hms_leaf$yi!="NA")

table(es_hms_leaf$HMs_type)



#HMs
data_hms_Cd <- subset(es_hms_leaf, HMs_type == "Cd")
data_hms_Pb <- subset(es_hms_leaf, HMs_type == "Pb")
data_hms_Cu <- subset(es_hms_leaf, HMs_type == "Cu")
data_hms_Zn <- subset(es_hms_leaf, HMs_type == "Zn")
data_hms_As <- subset(es_hms_leaf, HMs_type == "As")
data_hms_Ti <- subset(es_hms_leaf, HMs_type == "Ti")
data_hms_Ni <- subset(es_hms_leaf, HMs_type == "Ni")
data_hms_Cr <- subset(es_hms_leaf, HMs_type == "Cr")


#查看一共有多少个属(Ni和Cr删掉)
aggregate(es_hms_leaf$Genus, by = list(es_hms_leaf$Genus), FUN = length)
aggregate(data_hms_Cd$Genus, by = list(data_hms_Cd$Genus), FUN = length)
aggregate(data_hms_Pb$Genus, by = list(data_hms_Pb$Genus), FUN = length)
aggregate(data_hms_Cu$Genus, by = list(data_hms_Cu$Genus), FUN = length)
aggregate(data_hms_Zn$Genus, by = list(data_hms_Zn$Genus), FUN = length)
aggregate(data_hms_As$Genus, by = list(data_hms_As$Genus), FUN = length)
aggregate(data_hms_Ti$Genus, by = list(data_hms_Ti$Genus), FUN = length)

#Meta-analysis
#科属作为调节变量(Ni和Cr删掉)
res_hms_leaf <- rma.mv(yi, vi, mods = ~ Genus-1, data = es_hms_leaf, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Cd <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Pb <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Cu <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Zn <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_As <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Ti <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_hms_Ti, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)

#extracting table of results
res_hms_leaf_reml <- mod_results(res_hms_leaf, mod = "Genus", group = "Study_ID") 
res_hms_Cd_reml <- mod_results(res_hms_Cd, mod = "Genus", group = "Study_ID") 
res_hms_Pb_reml <- mod_results(res_hms_Pb, mod = "Genus", group = "Study_ID") 
res_hms_Cu_reml <- mod_results(res_hms_Cu, mod = "Genus", group = "Study_ID") 
res_hms_Zn_reml <- mod_results(res_hms_Zn, mod = "Genus", group = "Study_ID") 
res_hms_As_reml <- mod_results(res_hms_As, mod = "Genus", group = "Study_ID") 
res_hms_Ti_reml <- mod_results(res_hms_Ti, mod = "Genus", group = "Study_ID") 

###Overall
res_hms_overall_reml <- res_hms_leaf_reml$mod_table
res_hms_overall_reml$col <- ifelse((exp(res_hms_overall_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_overall_reml$Index <- c(rep("Overall",56))
res_hms_overall_reml$numb <- table(es_hms_leaf$Genus)

#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_overall <- (exp(res_hms_overall_reml$upperCL)-1)*100 + 6000
hms_leaf <-
  ggplot(data=res_hms_overall_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50000,250000), breaks=seq(-50000,250000, by=100000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_overall), size=5, color = "black")

hms_leaf

ggsave("Genus_hms_leaf_overall.pdf",hms_leaf, width = 18, height = 6, dpi = 600)

###Cd
res_hms_Cd_reml <- res_hms_Cd_reml$mod_table
res_hms_Cd_reml$col <- ifelse((exp(res_hms_Cd_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Cd_reml$Index <- c(rep("Cd",36))
res_hms_Cd_reml$numb <- table(data_hms_Cd$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cd <- (exp(res_hms_Cd_reml$upperCL)-1)*100 + 20000
hms_Cd <-
  ggplot(data=res_hms_Cd_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50000,950000), breaks=seq(-50000,950000, by=500000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cd), size=5, color = "black")

hms_Cd
ggsave("Genus_hms_leaf_Cd.pdf",hms_Cd, width = 18, height = 4.5, dpi = 600)



###Pb
res_hms_Pb_reml <- res_hms_Pb_reml$mod_table
res_hms_Pb_reml$col <- ifelse((exp(res_hms_Pb_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Pb_reml$Index <- c(rep("Pb",30))
res_hms_Pb_reml$numb <- table(data_hms_Pb$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Pb <- (exp(res_hms_Pb_reml$upperCL)-1)*100 + 12000
hms_Pb <-
  ggplot(data=res_hms_Pb_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,70000), breaks=seq(-100,70000, by=20000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Pb), size=5, color = "black")

hms_Pb
ggsave("Genus_hms_leaf_Pb.pdf",hms_Pb, width = 12, height = 4.5, dpi = 600)

###Cu
res_hms_Cu_reml <- res_hms_Cu_reml$mod_table
res_hms_Cu_reml$col <- ifelse((exp(res_hms_Cu_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Cu_reml$Index <- c(rep("Cu",9))
res_hms_Cu_reml$numb <- table(data_hms_Cu$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cu <- (exp(res_hms_Cu_reml$upperCL)-1)*100 + 30
hms_Cu <-
  ggplot(data=res_hms_Cu_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-1000,30000), breaks=seq(-1000,30000, by=10000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cu), size=5, color = "black")

hms_Cu
ggsave("Genus_hms_leaf_Cu.pdf",hms_Cu, width = 6, height = 4.5, dpi = 600)


###Zn
res_hms_Zn_reml <- res_hms_Zn_reml$mod_table
res_hms_Zn_reml$col <- ifelse((exp(res_hms_Zn_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Zn_reml$Index <- c(rep("Zn",21))
res_hms_Zn_reml$numb <- table(data_hms_Zn$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Zn <- (exp(res_hms_Zn_reml$upperCL)-1)*100 + 800
hms_Zn <-
  ggplot(data=res_hms_Zn_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,12000), breaks=seq(-100,12000, by=5000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Zn), size=5, color = "black")

hms_Zn
ggsave("Genus_hms_leaf_Zn.pdf",hms_Zn, width = 7.5, height = 4.5, dpi = 600)


###As
res_hms_As_reml <- res_hms_As_reml$mod_table
res_hms_As_reml$col <- ifelse((exp(res_hms_As_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_As_reml$Index <- c(rep("As",9))
res_hms_As_reml$numb <- table(data_hms_As$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_As <- (exp(res_hms_As_reml$upperCL)-1)*100 + 2000
hms_As <-
  ggplot(data=res_hms_As_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,60000), breaks=seq(-100,60000, by=20000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_As), size=5, color = "black")

hms_As
ggsave("Genus_hms_leaf_As.pdf",hms_As, width = 4, height = 4.5, dpi = 600)


###Ti
res_hms_Ti_reml <- res_hms_Ti_reml$mod_table
res_hms_Ti_reml$col <- ifelse((exp(res_hms_Ti_reml$lowerCL)-1)*100>=0,"No","Yes")
res_hms_Ti_reml$Index <- c(rep("Ti",5))
res_hms_Ti_reml$numb <- table(data_hms_Ti$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Ti <- (exp(res_hms_Ti_reml$upperCL)-1)*100 + 100
hms_Ti <-
  ggplot(data=res_hms_Ti_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,2000), breaks=seq(-100,2000, by=1000))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Ti), size=5, color = "black")

hms_Ti
ggsave("Genus_hms_leaf_Ti.pdf",hms_Ti, width = 4, height = 4.5, dpi = 600)
