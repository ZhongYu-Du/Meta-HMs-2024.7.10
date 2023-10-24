setwd("E:\\博士研究生\\Meta分析论文\\B重金属与木本植物meta")

#1 SOD in leaf####
rm(list = ls())
d_t_sod <- read_xlsx("data-all.xlsx", sheet = "SOD")

table(d_t_sod$HMs_type)


#effect size
es_sod_total <- escalc(measure = "ROM", 
                       m1i = Xt,
                       sd1i = SDt,
                       n1i = Nt,
                       m2i = Xc,
                       sd2i = SDc,
                       n2i = Nc,
                       data = d_t_sod)

#HMs(Pb/Cd/Cu/Zn)
data_sod_Cd <- subset(es_sod_total, HMs_type == "Cd")
data_sod_Pb <- subset(es_sod_total, HMs_type == "Pb")
data_sod_Cu <- subset(es_sod_total, HMs_type == "Cu")
data_sod_Zn <- subset(es_sod_total, HMs_type == "Zn")


#Meta-analysis
#科属作为调节变量
res_sod_total <- rma.mv(yi, vi, data = es_sod_total, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_sod_Cd <- rma.mv(yi, vi, data = data_sod_Cd, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_sod_Pb <- rma.mv(yi, vi, data = data_sod_Pb, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_sod_Cu <- rma.mv(yi, vi, data = data_sod_Cu, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_sod_Zn <- rma.mv(yi, vi, data = data_sod_Zn, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


#extracting table of results
res_sod_total_reml <- mod_results(res_sod_total, mod = "Genus", group = "Study_ID") 
res_sod_Cd_reml <- mod_results(res_sod_Cd, mod = "Genus", group = "Study_ID") 
res_sod_Pb_reml <- mod_results(res_sod_Pb, mod = "Genus", group = "Study_ID") 
res_sod_Cu_reml <- mod_results(res_sod_Cu, mod = "Genus", group = "Study_ID") 
res_sod_Zn_reml <- mod_results(res_sod_Zn, mod = "Genus", group = "Study_ID") 


###Overall
res_sod_overall_reml <- res_sod_total_reml$mod_table
res_sod_overall_reml$col <- ifelse((exp(res_sod_overall_reml$lowerCL)-1)*100>=0,"No","Yes")
res_sod_overall_reml$Index <- c(rep("Overall",22))
res_sod_overall_reml$numb <- table(es_sod_total$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_overall <- (exp(res_sod_overall_reml$upperCL)-1)*100 + 30
sod_total <-
  ggplot(data=res_sod_overall_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,450), breaks=seq(-100,450, by=100))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_overall), size=5, color = "black")

sod_total

ggsave("Genus_sod_total_overall.pdf",sod_total, width = 8, height = 4.5, dpi = 600)


###Cd
res_sod_Cd_reml <- res_sod_Cd_reml$mod_table
res_sod_Cd_reml$col <- ifelse((exp(res_sod_Cd_reml$lowerCL)-1)*100>=0,"No","Yes")
res_sod_Cd_reml$Index <- c(rep("Cd",14))
res_sod_Cd_reml$numb <- table(data_sod_Cd$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cd <- (exp(res_sod_Cd_reml$upperCL)-1)*100 + 30
sod_Cd <-
  ggplot(data=res_sod_Cd_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,450), breaks=seq(-100,450, by=100))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cd), size=5, color = "black")

sod_Cd

ggsave("Genus_sod_total_Cd.pdf", sod_Cd, width = 5, height = 4.5, dpi = 600)



###Cu
res_sod_Cu_reml <- res_sod_Cu_reml$mod_table
res_sod_Cu_reml$col <- ifelse((exp(res_sod_Cu_reml$lowerCL)-1)*100>=0,"No","Yes")
res_sod_Cu_reml$Index <- c(rep("Cu",4))
res_sod_Cu_reml$numb <- table(data_sod_Cu$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cu <- (exp(res_sod_Cu_reml$upperCL)-1)*100 + 30
sod_Cu <-
  ggplot(data=res_sod_Cu_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,450), breaks=seq(-100,450, by=100))+
  scale_colour_manual(values = c( "gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cu), size=5, color = "black")

sod_Cu

ggsave("Genus_sod_total_Cu.pdf", sod_Cu, width = 3, height = 4.5, dpi = 600)


###Pb
res_sod_Pb_reml <- res_sod_Pb_reml$mod_table
res_sod_Pb_reml$col <- ifelse((exp(res_sod_Pb_reml$lowerCL)-1)*100>=0,"No","Yes")
res_sod_Pb_reml$Index <- c(rep("Pb",14))
res_sod_Pb_reml$numb <- table(data_sod_Pb$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Pb <- (exp(res_sod_Pb_reml$upperCL)-1)*100 + 30
sod_Pb <-
  ggplot(data=res_sod_Pb_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,480), breaks=seq(-100,480, by=100))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Pb), size=5, color = "black")

sod_Pb

ggsave("Genus_sod_total_Pb.pdf", sod_Pb, width = 5, height = 4.5, dpi = 600)



###Zn
res_sod_Zn_reml <- res_sod_Zn_reml$mod_table
res_sod_Zn_reml$col <- ifelse((exp(res_sod_Zn_reml$lowerCL)-1)*100>=0,"No","Yes")
res_sod_Zn_reml$Index <- c(rep("Zn",5))
res_sod_Zn_reml$numb <- table(data_sod_Zn$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Zn <- (exp(res_sod_Zn_reml$upperCL)-1)*100 + 30
sod_Zn <-
  ggplot(data=res_sod_Zn_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,480), breaks=seq(-100,480, by=100))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Zn), size=5, color = "black")

sod_Zn

ggsave("Genus_sod_total_Zn.pdf", sod_Zn, width = 3, height = 4.5, dpi = 600)



#2 POD in leaf####
rm(list = ls())
d_t_pod <- read_xlsx("data-all.xlsx", sheet = "POD")

table(d_t_pod$HMs_type)


#effect size
es_pod_total <- escalc(measure = "ROM", 
                       m1i = Xt,
                       sd1i = SDt,
                       n1i = Nt,
                       m2i = Xc,
                       sd2i = SDc,
                       n2i = Nc,
                       data = d_t_pod)

#HMs(Pb/Cd/Cu/Zn)
data_pod_Cd <- subset(es_pod_total, HMs_type == "Cd")
data_pod_Pb <- subset(es_pod_total, HMs_type == "Pb")
data_pod_Cu <- subset(es_pod_total, HMs_type == "Cu")
data_pod_Zn <- subset(es_pod_total, HMs_type == "Zn")


#Meta-analysis
#科属作为调节变量
res_pod_total <- rma.mv(yi, vi, data = es_pod_total, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_pod_Cd <- rma.mv(yi, vi, data = data_pod_Cd, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_pod_Pb <- rma.mv(yi, vi, data = data_pod_Pb, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_pod_Cu <- rma.mv(yi, vi, data = data_pod_Cu, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_pod_Zn <- rma.mv(yi, vi, data = data_pod_Zn, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


#extracting table of results
res_pod_total_reml <- mod_results(res_pod_total, mod = "Genus", group = "Study_ID") 
res_pod_Cd_reml <- mod_results(res_pod_Cd, mod = "Genus", group = "Study_ID") 
res_pod_Pb_reml <- mod_results(res_pod_Pb, mod = "Genus", group = "Study_ID") 
res_pod_Cu_reml <- mod_results(res_pod_Cu, mod = "Genus", group = "Study_ID") 
res_pod_Zn_reml <- mod_results(res_pod_Zn, mod = "Genus", group = "Study_ID") 


###Overall
res_pod_overall_reml <- res_pod_total_reml$mod_table
res_pod_overall_reml$col <- ifelse((exp(res_pod_overall_reml$lowerCL)-1)*100>=0,"No","Yes")
res_pod_overall_reml$Index <- c(rep("Overall",20))
res_pod_overall_reml$numb <- table(es_pod_total$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_overall <- (exp(res_pod_overall_reml$upperCL)-1)*100 + 50
pod_total <-
  ggplot(data=res_pod_overall_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,1000), breaks=seq(-100,1000, by=300))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_overall), size=5, color = "black")

pod_total

ggsave("Genus_pod_total_overall.pdf",pod_total, width = 8, height = 4.5, dpi = 600)


###Cd
res_pod_Cd_reml <- res_pod_Cd_reml$mod_table
res_pod_Cd_reml$col <- ifelse((exp(res_pod_Cd_reml$lowerCL)-1)*100>=0,"No","Yes")
res_pod_Cd_reml$Index <- c(rep("Cd",13))
res_pod_Cd_reml$numb <- table(data_pod_Cd$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cd <- (exp(res_pod_Cd_reml$upperCL)-1)*100 + 30
pod_Cd <-
  ggplot(data=res_pod_Cd_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,450), breaks=seq(-100,450, by=100))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cd), size=5, color = "black")

pod_Cd

ggsave("Genus_pod_total_Cd.pdf", pod_Cd, width = 5, height = 4.5, dpi = 600)



###Cu
res_pod_Cu_reml <- res_pod_Cu_reml$mod_table
res_pod_Cu_reml$col <- ifelse((exp(res_pod_Cu_reml$lowerCL)-1)*100>=0,"No","Yes")
res_pod_Cu_reml$Index <- c(rep("Cu",3))
res_pod_Cu_reml$numb <- table(data_pod_Cu$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cu <- (exp(res_pod_Cu_reml$upperCL)-1)*100 + 50
pod_Cu <-
  ggplot(data=res_pod_Cu_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,900), breaks=seq(-100,900, by=300))+
  scale_colour_manual(values = c( "gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cu), size=5, color = "black")

pod_Cu

ggsave("Genus_pod_total_Cu.pdf", pod_Cu, width = 3, height = 4.5, dpi = 600)


###Pb
res_pod_Pb_reml <- res_pod_Pb_reml$mod_table
res_pod_Pb_reml$col <- ifelse((exp(res_pod_Pb_reml$lowerCL)-1)*100>=0,"No","Yes")
res_pod_Pb_reml$Index <- c(rep("Pb",12))
res_pod_Pb_reml$numb <- table(data_pod_Pb$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Pb <- (exp(res_pod_Pb_reml$upperCL)-1)*100 + 100
pod_Pb <-
  ggplot(data=res_pod_Pb_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,1500), breaks=seq(-100,1500, by=500))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Pb), size=5, color = "black")

pod_Pb

ggsave("Genus_pod_total_Pb.pdf", pod_Pb, width = 5, height = 4.5, dpi = 600)



###Zn
res_pod_Zn_reml <- res_pod_Zn_reml$mod_table
res_pod_Zn_reml$col <- ifelse((exp(res_pod_Zn_reml$lowerCL)-1)*100>=0,"No","Yes")
res_pod_Zn_reml$Index <- c(rep("Zn",5))
res_pod_Zn_reml$numb <- table(data_pod_Zn$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Zn <- (exp(res_pod_Zn_reml$upperCL)-1)*100 + 30
pod_Zn <-
  ggplot(data=res_pod_Zn_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,450), breaks=seq(-100,450, by=100))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Zn), size=5, color = "black")

pod_Zn

ggsave("Genus_pod_total_Zn.pdf", pod_Zn, width = 4, height = 4.5, dpi = 600)


####3 CAT in leaf####
rm(list = ls())
d_t_cat <- read_xlsx("data-all.xlsx", sheet = "CAT")

table(d_t_cat$HMs_type)

#effect size
es_cat_total <- escalc(measure = "ROM", 
                       m1i = Xt,
                       sd1i = SDt,
                       n1i = Nt,
                       m2i = Xc,
                       sd2i = SDc,
                       n2i = Nc,
                       data = d_t_cat)

#HMs(Pb/Cd/Cu/Zn)
data_cat_Cd <- subset(es_cat_total, HMs_type == "Cd")
data_cat_Pb <- subset(es_cat_total, HMs_type == "Pb")
data_cat_Cu <- subset(es_cat_total, HMs_type == "Cu")
data_cat_Zn <- subset(es_cat_total, HMs_type == "Zn")


#Meta-analysis
#科属作为调节变量
res_cat_total <- rma.mv(yi, vi, data = es_cat_total, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_cat_Cd <- rma.mv(yi, vi, data = data_cat_Cd, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_cat_Pb <- rma.mv(yi, vi, data = data_cat_Pb, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_cat_Cu <- rma.mv(yi, vi, data = data_cat_Cu, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_cat_Zn <- rma.mv(yi, vi, data = data_cat_Zn, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


#extracting table of results
res_cat_total_reml <- mod_results(res_cat_total, mod = "Genus", group = "Study_ID") 
res_cat_Cd_reml <- mod_results(res_cat_Cd, mod = "Genus", group = "Study_ID") 
res_cat_Pb_reml <- mod_results(res_cat_Pb, mod = "Genus", group = "Study_ID") 
res_cat_Cu_reml <- mod_results(res_cat_Cu, mod = "Genus", group = "Study_ID") 
res_cat_Zn_reml <- mod_results(res_cat_Zn, mod = "Genus", group = "Study_ID") 


###Overall
res_cat_overall_reml <- res_cat_total_reml$mod_table
res_cat_overall_reml$col <- ifelse((exp(res_cat_overall_reml$lowerCL)-1)*100>=0,"No","Yes")
res_cat_overall_reml$Index <- c(rep("Overall",17))
res_cat_overall_reml$numb <- table(es_cat_total$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_overall <- (exp(res_cat_overall_reml$upperCL)-1)*100 + 50
cat_total <-
  ggplot(data=res_cat_overall_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,1000), breaks=seq(-100,1000, by=300))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_overall), size=5, color = "black")

cat_total

ggsave("Genus_cat_total_overall.pdf",cat_total, width = 8, height = 4.5, dpi = 600)


###Cd
res_cat_Cd_reml <- res_cat_Cd_reml$mod_table
res_cat_Cd_reml$col <- ifelse((exp(res_cat_Cd_reml$lowerCL)-1)*100>=0,"No","Yes")
res_cat_Cd_reml$Index <- c(rep("Cd",13))
res_cat_Cd_reml$numb <- table(data_cat_Cd$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cd <- (exp(res_cat_Cd_reml$upperCL)-1)*100 + 60
cat_Cd <-
  ggplot(data=res_cat_Cd_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,1200), breaks=seq(-100,1200, by=500))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cd), size=5, color = "black")

cat_Cd

ggsave("Genus_cat_total_Cd.pdf", cat_Cd, width = 5, height = 4.5, dpi = 600)



###Cu
res_cat_Cu_reml <- res_cat_Cu_reml$mod_table
res_cat_Cu_reml$col <- ifelse((exp(res_cat_Cu_reml$lowerCL)-1)*100>=0,"No","Yes")
res_cat_Cu_reml$Index <- c(rep("Cu",3))
res_cat_Cu_reml$numb <- table(data_cat_Cu$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cu <- (exp(res_cat_Cu_reml$upperCL)-1)*100 + 20
cat_Cu <-
  ggplot(data=res_cat_Cu_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,150), breaks=seq(-100,150, by=100))+
  scale_colour_manual(values = c( "gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cu), size=5, color = "black")

cat_Cu

ggsave("Genus_cat_total_Cu.pdf", cat_Cu, width = 3, height = 4.5, dpi = 600)


###Pb
res_cat_Pb_reml <- res_cat_Pb_reml$mod_table
res_cat_Pb_reml$col <- ifelse((exp(res_cat_Pb_reml$lowerCL)-1)*100>=0,"No","Yes")
res_cat_Pb_reml$Index <- c(rep("Pb",9))
res_cat_Pb_reml$numb <- table(data_cat_Pb$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Pb <- (exp(res_cat_Pb_reml$upperCL)-1)*100 + 60
cat_Pb <-
  ggplot(data=res_cat_Pb_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,1500), breaks=seq(-100,1500, by=500))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Pb), size=5, color = "black")

cat_Pb

ggsave("Genus_cat_total_Pb.pdf", cat_Pb, width = 5, height = 4.5, dpi = 600)



###Zn
res_cat_Zn_reml <- res_cat_Zn_reml$mod_table
res_cat_Zn_reml$col <- ifelse((exp(res_cat_Zn_reml$lowerCL)-1)*100>=0,"No","Yes")
res_cat_Zn_reml$Index <- c(rep("Zn",5))
res_cat_Zn_reml$numb <- table(data_cat_Zn$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Zn <- (exp(res_cat_Zn_reml$upperCL)-1)*100 + 20
cat_Zn <-
  ggplot(data=res_cat_Zn_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,180), breaks=seq(-100,180, by=100))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Zn), size=5, color = "black")

cat_Zn

ggsave("Genus_cat_total_Zn.pdf", cat_Zn, width = 4, height = 4.5, dpi = 600)


####4 MDA in leaf####
rm(list = ls())
d_t_mda <- read_xlsx("data-all.xlsx", sheet = "MDA")

table(d_t_mda$HMs_type)
#effect size
es_mda_total <- escalc(measure = "ROM", 
                       m1i = Xt,
                       sd1i = SDt,
                       n1i = Nt,
                       m2i = Xc,
                       sd2i = SDc,
                       n2i = Nc,
                       data = d_t_mda)

#HMs(Pb/Cd/Cu/Zn)
data_mda_Cd <- subset(es_mda_total, HMs_type == "Cd")
data_mda_Pb <- subset(es_mda_total, HMs_type == "Pb")
data_mda_Cu <- subset(es_mda_total, HMs_type == "Cu")
data_mda_Zn <- subset(es_mda_total, HMs_type == "Zn")


#Meta-analysis
#科属作为调节变量
res_mda_total <- rma.mv(yi, vi, data = es_mda_total, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_mda_Cd <- rma.mv(yi, vi, data = data_mda_Cd, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_mda_Pb <- rma.mv(yi, vi, data = data_mda_Pb, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_mda_Cu <- rma.mv(yi, vi, data = data_mda_Cu, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_mda_Zn <- rma.mv(yi, vi, data = data_mda_Zn, mods = ~ Genus-1, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


#extracting table of results
res_mda_total_reml <- mod_results(res_mda_total, mod = "Genus", group = "Study_ID") 
res_mda_Cd_reml <- mod_results(res_mda_Cd, mod = "Genus", group = "Study_ID") 
res_mda_Pb_reml <- mod_results(res_mda_Pb, mod = "Genus", group = "Study_ID") 
res_mda_Cu_reml <- mod_results(res_mda_Cu, mod = "Genus", group = "Study_ID") 
res_mda_Zn_reml <- mod_results(res_mda_Zn, mod = "Genus", group = "Study_ID") 


###Overall
res_mda_overall_reml <- res_mda_total_reml$mod_table
res_mda_overall_reml$col <- ifelse((exp(res_mda_overall_reml$lowerCL)-1)*100>=0,"No","Yes")
res_mda_overall_reml$Index <- c(rep("Overall",18))
res_mda_overall_reml$numb <- table(es_mda_total$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_overall <- (exp(res_mda_overall_reml$upperCL)-1)*100 + 50
mda_total <-
  ggplot(data=res_mda_overall_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,500), breaks=seq(-100,500, by=200))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_overall), size=5, color = "black")

mda_total

ggsave("Genus_mda_total_overall.pdf",mda_total, width = 8, height = 4.5, dpi = 600)


###Cd
res_mda_Cd_reml <- res_mda_Cd_reml$mod_table
res_mda_Cd_reml$col <- ifelse((exp(res_mda_Cd_reml$lowerCL)-1)*100>=0,"No","Yes")
res_mda_Cd_reml$Index <- c(rep("Cd",10))
res_mda_Cd_reml$numb <- table(data_mda_Cd$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cd <- (exp(res_mda_Cd_reml$upperCL)-1)*100 + 60
mda_Cd <-
  ggplot(data=res_mda_Cd_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,500), breaks=seq(-100,500, by=200))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cd), size=5, color = "black")

mda_Cd

ggsave("Genus_mda_total_Cd.pdf", mda_Cd, width = 5, height = 4.5, dpi = 600)



###Cu
res_mda_Cu_reml <- res_mda_Cu_reml$mod_table
res_mda_Cu_reml$col <- ifelse((exp(res_mda_Cu_reml$lowerCL)-1)*100>=0,"No","Yes")
res_mda_Cu_reml$Index <- c(rep("Cu",4))
res_mda_Cu_reml$numb <- table(data_mda_Cu$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Cu <- (exp(res_mda_Cu_reml$upperCL)-1)*100 + 20
mda_Cu <-
  ggplot(data=res_mda_Cu_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("red","gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cu), size=5, color = "black")

mda_Cu

ggsave("Genus_mda_total_Cu.pdf", mda_Cu, width = 4, height = 4.5, dpi = 600)


###Pb
res_mda_Pb_reml <- res_mda_Pb_reml$mod_table
res_mda_Pb_reml$col <- ifelse((exp(res_mda_Pb_reml$lowerCL)-1)*100>=0,"No","Yes")
res_mda_Pb_reml$Index <- c(rep("Pb",10))
res_mda_Pb_reml$numb <- table(data_mda_Pb$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Pb <- (exp(res_mda_Pb_reml$upperCL)-1)*100 + 60
mda_Pb <-
  ggplot(data=res_mda_Pb_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,1000), breaks=seq(-100,1000, by=500))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Pb), size=5, color = "black")

mda_Pb

ggsave("Genus_mda_total_Pb.pdf", mda_Pb, width = 5, height = 4.5, dpi = 600)



###Zn
res_mda_Zn_reml <- res_mda_Zn_reml$mod_table
res_mda_Zn_reml$col <- ifelse((exp(res_mda_Zn_reml$lowerCL)-1)*100>=0,"No","Yes")
res_mda_Zn_reml$Index <- c(rep("Zn",3))
res_mda_Zn_reml$numb <- table(data_mda_Zn$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
lebal_Zn <- (exp(res_mda_Zn_reml$upperCL)-1)*100 + 20
mda_Zn <-
  ggplot(data=res_mda_Zn_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  labs(x='Genus', y='Percentage change (%)')+
  facet_grid(Index~.,switch = "x")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,180), breaks=seq(-100,180, by=100))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Zn), size=5, color = "black")

mda_Zn

ggsave("Genus_mda_total_Zn.pdf", mda_Zn, width = 3, height = 4.5, dpi = 600)
