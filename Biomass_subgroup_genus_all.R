setwd("E:\\博士研究生\\Meta分析论文\\B重金属与木本植物meta")


#1 Total biomass in genus####
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

#HMs(No As)
data_total_Cd <- subset(es_total_biomass, HMs_type == "Cd")
data_total_Pb <- subset(es_total_biomass, HMs_type == "Pb")
data_total_Cu <- subset(es_total_biomass, HMs_type == "Cu")
data_total_Zn <- subset(es_total_biomass, HMs_type == "Zn")
data_total_Sb <- subset(es_total_biomass, HMs_type == "Sb")


aggregate(data_total_Cd$Genus, by = list(data_total_Cd$Genus), FUN = length)
aggregate(data_total_Pb$Genus, by = list(data_total_Pb$Genus), FUN = length)
aggregate(data_total_Cu$Genus, by = list(data_total_Cu$Genus), FUN = length)
aggregate(data_total_Zn$Genus, by = list(data_total_Zn$Genus), FUN = length)
aggregate(data_total_Sb$Genus, by = list(data_total_Sb$Genus), FUN = length)


#Subgroup-genus
res_total_overall <- rma.mv(yi, vi, mods = ~ Genus-1, data = es_total_biomass, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_total_Cd <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_total_Cd, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_total_Pb <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_total_Pb, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_total_Cu <- rma(yi, vi, mods = ~ Genus-1, data = data_total_Cu, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_total_Zn <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_total_Zn, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_total_Sb <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_total_Sb, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#extracting table of results
res_total_overall_reml <- mod_results(res_total_overall, mod = "Genus", group = "Study_ID") 
res_total_Cd_reml <- mod_results(res_total_Cd, mod = "Genus", group = "Study_ID") 
res_total_Pb_reml <- mod_results(res_total_Pb, mod = "Genus", group = "Study_ID") 
res_total_Cu_reml <- mod_results(res_total_Cu, mod = "Genus", group = "Study_ID") 
res_total_Zn_reml <- mod_results(res_total_Zn, mod = "Genus", group = "Study_ID") 
res_total_Sb_reml <- mod_results(res_total_Sb, mod = "Genus", group = "Study_ID") 

table(es_total_biomass$Genus)

###Overall
res_total_overall_reml <- res_total_overall_reml$mod_table
res_total_overall_reml$col <- ifelse((exp(res_total_overall_reml$upperCL)-1)*100>=0,"No","Yes")
res_total_overall_reml$Index <- c(rep("Overall",45))
res_total_overall_reml$numb <- table(es_total_biomass$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_overall <- (exp(res_total_overall_reml$upperCL)-1)*100 + 20
overall <-ggplot(data=res_total_overall_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_overall), size=5, color = "black")
overall
ggsave("Genus_total_biomass_hms_overall.pdf",overall, width = 12, height = 5, dpi = 600)


###Cd
res_total_Cd <- res_total_Cd_reml$mod_table
res_total_Cd$col <- ifelse((exp(res_total_Cd$upperCL)-1)*100>=0,"No","Yes")
res_total_Cd$Index <- c(rep("Cd",32))
res_total_Cd$numb <- table(data_total_Cd$Genus)
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_Cd <- (exp(res_total_Cd$upperCL)-1)*100 + 20
Cd <-ggplot(data=res_total_Cd, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cd), size=5, color = "black")
Cd
ggsave("Genus_total_biomass_hms_Cd.pdf", width = 12, height = 5, dpi = 600)

###Cu
res_total_Cu <- res_total_Cu_reml$mod_table
res_total_Cu$col <- ifelse((exp(res_total_Cu$upperCL)-1)*100>=0,"No","Yes")
res_total_Cu$Index <- c(rep("Cu",5))
res_total_Cu$numb <- table(data_total_Cu$Genus)
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_Cu <- (exp(res_total_Cu$upperCL)-1)*100 + 20
Cu <-ggplot(data=res_total_Cu, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cu), size=5, color = "black")
Cu
ggsave("Genus_total_biomass_hms_Cu.pdf", width = 4, height = 5, dpi = 600)

###Zn
res_total_Zn <- res_total_Zn_reml$mod_table
res_total_Zn$col <- ifelse((exp(res_total_Zn$upperCL)-1)*100>=0,"No","Yes")
res_total_Zn$Index <- c(rep("Zn",7))
res_total_Zn$numb <- table(data_total_Zn$Genus)
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_Zn <- (exp(res_total_Zn$upperCL)-1)*100 + 20
Zn <-ggplot(data=res_total_Zn, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Zn), size=5, color = "black")
Zn
ggsave("Genus_total_biomass_hms_Zn.pdf", width = 4, height = 5, dpi = 600)


###Sb
res_total_Sb <- res_total_Sb_reml$mod_table
res_total_Sb$col <- ifelse((exp(res_total_Sb$upperCL)-1)*100>=0,"No","Yes")
res_total_Sb$Index <- c(rep("Sb",3))
res_total_Sb$numb <- table(data_total_Sb$Genus)
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_Sb <- (exp(res_total_Sb$upperCL)-1)*100 + 20
Sb <-ggplot(data=res_total_Sb, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Sb), size=5, color = "black")
Sb

ggsave("Genus_total_biomass_hms_Sb.pdf", width = 2.5, height = 5, dpi = 600)




#2 Aboveground biomass in genus####
rm(list = ls())
d_ab <- read_xlsx("data-all.xlsx", sheet = "Aboveground_biomass")


table(es_above_biomass$HMs_type)
#effect size
es_above_biomass<-escalc(measure = "ROM", 
                         m1i = Xt,
                         sd1i = SDt,
                         n1i = Nt,
                         m2i = Xc,
                         sd2i = SDc,
                         n2i = Nc,
                         data = d_ab)

#HMs
data_above_Cd <- subset(es_above_biomass, HMs_type == "Cd")
data_above_Pb <- subset(es_above_biomass, HMs_type == "Pb")
data_above_Cu <- subset(es_above_biomass, HMs_type == "Cu")
data_above_Zn <- subset(es_above_biomass, HMs_type == "Zn")
data_above_As <- subset(es_above_biomass, HMs_type == "As")


aggregate(data_above_Cd$Genus, by = list(data_above_Cd$Genus), FUN = length)
aggregate(data_above_Pb$Genus, by = list(data_above_Pb$Genus), FUN = length)
aggregate(data_above_Cu$Genus, by = list(data_above_Cu$Genus), FUN = length)
aggregate(data_above_Zn$Genus, by = list(data_above_Zn$Genus), FUN = length)
aggregate(data_above_As$Genus, by = list(data_above_As$Genus), FUN = length)


#Subgroup-genus
res_above_overall <- rma.mv(yi, vi, mods = ~ Genus-1, data = es_above_biomass, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_above_Cd <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_above_Cd, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_above_Pb <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_above_Pb, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_above_Cu <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_above_Cu, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_above_Zn <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_above_Zn, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_above_As <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_above_As, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#extracting table of results
res_above_overall_reml <- mod_results(res_above_overall, mod = "Genus", group = "Study_ID") 
res_above_Cd_reml <- mod_results(res_above_Cd, mod = "Genus", group = "Study_ID") 
res_above_Pb_reml <- mod_results(res_above_Pb, mod = "Genus", group = "Study_ID") 
res_above_Cu_reml <- mod_results(res_above_Cu, mod = "Genus", group = "Study_ID") 
res_above_Zn_reml <- mod_results(res_above_Zn, mod = "Genus", group = "Study_ID") 
res_above_As_reml <- mod_results(res_above_As, mod = "Genus", group = "Study_ID") 

table(es_above_biomass$Genus)

###Overall
res_above_overall_reml <- res_above_overall_reml$mod_table
res_above_overall_reml$col <- ifelse((exp(res_above_overall_reml$upperCL)-1)*100>=0,"No","Yes")
res_above_overall_reml$Index <- c(rep("Overall",29))
res_above_overall_reml$numb <- table(es_above_biomass$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_overall <- (exp(res_above_overall_reml$upperCL)-1)*100 + 20
overall <-ggplot(data=res_above_overall_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_overall), size=5, color = "black")
overall
ggsave("Genus_above_biomass_hms_overall.pdf",overall, width = 10, height = 5, dpi = 600)

###Cd
res_above_Cd <- res_above_Cd_reml$mod_table
res_above_Cd$col <- ifelse((exp(res_above_Cd$upperCL)-1)*100>=0,"No","Yes")
res_above_Cd$Index <- c(rep("Cd",21))
res_above_Cd$numb <- table(data_above_Cd$Genus)
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_Cd <- (exp(res_above_Cd$upperCL)-1)*100 + 20
Cd <-ggplot(data=res_above_Cd, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cd), size=5, color = "black")
Cd
ggsave("Genus_above_biomass_hms_Cd.pdf",Cd, width = 10, height = 5, dpi = 600)

###Cu
res_above_Cu <- res_above_Cu_reml$mod_table
res_above_Cu$col <- ifelse((exp(res_above_Cu$upperCL)-1)*100>=0,"No","Yes")
res_above_Cu$Index <- c(rep("Cu",5))
res_above_Cu$numb <- table(data_above_Cu$Genus)
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_Cu <- (exp(res_above_Cu$upperCL)-1)*100 + 20
Cu <-ggplot(data=res_above_Cu, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cu), size=5, color = "black")
Cu
ggsave("Genus_above_biomass_hms_Cu.pdf",Cu, width = 3.5, height = 5, dpi = 600)

###Zn
res_above_Zn <- res_above_Zn_reml$mod_table
res_above_Zn$col <- ifelse((exp(res_above_Zn$upperCL)-1)*100>=0,"No","Yes")
res_above_Zn$Index <- c(rep("Zn",9))
res_above_Zn$numb <- table(data_above_Zn$Genus)
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_Zn <- (exp(res_above_Zn$upperCL)-1)*100 + 20
Zn <-ggplot(data=res_above_Zn, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Zn), size=5, color = "black")
Zn
ggsave("Genus_above_biomass_hms_Zn.pdf",Zn, width = 4, height = 5, dpi = 600)


###As
res_above_As <- res_above_As_reml$mod_table
res_above_As$col <- ifelse((exp(res_above_As$upperCL)-1)*100>=0,"No","Yes")
res_above_As$Index <- c(rep("As",2))
res_above_As$numb <- table(data_above_As$Genus)
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_As <- (exp(res_above_As$upperCL)-1)*100 + 20
As <-ggplot(data=res_above_As, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_As), size=5, color = "black")
As

ggsave("Genus_above_biomass_hms_As.pdf",As, width = 2.3, height = 5, dpi = 600)



#2 Underground biomass in genus####
rm(list = ls())
d_ub <- read_xlsx("data-all.xlsx", sheet = "Root_biomass")


#effect size
es_under_biomass<-escalc(measure = "ROM", 
                         m1i = Xt,
                         sd1i = SDt,
                         n1i = Nt,
                         m2i = Xc,
                         sd2i = SDc,
                         n2i = Nc,
                         data = d_ab)
table(es_under_biomass$HMs_type)
#HMs
data_under_Cd <- subset(es_under_biomass, HMs_type == "Cd")
data_under_Pb <- subset(es_under_biomass, HMs_type == "Pb")
data_under_Cu <- subset(es_under_biomass, HMs_type == "Cu")
data_under_Zn <- subset(es_under_biomass, HMs_type == "Zn")
data_under_As <- subset(es_under_biomass, HMs_type == "As")


aggregate(data_under_Cd$Genus, by = list(data_under_Cd$Genus), FUN = length)
aggregate(data_under_Pb$Genus, by = list(data_under_Pb$Genus), FUN = length)
aggregate(data_under_Cu$Genus, by = list(data_under_Cu$Genus), FUN = length)
aggregate(data_under_Zn$Genus, by = list(data_under_Zn$Genus), FUN = length)
aggregate(data_under_As$Genus, by = list(data_under_As$Genus), FUN = length)


#Subgroup-genus
res_under_overall <- rma.mv(yi, vi, mods = ~ Genus-1, data = es_under_biomass, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_under_Cd <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_under_Cd, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_under_Pb <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_under_Pb, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_under_Cu <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_under_Cu, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_under_Zn <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_under_Zn, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_under_As <- rma.mv(yi, vi, mods = ~ Genus-1, data = data_under_As, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#extracting table of results
res_under_overall_reml <- mod_results(res_under_overall, mod = "Genus", group = "Study_ID") 
res_under_Cd_reml <- mod_results(res_under_Cd, mod = "Genus", group = "Study_ID") 
res_under_Pb_reml <- mod_results(res_under_Pb, mod = "Genus", group = "Study_ID") 
res_under_Cu_reml <- mod_results(res_under_Cu, mod = "Genus", group = "Study_ID") 
res_under_Zn_reml <- mod_results(res_under_Zn, mod = "Genus", group = "Study_ID") 
res_under_As_reml <- mod_results(res_under_As, mod = "Genus", group = "Study_ID") 

table(es_under_biomass$Genus)

###Overall
res_under_overall_reml <- res_under_overall_reml$mod_table
res_under_overall_reml$col <- ifelse((exp(res_under_overall_reml$upperCL)-1)*100>=0,"No","Yes")
res_under_overall_reml$Index <- c(rep("Overall",29))
res_under_overall_reml$numb <- table(es_under_biomass$Genus)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_overall <- (exp(res_under_overall_reml$upperCL)-1)*100 + 20
overall <-ggplot(data=res_under_overall_reml, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_overall), size=5, color = "black")
overall
ggsave("Genus_under_biomass_hms_overall.pdf",overall, width = 10, height = 5, dpi = 600)

###Cd
res_under_Cd <- res_under_Cd_reml$mod_table
res_under_Cd$col <- ifelse((exp(res_under_Cd$upperCL)-1)*100>=0,"No","Yes")
res_under_Cd$Index <- c(rep("Cd",21))
res_under_Cd$numb <- table(data_under_Cd$Genus)
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_Cd <- (exp(res_under_Cd$upperCL)-1)*100 + 20
Cd <-ggplot(data=res_under_Cd, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cd), size=5, color = "black")
Cd
ggsave("Genus_under_biomass_hms_Cd.pdf",Cd, width = 10, height = 5, dpi = 600)

###Cu
res_under_Cu <- res_under_Cu_reml$mod_table
res_under_Cu$col <- ifelse((exp(res_under_Cu$upperCL)-1)*100>=0,"No","Yes")
res_under_Cu$Index <- c(rep("Cu",5))
res_under_Cu$numb <- table(data_under_Cu$Genus)
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_Cu <- (exp(res_under_Cu$upperCL)-1)*100 + 20
Cu <-ggplot(data=res_under_Cu, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Cu), size=5, color = "black")
Cu
ggsave("Genus_under_biomass_hms_Cu.pdf",Cu, width = 3.5, height = 5, dpi = 600)

###Zn
res_under_Zn <- res_under_Zn_reml$mod_table
res_under_Zn$col <- ifelse((exp(res_under_Zn$upperCL)-1)*100>=0,"No","Yes")
res_under_Zn$Index <- c(rep("Zn",9))
res_under_Zn$numb <- table(data_under_Zn$Genus)
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_Zn <- (exp(res_under_Zn$upperCL)-1)*100 + 20
Zn <-ggplot(data=res_under_Zn, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_Zn), size=5, color = "black")
Zn
ggsave("Genus_under_biomass_hms_Zn.pdf",Zn, width = 4, height = 5, dpi = 600)


###As
res_under_As <- res_under_As_reml$mod_table
res_under_As$col <- ifelse((exp(res_under_As$upperCL)-1)*100>=0,"No","Yes")
res_under_As$Index <- c(rep("As",2))
res_under_As$numb <- table(data_under_As$Genus)
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#number
lebal_As <- (exp(res_under_As$upperCL)-1)*100 + 20
As <-ggplot(data=res_under_As, aes(x=reorder(name, estimate), y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  theme_bw()+
  facet_grid(Index~.,switch = "x")+
  labs(x='Genus', y='Percentage change (%)')+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,250), breaks=seq(-100,250, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.y = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  geom_text(aes(label=numb,x=name,y=lebal_As), size=5, color = "black")
As

ggsave("Genus_under_biomass_hms_As.pdf",As, width = 2.3, height = 5, dpi = 600)

