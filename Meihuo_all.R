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

#一步到位版
#HMs_type
res_total_sod_overall <- rma.mv(yi, vi, data = es_sod_total, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_total_sod_HMs <- rma.mv(yi, vi, data = es_sod_total, mod = ~HMs_type-1, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#提取结果
res_total_sod_overall_reml <- mod_results(res_total_sod_overall, mod = "1", group = "Study_ID") 
res_total_sod_HMs_reml <- mod_results(res_total_sod_HMs, mod = "HMs_type", group = "Study_ID")
#汇总数据
fig_data_total_sod_HMs <-rbind(res_total_sod_overall_reml$mod_table,
                               res_total_sod_HMs_reml$mod_table)

table(d_t_sod$HMs_type)
fig_data_total_sod_HMs$HMs <- c("Overall (311)",
                                "Cd (156)",
                                "Cu (87)",
                                "Pb (51)",
                                "Zn (17)")

fig_data_total_sod_HMs$col <- ifelse((exp(fig_data_total_sod_HMs$lowerCL)-1)*100<=0,"No","Yes")
fig_data_total_sod_HMs$Index <- c(rep("SOD",5))

fig_data_total_sod_HMs$HMs <- factor(fig_data_total_sod_HMs$HMs, levels = c("Zn (17)","Pb (51)", "Cu (87)", "Cd (156)", "Overall (311)"))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
Fig_3_a <-
  ggplot(data=fig_data_total_sod_HMs, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50,100), breaks=seq(-50,100, by=50))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 4.5, linetype = 'dashed', col = 'gray70')

Fig_3_a



#HMs(Pb/Cd/Cu/Zn)
data_sod_Cd <- subset(es_sod_total, HMs_type == "Cd")
data_sod_Pb <- subset(es_sod_total, HMs_type == "Pb")
data_sod_Cu <- subset(es_sod_total, HMs_type == "Cu")
data_sod_Zn <- subset(es_sod_total, HMs_type == "Zn")


#Meta-analysis
#科属作为调节变量
res_sod_total <- rma.mv(yi, vi, data = es_sod_total, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_sod_Cd <- rma.mv(yi, vi, data = data_sod_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_sod_Pb <- rma.mv(yi, vi, data = data_sod_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_sod_Cu <- rma.mv(yi, vi, data = data_sod_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_sod_Zn <- rma.mv(yi, vi, data = data_sod_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


#extracting table of results
res_sod_total_reml <- mod_results(res_sod_total, mod = "1", group = "Study_ID") 
res_sod_Cd_reml <- mod_results(res_sod_Cd, mod = "1", group = "Study_ID") 
res_sod_Pb_reml <- mod_results(res_sod_Pb, mod = "1", group = "Study_ID") 
res_sod_Cu_reml <- mod_results(res_sod_Cu, mod = "1", group = "Study_ID") 
res_sod_Zn_reml <- mod_results(res_sod_Zn, mod = "1", group = "Study_ID") 

#Association plot
fig_data_sod <-rbind(res_sod_total_reml$mod_table,
                     res_sod_Cd_reml$mod_table,
                     res_sod_Pb_reml$mod_table,
                     res_sod_Cu_reml$mod_table,
                     res_sod_Zn_reml$mod_table)
table(es_sod_total$HMs_type)
fig_data_sod$HMs <- c("Overall (311)",
                      "Cd (156)",
                      "Pb (51)",
                      "Cu (87)",
                      "Zn (17)")

fig_data_sod$col <- ifelse((exp(fig_data_sod$lowerCL)-1)*100>=0,"Yes","No")
fig_data_sod$Index <- c(rep("SOD",5))

fig_data_sod$HMs <- factor(fig_data_sod$HMs, levels = rev(fig_data_sod$HMs))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
sod_total <-
  ggplot(data=fig_data_sod, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50,200), breaks=seq(-50,200, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 4.5, linetype = 'dashed', col = 'black')
sod_total

#ggsave("total_biomass.pdf", width = 4.5, height = 6.5, dpi = 600)


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

#一步到位版
#HMs_type
res_total_pod_overall <- rma.mv(yi, vi, data = es_pod_total, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_total_pod_HMs <- rma.mv(yi, vi, data = es_pod_total, mod = ~HMs_type-1, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#提取结果
res_total_pod_overall_reml <- mod_results(res_total_pod_overall, mod = "1", group = "Study_ID") 
res_total_pod_HMs_reml <- mod_results(res_total_pod_HMs, mod = "HMs_type", group = "Study_ID")
#汇总数据
fig_data_total_pod_HMs <-rbind(res_total_pod_overall_reml$mod_table,
                               res_total_pod_HMs_reml$mod_table)

table(d_t_pod$HMs_type)
fig_data_total_pod_HMs$HMs <- c("Overall (265)",
                                "Cd (137)",
                                "Cu (71)",
                                "Pb (40)",
                                "Zn (17)")

fig_data_total_pod_HMs$col <- ifelse((exp(fig_data_total_pod_HMs$lowerCL)-1)*100<=0,"No","Yes")
fig_data_total_pod_HMs$Index <- c(rep("POD",5))

fig_data_total_pod_HMs$HMs <- factor(fig_data_total_pod_HMs$HMs, levels = c("Zn (17)","Pb (40)", "Cu (71)", "Cd (137)", "Overall (265)"))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
Fig_3_b <-
  ggplot(data=fig_data_total_pod_HMs, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50,100), breaks=seq(-50,100, by=50))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 4.5, linetype = 'dashed', col = 'gray70')

Fig_3_b



#HMs(Pb/Cd/Cu/Zn)
data_pod_Cd <- subset(es_pod_total, HMs_type == "Cd")
data_pod_Pb <- subset(es_pod_total, HMs_type == "Pb")
data_pod_Cu <- subset(es_pod_total, HMs_type == "Cu")
data_pod_Zn <- subset(es_pod_total, HMs_type == "Zn")


#Meta-analysis
#科属作为调节变量
res_pod_total <- rma.mv(yi, vi, data = es_pod_total, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_pod_Cd <- rma.mv(yi, vi, data = data_pod_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_pod_Pb <- rma.mv(yi, vi, data = data_pod_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_pod_Cu <- rma.mv(yi, vi, data = data_pod_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_pod_Zn <- rma.mv(yi, vi, data = data_pod_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


#extracting table of results
res_pod_total_reml <- mod_results(res_pod_total, mod = "1", group = "Study_ID") 
res_pod_Cd_reml <- mod_results(res_pod_Cd, mod = "1", group = "Study_ID") 
res_pod_Pb_reml <- mod_results(res_pod_Pb, mod = "1", group = "Study_ID") 
res_pod_Cu_reml <- mod_results(res_pod_Cu, mod = "1", group = "Study_ID") 
res_pod_Zn_reml <- mod_results(res_pod_Zn, mod = "1", group = "Study_ID") 

#Association plot
fig_data_pod <-rbind(res_pod_total_reml$mod_table,
                     res_pod_Cd_reml$mod_table,
                     res_pod_Pb_reml$mod_table,
                     res_pod_Cu_reml$mod_table,
                     res_pod_Zn_reml$mod_table)
table(es_pod_total$HMs_type)
fig_data_pod$HMs <- c("Overall (265)",
                      "Cd (137)",
                      "Pb (40)",
                      "Cu (71)",
                      "Zn (17)")

fig_data_pod$col <- ifelse((exp(fig_data_pod$lowerCL)-1)*100>=0,"Yes","No")
fig_data_pod$Index <- c(rep("POD",5))

fig_data_pod$HMs <- factor(fig_data_pod$HMs, levels = rev(fig_data_pod$HMs))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
pod_total <-
  ggplot(data=fig_data_pod, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50,150), breaks=seq(-50,150, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 4.5, linetype = 'dashed', col = 'black')
pod_total

#ggsave("total_biomass.pdf", width = 4.5, height = 6.5, dpi = 600)


#3 CAT in leaf####
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

#一步到位版
#HMs_type
res_total_cat_overall <- rma.mv(yi, vi, data = es_cat_total, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_total_cat_HMs <- rma.mv(yi, vi, data = es_cat_total, mod = ~HMs_type-1, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#提取结果
res_total_cat_overall_reml <- mod_results(res_total_cat_overall, mod = "1", group = "Study_ID") 
res_total_cat_HMs_reml <- mod_results(res_total_cat_HMs, mod = "HMs_type", group = "Study_ID")
#汇总数据
fig_data_total_cat_HMs <-rbind(res_total_cat_overall_reml$mod_table,
                               res_total_cat_HMs_reml$mod_table)

table(d_t_cat$HMs_type)
fig_data_total_cat_HMs$HMs <- c("Overall (263)",
                                "Cd (143)",
                                "Cu (80)",
                                "Pb (23)",
                                "Zn (17)")

fig_data_total_cat_HMs$col <- ifelse((exp(fig_data_total_cat_HMs$lowerCL)-1)*100<=0,"No","Yes")
fig_data_total_cat_HMs$Index <- c(rep("CAT",5))

fig_data_total_cat_HMs$HMs <- factor(fig_data_total_cat_HMs$HMs, levels = c("Zn (17)","Pb (23)", "Cu (80)", "Cd (143)", "Overall (263)"))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
Fig_3_c <-
  ggplot(data=fig_data_total_cat_HMs, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50,100), breaks=seq(-50,100, by=50))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 4.5, linetype = 'dashed', col = 'gray70')

Fig_3_c




#HMs(Pb/Cd/Cu/Zn)
data_cat_Cd <- subset(es_cat_total, HMs_type == "Cd")
data_cat_Pb <- subset(es_cat_total, HMs_type == "Pb")
data_cat_Cu <- subset(es_cat_total, HMs_type == "Cu")
data_cat_Zn <- subset(es_cat_total, HMs_type == "Zn")


#Meta-analysis
#科属作为调节变量
res_cat_total <- rma.mv(yi, vi, data = es_cat_total, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_cat_Cd <- rma.mv(yi, vi, data = data_cat_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_cat_Pb <- rma.mv(yi, vi, data = data_cat_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_cat_Cu <- rma.mv(yi, vi, data = data_cat_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_cat_Zn <- rma.mv(yi, vi, data = data_cat_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


#extracting table of results
res_cat_total_reml <- mod_results(res_cat_total, mod = "1", group = "Study_ID") 
res_cat_Cd_reml <- mod_results(res_cat_Cd, mod = "1", group = "Study_ID") 
res_cat_Pb_reml <- mod_results(res_cat_Pb, mod = "1", group = "Study_ID") 
res_cat_Cu_reml <- mod_results(res_cat_Cu, mod = "1", group = "Study_ID") 
res_cat_Zn_reml <- mod_results(res_cat_Zn, mod = "1", group = "Study_ID") 

#Association plot
fig_data_cat <-rbind(res_cat_total_reml$mod_table,
                     res_cat_Cd_reml$mod_table,
                     res_cat_Pb_reml$mod_table,
                     res_cat_Cu_reml$mod_table,
                     res_cat_Zn_reml$mod_table)
table(es_cat_total$HMs_type)
fig_data_cat$HMs <- c("Overall (263)",
                      "Cd (143)",
                      "Pb (23)",
                      "Cu (80)",
                      "Zn (17)")

fig_data_cat$col <- ifelse((exp(fig_data_cat$lowerCL)-1)*100>=0,"Yes","No")
fig_data_cat$Index <- c(rep("CAT",5))

fig_data_cat$HMs <- factor(fig_data_cat$HMs, levels = rev(fig_data_cat$HMs))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
cat_total <-
  ggplot(data=fig_data_cat, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50,150), breaks=seq(-50,150, by=100))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 4.5, linetype = 'dashed', col = 'black')
cat_total


#4 MDA in leaf####
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

#一步到位版
#HMs_type
res_total_mda_overall <- rma.mv(yi, vi, data = es_mda_total, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_total_mda_HMs <- rma.mv(yi, vi, data = es_mda_total, mod = ~HMs_type-1, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#提取结果
res_total_mda_overall_reml <- mod_results(res_total_mda_overall, mod = "1", group = "Study_ID") 
res_total_mda_HMs_reml <- mod_results(res_total_mda_HMs, mod = "HMs_type", group = "Study_ID")
#汇总数据
fig_data_total_mda_HMs <-rbind(res_total_mda_overall_reml$mod_table,
                               res_total_mda_HMs_reml$mod_table)

table(d_t_mda$HMs_type)
fig_data_total_mda_HMs$HMs <- c("Overall (238)",
                                "Cd (141)",
                                "Cu (51)",
                                "Pb (35)",
                                "Zn (11)")

fig_data_total_mda_HMs$col <- ifelse((exp(fig_data_total_mda_HMs$lowerCL)-1)*100>=0,"Yes","No")
fig_data_total_mda_HMs$Index <- c(rep("MDA",5))

fig_data_total_mda_HMs$HMs <- factor(fig_data_total_mda_HMs$HMs, levels = c("Zn (11)","Pb (35)", "Cu (51)", "Cd (141)", "Overall (238)"))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
Fig_3_d <-
  ggplot(data=fig_data_total_mda_HMs, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50,100), breaks=seq(-50,100, by=50))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 4.5, linetype = 'dashed', col = 'gray70')

Fig_3_d

#HMs(Pb/Cd/Cu/Zn)
data_mda_Cd <- subset(es_mda_total, HMs_type == "Cd")
data_mda_Pb <- subset(es_mda_total, HMs_type == "Pb")
data_mda_Cu <- subset(es_mda_total, HMs_type == "Cu")
data_mda_Zn <- subset(es_mda_total, HMs_type == "Zn")


#Meta-analysis
#科属作为调节变量
res_mda_total <- rma.mv(yi, vi, data = es_mda_total, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_mda_Cd <- rma.mv(yi, vi, data = data_mda_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_mda_Pb <- rma.mv(yi, vi, data = data_mda_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_mda_Cu <- rma.mv(yi, vi, data = data_mda_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_mda_Zn <- rma.mv(yi, vi, data = data_mda_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


#extracting table of results
res_mda_total_reml <- mod_results(res_mda_total, mod = "1", group = "Study_ID") 
res_mda_Cd_reml <- mod_results(res_mda_Cd, mod = "1", group = "Study_ID") 
res_mda_Pb_reml <- mod_results(res_mda_Pb, mod = "1", group = "Study_ID") 
res_mda_Cu_reml <- mod_results(res_mda_Cu, mod = "1", group = "Study_ID") 
res_mda_Zn_reml <- mod_results(res_mda_Zn, mod = "1", group = "Study_ID") 

#Association plot
fig_data_mda <-rbind(res_mda_total_reml$mod_table,
                     res_mda_Cd_reml$mod_table,
                     res_mda_Pb_reml$mod_table,
                     res_mda_Cu_reml$mod_table,
                     res_mda_Zn_reml$mod_table)
table(es_mda_total$HMs_type)
fig_data_mda$HMs <- c("Overall (238)",
                      "Cd (141)",
                      "Pb (35)",
                      "Cu (51)",
                      "Zn (11)")

fig_data_mda$col <- ifelse((exp(fig_data_mda$lowerCL)-1)*100>=0,"Yes","No")
fig_data_mda$Index <- c(rep("MDA",5))

fig_data_mda$HMs <- factor(fig_data_mda$HMs, levels = rev(fig_data_mda$HMs))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
mda_total <-
  ggplot(data=fig_data_mda, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-50,100), breaks=seq(-50,100, by=50))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 4.5, linetype = 'dashed', col = 'black')
mda_total

meihuo <- cowplot::plot_grid(sod_total, pod_total, cat_total, mda_total, ncol = 2, align = "hv",labels = c("(a)","(b)","(c)","(d)"), label_size = 24)
#ggsave("Meihuo.pdf", width = 12, height = 10)

Meihuo_er <- cowplot::plot_grid(Fig_3_a,Fig_3_b,Fig_3_c, Fig_3_d, ncol = 2,align = "hv", labels = c("(a)","(b)","(c)", "(d)"), label_size = 24)
ggsave("Enzyme_v2.pdf", Meihuo_er, width = 9, height = 10,dpi = 600)


####model fit assessment
par(mfrow = c(2,3))
#SOD
##Rosenthal's Fail-safe N Calculation: N > 5k+10
fsn(es_sod_total$yi, es_sod_total$vi, data = es_sod_total, type = "Rosenthal")

#funnel plot
funnel(res_total_sod_overall,ylim=c(0,0.4), main = "(h) SOD")

#POD
##Rosenthal's Fail-safe N Calculation: N > 5k+10
fsn(es_pod_total$yi, es_pod_total$vi, data = es_pod_total, type = "Rosenthal")

#funnel plot
funnel(res_total_pod_overall,ylim=c(0,0.3), main = "(i) POD")


#CAT
##Rosenthal's Fail-safe N Calculation: N > 5k+10
fsn(es_cat_total$yi, es_cat_total$vi, data = es_cat_total, type = "Rosenthal")

#funnel plot
funnel(res_total_cat_overall,ylim=c(0,1.35), main = "(j) CAT")


#MDA
##Rosenthal's Fail-safe N Calculation: N > 5k+10
fsn(es_mda_total$yi, es_mda_total$vi, data = es_mda_total, type = "Rosenthal")

#funnel plot
funnel(res_total_mda_overall,ylim=c(0,0.3), main = "(k) MDA")

#9 * 3.5
