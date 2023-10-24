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

#一步到位版
#HMs_type
res_total_hms_overall <- rma.mv(yi, vi, data = es_hms_total, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_total_hms_HMs <- rma.mv(yi, vi, data = es_hms_total, mod = ~HMs_type-1, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#提取结果
res_total_hms_overall_reml <- mod_results(res_total_hms_overall, mod = "1", group = "Study_ID") 
res_total_hms_HMs_reml <- mod_results(res_total_hms_HMs, mod = "HMs_type", group = "Study_ID")
#汇总数据
fig_data_total_hms_HMs <-rbind(res_total_hms_overall_reml$mod_table,
                               res_total_hms_HMs_reml$mod_table)

table(d_t_hms$HMs_type)
fig_data_total_hms_HMs$HMs <- c("Overall (35)",
                                    "Cd (15)",
                                    "Cu (5)",
                                    "Pb (15)")

fig_data_total_hms_HMs$col <- ifelse((exp(fig_data_total_hms_HMs$lowerCL)-1)*100<=0,"No","Yes")
fig_data_total_hms_HMs$Index <- c(rep("Total HMs concentration",4))

fig_data_total_hms_HMs$HMs <- factor(fig_data_total_hms_HMs$HMs, levels = c("Cu (5)","Pb (15)", "Cd (15)", "Overall (35)"))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
Fig_2_a <-
  ggplot(data=fig_data_total_hms_HMs, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-10000,160000), breaks=seq(-10000,160000, by=50000))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 3.5, linetype = 'dashed', col = 'gray70')

Fig_2_a


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
res_hms_total <- rma.mv(yi, vi, data = es_hms_total, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Cd <- rma.mv(yi, vi, data = data_hms_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Pb <- rma.mv(yi, vi, data = data_hms_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_Cu <- rma.mv(yi, vi, data = data_hms_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


#extracting table of results
res_hms_total_reml <- mod_results(res_hms_total, mod = "1", group = "Study_ID") 
res_hms_Cd_reml <- mod_results(res_hms_Cd, mod = "1", group = "Study_ID") 
res_hms_Pb_reml <- mod_results(res_hms_Pb, mod = "1", group = "Study_ID") 
res_hms_Cu_reml <- mod_results(res_hms_Cu, mod = "1", group = "Study_ID") 


#Association plot
fig_data_hms <-rbind(res_hms_total_reml$mod_table,
                     res_hms_Cd_reml$mod_table,
                     res_hms_Pb_reml$mod_table,
                     res_hms_Cu_reml$mod_table)
table(es_hms_total$HMs_type)
fig_data_hms$HMs <- c("Overall (35)",
                      "Cd (15)",
                      "Pb (15)",
                      "Cu (5)")

fig_data_hms$col <- ifelse((exp(fig_data_hms$lowerCL)-1)*100>=0,"Yes","No")
fig_data_hms$Index <- c(rep("Total HMs concentration",4))

fig_data_hms$HMs <- factor(fig_data_hms$HMs, levels = rev(fig_data_hms$HMs))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
hms_total <-
  ggplot(data=fig_data_hms, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-5000,20000), breaks=seq(-5000,20000, by=10000))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 3.5, linetype = 'dashed', col = 'black')
hms_total

#ggsave("total_biomass.pdf", width = 4.5, height = 6.5, dpi = 600)


#2 Aboveground HMs concentration####
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


#一步到位版
#HMs_type
res_above_hms_overall <- rma.mv(yi, vi, data = es_hms_above, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_above_hms_HMs <- rma.mv(yi, vi, data = es_hms_above, mod = ~HMs_type-1, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#提取结果
res_above_hms_overall_reml <- mod_results(res_above_hms_overall, mod = "1", group = "Study_ID") 
res_above_hms_HMs_reml <- mod_results(res_above_hms_HMs, mod = "HMs_type", group = "Study_ID")
#汇总数据
fig_data_above_hms_HMs <-rbind(res_above_hms_overall_reml$mod_table,
                               res_above_hms_HMs_reml$mod_table)

table(d_ab_hms$HMs_type)
fig_data_above_hms_HMs$HMs <- c("Overall (134)",
                                "As (3)",
                                "Cd (70)",
                                "Cu (14)",
                                "Pb (31)",
                                "Zn (16)")

fig_data_above_hms_HMs$col <- ifelse((exp(fig_data_above_hms_HMs$lowerCL)-1)*100<=0,"No","Yes")
fig_data_above_hms_HMs$Index <- c(rep("Above HMs concentration",6))

fig_data_above_hms_HMs$HMs <- factor(fig_data_above_hms_HMs$HMs, levels = c("As (3)", "Cu (14)", "Zn (16)","Pb (31)", "Cd (70)", "Overall (134)"))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
Fig_2_b <-
  ggplot(data=fig_data_above_hms_HMs, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-10000,15000), breaks=seq(-10000,15000, by=10000))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 5.5, linetype = 'dashed', col = 'gray70')

Fig_2_b



#HMs(Cd/Pb/Zn/Cu/As)
data_hms_ab_Cd <- subset(es_hms_above, HMs_type == "Cd")
data_hms_ab_Pb <- subset(es_hms_above, HMs_type == "Pb")
data_hms_ab_Zn <- subset(es_hms_above, HMs_type == "Zn")
data_hms_ab_Cu <- subset(es_hms_above, HMs_type == "Cu")
data_hms_ab_As <- subset(es_hms_above, HMs_type == "As")


#Meta-analysis
#科属作为调节变量
res_hms_above <- rma.mv(yi, vi, data = es_hms_above, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_ab_Cd <- rma.mv(yi, vi, data = data_hms_ab_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_ab_Pb <- rma.mv(yi, vi, data = data_hms_ab_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_ab_Zn <- rma.mv(yi, vi, data = data_hms_ab_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_ab_Cu <- rma.mv(yi, vi, data = data_hms_ab_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_ab_As <- rma.mv(yi, vi, data = data_hms_ab_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)


#extracting table of results
res_hms_above_reml <- mod_results(res_hms_above, mod = "1", group = "Study_ID") 
res_hms_ab_Cd_reml <- mod_results(res_hms_ab_Cd, mod = "1", group = "Study_ID") 
res_hms_ab_Pb_reml <- mod_results(res_hms_ab_Pb, mod = "1", group = "Study_ID")
res_hms_ab_Zn_reml <- mod_results(res_hms_ab_Zn, mod = "1", group = "Study_ID")
res_hms_ab_Cu_reml <- mod_results(res_hms_ab_Cu, mod = "1", group = "Study_ID") 
res_hms_ab_As_reml <- mod_results(res_hms_ab_As, mod = "1", group = "Study_ID")

#Association plot
fig_data_hms_ab <-rbind(res_hms_above_reml$mod_table,
                        res_hms_ab_Cd_reml$mod_table,
                        res_hms_ab_Pb_reml$mod_table,
                        res_hms_ab_Zn_reml$mod_table,
                        res_hms_ab_Cu_reml$mod_table,
                        res_hms_ab_As_reml$mod_table)
table(es_hms_above$HMs_type)
fig_data_hms_ab$HMs <- c("Overall (134)",
                      "Cd (70)",
                      "Pb (31)",
                      "Zn (16)",
                      "Cu (14)",
                      "As (3)")

fig_data_hms_ab$col <- ifelse((exp(fig_data_hms_ab$lowerCL)-1)*100>=0,"No","Yes")
fig_data_hms_ab$Index <- c(rep("Aboveground HMs concentration",6))

fig_data_hms_ab$HMs <- factor(fig_data_hms_ab$HMs, levels = rev(fig_data_hms_ab$HMs))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
hms_above <-
  ggplot(data=fig_data_hms_ab, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-5000,15000), breaks=seq(-5000,15000, by=10000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 5.5, linetype = 'dashed', col = 'black')
hms_above


#2 Leaf HMs concentration####
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

#一步到位版
#HMs_type
res_leaf_hms_overall <- rma.mv(yi, vi, data = es_hms_leaf, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_leaf_hms_HMs <- rma.mv(yi, vi, data = es_hms_leaf, mod = ~HMs_type-1, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#提取结果
res_leaf_hms_overall_reml <- mod_results(res_leaf_hms_overall, mod = "1", group = "Study_ID") 
res_leaf_hms_HMs_reml <- mod_results(res_leaf_hms_HMs, mod = "HMs_type", group = "Study_ID")
#汇总数据
fig_data_leaf_hms_HMs <-rbind(res_leaf_hms_overall_reml$mod_table,
                              res_leaf_hms_HMs_reml$mod_table)

table(d_leaf_hms$HMs_type)
fig_data_leaf_hms_HMs$HMs <- c("Overall (597)",
                               "As (24)",
                               "Cd (293)",
                               "Cr (6)",
                               "Cu (93)",
                               "Ni (6)",
                               "Pb (145)",
                               "Ti (6)",
                               "Zn (85)")

fig_data_leaf_hms_HMs$col <- ifelse((exp(fig_data_leaf_hms_HMs$lowerCL)-1)*100<=0,"No","Yes")
fig_data_leaf_hms_HMs$Index <- c(rep("Leaf HMs concentration",9))

fig_data_leaf_hms_HMs$HMs <- factor(fig_data_leaf_hms_HMs$HMs, levels = c("Ti (6)", "Cr (6)","Ni (6)","As (24)",
                                                                          "Zn (85)", "Cu (93)","Pb (145)", "Cd (293)", "Overall (597)"))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
Fig_2_c <-
  ggplot(data=fig_data_leaf_hms_HMs, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-4000,7000), breaks=seq(-4000,7000, by=4000))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 8.5, linetype = 'dashed', col = 'gray70')

Fig_2_c




#HMs(Cd/Pb/Zn/Cu/As)
data_hms_leaf_Cd <- subset(es_hms_leaf, HMs_type == "Cd")
data_hms_leaf_Pb <- subset(es_hms_leaf, HMs_type == "Pb")
data_hms_leaf_Cu <- subset(es_hms_leaf, HMs_type == "Cu")
data_hms_leaf_Zn <- subset(es_hms_leaf, HMs_type == "Zn")
data_hms_leaf_As <- subset(es_hms_leaf, HMs_type == "As")
#data_hms_leaf_Sb <- subset(es_hms_leaf, HMs_type == "Sb")
data_hms_leaf_Ti <- subset(es_hms_leaf, HMs_type == "Ti")
data_hms_leaf_Ni <- subset(es_hms_leaf, HMs_type == "Ni")
data_hms_leaf_Cr <- subset(es_hms_leaf, HMs_type == "Cr")

#Meta-analysis
#科属作为调节变量
res_hms_leaf <- rma.mv(yi, vi, data = es_hms_leaf, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_leaf_Cd <- rma.mv(yi, vi, data = data_hms_leaf_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_leaf_Pb <- rma.mv(yi, vi, data = data_hms_leaf_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_leaf_Cu <- rma.mv(yi, vi, data = data_hms_leaf_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_leaf_Zn <- rma.mv(yi, vi, data = data_hms_leaf_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_leaf_As <- rma.mv(yi, vi, data = data_hms_leaf_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
#res_hms_leaf_Sb <- rma.mv(yi, vi, data = data_hms_leaf_Sb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_leaf_Ti <- rma.mv(yi, vi, data = data_hms_leaf_Ti, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_leaf_Ni <- rma.mv(yi, vi, data = data_hms_leaf_Ni, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_leaf_Cr <- rma.mv(yi, vi, data = data_hms_leaf_Cr, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)




#extracting table of results
res_hms_leaf_reml <- mod_results(res_hms_leaf, mod = "1", group = "Study_ID") 
res_hms_leaf_Cd_reml <- mod_results(res_hms_leaf_Cd, mod = "1", group = "Study_ID") 
res_hms_leaf_Pb_reml <- mod_results(res_hms_leaf_Pb, mod = "1", group = "Study_ID")

res_hms_leaf_Cu_reml <- mod_results(res_hms_leaf_Cu, mod = "1", group = "Study_ID")
res_hms_leaf_Zn_reml <- mod_results(res_hms_leaf_Zn, mod = "1", group = "Study_ID")
res_hms_leaf_As_reml <- mod_results(res_hms_leaf_As, mod = "1", group = "Study_ID")

#res_hms_leaf_Sb_reml <- mod_results(res_hms_leaf_Sb, mod = "1", group = "Study_ID")
res_hms_leaf_Ti_reml <- mod_results(res_hms_leaf_Ti, mod = "1", group = "Study_ID")
res_hms_leaf_Ni_reml <- mod_results(res_hms_leaf_Ni, mod = "1", group = "Study_ID")
res_hms_leaf_Cr_reml <- mod_results(res_hms_leaf_Cr, mod = "1", group = "Study_ID")

#Association plot
fig_data_hms_leaf <-rbind(res_hms_leaf_reml$mod_table,
                          res_hms_leaf_Cd_reml$mod_table,
                          res_hms_leaf_Pb_reml$mod_table,
                          res_hms_leaf_Cu_reml$mod_table,
                          res_hms_leaf_Zn_reml$mod_table,
                          res_hms_leaf_As_reml$mod_table,
                          res_hms_leaf_Ti_reml$mod_table,
                          res_hms_leaf_Ni_reml$mod_table,
                          res_hms_leaf_Cr_reml$mod_table)
table(es_hms_leaf$HMs_type)
fig_data_hms_leaf$HMs <- c("Overall (614)",
                         "Cd (246)",
                         "Pb (132)",
                         "Cu (93)",
                         "Zn (85)",
                         "As (23)",
                         "Ti (6)",
                         "Ni (6)",
                         "Cr (6)")

fig_data_hms_leaf$col <- ifelse((exp(fig_data_hms_leaf$lowerCL)-1)*100>=0,"No","Yes")
fig_data_hms_leaf$Index <- c(rep("Leaf HMs concentration",9))

fig_data_hms_leaf$HMs <- factor(fig_data_hms_leaf$HMs, levels = rev(fig_data_hms_leaf$HMs))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
hms_leaf <-
  ggplot(data=fig_data_hms_leaf, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,2000), breaks=seq(-100,2000, by=2000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 8.5, linetype = 'dashed', col = 'black')
hms_leaf


#2 Root HMs concentration####
d_root_hms <- read_xlsx("data-all.xlsx", sheet = "Root_HMs_concentration")

table(d_root_hms$HMs_type)


#effect size
es_hms_root <- escalc(measure = "ROM", 
                      m1i = Xt,
                      sd1i = SDt,
                      n1i = Nt,
                      m2i = Xc,
                      sd2i = SDc,
                      n2i = Nc,
                      data = d_root_hms)
es_hms_root <- filter(es_hms_root, es_hms_root$yi!="NA")

#一步到位版
#HMs_type
res_root_hms_overall <- rma.mv(yi, vi, data = es_hms_root, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_root_hms_HMs <- rma.mv(yi, vi, data = es_hms_root, mod = ~HMs_type-1, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#提取结果
res_root_hms_overall_reml <- mod_results(res_root_hms_overall, mod = "1", group = "Study_ID") 
res_root_hms_HMs_reml <- mod_results(res_root_hms_HMs, mod = "HMs_type", group = "Study_ID")
#汇总数据
fig_data_root_hms_HMs <-rbind(res_root_hms_overall_reml$mod_table,
                              res_root_hms_HMs_reml$mod_table)

table(d_root_hms$HMs_type)
fig_data_root_hms_HMs$HMs <- c("Overall (744)",
                               "As (27)",
                               "Cd (372)",
                               "Cr (6)",
                               "Cu (108)",
                               "Ni (9)",
                               "Pb (169)",
                               "Sb (17)",
                               "Zn (110)")

fig_data_root_hms_HMs$col <- ifelse((exp(fig_data_root_hms_HMs$lowerCL)-1)*100<=0,"No","Yes")
fig_data_root_hms_HMs$Index <- c(rep("Underground HMs concentration",9))

fig_data_root_hms_HMs$HMs <- factor(fig_data_root_hms_HMs$HMs, levels = c("Cr (6)","Ni (9)", "Sb (17)","As (27)","Cu (108)",
                                                                          "Zn (110)", "Pb (169)", "Cd (372)", "Overall (744)"))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
Fig_2_d <-
  ggplot(data=fig_data_root_hms_HMs, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-5000,18000), breaks=seq(-5000,18000, by=5000))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 8.5, linetype = 'dashed', col = 'gray70')

Fig_2_d



#HMs(Cd/Pb/Zn/Cu/As)
data_hms_root_Cd <- subset(es_hms_root, HMs_type == "Cd")
data_hms_root_Pb <- subset(es_hms_root, HMs_type == "Pb")
data_hms_root_Cu <- subset(es_hms_root, HMs_type == "Cu")
data_hms_root_Zn <- subset(es_hms_root, HMs_type == "Zn")
data_hms_root_As <- subset(es_hms_root, HMs_type == "As")
data_hms_root_Sb <- subset(es_hms_root, HMs_type == "Sb")
data_hms_root_Ti <- subset(es_hms_root, HMs_type == "Ti")
data_hms_root_Ni <- subset(es_hms_root, HMs_type == "Ni")
data_hms_root_Cr <- subset(es_hms_root, HMs_type == "Cr")

#Meta-analysis
#科属作为调节变量
res_hms_root <- rma.mv(yi, vi, data = es_hms_root, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_root_Cd <- rma.mv(yi, vi, data = data_hms_root_Cd, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_root_Pb <- rma.mv(yi, vi, data = data_hms_root_Pb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_root_Cu <- rma.mv(yi, vi, data = data_hms_root_Cu, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_root_Zn <- rma.mv(yi, vi, data = data_hms_root_Zn, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_root_As <- rma.mv(yi, vi, data = data_hms_root_As, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_root_Sb <- rma.mv(yi, vi, data = data_hms_root_Sb, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
#res_hms_root_Ti <- rma.mv(yi, vi, data = data_hms_root_Ti, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_root_Ni <- rma.mv(yi, vi, data = data_hms_root_Ni, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_hms_root_Cr <- rma.mv(yi, vi, data = data_hms_root_Cr, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)




#extracting table of results
res_hms_root_reml <- mod_results(res_hms_root, mod = "1", group = "Study_ID") 
res_hms_root_Cd_reml <- mod_results(res_hms_root_Cd, mod = "1", group = "Study_ID") 
res_hms_root_Pb_reml <- mod_results(res_hms_root_Pb, mod = "1", group = "Study_ID")

res_hms_root_Cu_reml <- mod_results(res_hms_root_Cu, mod = "1", group = "Study_ID")
res_hms_root_Zn_reml <- mod_results(res_hms_root_Zn, mod = "1", group = "Study_ID")
res_hms_root_As_reml <- mod_results(res_hms_root_As, mod = "1", group = "Study_ID")

res_hms_root_Sb_reml <- mod_results(res_hms_root_Sb, mod = "1", group = "Study_ID")
#res_hms_root_Ti_reml <- mod_results(res_hms_root_Ti, mod = "1", group = "Study_ID")
res_hms_root_Ni_reml <- mod_results(res_hms_root_Ni, mod = "1", group = "Study_ID")
res_hms_root_Cr_reml <- mod_results(res_hms_root_Cr, mod = "1", group = "Study_ID")

#Association plot
fig_data_hms_root <-rbind(res_hms_root_reml$mod_table,
                          res_hms_root_Cd_reml$mod_table,
                          res_hms_root_Pb_reml$mod_table,
                          res_hms_root_Cu_reml$mod_table,
                          res_hms_root_Zn_reml$mod_table,
                          res_hms_root_As_reml$mod_table,
                          res_hms_root_Sb_reml$mod_table,
                          res_hms_root_Ni_reml$mod_table,
                          res_hms_root_Cr_reml$mod_table)
table(es_hms_root$HMs_type)
fig_data_hms_root$HMs <- c("Overall (744)",
                           "Cd (323)",
                           "Pb (158)",
                           "Cu (102)",
                           "Zn (108)",
                           "As (21)",
                           "Sb (17)",
                           "Ni (9)",
                           "Cr (6)")

fig_data_hms_root$col <- ifelse((exp(fig_data_hms_root$lowerCL)-1)*100>=0,"No","Yes")
fig_data_hms_root$Index <- c(rep("Underground HMs concentration",9))

fig_data_hms_root$HMs <- factor(fig_data_hms_root$HMs, levels = rev(fig_data_hms_root$HMs))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
hms_root <-
  ggplot(data=fig_data_hms_root, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Heavy metals', y='Percentage change (%)')+
  facet_grid(.~Index,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,9000), breaks=seq(-100,9000, by=4500))+
  scale_colour_manual(values = c("red", "gray60"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 8.5, linetype = 'dashed', col = 'black')
hms_root

hms <- cowplot::plot_grid(hms_total, hms_above, hms_leaf, hms_root, ncol = 2, align = "hv",labels = c("(a)","(b)","(c)","(d)"), label_size = 24)
#ggsave("HMs.pdf", width = 12, height = 10)


HMs_er <- cowplot::plot_grid(Fig_2_a,Fig_2_b,Fig_2_c, Fig_2_d, ncol = 2,align = "hv", labels = c("(a)","(b)","(c)", "(d)"), label_size = 24)
ggsave("HMs_concentration_v2_v1.pdf",HMs_er, width = 12, height = 12,dpi = 600)


####model fit assessment
par(mfrow = c(2,3))
#Total HMs concentration
##Rosenthal's Fail-safe N Calculation: N > 5k+10
fsn(es_hms_total$yi, es_hms_total$vi, data = es_hms_total, type = "Rosenthal")

#funnel plot
funnel(res_total_hms_overall,ylim=c(0,0.8), main = "(d) Total HMs concentration")

#Aboveground HMs concentration
##Rosenthal's Fail-safe N Calculation: N > 5k+10
fsn(es_hms_above$yi, es_hms_above$vi, data = es_hms_above, type = "Rosenthal")

#funnel plot
funnel(res_above_hms_overall,ylim=c(0,4), main = "(e) Aboveground HMs concentration")


#Leaf HMs concentration
##Rosenthal's Fail-safe N Calculation: N > 5k+10
fsn(es_hms_leaf$yi, es_hms_leaf$vi, data = es_hms_leaf, type = "Rosenthal")

#funnel plot
funnel(res_leaf_hms_overall,ylim=c(0,2.7), main = "(f) Leaf HMs concentration")


#Underground HMs concentration
##Rosenthal's Fail-safe N Calculation: N > 5k+10
fsn(es_hms_root$yi, es_hms_root$vi, data = es_hms_root, type = "Rosenthal")

#funnel plot
funnel(res_root_hms_overall,ylim=c(0,5.5), main = "(g) Underground HMs concentration")

#9 * 3.5