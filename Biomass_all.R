setwd("E:\\博士研究生\\Meta分析论文\\B重金属与木本植物meta")


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


####单个分析版
#HMs(No As)
data_total_Cd <- subset(es_total_biomass, HMs_type == "Cd")
data_total_Pb <- subset(es_total_biomass, HMs_type == "Pb")
data_total_Cu <- subset(es_total_biomass, HMs_type == "Cu")
data_total_Zn <- subset(es_total_biomass, HMs_type == "Zn")
data_total_Sb <- subset(es_total_biomass, HMs_type == "Sb")


aggregate(data_total_Cd$Genus, by = list(data_total_Cd$Genus), FUN = length)
aggregate(data_total_Pb$Genus, by = list(data_total_Pb$Genus), FUN = length)


#Meta-analysis
#科属作为调节变量
res_total_overall <- rma.mv(yi, vi, data = es_total_biomass, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_total_Cd <- rma.mv(yi, vi, data = data_total_Cd, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_total_Pb <- rma.mv(yi, vi, data = data_total_Pb, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_total_Cu <- rma.mv(yi, vi, data = data_total_Cu, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_total_Zn <- rma.mv(yi, vi, data = data_total_Zn, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_total_Sb <- rma.mv(yi, vi, data = data_total_Sb, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#extracting table of results
res_total_overall_reml <- mod_results(res_total_overall, mod = "1", group = "Study_ID") 
res_total_Cd_reml <- mod_results(res_total_Cd, mod = "1", group = "Study_ID") 
res_total_Pb_reml <- mod_results(res_total_Pb, mod = "1", group = "Study_ID") 
res_total_Cu_reml <- mod_results(res_total_Cu, mod = "1", group = "Study_ID") 
res_total_Zn_reml <- mod_results(res_total_Zn, mod = "1", group = "Study_ID") 
res_total_Sb_reml <- mod_results(res_total_Sb, mod = "1", group = "Study_ID") 

#Association plot
fig_data_total_biomass <-rbind(res_total_overall_reml$mod_table,
                               res_total_Cd_reml$mod_table,
                               res_total_Pb_reml$mod_table,
                               res_total_Cu_reml$mod_table,
                               res_total_Zn_reml$mod_table,
                               res_total_Sb_reml$mod_table)
table(d_tb_1$HMs_type)
fig_data_total_biomass$HMs <- c("Overall (409)",
                                "Cd (184)",
                                "Pb (123)",
                                "Cu (53)",
                                "Zn (26)",
                                "Sb (16)")

fig_data_total_biomass$col <- ifelse((exp(fig_data_total_biomass$upperCL)-1)*100>=0,"No","Yes")
fig_data_total_biomass$Index <- c(rep("Total biomass",6))

fig_data_total_biomass$HMs <- factor(fig_data_total_biomass$HMs, levels = rev(fig_data_total_biomass$HMs))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
total_biomass <-
  ggplot(data=fig_data_total_biomass, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
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
  geom_vline(xintercept = 5.5, linetype = 'dashed', col = 'black')
total_biomass

#绘图中不包含As
#ggsave("total_biomass.pdf", width = 4.5, height = 6.5, dpi = 600)



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



#HMs(No Sb)
data_above_Cd <- subset(es_above_biomass, HMs_type == "Cd")
data_above_Pb <- subset(es_above_biomass, HMs_type == "Pb")
data_above_Cu <- subset(es_above_biomass, HMs_type == "Cu")
data_above_Zn <- subset(es_above_biomass, HMs_type == "Zn")
data_above_As <- subset(es_above_biomass, HMs_type == "As")



#Meta-analysis
res_above_overall <- rma.mv(yi, vi, data = es_above_biomass, random = ~1|Study_ID/Co_ID, method = "REML",digits = 4)
res_above_Cd <- rma.mv(yi, vi, data = data_above_Cd, random = list(~1|Study_ID/Co_ID), method = "ML",digits = 4)
res_above_Pb <- rma.mv(yi, vi, data = data_above_Pb, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_above_Cu <- rma.mv(yi, vi, data = data_above_Cu, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_above_Zn <- rma.mv(yi, vi, data = data_above_Zn, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)
res_above_As <- rma.mv(yi, vi, data = data_above_As, random = list(~1|Study_ID/Co_ID), method = "REML",digits = 4)

#extracting table of results
res_above_overall_reml <- mod_results(res_above_overall, mod = "1", group = "Study_ID") 
res_above_Cd_reml <- mod_results(res_above_Cd, mod = "1", group = "Study_ID") 
res_above_Pb_reml <- mod_results(res_above_Pb, mod = "1", group = "Study_ID") 
res_above_Cu_reml <- mod_results(res_above_Cu, mod = "1", group = "Study_ID") 
res_above_Zn_reml <- mod_results(res_above_Zn, mod = "1", group = "Study_ID") 
res_above_As_reml <- mod_results(res_above_As, mod = "1", group = "Study_ID") 

#Association plots
fig_data_above_biomass <-rbind(res_above_overall_reml$mod_table,
                               res_above_Cd_reml$mod_table,
                               res_above_Pb_reml$mod_table,
                               res_above_Cu_reml$mod_table,
                               res_above_Zn_reml$mod_table,
                               res_above_As_reml$mod_table)
table(d_ab_1$HMs_type)
fig_data_above_biomass$HMs <- c("Overall (286)",
                                "Cd (118)",
                                "Pb (103)",
                                "Cu (24)",
                                "Zn (34)",
                                "As (7)")

fig_data_above_biomass$col <- ifelse((exp(fig_data_above_biomass$upperCL)-1)*100>=0,"No","Yes")
fig_data_above_biomass$Index <- c(rep("Aboveground biomass",6))

fig_data_above_biomass$HMs <- factor(fig_data_above_biomass$HMs, levels = rev(fig_data_above_biomass$HMs))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
above_biomass <-
  ggplot(data=fig_data_above_biomass, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
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
  geom_vline(xintercept = 5.5, linetype = 'dashed', col = 'black')
above_biomass


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





#HMs(No Sb)
data_root_Cd <- subset(es_root_biomass, HMs_type == "Cd")
data_root_Pb <- subset(es_root_biomass, HMs_type == "Pb")
data_root_Cu <- subset(es_root_biomass, HMs_type == "Cu")
data_root_Zn <- subset(es_root_biomass, HMs_type == "Zn")
data_root_As <- subset(es_root_biomass, HMs_type == "As")

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

#Meta-analysis
#Overall-VCV
res_root_overall <- rma.mv(yi, V.root.bio, data = es_root_biomass,random = list(~1|Study_ID, ~1|Family/Genus), method = "REML",digits = 4,
                           control = control)
res_root_Cd <- rma.mv(yi, vi, data = data_root_Cd, random = list(~1|Study_ID, ~1|Family/Genus), method = "REML",digits = 4)

#Pb-VCV
V.root.bio.pb<-metaAidR::make_VCV_matrix(data = data_root_Pb, V = "vi", 
                                      cluster = "Co_ID",type = "vcv", rho = 0.5)
V.root.bio.pb[is.na(V.root.bio.pb)] <- 0###去除表内含有NA的数值
# 使用nearPD来获得近似的正定矩阵
result_pb <- nearPD(V.root.bio.pb, corr = FALSE, do2eigen = TRUE, ensureSymmetry = TRUE)
# 检查result$mat是不是正定的
V.root.bio.pb.posdef <- result_pb$mat
V.root.bio.pb.posdef <- as.matrix(V.root.bio.pb.posdef)
is.positive.definite(V.root.bio.pb.posdef, tol=1e-18)
###确保为TRUE
V.root.bio.pb <- V.root.bio.pb.posdef
res_root_Pb <- rma.mv(yi, V.root.bio.pb, data = data_root_Pb, random = list(~1|Study_ID, ~1|Family/Genus), method = "REML",digits = 4,
                      control = control)

res_root_Cu <- rma.mv(yi, vi, data = data_root_Cu, random = list(~1|Study_ID, ~1|Family/Genus), method = "REML",digits = 4)
res_root_Zn <- rma.mv(yi, vi, data = data_root_Zn, random = list(~1|Study_ID, ~1|Family/Genus), method = "REML",digits = 4)
res_root_As <- rma.mv(yi, vi, data = data_root_As, random = list(~1|Study_ID, ~1|Family/Genus/Species), method = "REML",digits = 4)

#extracting table of results
res_root_overall_reml <- mod_results(res_root_overall, mod = "1", group = "Study_ID") 
res_root_Cd_reml <- mod_results(res_root_Cd, mod = "1", group = "Study_ID") 
res_root_Pb_reml <- mod_results(res_root_Pb, mod = "1", group = "Study_ID") 
res_root_Cu_reml <- mod_results(res_root_Cu, mod = "1", group = "Study_ID") 
res_root_Zn_reml <- mod_results(res_root_Zn, mod = "1", group = "Study_ID") 
res_root_As_reml <- mod_results(res_root_As, mod = "1", group = "Study_ID") 

#Association plots####
fig_data_root_biomass <-rbind(res_root_overall_reml$mod_table,
                              res_root_Cd_reml$mod_table,
                              res_root_Pb_reml$mod_table,
                              res_root_Cu_reml$mod_table,
                              res_root_Zn_reml$mod_table,
                              res_root_As_reml$mod_table)
table(d_ab_1$HMs_type)
fig_data_root_biomass$HMs <- c("Overall (721)",
                               "Cd (118)",
                               "Pb (103)",
                               "Cu (24)",
                               "Zn (34)",
                               "As (7)")

fig_data_root_biomass$col <- ifelse((exp(fig_data_root_biomass$upperCL)-1)*100>=0,"No","Yes")
fig_data_root_biomass$Index <- c(rep("Underground biomass",6))

fig_data_root_biomass$HMs <- factor(fig_data_root_biomass$HMs, levels = rev(fig_data_root_biomass$HMs))
#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

#作图
root_biomass <-
  ggplot(data=fig_data_root_biomass, aes(x=HMs, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
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
  geom_vline(xintercept = 5.5, linetype = 'dashed', col = 'black')
root_biomass

biomass<-cowplot::plot_grid(total_biomass, above_biomass, root_biomass, ncol = 3,align = "hv", labels = c("(a)","(b)","(c)"), label_size = 24)
biomass

#ggsave("biomass.pdf", width = 14, height = 6,dpi = 600)

biomass_er <- cowplot::plot_grid(Fig_1_a,Fig_1_b,Fig_1_c, ncol = 3,align = "hv", labels = c("(a)","(b)","(c)"), label_size = 24)
ggsave("biomass_v2.pdf",biomass_er, width = 14, height = 6,dpi = 600)


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
fsn(es_root_biomass$yi, es_root_biomass$vi, data = es_root_biomass, type = "Rosenthal")

#funnel plot
funnel(res_root_overall,ylim=c(0,1), main = "(c) Underground biomass")

#9 * 3.5