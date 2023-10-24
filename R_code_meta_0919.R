###A global meta-analysis of ability on remediation of 
###heavy metal contaminated soil by woody plants

#Author: Zhongyu Du

#Time: 2023-09-19
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
  library(Matrix)
}
#effect size and variance
setwd("E:\\博士研究生\\Meta分析论文\\B重金属与木本植物meta")
rm(list = ls())

#2 Total biomass####
rm(list = ls())
d_tb <- read_xlsx("data-all.xlsx", sheet = "Total_biomass")

#remove NA
d_tb_1 <- filter(d_tb, d_tb$Xc!="NA")
d_tb_1$Study_ID <- factor(d_tb_1$Study_ID)
head(d_tb_1)
str(d_tb_1)

#effect size
es_total_biomass<-escalc(measure = "ROM", 
                         m1i = Xt,
                         sd1i = SDt,
                         n1i = Nt,
                         m2i = Xc,
                         sd2i = SDc,
                         n2i = Nc,
                         data = d_tb_1)

#Cd (899)
data_total_Cd <- subset(es_total_biomass, HMs_type == "Cd")

#VCV
V_total_Cd<-metaAidR::make_VCV_matrix(data = data_total_Cd, V = "vi", 
                                     cluster = "Co_ID", 
                                     obs = "Inf_ID", type = "vcv")

matrixcalc::is.positive.definite(V_total_Cd)

#Meta-analysis
res_total_Cd <- rma.va(yi, V_total_Cd, random = list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML",data = data_total_Cd, digits = 4)
summary(res_total_Cd)
# Overall estimate and CI
(exp(res_total_Cd$b[[1]])-1)*100 # estimate

res_total_Cd_reml <- mod_results(res_total_Cd, mod = "1", group = "Study_ID") #extracting table of results


(exp(res_total_Cd_reml$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cd_reml$mod_table$upperCL)-1)*100 # upper CI boundary

#Moderators(Experimental_method、Plant_method、Functional_group、stress_time、Tree_age、pH)

#Experimental_method(3 levels)
table(data_total_Cd$Experimental_method)
experimental_Cd <- rma.mv(yi, V_total_Cd, mods = ~ Experimental_method-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Cd)

#estimate and calculating percentage exp
res_total_Cd_Exp <- mod_results(experimental_Cd, mod = "Experimental_method", group = "Experimental_method")
(exp(res_total_Cd_Exp$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Cd_Exp$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cd_Exp$mod_table$upperCL)-1)*100 # upper CI boundary

#Plant_method(3 levels)
table(data_total_Cd$Plant_method)
Plant_method_Cd <- rma.mv(yi, V_total_Cd, mods = ~ Plant_method-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Cd)

#estimate and calculating percentage exp
res_total_Cd_plant_method <- mod_results(Plant_method_Cd, mod = "Plant_method", group = "Plant_method")
(exp(res_total_Cd_plant_method$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Cd_plant_method$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cd_plant_method$mod_table$upperCL)-1)*100 # upper CI boundary


#Functional_group(2 levels)
table(data_total_Cd$Functional_group)
Functional_group_Cd <- rma.mv(yi, V_total_Cd, mods = ~ Functional_group-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Cd)

#estimate and calculating percentage exp
res_total_Cd_Functional_group <- mod_results(Functional_group_Cd, mod = "Functional_group", group = "Functional_group")
(exp(res_total_Cd_Functional_group$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Cd_Functional_group$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cd_Functional_group$mod_table$upperCL)-1)*100 # upper CI boundary



#Stress_time(连续型变量)
table(data_total_Cd$stress_time)
data_total_Cd$stress_time <- cut(data_total_Cd$stress_time, breaks = c(-Inf, 90, 360, Inf),labels = c("Short","Mid","Long"), right=FALSE)

stress_time_Cd <- rma.mv(yi, V_total_Cd, mods = ~ stress_time-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Cd)

#estimate and calculating percentage exp
res_total_Cd_stress_time <- mod_results(stress_time_Cd, mod = "stress_time", group = "stress_time")
(exp(res_total_Cd_stress_time$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Cd_stress_time$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cd_stress_time$mod_table$upperCL)-1)*100 # upper CI boundary


#Tree_age(连续型变量)
#as.numeric格式(Unknow转换为NA值, 无需去除，不影响结果)
table(data_total_Cd$Tree_age)
#只能运行一次
data_total_Cd$Tree_age <- as.numeric(data_total_Cd$Tree_age)
data_total_Cd$Tree_age <- cut(data_total_Cd$Tree_age, breaks = c(-Inf, 360, 720, Inf),labels = c("Young","Mature","Old"), right=FALSE)
table(data_total_Cd$Tree_age)

Tree_age_Cd <- rma.mv(yi, V_total_Cd, mods = ~ Tree_age-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Cd)

#estimate and calculating percentage exp
res_total_Cd_Tree_age <- mod_results(Tree_age_Cd, mod = "Tree_age", group = "Tree_age")
(exp(res_total_Cd_Tree_age$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Cd_Tree_age$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cd_Tree_age$mod_table$upperCL)-1)*100 # upper CI boundary


#pH(连续型变量)
#as.numeric格式(Unknow转换为NA值, 无需去除，不影响结果)
table(data_total_Cd$pH)
#只能运行一次
data_total_Cd$pH <- as.numeric(data_total_Cd$pH)

data_total_Cd$pH <- cut(data_total_Cd$pH, breaks = c(-Inf, 6, 8, Inf),labels = c("Low","Midph","High"), right=FALSE)
table(data_total_Cd$pH)


pH_Cd <- rma.mv(yi, V_total_Cd, mods = ~ pH-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Cd)

#estimate and calculating percentage exp
res_total_Cd_pH <- mod_results(pH_Cd, mod = "pH", group = "pH")
(exp(res_total_Cd_pH$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Cd_pH$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cd_pH$mod_table$upperCL)-1)*100 # upper CI boundary


#Association plots####
fig_data_Cd_total<-rbind(res_total_Cd_reml$mod_table,
                         res_total_Cd_Exp$mod_table,
                         res_total_Cd_plant_method$mod_table,
                         res_total_Cd_Functional_group$mod_table,
                         res_total_Cd_stress_time$mod_table,
                         res_total_Cd_Tree_age$mod_table,
                         res_total_Cd_pH$mod_table)

fig_data_Cd_total$group<-dplyr::recode_factor(fig_data_Cd_total$name,
                                              Intrcpt ="a",
                                              Field="c", Hydroponics="c",Medium="c", Potted="c",
                                              Cutting="g", Plant="g", Seedling="g",
                                              Shrub="d", Tree="d",
                                              Short="e", Mid="e",Long="e",
                                              Young="f", Mature="f", Old="f",
                                              Low="h",Midph="h",High="h")

fig_data_Cd_total$col <- ifelse((exp(fig_data_Cd_total$upperCL)-1)*100>=0,"No","Yes")


table(data_total_Cd$Experimental_method)
table(data_total_Cd$Plant_method)
table(data_total_Cd$Functional_group)
table(data_total_Cd$stress_time)
table(data_total_Cd$Tree_age)
table(data_total_Cd$pH)

fig_data_Cd_total$name <- dplyr::recode_factor(fig_data_Cd_total$name,
                                               Intrcpt = "Overall (409)",
                                               Field="Field (2)", Hydroponics="Hydroponics (67)", Potted="Potted (115)",
                                               Cutting="Cutting (41)", Plant="Plant (6)", Seedling="Seedling (137)",
                                               Shrub="Shrub (58)", Tree="Tree (126)",
                                               Short="Short (77)", Mid="Mid (104)",Long="Long (3)",
                                               Young="Young (12)", Mature="Mature (38)", Old="Old (24)",
                                               Low="Low (50)",Midph="Mid (70)",High="High (34)")


fig_data_Cd_total$HMs <- c(rep("Cd",18))


fig_data_Cd_total$name <- factor(fig_data_Cd_total$name, levels = rev(fig_data_Cd_total$name))

#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))


#作图
Cd_total<-
  ggplot(data=fig_data_Cd_total, aes(x=name, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Moderators', y='% change with heavy metal pollution\ncompared to natural environments')+
  facet_grid(.~HMs,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,100), breaks=seq(-100,100, by=50))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 17.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 14.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 11.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 9.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 6.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 3.5, linetype = 'dashed', col = 'black')

Cd_total

#ggsave("Cd_total.pdf", width = 6.5, height = 9.5, dpi = 600)

#Pb (123)
data_total_Pb <- subset(es_total_biomass, HMs_type == "Pb")

#VCV
V_total_Pb<-metaAidR::make_VCV_matrix(data = data_total_Pb, V = "vi", 
                                      cluster = "Co_ID", 
                                      obs = "Inf_ID", type = "vcv")

matrixcalc::is.positive.definite(V_total_Pb)

#Meta-analysis
res_total_Pb <- rma.mv(yi, V_total_Pb, random = list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML",data = data_total_Pb, digits = 4)
summary(res_total_Pb)
# Overall estimate and CI
(exp(res_total_Pb$b[[1]])-1)*100 # estimate

res_total_Pb_reml <- mod_results(res_total_Pb, mod = "1", group = "Study_ID") #extracting table of results


(exp(res_total_Pb_reml$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Pb_reml$mod_table$upperCL)-1)*100 # upper CI boundary

#Moderators(Experimental_method、Plant_method、Functional_group、stress_time、Tree_age、pH)

#Experimental_method(3 levels)
table(data_total_Pb$Experimental_method)
experimental_Pb <- rma.mv(yi, V_total_Pb, mods = ~ Experimental_method-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Pb)

#estimate and calculating percentage exp
res_total_Pb_Exp <- mod_results(experimental_Pb, mod = "Experimental_method", group = "Experimental_method")
(exp(res_total_Pb_Exp$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Pb_Exp$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Pb_Exp$mod_table$upperCL)-1)*100 # upper CI boundary

#Plant_method(3 levels)
table(data_total_Pb$Plant_method)
Plant_method_Pb <- rma.mv(yi, V_total_Pb, mods = ~ Plant_method-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Pb)

#estimate and calculating percentage exp
res_total_Pb_plant_method <- mod_results(Plant_method_Pb, mod = "Plant_method", group = "Plant_method")
(exp(res_total_Pb_plant_method$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Pb_plant_method$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Pb_plant_method$mod_table$upperCL)-1)*100 # upper CI boundary


#Functional_group(2 levels)
table(data_total_Pb$Functional_group)
Functional_group_Pb <- rma.mv(yi, V_total_Pb, mods = ~ Functional_group-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Pb)

#estimate and calculating percentage exp
res_total_Pb_Functional_group <- mod_results(Functional_group_Pb, mod = "Functional_group", group = "Functional_group")
(exp(res_total_Pb_Functional_group$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Pb_Functional_group$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Pb_Functional_group$mod_table$upperCL)-1)*100 # upper CI boundary



#Stress_time(连续型变量)
table(data_total_Pb$stress_time)
data_total_Pb$stress_time <- cut(data_total_Pb$stress_time, breaks = c(-Inf, 90, 360, Inf),labels = c("Short","Mid","Long"), right=FALSE)

stress_time_Pb <- rma.mv(yi, V_total_Pb, mods = ~ stress_time-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Pb)

#estimate and calculating percentage exp
res_total_Pb_stress_time <- mod_results(stress_time_Pb, mod = "stress_time", group = "stress_time")
(exp(res_total_Pb_stress_time$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Pb_stress_time$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Pb_stress_time$mod_table$upperCL)-1)*100 # upper CI boundary


#Tree_age(连续型变量)
#as.numeric格式(Unknow转换为NA值, 无需去除，不影响结果)
table(data_total_Pb$Tree_age)
#只能运行一次
data_total_Pb$Tree_age <- as.numeric(data_total_Pb$Tree_age)
data_total_Pb$Tree_age <- cut(data_total_Pb$Tree_age, breaks = c(-Inf, 360, 720, Inf),labels = c("Young","Mature","Old"), right=FALSE)
table(data_total_Pb$Tree_age)

Tree_age_Pb <- rma.mv(yi, V_total_Pb, mods = ~ Tree_age-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Pb)

#estimate and calculating percentage exp
res_total_Pb_Tree_age <- mod_results(Tree_age_Pb, mod = "Tree_age", group = "Tree_age")
(exp(res_total_Pb_Tree_age$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Pb_Tree_age$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Pb_Tree_age$mod_table$upperCL)-1)*100 # upper CI boundary


#pH(连续型变量)
#as.numeric格式(Unknow转换为NA值, 无需去除，不影响结果)
table(data_total_Pb$pH)
#只能运行一次
data_total_Pb$pH <- as.numeric(data_total_Pb$pH)

data_total_Pb$pH <- cut(data_total_Pb$pH, breaks = c(-Inf, 6, 8, Inf),labels = c("Low","Midph","High"), right=FALSE)
table(data_total_Pb$pH)


pH_Pb <- rma.mv(yi, V_total_Pb, mods = ~ pH-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Pb)

#estimate and calculating percentage exp
res_total_Pb_pH <- mod_results(pH_Pb, mod = "pH", group = "pH")
(exp(res_total_Pb_pH$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Pb_pH$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Pb_pH$mod_table$upperCL)-1)*100 # upper CI boundary


#Association plots####
fig_data_Pb_total<-rbind(res_total_Pb_reml$mod_table,
                         res_total_Pb_Exp$mod_table,
                         res_total_Pb_plant_method$mod_table,
                         res_total_Pb_Functional_group$mod_table,
                         res_total_Pb_stress_time$mod_table,
                         res_total_Pb_Tree_age$mod_table,
                         res_total_Pb_pH$mod_table)

fig_data_Pb_total$group<-dplyr::recode_factor(fig_data_Pb_total$name,
                                              Intrcpt ="a",
                                              Field="c", Hydroponics="c",Medium="c", Potted="c",
                                              Cutting="g", Plant="g", Seedling="g",
                                              Shrub="d", Tree="d",
                                              Short="e", Mid="e",Long="e",
                                              Young="f", Mature="f", Old="f",
                                              Low="h",Midph="h",High="h")

fig_data_Pb_total$col <- ifelse((exp(fig_data_Pb_total$upperCL)-1)*100>=0,"No","Yes")


table(data_total_Pb$Experimental_method)
table(data_total_Pb$Plant_method)
table(data_total_Pb$Functional_group)
table(data_total_Pb$stress_time)
table(data_total_Pb$Tree_age)
table(data_total_Pb$pH)

fig_data_Pb_total$name <- dplyr::recode_factor(fig_data_Pb_total$name,
                                               Intrcpt = "Overall (123)",
                                               Field="Field ( )", Hydroponics="Hydroponics (36)", Potted="Potted (87)",
                                               Cutting="Cutting (18)", Plant="Plant ( )", Seedling="Seedling (105)",
                                               Shrub="Shrub (23)", Tree="Tree (100)",
                                               Short="Short (29)", Mid="Mid (78)",Long="Long (16)",
                                               Young="Young (34)", Mature="Mature (16)", Old="Old (20)",
                                               Low="Low (37)",Midph="Mid (26)",High="High (28)")


fig_data_Pb_total$HMs <- c(rep("Pb",16))


fig_data_Pb_total$name <- factor(fig_data_Pb_total$name, levels = rev(fig_data_Pb_total$name))

#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))


#作图
Pb_total<-
  ggplot(data=fig_data_Pb_total, aes(x=name, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Moderators', y='% change with heavy metal pollution\ncompared to natural environments')+
  facet_grid(.~HMs,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,100), breaks=seq(-100,100, by=50))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 15.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 13.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 11.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 9.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 6.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 3.5, linetype = 'dashed', col = 'black')

Pb_total

#ggsave("Pb_total_biomass.pdf", width = 6.5, height = 9.5, dpi = 600)


#Cu (53)
data_total_Cu <- subset(es_total_biomass, HMs_type == "Cu")

#VCV
V_total_Cu<-metaAidR::make_VCV_matrix(data = data_total_Cu, V = "vi", 
                                      cluster = "Co_ID", 
                                      obs = "Inf_ID", type = "vcv")

matrixcalc::is.positive.definite(V_total_Cu)

#Meta-analysis
res_total_Cu <- rma.mv(yi, V_total_Cu, random = list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML",data = data_total_Cu, digits = 4)
summary(res_total_Cu)
# Overall estimate and CI
(exp(res_total_Cu$b[[1]])-1)*100 # estimate

res_total_Cu_reml <- mod_results(res_total_Cu, mod = "1", group = "Study_ID") #extracting table of results


(exp(res_total_Cu_reml$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cu_reml$mod_table$upperCL)-1)*100 # upper CI boundary

#Moderators(Experimental_method、Plant_method、Functional_group、stress_time、Tree_age、pH)

#Experimental_method(3 levels)
table(data_total_Cu$Experimental_method)
experimental_Cu <- rma.mv(yi, vi, mods = ~ Experimental_method-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Cu)

#estimate and calculating percentage exp
res_total_Cu_Exp <- mod_results(experimental_Cu, mod = "Experimental_method", group = "Experimental_method")
(exp(res_total_Cu_Exp$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Cu_Exp$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cu_Exp$mod_table$upperCL)-1)*100 # upper CI boundary

#Plant_method(3 levels)
table(data_total_Cu$Plant_method)
Plant_method_Cu <- rma.mv(yi, V_total_Cu, mods = ~ Plant_method-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Cu)

#estimate and calculating percentage exp
res_total_Cu_plant_method <- mod_results(Plant_method_Cu, mod = "Plant_method", group = "Plant_method")
(exp(res_total_Cu_plant_method$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Cu_plant_method$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cu_plant_method$mod_table$upperCL)-1)*100 # upper CI boundary


#Functional_group(2 levels)
table(data_total_Cu$Functional_group)
Functional_group_Cu <- rma.mv(yi, V_total_Cu, mods = ~ Functional_group-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Cu)

#estimate and calculating percentage exp
res_total_Cu_Functional_group <- mod_results(Functional_group_Cu, mod = "Functional_group", group = "Functional_group")
(exp(res_total_Cu_Functional_group$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Cu_Functional_group$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cu_Functional_group$mod_table$upperCL)-1)*100 # upper CI boundary



#Stress_time(连续型变量)
table(data_total_Cu$stress_time)
data_total_Cu$stress_time <- cut(data_total_Cu$stress_time, breaks = c(-Inf, 90, 360, Inf),labels = c("Short","Mid","Long"), right=FALSE)

stress_time_Cu <- rma.mv(yi, V_total_Cu, mods = ~ stress_time-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Cu)

#estimate and calculating percentage exp
res_total_Cu_stress_time <- mod_results(stress_time_Cu, mod = "stress_time", group = "stress_time")
(exp(res_total_Cu_stress_time$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Cu_stress_time$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cu_stress_time$mod_table$upperCL)-1)*100 # upper CI boundary


#Tree_age(连续型变量)
#as.numeric格式(Unknow转换为NA值, 无需去除，不影响结果)
table(data_total_Cu$Tree_age)
#只能运行一次
data_total_Cu$Tree_age <- as.numeric(data_total_Cu$Tree_age)
data_total_Cu$Tree_age <- cut(data_total_Cu$Tree_age, breaks = c(-Inf, 360, 720, Inf),labels = c("Young","Mature","Old"), right=FALSE)
table(data_total_Cu$Tree_age)

Tree_age_Cu <- rma.mv(yi, vi, mods = ~ Tree_age-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Cu)

#estimate and calculating percentage exp
res_total_Cu_Tree_age <- mod_results(Tree_age_Cu, mod = "1", group = "Study_ID")

res_total_Cu_Tree_age <- data.frame(cbind(Tree_age_Cu$b, Tree_age_Cu$ci.lb, Tree_age_Cu$ci.ub))

res_total_Cu_Tree_age <- tibble::rownames_to_column(res_total_Cu_Tree_age, "name")
names(res_total_Cu_Tree_age) <- c("name", "estimate", "lowerCL", "upperCL")
res_total_Cu_Tree_age


(exp(res_total_Cu_Tree_age$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Cu_Tree_age$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cu_Tree_age$upperCL)-1)*100 # upper CI boundary


#pH(连续型变量)
#as.numeric格式(Unknow转换为NA值, 无需去除，不影响结果)
table(data_total_Cu$pH)
#只能运行一次
data_total_Cu$pH <- as.numeric(data_total_Cu$pH)

data_total_Cu$pH <- cut(data_total_Cu$pH, breaks = c(-Inf, 6, 8, Inf),labels = c("Low","Midph","High"), right=FALSE)
table(data_total_Cu$pH)


pH_Cu <- rma.mv(yi, V_total_Cu, mods = ~ pH-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Cu)

#estimate and calculating percentage exp
res_total_Cu_pH <- mod_results(pH_Cu, mod = "pH", group = "pH")
(exp(res_total_Cu_pH$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_total_Cu_pH$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Cu_pH$mod_table$upperCL)-1)*100 # upper CI boundary


#Association plots####
fig_data_Cu_total<-rbind(res_total_Cu_reml$mod_table,
                         #res_total_Cu_Exp$mod_table,
                         res_total_Cu_plant_method$mod_table,
                         res_total_Cu_Functional_group$mod_table,
                         res_total_Cu_stress_time$mod_table,
                         #res_total_Cu_Tree_age$mod_table,
                         res_total_Cu_pH$mod_table)

fig_data_Cu_total$group<-dplyr::recode_factor(fig_data_Cu_total$name,
                                              Intrcpt ="a",
                                              Field="c", Hydroponics="c",Medium="c", Potted="c",
                                              Cutting="g", Plant="g", Seedling="g",
                                              Shrub="d", Tree="d",
                                              Short="e", Mid="e",Long="e",
                                              Young="f", Mature="f", Old="f",
                                              Low="h",Midph="h",High="h")

fig_data_Cu_total$col <- ifelse((exp(fig_data_Cu_total$upperCL)-1)*100>=0,"No","Yes")


table(data_total_Cu$Experimental_method)
table(data_total_Cu$Plant_method)
table(data_total_Cu$Functional_group)
table(data_total_Cu$stress_time)
table(data_total_Cu$Tree_age)
table(data_total_Cu$pH)

fig_data_Cu_total$name <- dplyr::recode_factor(fig_data_Cu_total$name,
                                               Intrcpt = "Overall (53)",
                                               Field="Field ( )", Hydroponics="Hydroponics (39)", Potted="Potted (9)",Medium = "Medium (5)",
                                               Cutting="Cutting (43)", Plant="Plant ( )", Seedling="Seedling (10)",
                                               Shrub="Shrub (10)", Tree="Tree (43)",
                                               Short="Short (41)", Mid="Mid (11)",Long="Long (1)",
                                               Young="Young ( )", Mature="Mature (15)", Old="Old ( )",
                                               Low="Low (15)",Midph="Mid (24)",High="High ( )")


fig_data_Cu_total$HMs <- c(rep("Cu",10))


fig_data_Cu_total$name <- factor(fig_data_Cu_total$name, levels = rev(fig_data_Cu_total$name))

#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))


#作图
Cu_total<-
  ggplot(data=fig_data_Cu_total, aes(x=name, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Moderators', y='% change with heavy metal pollution\ncompared to natural environments')+
  facet_grid(.~HMs,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,100), breaks=seq(-100,100, by=50))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+

  geom_vline(xintercept = 9.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 7.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 5.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 2.5, linetype = 'dashed', col = 'black')

Cu_total


#ggsave("Cu_total_biomass.pdf", width = 6.5, height = 9.5, dpi = 600)


#Zn (26)
table(es_total_biomass$HMs_type)
data_total_Zn <- subset(es_total_biomass, HMs_type == "Zn")

#VCV
V_total_Zn<-metaAidR::make_VCV_matrix(data = data_total_Zn, V = "vi", 
                                      cluster = "Co_ID", 
                                      obs = "Inf_ID", type = "vcv")

matrixcalc::is.positive.definite(V_total_Zn)

#Meta-analysis
res_total_Zn <- rma(yi, V_total_Zn, random = list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML",data = data_total_Zn, digits = 4)
summary(res_total_Zn)
# Overall estimate and CI
(exp(res_total_Zn$b[[1]])-1)*100 # estimate

res_total_Zn_reml <- mod_results(res_total_Zn, mod = "1", group = "Study_ID") #extracting table of results


(exp(res_total_Zn_reml$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Zn_reml$mod_table$upperCL)-1)*100 # upper CI boundary

#Moderators(Experimental_method、Plant_method、Functional_group、stress_time、Tree_age、pH)

#Experimental_method(3 levels)
table(data_total_Zn$Experimental_method)
experimental_Zn <- rma.mv(yi, V_total_Zn, mods = ~ Experimental_method-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Zn)

#estimate and calZnlating percentage exp
res_total_Zn_Exp <- mod_results(experimental_Zn, mod = "Experimental_method", group = "Experimental_method")
(exp(res_total_Zn_Exp$mod_table$estimate)-1)*100 # we calZnlate percentage increase in quality for every level
(exp(res_total_Zn_Exp$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Zn_Exp$mod_table$upperCL)-1)*100 # upper CI boundary

#Plant_method(3 levels)
table(data_total_Zn$Plant_method)
Plant_method_Zn <- rma.mv(yi, V_total_Zn, mods = ~ Plant_method-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Zn)

#estimate and calZnlating percentage exp
res_total_Zn_plant_method <- mod_results(Plant_method_Zn, mod = "Plant_method", group = "Plant_method")
(exp(res_total_Zn_plant_method$mod_table$estimate)-1)*100 # we calZnlate percentage increase in quality for every level
(exp(res_total_Zn_plant_method$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Zn_plant_method$mod_table$upperCL)-1)*100 # upper CI boundary


#Functional_group(2 levels)
table(data_total_Zn$Functional_group)
Functional_group_Zn <- rma.mv(yi, V_total_Zn, mods = ~ Functional_group-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Zn)

#estimate and calZnlating percentage exp
res_total_Zn_Functional_group <- mod_results(Functional_group_Zn, mod = "Functional_group", group = "Functional_group")
(exp(res_total_Zn_Functional_group$mod_table$estimate)-1)*100 # we calZnlate percentage increase in quality for every level
(exp(res_total_Zn_Functional_group$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Zn_Functional_group$mod_table$upperCL)-1)*100 # upper CI boundary



#Stress_time(连续型变量)
table(data_total_Zn$stress_time)
data_total_Zn$stress_time <- cut(data_total_Zn$stress_time, breaks = c(-Inf, 90, 360, Inf),labels = c("Short","Mid","Long"), right=FALSE)

stress_time_Zn <- rma.mv(yi, V_total_Zn, mods = ~ stress_time-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Zn)

#estimate and calZnlating percentage exp
res_total_Zn_stress_time <- mod_results(stress_time_Zn, mod = "stress_time", group = "stress_time")
(exp(res_total_Zn_stress_time$mod_table$estimate)-1)*100 # we calZnlate percentage increase in quality for every level
(exp(res_total_Zn_stress_time$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Zn_stress_time$mod_table$upperCL)-1)*100 # upper CI boundary


#Tree_age(连续型变量)
#as.numeric格式(Unknow转换为NA值, 无需去除，不影响结果)
table(data_total_Zn$Tree_age)
#只能运行一次
data_total_Zn$Tree_age <- as.numeric(data_total_Zn$Tree_age)
data_total_Zn$Tree_age <- cut(data_total_Zn$Tree_age, breaks = c(-Inf, 360, 720, Inf),labels = c("Young","Mature","Old"), right=FALSE)
table(data_total_Zn$Tree_age)

Tree_age_Zn <- rma.mv(yi, V_total_Zn, mods = ~ Tree_age-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Zn)

#estimate and calZnlating percentage exp
res_total_Zn_Tree_age <- mod_results(Tree_age_Zn, mod = "Tree_age", group = "Tree_age")
(exp(res_total_Zn_Tree_age$mod_table$estimate)-1)*100 # we calZnlate percentage increase in quality for every level
(exp(res_total_Zn_Tree_age$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Zn_Tree_age$mod_table$upperCL)-1)*100 # upper CI boundary


#pH(连续型变量)
#as.numeric格式(Unknow转换为NA值, 无需去除，不影响结果)
table(data_total_Zn$pH)
#只能运行一次
data_total_Zn$pH <- as.numeric(data_total_Zn$pH)

data_total_Zn$pH <- cut(data_total_Zn$pH, breaks = c(-Inf, 6, 8, Inf),labels = c("Low","Midph","High"), right=FALSE)
table(data_total_Zn$pH)


pH_Zn <- rma.mv(yi, V_total_Zn, mods = ~ pH-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Zn)

#estimate and calZnlating percentage exp
res_total_Zn_pH <- mod_results(pH_Zn, mod = "pH", group = "pH")
(exp(res_total_Zn_pH$mod_table$estimate)-1)*100 # we calZnlate percentage increase in quality for every level
(exp(res_total_Zn_pH$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Zn_pH$mod_table$upperCL)-1)*100 # upper CI boundary


#Association plots####
fig_data_Zn_total<-rbind(res_total_Zn_reml$mod_table,
                         res_total_Zn_Exp$mod_table,
                         res_total_Zn_plant_method$mod_table,
                         res_total_Zn_Functional_group$mod_table,
                         res_total_Zn_stress_time$mod_table,
                         #res_total_Zn_Tree_age$mod_table,
                         res_total_Zn_pH$mod_table)

fig_data_Zn_total$group<-dplyr::recode_factor(fig_data_Zn_total$name,
                                              Intrcpt ="a",
                                              Field="c", Hydroponics="c",Medium="c", Potted="c",
                                              Zntting="g", Plant="g", Seedling="g",
                                              Shrub="d", Tree="d",
                                              Short="e", Mid="e",Long="e",
                                              Young="f", Mature="f", Old="f",
                                              Low="h",Midph="h",High="h")

fig_data_Zn_total$col <- ifelse((exp(fig_data_Zn_total$upperCL)-1)*100>=0,"No","Yes")


table(data_total_Zn$Experimental_method)
table(data_total_Zn$Plant_method)
table(data_total_Zn$Functional_group)
table(data_total_Zn$stress_time)
table(data_total_Zn$Tree_age)
table(data_total_Zn$pH)

fig_data_Zn_total$name <- dplyr::recode_factor(fig_data_Zn_total$name,
                                               Intrcpt = "Overall (409)",
                                               Field="Field (2)", Hydroponics="Hydroponics (67)", Potted="Potted (115)",
                                               Zntting="Zntting (41)", Plant="Plant (6)", Seedling="Seedling (137)",
                                               Shrub="Shrub (58)", Tree="Tree (126)",
                                               Short="Short (77)", Mid="Mid (104)",Long="Long (3)",
                                               Young="Young (12)", Mature="Mature (38)", Old="Old (24)",
                                               Low="Low (50)",Midph="Mid (70)",High="High (34)")


fig_data_Zn_total$HMs <- c(rep("Zn",12))


mods_names <- as_labeller(c('a'="Overall",
                            'c'="Experiment\nmethod",
                            'g'="Plant pattern",
                            'd'="Functional group",
                            'e'="Stress period",
                            'f'="Tree age",
                            'h'="pH"))

lowercl.totl.bio<-fig_data_Zn_total$lowerCL
uppercl.totl.bio<-fig_data_Zn_total$upperCL



fig_data_Zn_total$name <- factor(fig_data_Zn_total$name, levels = rev(fig_data_Zn_total$name))

#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))


#作图
Zn_total<-
  ggplot(data=fig_data_Zn_total, aes(x=name, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Moderators', y='% change with heavy metal pollution\ncompared to natural environments')+
  facet_grid(.~HMs,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-100,100), breaks=seq(-100,100, by=50))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 17.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 14.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 11.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 9.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 6.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 3.5, linetype = 'dashed', col = 'black')

Zn_total

#ggsave("Zn_total_biomass.pdf", width = 6.5, height = 9.5, dpi = 600)

#As (26)
table(es_total_biomass$HMs_type)
data_total_As <- subset(es_total_biomass, HMs_type == "As")

#VCV
V_total_As<-metaAidR::make_VCV_matrix(data = data_total_As, V = "vi", 
                                      cluster = "Co_ID", 
                                      obs = "Inf_ID", type = "vcv")

matrixcalc::is.positive.definite(V_total_As)

#Meta-analysis
res_total_As <- rma.mv(yi, V_total_As, random = list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML",data = data_total_As, digits = 4)
summary(res_total_As)
# Overall estimate and CI
(exp(res_total_As$b[[1]])-1)*100 # estimate

res_total_As_reml <- mod_results(res_total_As, mod = "1", group = "Study_ID") #extracting table of results


(exp(res_total_As_reml$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_As_reml$mod_table$upperCL)-1)*100 # upper CI boundary

#Moderators(Experimental_method、Plant_method、Functional_group、stress_time、Tree_age、pH)

#Experimental_method(3 levels)
table(data_total_As$Experimental_method)
experimental_As <- rma.mv(yi, V_total_As, mods = ~ Experimental_method-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_As)

#estimate and calAslating percentage exp
res_total_As_Exp <- mod_results(experimental_As, mod = "Experimental_method", group = "Experimental_method")
(exp(res_total_As_Exp$mod_table$estimate)-1)*100 # we calAslate percentage increase in quality for every level
(exp(res_total_As_Exp$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_As_Exp$mod_table$upperCL)-1)*100 # upper CI boundary

#Plant_method(3 levels)
table(data_total_As$Plant_method)
Plant_method_As <- rma.mv(yi, V_total_As, mods = ~ Plant_method-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_As)

#estimate and calAslating percentage exp
res_total_As_plant_method <- mod_results(Plant_method_As, mod = "Plant_method", group = "Plant_method")
(exp(res_total_As_plant_method$mod_table$estimate)-1)*100 # we calAslate percentage increase in quality for every level
(exp(res_total_As_plant_method$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_As_plant_method$mod_table$upperCL)-1)*100 # upper CI boundary


#Functional_group(2 levels)
table(data_total_As$Functional_group)
Functional_group_As <- rma.mv(yi, V_total_As, mods = ~ Functional_group-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_As)

#estimate and calAslating percentage exp
res_total_As_Functional_group <- mod_results(Functional_group_As, mod = "Functional_group", group = "Functional_group")
(exp(res_total_As_Functional_group$mod_table$estimate)-1)*100 # we calAslate percentage increase in quality for every level
(exp(res_total_As_Functional_group$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_As_Functional_group$mod_table$upperCL)-1)*100 # upper CI boundary



#Stress_time(连续型变量)
table(data_total_As$stress_time)
data_total_As$stress_time <- cut(data_total_As$stress_time, breaks = c(-Inf, 90, 360, Inf),labels = c("Short","Mid","Long"), right=FALSE)

stress_time_As <- rma.mv(yi, V_total_As, mods = ~ stress_time-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_As)

#estimate and calAslating percentage exp
res_total_As_stress_time <- mod_results(stress_time_As, mod = "stress_time", group = "stress_time")
(exp(res_total_As_stress_time$mod_table$estimate)-1)*100 # we calAslate percentage increase in quality for every level
(exp(res_total_As_stress_time$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_As_stress_time$mod_table$upperCL)-1)*100 # upper CI boundary


#Tree_age(连续型变量)
#as.numeric格式(Unknow转换为NA值, 无需去除，不影响结果)
table(data_total_As$Tree_age)
#只能运行一次
data_total_As$Tree_age <- as.numeric(data_total_As$Tree_age)
data_total_As$Tree_age <- cut(data_total_As$Tree_age, breaks = c(-Inf, 360, 720, Inf),labels = c("Young","Mature","Old"), right=FALSE)
table(data_total_As$Tree_age)

Tree_age_As <- rma.mv(yi, V_total_As, mods = ~ Tree_age-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_As)

#estimate and calAslating percentage exp
res_total_As_Tree_age <- mod_results(Tree_age_As, mod = "Tree_age", group = "Tree_age")
(exp(res_total_As_Tree_age$mod_table$estimate)-1)*100 # we calAslate percentage increase in quality for every level
(exp(res_total_As_Tree_age$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_As_Tree_age$mod_table$upperCL)-1)*100 # upper CI boundary


#pH(连续型变量)
#as.numeric格式(Unknow转换为NA值, 无需去除，不影响结果)
table(data_total_As$pH)
#只能运行一次
data_total_As$pH <- as.numeric(data_total_As$pH)

data_total_As$pH <- cut(data_total_As$pH, breaks = c(-Inf, 6, 8, Inf),labels = c("Low","Midph","High"), right=FALSE)
table(data_total_As$pH)


pH_As <- rma.mv(yi, V_total_As, mods = ~ pH-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_As)

#estimate and calAslating percentage exp
res_total_As_pH <- mod_results(pH_As, mod = "pH", group = "pH")
(exp(res_total_As_pH$mod_table$estimate)-1)*100 # we calAslate percentage increase in quality for every level
(exp(res_total_As_pH$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_As_pH$mod_table$upperCL)-1)*100 # upper CI boundary


#Association plots####
fig_data_As_total<-rbind(res_total_As_reml$mod_table,
                         #res_total_As_Exp$mod_table,
                         res_total_As_plant_method$mod_table)
                         #res_total_As_Functional_group$mod_table,
                         #res_total_As_stress_time$mod_table,
                         #res_total_As_Tree_age$mod_table,
                         #res_total_As_pH$mod_table)

fig_data_As_total$group<-dplyr::recode_factor(fig_data_As_total$name,
                                              Intrcpt ="a",
                                              Field="c", Hydroponics="c",Medium="c", Potted="c",
                                              Astting="g", Plant="g", Seedling="g",
                                              Shrub="d", Tree="d",
                                              Short="e", Mid="e",Long="e",
                                              Young="f", Mature="f", Old="f",
                                              Low="h",Midph="h",High="h")

fig_data_As_total$col <- ifelse((exp(fig_data_As_total$upperCL)-1)*100>=0,"No","Yes")


table(data_total_As$Experimental_method)
table(data_total_As$Plant_method)
table(data_total_As$Functional_group)
table(data_total_As$stress_time)
table(data_total_As$Tree_age)
table(data_total_As$pH)

fig_data_As_total$name <- dplyr::recode_factor(fig_data_As_total$name,
                                               Intrcpt = "Overall (409)",
                                               Field="Field (2)", Hydroponics="Hydroponics (67)", Potted="Potted (115)",
                                               Astting="Astting (41)", Plant="Plant (6)", Seedling="Seedling (137)",
                                               Shrub="Shrub (58)", Tree="Tree (126)",
                                               Short="Short (77)", Mid="Mid (104)",Long="Long (3)",
                                               Young="Young (12)", Mature="Mature (38)", Old="Old (24)",
                                               Low="Low (50)",Midph="Mid (70)",High="High (34)")


fig_data_As_total$HMs <- c(rep("As",3))


mods_names <- as_labeller(c('a'="Overall",
                            'c'="Experiment\nmethod",
                            'g'="Plant pattern",
                            'd'="Functional group",
                            'e'="Stress period",
                            'f'="Tree age",
                            'h'="pH"))

lowercl.totl.bio<-fig_data_As_total$lowerCL
uppercl.totl.bio<-fig_data_As_total$upperCL



fig_data_As_total$name <- factor(fig_data_As_total$name, levels = rev(fig_data_As_total$name))

#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))


#作图
As_total<-
  ggplot(data=fig_data_As_total, aes(x=name, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Moderators', y='% change with heavy metal pollution\ncompared to natural environments')+
  facet_grid(.~HMs,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_y_continuous(limits = c(-150,150), breaks=seq(-150,150, by=50))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 1.5, linetype = 'dashed', col = 'black')+
  geom_vline(xintercept = 2.5, linetype = 'dashed', col = 'black')

As_total

#ggsave("As_total_biomss.pdf", width = 6.5, height = 3, dpi = 600)



#Sb (26)
table(es_total_biomass$HMs_type)
data_total_Sb <- subset(es_total_biomass, HMs_type == "Sb")

#VCV
V_total_Sb<-metaAidR::make_VCV_matrix(data = data_total_Sb, V = "vi", 
                                      cluster = "Co_ID", 
                                      obs = "Inf_ID", type = "vcv")

matrixcalc::is.positive.definite(V_total_Sb)

#Meta-analysis
res_total_Sb <- rma.mv(yi, V_total_Sb, random = list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML",data = data_total_Sb, digits = 4)
summary(res_total_Sb)
# Overall estimate and CI
(exp(res_total_Sb$b[[1]])-1)*100 # estimate

res_total_Sb_reml <- mod_results(res_total_Sb, mod = "1", group = "Study_ID") #extracting table of results


(exp(res_total_Sb_reml$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Sb_reml$mod_table$upperCL)-1)*100 # upper CI boundary

#Moderators(Experimental_method、Plant_method、Functional_group、stress_time、Tree_age、pH)

#Experimental_method(3 levels)
table(data_total_Sb$Experimental_method)
experimental_Sb <- rma.mv(yi, V_total_Sb, mods = ~ Experimental_method-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Sb)

#estimate and calSblating percentage exp
res_total_Sb_Exp <- mod_results(experimental_Sb, mod = "Experimental_method", group = "Experimental_method")
(exp(res_total_Sb_Exp$mod_table$estimate)-1)*100 # we calSblate percentage increSbe in quality for every level
(exp(res_total_Sb_Exp$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Sb_Exp$mod_table$upperCL)-1)*100 # upper CI boundary

#Plant_method(3 levels)
table(data_total_Sb$Plant_method)
Plant_method_Sb <- rma.mv(yi, V_total_Sb, mods = ~ Plant_method-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Sb)

#estimate and calSblating percentage exp
res_total_Sb_plant_method <- mod_results(Plant_method_Sb, mod = "Plant_method", group = "Plant_method")
(exp(res_total_Sb_plant_method$mod_table$estimate)-1)*100 # we calSblate percentage increSbe in quality for every level
(exp(res_total_Sb_plant_method$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Sb_plant_method$mod_table$upperCL)-1)*100 # upper CI boundary


#Functional_group(2 levels)
table(data_total_Sb$Functional_group)
Functional_group_Sb <- rma.mv(yi, V_total_Sb, mods = ~ Functional_group-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Sb)

#estimate and calSblating percentage exp
res_total_Sb_Functional_group <- mod_results(Functional_group_Sb, mod = "Functional_group", group = "Functional_group")
(exp(res_total_Sb_Functional_group$mod_table$estimate)-1)*100 # we calSblate percentage increSbe in quality for every level
(exp(res_total_Sb_Functional_group$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Sb_Functional_group$mod_table$upperCL)-1)*100 # upper CI boundary



#Stress_time(连续型变量)
table(data_total_Sb$stress_time)
data_total_Sb$stress_time <- cut(data_total_Sb$stress_time, breaks = c(-Inf, 90, 360, Inf),labels = c("Short","Mid","Long"), right=FALSE)

stress_time_Sb <- rma.mv(yi, V_total_Sb, mods = ~ stress_time-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Sb)

#estimate and calSblating percentage exp
res_total_Sb_stress_time <- mod_results(stress_time_Sb, mod = "stress_time", group = "stress_time")
(exp(res_total_Sb_stress_time$mod_table$estimate)-1)*100 # we calSblate percentage increSbe in quality for every level
(exp(res_total_Sb_stress_time$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Sb_stress_time$mod_table$upperCL)-1)*100 # upper CI boundary


#Tree_age(连续型变量)
#Sb.numeric格式(Unknow转换为NA值, 无需去除，不影响结果)
table(data_total_Sb$Tree_age)
#只能运行一次
data_total_Sb$Tree_age <- Sb.numeric(data_total_Sb$Tree_age)
data_total_Sb$Tree_age <- cut(data_total_Sb$Tree_age, breaks = c(-Inf, 360, 720, Inf),labels = c("Young","Mature","Old"), right=FALSE)
table(data_total_Sb$Tree_age)

Tree_age_Sb <- rma.mv(yi, V_total_Sb, mods = ~ Tree_age-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Sb)

#estimate and calSblating percentage exp
res_total_Sb_Tree_age <- mod_results(Tree_age_Sb, mod = "Tree_age", group = "Tree_age")
(exp(res_total_Sb_Tree_age$mod_table$estimate)-1)*100 # we calSblate percentage increSbe in quality for every level
(exp(res_total_Sb_Tree_age$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Sb_Tree_age$mod_table$upperCL)-1)*100 # upper CI boundary


#pH(连续型变量)
#Sb.numeric格式(Unknow转换为NA值, 无需去除，不影响结果)
table(data_total_Sb$pH)
#只能运行一次
data_total_Sb$pH <- Sb.numeric(data_total_Sb$pH)

data_total_Sb$pH <- cut(data_total_Sb$pH, breaks = c(-Inf, 6, 8, Inf),labels = c("Low","Midph","High"), right=FALSE)
table(data_total_Sb$pH)


pH_Sb <- rma.mv(yi, V_total_Sb, mods = ~ pH-1, random =  list(~1|Study_ID/Co_ID, ~1|Family/Genus/Species), method = "REML", data = data_total_Sb)

#estimate and calSblating percentage exp
res_total_Sb_pH <- mod_results(pH_Sb, mod = "pH", group = "pH")
(exp(res_total_Sb_pH$mod_table$estimate)-1)*100 # we calSblate percentage increSbe in quality for every level
(exp(res_total_Sb_pH$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_total_Sb_pH$mod_table$upperCL)-1)*100 # upper CI boundary


#Sbsociation plots####
fig_data_Sb_total<-rbind(res_total_Sb_reml$mod_table,
                         #res_total_Sb_Exp$mod_table,
                         res_total_Sb_plant_method$mod_table)
#res_total_Sb_Functional_group$mod_table,
#res_total_Sb_stress_time$mod_table,
#res_total_Sb_Tree_age$mod_table,
#res_total_Sb_pH$mod_table)

fig_data_Sb_total$group<-dplyr::recode_factor(fig_data_Sb_total$name,
                                              Intrcpt ="a",
                                              Field="c", Hydroponics="c",Medium="c", Potted="c",
                                              Sbtting="g", Plant="g", Seedling="g",
                                              Shrub="d", Tree="d",
                                              Short="e", Mid="e",Long="e",
                                              Young="f", Mature="f", Old="f",
                                              Low="h",Midph="h",High="h")

fig_data_Sb_total$col <- ifelse((exp(fig_data_Sb_total$upperCL)-1)*100>=0,"No","Yes")


table(data_total_Sb$Experimental_method)
table(data_total_Sb$Plant_method)
table(data_total_Sb$Functional_group)
table(data_total_Sb$stress_time)
table(data_total_Sb$Tree_age)
table(data_total_Sb$pH)

fig_data_Sb_total$name <- dplyr::recode_factor(fig_data_Sb_total$name,
                                               Intrcpt = "Overall (409)",
                                               Field="Field (2)", Hydroponics="Hydroponics (67)", Potted="Potted (115)",
                                               Sbtting="Sbtting (41)", Plant="Plant (6)", Seedling="Seedling (137)",
                                               Shrub="Shrub (58)", Tree="Tree (126)",
                                               Short="Short (77)", Mid="Mid (104)",Long="Long (3)",
                                               Young="Young (12)", Mature="Mature (38)", Old="Old (24)",
                                               Low="Low (50)",Midph="Mid (70)",High="High (34)")


fig_data_Sb_total$HMs <- c(rep("Sb",3))


mods_names <- Sb_labeller(c('a'="Overall",
                            'c'="Experiment\nmethod",
                            'g'="Plant pattern",
                            'd'="Functional group",
                            'e'="Stress period",
                            'f'="Tree age",
                            'h'="pH"))

lowercl.totl.bio<-fig_data_Sb_total$lowerCL
uppercl.totl.bio<-fig_data_Sb_total$upperCL



fig_data_Sb_total$name <- factor(fig_data_Sb_total$name, levels = rev(fig_data_Sb_total$name))

#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))


#作图
Sb_total<-
  ggplot(data=fig_data_Sb_total, aes(x=name, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=col),shape=20, size=2, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  theme_bw()+
  labs(x='Moderators', y='% change with heavy metal pollution\ncompared to natural environments')+
  facet_grid(.~HMs,switch = "y")+
  geom_hline(yintercept=0, linetype = 'dSbhed', col = 'black')+
  scale_y_continuous(limits = c(-150,150), breaks=seq(-150,150, by=50))+
  scale_colour_manual(values = c("gray60", "red"))+
  theme(legend.position = "none")+
  font+
  theme(strip.text.x = element_text(size = 20))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'))+
  geom_vline(xintercept = 1.5, linetype = 'dSbhed', col = 'black')+
  geom_vline(xintercept = 2.5, linetype = 'dSbhed', col = 'black')

Sb_total

#ggsave("Sb_total_biomss.pdf", width = 6.5, height = 3, dpi = 600)