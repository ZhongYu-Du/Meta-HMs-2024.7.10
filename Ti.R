###Ti index in Genus####
data_Ti <- read_xlsx("data-all.xlsx", sheet = "Total_biomass")
data_Ti$Var <- rep("Ti", 402)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

Ti <- ggplot(data_Ti, aes(x = reorder(Genus, -Ti), y = Ti)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5, fill = "gray80") +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun="mean", geom="point", shape=20, size=2.5, color="red", fill="red",alpha=1)+
  theme_bw()+
  facet_wrap(~Var)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Tolerance index', fill = '', color = '') +
  scale_y_continuous(limits = c(0,2), breaks=seq(0,2, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = "none")+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.6, linetype = 'dashed', col = 'red', size = 0.8)+
  geom_hline(yintercept=0.35, linetype = 'dashed', col = 'black', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 42 , y = 1.07,label = "Ti = 1.0", colour="blue", size = 6)+
  annotate("text", x = 4 , y = 0.67,label = "Ti = 0.60", colour="red", size = 6)+
  annotate("text", x = 4 , y = 0.42,label = "Ti = 0.35",colour="black", size = 6)

#ggsave("Ti-genus.pdf", Ti, width = 12, height = 7)


###Ti index in Genus####
data_Ti <- read_xlsx("data-all.xlsx", sheet = "Total_biomass")


######Cd####
table(data_Ti$HMs_type)

data_Ti_Cd <- subset(data_Ti, HMs_type == "Cd")

data_Ti_Cd$HM <- rep("Cd", 184)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

Ti_Cd <- ggplot(data_Ti_Cd, aes(x = reorder(Genus, -Ti), y = Ti)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5, fill = "gray80") +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun="mean", geom="point", shape=20, size=2.5, color="red", fill="red",alpha=1)+
  theme_bw()+
  facet_wrap(~HM)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Tolerance index', fill = '', color = '') +
  scale_y_continuous(limits = c(0,2), breaks=seq(0,2, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = "none")+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.6, linetype = 'dashed', col = 'red', size = 0.8)+
  geom_hline(yintercept=0.35, linetype = 'dashed', col = 'black', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 30 , y = 1.07,label = "Ti = 1.0", colour="blue", size = 6)+
  annotate("text", x = 4 , y = 0.67,label = "Ti = 0.60", colour="red", size = 6)+
  annotate("text", x = 4 , y = 0.42,label = "Ti = 0.35",colour="black", size = 6)
Ti_Cd
#ggsave("Ti-genus.pdf", Ti, width = 12, height = 7)


######Pb####
table(data_Ti$HMs_type)

data_Ti_Pb <- subset(data_Ti, HMs_type == "Pb")

data_Ti_Pb$HM <- rep("Pb", 123)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

Ti_Pb <- ggplot(data_Ti_Pb, aes(x = reorder(Genus, -Ti), y = Ti)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5, fill = "gray80") +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun="mean", geom="point", shape=20, size=2.5, color="red", fill="red",alpha=1)+
  theme_bw()+
  facet_wrap(~HM)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Tolerance index', fill = '', color = '') +
  scale_y_continuous(limits = c(0,2), breaks=seq(0,2, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = "none")+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.6, linetype = 'dashed', col = 'red', size = 0.8)+
  geom_hline(yintercept=0.35, linetype = 'dashed', col = 'black', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 25 , y = 1.07,label = "Ti = 1.0", colour="blue", size = 6)+
  annotate("text", x = 4 , y = 0.67,label = "Ti = 0.60", colour="red", size = 6)+
  annotate("text", x = 4 , y = 0.42,label = "Ti = 0.35",colour="black", size = 6)
Ti_Pb
#ggsave("Ti-genus.pdf", Ti, width = 12, height = 7)


######Cu####
table(data_Ti$HMs_type)

data_Ti_Cu <- subset(data_Ti, HMs_type == "Cu")

data_Ti_Cu$HM <- rep("Cu", 53)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

Ti_Cu <- ggplot(data_Ti_Cu, aes(x = reorder(Genus, -Ti), y = Ti)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5, fill = "gray80") +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun="mean", geom="point", shape=20, size=2.5, color="red", fill="red",alpha=1)+
  theme_bw()+
  facet_wrap(~HM)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Tolerance index', fill = '', color = '') +
  scale_y_continuous(limits = c(0,1.5), breaks=seq(0,1.5, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = "none")+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.6, linetype = 'dashed', col = 'red', size = 0.8)+
  geom_hline(yintercept=0.35, linetype = 'dashed', col = 'black', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 1 , y = 1.07,label = "Ti = 1.0", colour="blue", size = 6)+
  annotate("text", x = 1 , y = 0.67,label = "Ti = 0.60", colour="red", size = 6)+
  annotate("text", x = 1 , y = 0.42,label = "Ti = 0.35",colour="black", size = 6)
Ti_Cu
#ggsave("Ti-genus.pdf", Ti, width = 12, height = 7)



######Zn####
table(data_Ti$HMs_type)

data_Ti_Zn <- subset(data_Ti, HMs_type == "Zn")

data_Ti_Zn$HM <- rep("Zn", 26)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

Ti_Zn <- ggplot(data_Ti_Zn, aes(x = reorder(Genus, -Ti), y = Ti)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5, fill = "gray80") +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun="mean", geom="point", shape=20, size=2.5, color="red", fill="red",alpha=1)+
  theme_bw()+
  facet_wrap(~HM)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Tolerance index', fill = '', color = '') +
  scale_y_continuous(limits = c(0,1.8), breaks=seq(0,1.8, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = "none")+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.6, linetype = 'dashed', col = 'red', size = 0.8)+
  geom_hline(yintercept=0.35, linetype = 'dashed', col = 'black', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 1 , y = 1.07,label = "Ti = 1.0", colour="blue", size = 6)+
  annotate("text", x = 1 , y = 0.67,label = "Ti = 0.60", colour="red", size = 6)+
  annotate("text", x = 1 , y = 0.42,label = "Ti = 0.35",colour="black", size = 6)
Ti_Zn
#ggsave("Ti-genus.pdf", Ti, width = 12, height = 7)


######Sb####
table(data_Ti$HMs_type)

data_Ti_Sb <- subset(data_Ti, HMs_type == "Sb")

data_Ti_Sb$HM <- rep("Sb", 16)


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

Ti_Sb <- ggplot(data_Ti_Sb, aes(x = reorder(Genus, -Ti), y = Ti)) +
  geom_boxplot(outlier.size = 0.5, size = 0.5, fill = "gray80") +
  #geom_text(data = stat, aes(label = sig), vjust = -0.5, size=5) +
  stat_summary(fun="mean", geom="point", shape=20, size=2.5, color="red", fill="red",alpha=1)+
  theme_bw()+
  facet_wrap(~HM)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black')) +
  labs(x = 'Genus', y = 'Tolerance index', fill = '', color = '') +
  scale_y_continuous(limits = c(0,1.5), breaks=seq(0,1.5, by=0.6))+
  font+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(legend.position = "none")+
  geom_hline(yintercept=1, linetype = 'dashed', col = 'blue', size = 0.8)+
  geom_hline(yintercept=0.6, linetype = 'dashed', col = 'red', size = 0.8)+
  geom_hline(yintercept=0.35, linetype = 'dashed', col = 'black', size = 0.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, face = "italic"))+
  annotate("text", x = 1 , y = 1.07,label = "Ti = 1.0", colour="blue", size = 6)+
  annotate("text", x = 1 , y = 0.67,label = "Ti = 0.60", colour="red", size = 6)+
  annotate("text", x = 1 , y = 0.42,label = "Ti = 0.35",colour="black", size = 6)
Ti_Sb
#ggsave("Ti-genus.pdf", Ti, width = 12, height = 7)