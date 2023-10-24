########################################
# Code to replicate the analyses and figures of:
# "Global meta-analysis shows reduced quality of food crops under inadequate animal pollination"
# developed by Elena Gazzea, P閠er Batary, Lorenzo Marini
########################################


# Load libraries ----
# Install orchaRd package (experimental package, not in CRAN)
# install.packages("pacman")
# pacman::p_load(devtools, tidyverse, patchwork, R.rsp, emmeans)
# devtools::install_github("daniel1noble/orchaRd", force = TRUE)

# Install metaAidR package (experimental package, not in CRAN)
# devtools::install_github("daniel1noble/metaAidR")

pacman::p_load(vcd, # visualising association among moderators
               metafor, # meta-analytical models
               matrixcalc, # test matrix for positive definiteness
               orchaRd, # orchard plots and model estimates
               metaAidR, # variance-covariance matrix
               ggplot2, # visualising data
               dplyr) # modifying variables

setwd("E:\\博士研究生\\Meta分析论文\\B重金属与木本植物meta")
# Load data ----
data7<-read.table(file="data7.txt", header=TRUE, sep="\t")

write.csv(data7,"data7_NC.csv")
ser1<-droplevels(subset(data7, pollination =="service")) # pollination service
def1<-droplevels(subset(data7, pollination =="deficit")) # pollination deficit

write.csv(ser1,"ser1.csv")
write.csv(def1,"def1.csv")
ser1 <- read.csv("ser1.csv")
def1 <- read.csv("def1.csv")
####################### POLLINATION SERVICE ###############

# Variance-covariance matrix ----
# Generating a variance-covariance matrix assuming 0.5 correlation of sampling errors between effect sizes that share a common cluster ID
# Effect sizes are clustered when more than one effect size is reported from one experimental unit
V.ser1.endpoints_0.5<-metaAidR::make_VCV_matrix(data = ser1, V = "R_vi", cluster = "multiple_endpoint_clusterID", obs = "infoID", type = "vcv", rho = 0.5)
is.positive.definite (V.ser1.endpoints_0.5) # testing matrix for positive definiteness


# Random effects structure ----
# We build null models and compare their AIC to identify the best random effects structure
RES1 <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, random = list(~1|studyID, ~1|infoID), data = ser1, method = "ML")
AIC(RES1)

RES2 <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, random = list(~1|studyID, ~1|country, ~1|infoID), data = ser1, method = "ML")
AIC(RES2)

RES3 <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, random = list(~1|studyID/year_exp, ~1|infoID), data = ser1, method = "ML")
AIC(RES3)

RES4 <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, random = list(~1|country/studyID, ~1|infoID), data = ser1, method = "ML")
AIC(RES4)

RES5 <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "ML")
AIC(RES5)

# The best null model is fitted with REML
RES5.reml <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(RES5.reml)


# Overall estimate and CI ----
# We calculate the estimates of the natural logarithm of the response ratio as mean percentage of quality change 
(exp(RES5.reml$b[[1]])-1)*100 # estimate

res_RES5.reml <- mod_results(RES5.reml, mod = "1", group="infoID") # extracting table of results
print(res_RES5.reml)
(exp(res_RES5.reml$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_RES5.reml$mod_table$upperCL)-1)*100 # upper CI boundary


# Testing moderators ----
# We build univariate meta-analytical models
# For each moderator we follow the same procedure, described below in detail

## Quality trait (7 levels) ----
# We first fit the model with moderator with ML to compare it to the null model (AIC and likelihood ratio test)
# We fit the model without intercept to get estimated effect (slope) for each level of a categorical moderator
QUS1 <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~quality_aggr_7levels - 1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "ML")
anova(QUS1, RES5)
# We fit the model with REML to get estimates
QUS1.reml <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~quality_aggr_7levels - 1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(QUS1.reml)
# We calculate marginal R^2 of model with moderator
r2_ml(QUS1.reml)

# We extract table of results
res_QUS1 <- mod_results(QUS1.reml, mod ="quality_aggr_7levels", group="quality_aggr_7levels")
print(res_QUS1)
(exp(res_QUS1$mod_table$estimate)-1)*100 # we calculate percentage increase in quality for every level
(exp(res_QUS1$mod_table$lowerCL)-1)*100 # lower CI boundary
(exp(res_QUS1$mod_table$upperCL)-1)*100 # upper CI boundary

# We now fit the same model with the intercept to calculate the heterogeneity explained by the levels of the moderator
QUS1.reml.intercept <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~quality_aggr_7levels, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(QUS1.reml.intercept)
?orchard_plot
# Orchard plot
box<-res_QUS1$mod_table
orchard_plot(QUS1.reml, mod="quality_aggr_7levels", xlab="lnRR (effect size)", group="studyID", angle=0, cb=T, alpha=0.1, k=T, g=T)+
  guides(y=guide_axis(angle=0)) +
  theme(legend.position="none",
        axis.title=element_text(size=20),
        axis.text=element_text(size=20)) +

  annotate(geom="text", x = 6.1, y = 2.4,
           label = paste0(round((exp(box$estimate[[6]])-1),3) * 100, 
                          "% (",round((exp(box$lowerCL[[6]])-1),3) * 100, "%", " : ", round((exp(box$upperCL[[6]])-1),3) * 100, "%)"))+
  annotate(geom="text", x = 5.1, y = 2.4,
           label = paste0(round((exp(box$estimate[[5]])-1),3) * 100, 
                          "% (",round((exp(box$lowerCL[[5]])-1),3) * 100, "%", " : ", round((exp(box$upperCL[[5]])-1),3) * 100, "%)"))+
  annotate(geom="text", x = 4.1, y = 2.4,
           label = paste0(round((exp(box$estimate[[4]])-1),3) * 100,
                          "% (",round((exp(box$lowerCL[[4]])-1),3) * 100, "%", " : ", round((exp(box$upperCL[[4]])-1),3) * 100, "%)"))+
  annotate(geom="text", x = 3.1, y = 2.4,
           label = paste0(round((exp(box$estimate[[3]])-1),3) * 100, 
                          "% (", round((exp(box$lowerCL[[3]])-1),3) * 100, "%", " : ", round((exp(box$upperCL[[3]])-1),3) * 100, "%)"))+
  annotate(geom="text", x = 2.1, y = 2.4,
           label = paste0(round((exp(box$estimate[[2]])-1),3) * 100, 
                          "% (", round((exp(box$lowerCL[[2]])-1),3) * 100, "%", " : ", round((exp(box$upperCL[[2]])-1),3) * 100, "%)"))+
  annotate(geom="text", x = 1.1, y = 2.4,
           label = paste0(round((exp(box$estimate[[1]])-1),3) * 100, 
                          "% (", round((exp(box$lowerCL[[1]])-1),3) * 100, "%", " : ", round((exp(box$upperCL[[1]])-1),3) * 100, "%)"))


## Quality trait (2 levels) ----
QUS2 <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~quality_broad - 1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "ML")
anova(QUS2, RES5)

QUS2.reml <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~quality_broad - 1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(QUS2.reml)

r2_ml(QUS2.reml)

res_QUS2 <- mod_results(QUS2.reml, mod = "quality_broad", group="quality_broad")
print(res_QUS2)
(exp(res_QUS2$mod_table$estimate)-1)*100
(exp(res_QUS2$mod_table$lowerCL)-1)*100
(exp(res_QUS2$mod_table$upperCL)-1)*100

QUS2.reml.intercept <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~quality_broad, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(QUS2.reml.intercept)


## Pollinator group ----
POS<- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~species_aggr1 - 1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "ML")
anova(POS, RES5)

POS.reml<- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~species_aggr1 - 1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(POS.reml)

res_POS <- mod_results(POS.reml, mod ="species_aggr1", group="species_aggr1")
print(res_POS)
(exp(res_POS$mod_table$estimate)-1)*100
(exp(res_POS$mod_table$lowerCL)-1)*100
(exp(res_POS$mod_table$upperCL)-1)*100

POS.reml.intercept<- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~relevel(factor(species_aggr1), ref="bombus"), random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(POS.reml.intercept)


## Crop type ----
CRS<- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~crop_aggr_klein - 1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "ML")
anova(CRS, RES5)

CRS.reml<- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~crop_aggr_klein - 1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(CRS.reml)

res_CRS <- mod_results(CRS.reml, mod ="crop_aggr_klein", group="crop_aggr_klein")
print(res_CRS)
(exp(res_CRS$mod_table$estimate)-1)*100
(exp(res_CRS$mod_table$lowerCL)-1)*100
(exp(res_CRS$mod_table$upperCL)-1)*100

CRS.reml.intercept<- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~crop_aggr_klein, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(CRS.reml.intercept)


## Experimental scale ----
# We remove the effects sizes, for which info on experimental scale was not available (n=11)
ser1.scale.subset<-subset(ser1, scale!="NA")
# We construct a new variance-covariance matrix with the subset dataset
V.ser1.scale.subset.endpoints_0.5 <- metaAidR::make_VCV_matrix(data = ser1.scale.subset, V = "R_vi", cluster = "multiple_endpoint_clusterID", obs = "infoID", type = "vcv", rho = 0.5)
is.positive.definite (V.ser1.scale.subset.endpoints_0.5)

# we build a new null model with the subset dataset
RES5.scale.subset <- rma.mv(yi = R_yi, V = V.ser1.scale.subset.endpoints_0.5, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1.scale.subset, method = "ML")

# We add scale as moderator
SCS<- rma.mv(yi = R_yi, V =V.ser1.scale.subset.endpoints_0.5, mods = ~scale -1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1.scale.subset, method = "ML")
anova(SCS, RES5.scale.subset)

SCS.reml<- rma.mv(yi = R_yi, V = V.ser1.scale.subset.endpoints_0.5, mods = ~scale - 1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1.scale.subset, method = "REML")
summary(SCS.reml)

res_SCS <- mod_results (SCS.reml, mod="scale", group="scale")

SCS.reml.intercept<- rma.mv(yi = R_yi, V = V.ser1.scale.subset.endpoints_0.5, mods = ~scale, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1.scale.subset, method = "REML")
summary(SCS.reml.intercept)


## Cropping environment ----
ser1$greenhouse2 <- ser1$greenhouse
ser1$greenhouse2[ser1$greenhouse2 == "0"] <- "no"
ser1$greenhouse2[ser1$greenhouse2 == "1"] <- "yes"

GRS<- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~greenhouse2 -1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "ML")
anova(GRS, RES5)

GRS.reml<-rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~greenhouse2 -1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(GRS.reml)

res_GRS <- mod_results (GRS.reml, mod="greenhouse2", group="greenhouse2",)

GRS.reml.intercept<-rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~greenhouse2, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(GRS.reml.intercept)


## Climate ----
BIS<- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~biome - 1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "ML")
anova(BIS, RES5)

BIS.reml<- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~biome - 1, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(BIS.reml)

res_BIS <- mod_results (BIS.reml, mod="biome", group="biome")
BIS.reml.intercept<- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~biome, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(BIS.reml.intercept)


# Publication bias ----
## Time-lag bias ----
# We construct a model with publication year as moderator. Here we allow the intercept so the model output reports the intercept and slope for any time trend
YRS<- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~year, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "ML")
anova(YRS, RES5)

YRS.reml<- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~year, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(YRS.reml)

# Time-lag bias figure
pred_time <- predict.rma(YRS.reml)
predict.rma(YRS.reml, newmods = c(1968, 2023))
time_lag <- ser1 %>% dplyr::mutate(ymin = pred_time$ci.lb, ymax = pred_time$ci.ub,
                                   ymin2 = pred_time$cr.lb, ymax2 = pred_time$cr.ub, pred = pred_time$pred) %>%ggplot(aes(x = year, y = R_yi, size = sqrt(1/R_vi))) +
  geom_point(shape = 21, fill = "grey90") + 
  geom_smooth(aes(y = ymin2), method = "loess", se = FALSE, lty = "dotted",
              lwd = 0.75, colour = "#0072B2") +
  geom_smooth(aes(y = ymax2),method = "loess", se = FALSE, lty = "dotted", lwd = 0.75, colour = "#0072B2") +
  geom_smooth(aes(y = ymin), method = "loess",se = FALSE, lty = "dotted", lwd = 0.75, colour = "#D55E00") +
  geom_smooth(aes(y = ymax), method = "loess", se = FALSE,
              lty = "dotted", lwd = 0.75, colour = "#D55E00") +
  geom_smooth(aes(y = pred),method = "loess", se = FALSE, lty = "dashed", lwd = 1, colour = "black") +
  labs(x = "Year", y = "lnRR (effect size)",size = "Precision (1/SE)") +
  guides(fill = "none", colour = "none")
time_lag+
  theme(legend.position='bottom')+
  scale_x_continuous(breaks = seq(1970, 2023, by = 10))


## Funnel plot ----
# Funnel plot of residuals
# Quality trait was added as moderator to account for some heterogeneity
funnel_ser <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.5, mods = ~quality_broad,
                     random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
f1 <- funnel(funnel_ser, yaxis = "seinv", level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             refline = 0, legend="topleft")


## Egger's regression ----
# Using the square-root of the inverse of the effective sample size to handle nonindependence
# Quality trait was added as moderator to account for some heterogeneity
ser1$inv_effect_n<-(1/ser1$nopen)+(1/ser1$nclose)
ser1$sqrt_inv_effect_n<-sqrt(ser1$inv_effect_n)
egger1 <- rma.mv(yi=R_yi,
                 V=V.ser1.endpoints_0.5,
                 mods=~quality_aggr_7levels+sqrt_inv_effect_n, random = list(~1|studyID, ~1|year_exp, ~1|infoID),
                 data=ser1,method="REML", sparse=TRUE)
summary(egger1)
r2_ml(egger1)


## Hat values ----
# To identify outliers in the dataset
rsRES5.reml=rstandard(RES5.reml)
hatRES5.reml=hatvalues(RES5.reml)/mean(hatvalues(RES5.reml))
plot(hatRES5.reml,rsRES5.reml$resid, xlab="Hat values", ylab="lnRR residuals", yaxt = "n", xaxt="n", ylim=c(-1.2, 3.5))
abline(h=3, lty=3)
abline(v=2, lty=3)
axis(side = 2,las = 2,mgp = c(3,0.75,0))
axis(side=1, mgp=c(3,0.75,0))


# Sensitivity analyses (SA) ----
## W/o studies contributing more than 5% of the dataset (SA1) ----
SA1.ser.remove<-c(247)
ser1.SA1<-subset(ser1, !(studyID %in% SA1.ser.remove))

# We construct a new variance-covariance matrix with the subset dataset
V.ser1.SA1.endpoints_0.5 <- metaAidR::make_VCV_matrix(data = ser1.SA1, V = "R_vi", cluster = "multiple_endpoint_clusterID", obs = "infoID", type = "vcv", rho = 0.5)
is.positive.definite (V.ser1.SA1.endpoints_0.5)

# Run the null model to get the overall estimate
RES5.reml.SA1 <- rma.mv(yi = R_yi, V = V.ser1.SA1.endpoints_0.5, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1.SA1, method = "REML")
summary(RES5.reml.SA1)

RES5.reml.SA1.res <- (orchaRd::mod_results(RES5.reml.SA1, mod = "1", group="infoID")) # table of results
print(RES5.reml.SA1.res)
(exp(RES5.reml.SA1.res$mod_table$estimate) - 1)*100
(exp(RES5.reml.SA1.res$mod_table$lowerCL) - 1)*100
(exp(RES5.reml.SA1.res$mod_table$upperCL) - 1)*100


## Cook's distance (SA2) ----
# Calculate and plot Cook's distance for every effect size
cooksd <- cooks.distance(RES5.reml, progbar=TRUE, reestimate=F)
sample_size <- nrow(ser1)
plot(cooksd, pch="*", cex=2, xlab="Observation number", ylab="Cook's distance", yaxt = "n", xaxt="n")
axis(side=2,las =2,mgp = c(3,0.75, 0))
axis(side=1, mgp=c(3,0.75,0))
abline(h = 4/sample_size, col="red")

influential.1 <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
ser1.SA2 <- ser1[-influential.1, ] # create new dataset removing influential observations

# We construct a new variance-covariance matrix with the subset dataset
V.ser1.SA2.endpoints_0.5 <- metaAidR::make_VCV_matrix(data = ser1.SA2, V = "R_vi", cluster = "multiple_endpoint_clusterID", obs = "infoID", type = "vcv", rho = 0.5)
is.positive.definite (V.ser1.SA2.endpoints_0.5)

# Run the null model to get the overall estimate
RES5.reml.SA2 <- rma.mv(yi = R_yi, V = V.ser1.SA2.endpoints_0.5, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1.SA2, method = "REML")
summary(RES5.reml.SA2)

RES5.reml.SA2.res <- (orchaRd::mod_results(RES5.reml.SA2, mod = "1", group="infoID")) # table of results
print(RES5.reml.SA2.res)
(exp(RES5.reml.SA2.res$mod_table$estimate) - 1)*100
(exp(RES5.reml.SA2.res$mod_table$lowerCL) - 1)*100
(exp(RES5.reml.SA2.res$mod_table$upperCL) - 1)*100


## Vcv matrix with rho=0.8 (SA3) ----
# Here we perform a sensitivity analysis to examine the extent to which the pollination service effect is sensitive to the assumption of correlation of sampling errors among clustered effect sizes 
# We now assume a 0.8 correlation of sampling errors between clustered effect sizes  (sharing the same multiple endpoint cluster)
V.ser1.endpoints_0.8<-metaAidR::make_VCV_matrix(data = ser1, V = "R_vi", cluster = "multiple_endpoint_clusterID", obs = "infoID", type = "vcv", rho = 0.8)
is.positive.definite (V.ser1.endpoints_0.8) # testing matrix for positive definiteness

RES5.reml.SA3 <- rma.mv(yi = R_yi, V = V.ser1.endpoints_0.8, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(RES5.reml.SA3)

RES5.reml.SA3.res <- (orchaRd::mod_results(RES5.reml.SA3, mod = "1", group="infoID")) # table of results
print(RES5.reml.SA3.res)
(exp(RES5.reml.SA3.res$mod_table$estimate) - 1)*100
(exp(RES5.reml.SA3.res$mod_table$lowerCL) - 1)*100
(exp(RES5.reml.SA3.res$mod_table$upperCL) - 1)*100


## Vcv matrix with studyID as clustering variable (SA4) ----
# Clustering the effect sizes at the level of individual studies 
V.ser1.study_0.5<-metaAidR::make_VCV_matrix(data = ser1, V = "R_vi", cluster = "studyID", obs = "infoID", type = "vcv", rho = 0.5)
is.positive.definite (V.ser1.study_0.5) # testing matrix for positive definiteness

RES5.reml.SA4 <- rma.mv(yi = R_yi, V = V.ser1.study_0.5, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = ser1, method = "REML")
summary(RES5.reml.SA4)

RES5.reml.SA4.res <- (orchaRd::mod_results(RES5.reml.SA4, mod = "1", group="infoID")) # table of results
print(RES5.reml.SA4.res)
(exp(RES5.reml.SA4.res$mod_table$estimate) - 1)*100
(exp(RES5.reml.SA4.res$mod_table$lowerCL) - 1)*100
(exp(RES5.reml.SA4.res$mod_table$upperCL) - 1)*100


####################### POLLINATION DEFICIT ###############
# We repeat the analyses with the pollination deficit dataset

# Variance-covariance matrix ----
# Generating a variance-covariance matrix assuming 0.5 correlation of sampling errors between effect sizes that share a common cluster ID
V.def1.endpoints_0.5 <- metaAidR::make_VCV_matrix(data = def1, V = "R_vi", cluster = "multiple_endpoint_clusterID", obs = "infoID", type = "vcv", rho = 0.5)
is.positive.definite (V.def1.endpoints_0.5) # test matrix for positive definiteness

# Testing candidate random effects structures. Model comparisons are conducted on models fit with ML
RED1 <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, random = list(~1|studyID, ~1|infoID), data = def1, method = "ML")
AIC(RED1)

RED2 <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, random = list(~1|studyID, ~1|country, ~1|infoID), data = def1, method = "ML")
AIC(RED2)

RED3 <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, random = list(~1|studyID/year_exp, ~1|infoID), data = def1, method = "ML")
AIC(RED3)

RED4 <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, random = list(~1|country/studyID, ~1|infoID), data = def1, method = "ML")
AIC(RED4)

RED5 <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, random = list(~1|studyID, ~1|year_exp, ~1|infoID), data = def1, method = "ML")
AIC(RED5)

# The best null model is fitted with REML
RED1.reml <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(RED1.reml)

# Overall estimate and CI ----
# We calculate the estimates of the natural logarithm of the response ratio as mean percentage of quality change 
(exp(RED1.reml$b[[1]])-1)*100 # estimate

RED1.reml.res <- mod_results(RED1.reml, mod = "1", group="infoID") # extracting table of results
print(RED1.reml.res)
(exp(RED1.reml.res$mod_table$lowerCL) - 1)*100 # lower CI boundary
(exp(RED1.reml.res$mod_table$upperCL) - 1)*100 # upper CI boundary

# Testing moderators ----
# We build univariate meta-analytical models
# For each moderator we follow the same procedure, described in detail below (for the first moderator only)

## Quality trait (7 levels) ----
# We first fit the model with moderator with ML to compare it to the null model (AIC and likelihood ratio test)
# We fit the model without intercept to get estimated effect (slope) for each level of a categorical moderator
QUD1 <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~quality_aggr_7levels - 1, random = list(~1|studyID, ~1|infoID), data = def1, method = "ML")
anova(QUD1, RED1)

# We fit the model with REML to get estimates
QUD1.reml <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~quality_aggr_7levels - 1, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(QUD1.reml)

# We calculate marginal R^2 of model with moderator
r2_ml(QUD1.reml)

# We extract table of results
res_QUD1 <- mod_results(QUD1.reml, mod ="quality_aggr_7levels", group="quality_aggr_7levels")

# We now fit the same model with the intercept to calculate the heterogeneity explained by the levels of the moderator
QUD1.reml.intercept <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~quality_aggr_7levels, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(QUD1.reml.intercept)

# Orchard plot
box2<-res_QUD1$mod_table
orchard_plot(QUD1.reml, mod = "quality_aggr_7levels", xlab = "lnRR (effect size)", group="studyID", angle=0, cb=T, alpha=0.1, k=T, g=T)+
  guides(y =  guide_axis(angle = 0))+
  theme(legend.position = "none",
        axis.title = element_text(size=20),
        axis.text =element_text(size=20)) +

  annotate(geom="text", x = 4.1, y = 1.2,
           label = paste0(round((exp(box2$estimate[[4]])-1),3) * 100, 
                          "% (", round((exp(box2$lowerCL[[4]])-1),3) * 100, "%", " : ", round((exp(box2$upperCL[[4]])-1),3) * 100, "%)"))+
  annotate(geom="text", x = 3.1, y = 1.2,
           label = paste0(round((exp(box2$estimate[[3]])-1),3) * 100, 
                          "% (", round((exp(box2$lowerCL[[3]])-1),3) * 100, "%", " : ", round((exp(box2$upperCL[[3]])-1),3) * 100, "%)"))+
  annotate(geom="text", x = 2.1, y = 1.2,
           label = paste0(round((exp(box2$estimate[[2]])-1),3) * 100, 
                          "% (", round((exp(box2$lowerCL[[2]])-1),3) * 100, "%", " : ", round((exp(box2$upperCL[[2]])-1),3) * 100, "%)"))+
  annotate(geom="text", x = 1.1, y = 1.2,
           label = paste0(round((exp(box2$estimate[[1]])-1),3) * 100, 
                          "% (", round((exp(box2$lowerCL[[1]])-1),3) * 100, "%", " : ", round((exp(box2$upperCL[[1]])-1),3) * 100, "%)"))


## Quality trait (2 levels) ----
QUD2 <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~quality_broad - 1, random = list(~1|studyID, ~1|infoID), data = def1, method = "ML")
anova(QUD2, RED1)
QUD2.reml <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~quality_broad - 1, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(QUD2.reml)

r2_ml(QUD2.reml)

res_QUD2 <- mod_results (QUD2.reml, mod="quality_broad", group="quality_broad")
print(res_QUD2)
(exp(res_QUD2$mod_table$estimate)-1)*100
(exp(res_QUD2$mod_table$lowerCL)-1)*100
(exp(res_QUD2$mod_table$upperCL)-1)*100

QUD2.reml.intercept <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~quality_broad, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(QUD2.reml.intercept)


## Pollinator group ----
POD<- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~species_aggr1 - 1, random = list(~1|studyID, ~1|infoID), data = def1, method = "ML")
anova(POD, RED1)
POD.reml<- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~species_aggr1 - 1, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(POD.reml)

res_POD <- mod_results (POD.reml, mod="species_aggr1", group="species_aggr1")

POD.reml.intercept<- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~relevel(factor(species_aggr1), ref="natural"), random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(POD.reml.intercept)


## Crop type ----
CRD<- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~crop_aggr_klein - 1, random = list(~1|studyID, ~1|infoID), data = def1, method = "ML")
anova(CRD, RED1)

CRD.reml<- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~crop_aggr_klein - 1, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(CRD.reml)

res_CRD <- mod_results(CRD.reml, mod ="crop_aggr_klein", group="crop_aggr_klein")
print(res_CRD)
(exp(res_CRD$mod_table$estimate)-1)*100
(exp(res_CRD$mod_table$lowerCL)-1)*100
(exp(res_CRD$mod_table$upperCL)-1)*100

CRD.reml.intercept<- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~crop_aggr_klein, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(CRD.reml.intercept)


## Experimental scale ----
# We remove the effects sizes, for which info on experimental scale was not available (n=17)
def1.scale.subset<-subset(def1, scale!="NA")

V.def1.scale.subset.endpoints_0.5 <- metaAidR::make_VCV_matrix(data = def1.scale.subset, V = "R_vi", cluster = "multiple_endpoint_clusterID", obs = "infoID", type = "vcv", rho = 0.5)
is.positive.definite (V.def1.scale.subset.endpoints_0.5) 

# we build a new null model with the subset dataset
RED1.scale.subset <- rma.mv(yi = R_yi, V = V.def1.scale.subset.endpoints_0.5, random = list(~1|studyID, ~1|infoID), data = def1.scale.subset, method = "ML")

# We add scale as moderator
SCD<- rma.mv(yi = R_yi, V = V.def1.scale.subset.endpoints_0.5, mods = ~scale -1, random = list(~1|studyID, ~1|infoID), data = def1.scale.subset, method = "ML")
anova(SCD, RED1.scale.subset)

SCD.reml<- rma.mv(yi = R_yi, V = V.def1.scale.subset.endpoints_0.5, mods = ~scale -1, random = list(~1|studyID, ~1|infoID), data = def1.scale.subset, method = "REML")
summary(SCD.reml)

res_SCD <- mod_results (SCD.reml, mod="scale", group="scale")

SCD.reml.intercept<- rma.mv(yi = R_yi, V = V.def1.scale.subset.endpoints_0.5, mods = ~scale, random = list(~1|studyID, ~1|infoID), data = def1.scale.subset, method = "REML")
summary(SCD.reml.intercept)


## Cropping environment ----
def1$greenhouse2 <- def1$greenhouse
def1$greenhouse2[def1$greenhouse2 == "0"] <- "field"
def1$greenhouse2[def1$greenhouse2 == "1"] <- "greenhouse"

GRD<- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~greenhouse2 -1, random = list(~1|studyID, ~1|infoID), data = def1, method = "ML")
anova(GRD, RED1)

GRD.reml<-rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~greenhouse2 -1, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(GRD.reml)

res_GRD <- mod_results (GRD.reml, mod="greenhouse2", group="greenhouse2")

GRD.reml.intercept<-rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~greenhouse2, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(GRD.reml.intercept)


## Climate ----
BID<- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~biome - 1, random = list(~1|studyID, ~1|infoID), data = def1, method = "ML")
anova(BID, RED1)

BID.reml<- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~biome - 1, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(BID.reml)

res_BID <- mod_results (BID.reml, mod="biome", group="biome")

BID.reml.intercept<- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~biome, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(BID.reml.intercept)


# Publication bias ----
## Time-lag bias ----
YRD<- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~year, random = list(~1|studyID, ~1|infoID), data = def1, method = "ML")
anova(YRD, RED1)

YRD.reml<- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~year, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(YRD.reml)

# Time-lag bias figure
pred_time.def <- predict.rma(YRD.reml)
predict.rma(YRD.reml, newmods = c(1968, 2023))
time_lag.def <- def1 %>% dplyr::mutate(ymin = pred_time.def$ci.lb, ymax = pred_time.def$ci.ub,
                                       ymin2 = pred_time.def$cr.lb, ymax2 = pred_time.def$cr.ub, pred = pred_time.def$pred) %>%ggplot(aes(x = year, y = R_yi, size = sqrt(1/R_vi))) +
  geom_point(shape = 21, fill = "grey90") + 
  geom_smooth(aes(y = ymin2), method = "loess", se = FALSE, lty = "dotted",
              lwd = 0.75, colour = "#0072B2") +
  geom_smooth(aes(y = ymax2),method = "loess", se = FALSE, lty = "dotted", lwd = 0.75, colour = "#0072B2") +
  geom_smooth(aes(y = ymin), method = "loess",se = FALSE, lty = "dotted", lwd = 0.75, colour = "#D55E00") +
  geom_smooth(aes(y = ymax), method = "loess", se = FALSE,
              lty = "dotted", lwd = 0.75, colour = "#D55E00") +
  geom_smooth(aes(y = pred),method = "loess", se = FALSE, lty = "dashed", lwd = 1, colour = "black") +
  labs(x = "Year", y = "lnRR (effect size)",size = "Precision (1/SE)") +
  guides(fill = "none", colour = "none")
time_lag.def+
  theme(legend.position='bottom')+
  scale_x_continuous(breaks = seq(1980, 2023, by = 10))


## Funnel plot ----
# Funnel plot of residuals
# Quality trait was added as moderator to account for some heterogeneity
funnel_def <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.5, mods = ~quality_broad,
                     random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
f2 <- funnel(funnel_def,yaxis = "seinv", level = c(90, 95, 99),
             shade = c("white", "gray55", "gray75"),
             refline = 0, legend = "topleft")


## Egger's regression ----
# Using the square-root of the inverse of the effective sample size to handle nonindependence
# Quality trait was added as moderator to account for some heterogeneity
def1$inv_effect_n<-(1/def1$nopen)+(1/def1$nhand)
def1$sqrt_inv_effect_n<-sqrt(def1$inv_effect_n)
egger2 <- rma.mv(yi=R_yi,
                 V=V.def1.endpoints_0.5,
                 mods=~quality_aggr_7levels+sqrt_inv_effect_n, random = list(~1|studyID, ~1|infoID),
                 data=def1,method="REML", sparse=TRUE)
summary(egger2)
r2_ml(egger2)


## Hat values ----
# To identify outliers in the dataset
rsRED1.reml=rstandard(RED1.reml)
hatRED1.reml=hatvalues(RED1.reml)/mean(hatvalues(RED1.reml))
plot(hatRED1.reml,rsRED1.reml$resid, xlab="Hat values", ylab="lnRR residuals", yaxt = "n", xaxt="n", ylim=c(-1.2, 3.5))
abline(h=3, lty=3)
abline(v=2, lty=3)
axis(side = 2,las = 2,mgp = c(3,0.75, 0))
axis(side=1, mgp=c(3,0.75,0))


# Sensitivity analyses (SA) ----
## W/o studies contributing more than 5% of the dataset (SA1) ----
SA1.def.remove<-c(358,363)
def1.SA1<-subset(def1, !(studyID %in% SA1.def.remove))

# We construct a new variance-covariance matrix with the subset dataset
V.def1.SA1.endpoints_0.5 <- metaAidR::make_VCV_matrix(data = def1.SA1, V = "R_vi", cluster = "multiple_endpoint_clusterID", obs = "infoID", type = "vcv", rho = 0.5)
is.positive.definite (V.def1.SA1.endpoints_0.5)

# Run the null model to get the overall estimate
RED1.reml.SA1 <- rma.mv(yi = R_yi, V = V.def1.SA1.endpoints_0.5, random = list(~1|studyID, ~1|infoID), data = def1.SA1, method = "REML")
summary(RED1.reml.SA1)

RED1.reml.SA1.res <- (orchaRd::mod_results(RED1.reml.SA1, mod = "1", group="infoID")) 
print(RED1.reml.SA1.res)
(exp(RED1.reml.SA1$b[[1]])-1)*100
(exp(RED1.reml.SA1.res$mod_table$lowerCL) - 1)*100
(exp(RED1.reml.SA1.res$mod_table$upperCL) - 1)*100


## Cook's distance (SA2) ----
cooksd.D <- cooks.distance(RED1.reml, progbar=TRUE, reestimate=F)
sample_size.D <- nrow(def1)
plot(cooksd.D, pch="*", cex=2, xlab="Observation number", ylab="Cook's distance", yaxt = "n", xaxt="n")
axis(side = 2,las = 2,mgp = c(3,0.75, 0))
axis(side=1, mgp=c(3,0.75,0))
abline(h = 4/sample_size.D, col="red")

influential.2 <- as.numeric(names(cooksd.D)[(cooksd.D > (4/sample_size.D))])
def1.SA2 <- def1[-influential.2, ]

V.def1.SA2.endpoints_0.5 <- metaAidR::make_VCV_matrix(data = def1.SA2, V = "R_vi", cluster = "multiple_endpoint_clusterID", obs = "infoID", type = "vcv", rho = 0.5)
is.positive.definite (V.def1.SA2.endpoints_0.5)

RED1.reml.SA2 <- rma.mv(yi = R_yi, V = V.def1.SA2.endpoints_0.5, random = list(~1|studyID, ~1|infoID), data = def1.SA2, method = "REML")
summary(RED1.reml.SA2)

RED1.reml.SA2.res <- (orchaRd::mod_results(RED1.reml.SA2, mod = "1", group="infoID")) 
print(RED1.reml.SA2.res)
(exp(RED1.reml.SA2$b[[1]])-1)*100
(exp(RED1.reml.SA2.res$mod_table$lowerCL) - 1)*100
(exp(RED1.reml.SA2.res$mod_table$upperCL) - 1)*100


## Vcv matrix with rho=0.8 (SA3) ----
#Generating a variance-covariance matrix assuming 0.8 correlation between sampling errors and effect sizes sharing a common cluster ID
V.def1.endpoints_0.8 <- metaAidR::make_VCV_matrix(data = def1, V = "R_vi", cluster = "multiple_endpoint_clusterID", obs = "infoID", type = "vcv", rho = 0.8)
is.positive.definite (V.def1.endpoints_0.8) # test matrix for positive definiteness

RED1.reml.SA3 <- rma.mv(yi = R_yi, V = V.def1.endpoints_0.8, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(RED1.reml.SA3)

RED1.reml.SA3.res <- (orchaRd::mod_results(RED1.reml.SA3, mod = "1", group="infoID")) # table of results
print(RED1.reml.SA3.res)
(exp(RED1.reml.SA3.res$mod_table$estimate) - 1)*100
(exp(RED1.reml.SA3.res$mod_table$lowerCL) - 1)*100
(exp(RED1.reml.SA3.res$mod_table$upperCL) - 1)*100


## Vcv matrix with studyID as clustering variable (SA4) ----
# Clustering the effect sizes at the level of individual studies 
V.def1.study_0.5<-metaAidR::make_VCV_matrix(data = def1, V = "R_vi", cluster = "studyID", obs = "infoID", type = "vcv", rho = 0.5)
is.positive.definite (V.def1.study_0.5) # testing matrix for positive definiteness

RED1.reml.SA4 <- rma.mv(yi = R_yi, V = V.def1.study_0.5, random = list(~1|studyID, ~1|infoID), data = def1, method = "REML")
summary(RED1.reml.SA4)

RED1.reml.SA4.res <- (orchaRd::mod_results(RED1.reml.SA4, mod = "1", group="infoID")) # table of results
print(RED1.reml.SA4.res)
(exp(RED1.reml.SA4.res$mod_table$estimate) - 1)*100
(exp(RED1.reml.SA4.res$mod_table$lowerCL) - 1)*100
(exp(RED1.reml.SA4.res$mod_table$upperCL) - 1)*100


# Association plots ----
ser1$greenhouse3 <- ser1$greenhouse
ser1$greenhouse3[ser1$greenhouse3 == "0"] <- "field"
ser1$greenhouse3[ser1$greenhouse3 == "1"] <- "greenhouse"

# for figure clarity, we subset the datasets to display only fruit and vegetable crops
ser1$crop_aggr_klein_fruveg<-ser1$crop_aggr_klein
ser1$crop_aggr_klein_fruveg[ser1$crop_aggr_klein_fruveg =="oil_proteinaceous_crops"]<-NA
ser1$crop_aggr_klein_fruveg[ser1$crop_aggr_klein_fruveg =="spices_condiments"]<-NA
ser1$crop_aggr_klein_fruveg[ser1$crop_aggr_klein_fruveg =="stimulant_crops"]<-NA
ser1$crop_aggr_klein_fruveg[ser1$crop_aggr_klein_fruveg =="nut_crops"]<-NA

def1$crop_aggr_klein_fruveg<-def1$crop_aggr_klein
def1$crop_aggr_klein_fruveg[def1$crop_aggr_klein_fruveg =="oil_proteinaceous_crops"]<-NA
def1$crop_aggr_klein_fruveg[def1$crop_aggr_klein_fruveg =="spices_condiments"]<-NA
def1$crop_aggr_klein_fruveg[def1$crop_aggr_klein_fruveg =="stimulant_crops"]<-NA
def1$crop_aggr_klein_fruveg[def1$crop_aggr_klein_fruveg =="nut_crops"]<-NA


sertable<-data.frame(Quality.trait=ser1$quality_broad, Pollinator.group=ser1$species_aggr1, Crop.type=ser1$crop_aggr_klein_fruveg, Experimental.scale=ser1$scale, Cropping.environment=ser1$greenhouse3, Climate=ser1$biome)
con.table.ser<-table(sertable)
pairs(con.table.ser, upper_panel= pairs_assoc,  shade = TRUE) # small figure display edits were made separately in Inkscape software

deftable<-data.frame(Quality.trait=def1$quality_broad, Pollinator.group=def1$species_aggr1, Crop.type=def1$crop_aggr_klein_fruveg, Experimental.scale=def1$scale, Cropping.environment=def1$greenhouse2, Climate=def1$biome)
con.table.def<-table(deftable)
pairs(con.table.def, upper_panel= pairs_assoc,  shade = TRUE) # small figure display edits were made separately in Inkscape software


## Figure 3 (Forest plot) ----
ESser<-rbind(res_RES5.reml$mod_table,res_QUS2$mod_table, res_POS$mod_table, res_CRS$mod_table, res_SCS$mod_table,res_GRS$mod_table, res_BIS$mod_table)
ESdef<-rbind(RED1.reml.res$mod_table,res_QUD2$mod_table, res_POD$mod_table, res_CRD$mod_table, res_SCD$mod_table, res_GRD$mod_table, res_BID$mod_table)

ESser$group<-dplyr::recode_factor(ESser$name, Intrcpt ="a",
                                  Nutritional="b", Organoleptic="b",
                                  Apis="c", Bombus="c",Natural= "c", Other="c",
                                  Fruit_crops="g", Nut_crops="g", Oil_proteinaceous_crops="g", Spices_condiments="g", Stimulant_crops="g", Vegetable_crops="g",
                                  Branch="d", Flower="d", Plant="d",
                                  No="e", Yes="e",
                                  Subtropical="f", Temperate="f", Tropical="f")
ESdef$group<-dplyr::recode_factor(ESdef$name, Intrcpt ="a",
                                  Nutritional="b", Organoleptic="b",
                                  Apis="c", Bombus="c",Natural= "c", Other="c",
                                  Fruit_crops="g", Nut_crops="g", Oil_proteinaceous_crops="g", Spices_condiments="g", Stimulant_crops="g", Vegetable_crops="g",
                                  Branch="d", Flower="d", Plant="d",
                                  Field="e", Greenhouse="e",
                                  Subtropical="f", Temperate="f", Tropical="f")

ESser$name <- factor(ESser$name, levels = ESser$name)
ESser$name <- dplyr::recode_factor(ESser$name, Intrcpt = "(1197,153)",
                                   Nutritional = "Nutritional (341,59)", Organoleptic="Organoleptic (856,136)",
                                   Apis="Honeybee (153,34)", Bombus="Bumblebee (178,24)",Natural= "Pollinator community (597,108)", Other="Other single species (296,41)",
                                   Fruit_crops="Fruits (572,78)", Nut_crops ="Nuts (7,3)", Oil_proteinaceous_crops="Edible oil and proteinaceous (51,14)", Spices_condiments="Spices and condiments* (70,2)", Stimulant_crops="Stimulants (36,5)", Vegetable_crops="Vegetables (461,52)",
                                   Branch="Branch (133,23)", Flower="Flower (400,55)", Plant="Plant (653,75)",
                                   No="Field (797,112)", Yes="Greenhouse (400,44)",
                                   Subtropical="Subtropical (197,16)", Temperate="Temperate (548,79)", Tropical="Tropical (452,59)")

ESdef$name <- factor(ESdef$name, levels = ESdef$name)
ESdef$name <- dplyr::recode_factor(ESdef$name, Intrcpt = "(682,86)",
                                   Nutritional = "(188,38)", Organoleptic="(494,80)",
                                   Apis="(50,11)", Bombus="(93,12)",Natural= "(365,58)", Other="(174,20)",
                                   Fruit_crops="(313,41)", Nut_crops ="(12,3)", Oil_proteinaceous_crops="(15,3)", Spices_condiments="(1,1)", Stimulant_crops="(8,2)", Vegetable_crops="(333,36)",
                                   Branch="(136,17)", Flower="(360,47)", Plant="(169,21)",
                                   Field="(413,58)", Greenhouse="(269,28)",
                                   Subtropical="(67,4)", Temperate="(305,45)", Tropical="(310,38)")


mods_names <- as_labeller(c('a'="Overall",
                            'b'="Quality trait",
                            'c'="Pollinator\ngroup",
                            'g'="Crop type",
                            'd'="Experimental\nscale",
                            'e'="Cropping\nenvironment",
                            'f'="Climate"))

lowercl.ser<-ESser$lowerCL
uppercl.ser<-ESser$upperCL
lowercl.def<-ESdef$lowerCL
uppercl.def<-ESdef$upperCL
ESser$highlight <- ifelse((exp(lowercl.ser)-1)*100>0 , TRUE, FALSE)
ESdef$highlight <- ifelse((exp(lowercl.def)-1)*100>0 |(exp(uppercl.def)-1)*100<0, TRUE, FALSE)

ser.fig<-ggplot(data=ESser, aes(x=name, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=highlight),shape=20, size=1.6, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  labs(x='Moderators', y='% change with open pollination\ncompared to pollinator exclusion', title='Pollination service')+
  theme_classic()+
  scale_y_continuous(limits = c(-30,50), breaks=seq(-30,50, by=10))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_colour_manual(values = c("gray60", "black"))+
  facet_grid(group ~ ., scales = "free_y", space = "free", switch = "y", labeller = mods_names)+
  theme(legend.position='None', panel.spacing.y = unit(20, "pt"),
        strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,hjust=0,vjust = 0.5,size=20),
        strip.background = element_blank(),
        plot.title= element_text(size=22, hjust = 0, vjust = 10),
        plot.margin = margin(50, 20, 0, 0, "pt"),
        axis.text=element_text(size=16, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 20)))
ser.fig # CI for spices and condiments exceed figure limits. Mean estimate was removed in Inkscape software


def.fig<-ggplot(data=ESdef, aes(x=name, y=(exp(estimate)-1)*100, ymin=(exp(lowerCL)-1)*100, ymax=(exp(upperCL)-1)*100))+  
  geom_pointrange(aes(col=highlight),shape=20, size=1.6, position=position_dodge(width=c(0.1)))+
  coord_flip() +
  labs(x='', y='% change with hand pollination\ncompared to open pollination', title='Pollination deficit')+
  theme_classic()+
  scale_y_continuous(limits = c(-25,25), breaks=seq(-30,30, by=10))+
  geom_hline(yintercept=0, linetype = 'dashed', col = 'black')+
  scale_colour_manual(values = c("gray60", "black"))+
  facet_grid(group ~ ., scales = "free_y", space = "free")+
  theme(legend.position='None', strip.text = element_blank(), panel.spacing.y = unit(20, "pt"),
        plot.title= element_text(size=22, hjust = 0, vjust = 10),
        plot.margin = margin(50, 50, 0, 0, "pt"),
        axis.text=element_text(size=16, color="black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_text(size = 20),
        axis.title.x = element_text(margin = margin(t = 10)))
def.fig # mean estimate and CI for spices and condiments exceed figure limits

cowplot::plot_grid(ser.fig,def.fig, nrow = 1, ncol = 2, align = "h", rel_widths = c(2,1.1))

# Supplementary Fig. 4 containing all estimate points and CI was produced with same code, changing figure limits


# Software and package version ----
sessionInfo()

# sessionInfo() output

#R version 4.2.2 (2022-10-31 ucrt)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 22621)

#Matrix products: default

#locale:
#[1] LC_COLLATE=Italian_Italy.utf8  LC_CTYPE=Italian_Italy.utf8    LC_MONETARY=Italian_Italy.utf8 LC_NUMERIC=C                  
#[5] LC_TIME=Italian_Italy.utf8    

#attached base packages:
#[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#[1] ggplot2_3.4.1       metaAidR_0.0.0.9000 orchaRd_2.0         matrixcalc_1.0-6    metafor_4.2-0       numDeriv_2016.8-1.1
#[7] metadat_1.2-0       Matrix_1.5-1        vcd_1.4-11         

#loaded via a namespace (and not attached):
#[1] pkgload_1.3.2      splines_4.2.2      shiny_1.7.4        vipor_0.4.5        remotes_2.4.2      sessioninfo_1.2.2 
#[7] pillar_1.8.1       lattice_0.20-45    glue_1.6.2         digest_0.6.31      promises_1.2.0.1   colorspace_2.0-3  
#[13] sandwich_3.0-2     cowplot_1.1.1      htmltools_0.5.4    httpuv_1.6.9       pkgconfig_2.0.3    devtools_2.4.5    
#[19] purrr_1.0.1        xtable_1.8-4       mvtnorm_1.1-3      scales_1.2.1       processx_3.8.0     later_1.3.0       
#[25] emmeans_1.8.4-1    tibble_3.1.8       mgcv_1.8-41        farver_2.1.1       generics_0.1.3     usethis_2.1.6     
#[31] ellipsis_0.3.2     TH.data_1.1-1      cachem_1.0.7       pacman_0.5.1       withr_2.5.0        cli_3.6.0         
#[37] survival_3.4-0     magrittr_2.0.3     crayon_1.5.2       mime_0.12          memoise_2.0.1      estimability_1.4.1
#[43] ps_1.7.2           fs_1.5.2           fansi_1.0.3        nlme_3.1-160       MASS_7.3-58.1      beeswarm_0.4.0    
#[49] pkgbuild_1.4.0     profvis_0.3.7      tools_4.2.2        prettyunits_1.1.1  lifecycle_1.0.3    multcomp_1.4-22   
#[55] stringr_1.5.0      munsell_0.5.0      callr_3.7.3        compiler_4.2.2     rlang_1.1.0        rstudioapi_0.14   
#[61] htmlwidgets_1.6.1  miniUI_0.1.1.1     labeling_0.4.2     codetools_0.2-18   gtable_0.3.1       DBI_1.1.3         
#[67] R6_2.5.1           zoo_1.8-11         dplyr_1.0.10       fastmap_1.1.1      utf8_1.2.2         mathjaxr_1.6-0    
#[73] latex2exp_0.9.6    ggbeeswarm_0.7.2   stringi_1.7.12     Rcpp_1.0.10        vctrs_0.6.1        tidyselect_1.2.0  
#[79] urlchecker_1.0.1   coda_0.19-4        lmtest_0.9-40     

