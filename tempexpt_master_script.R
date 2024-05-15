library(plotly)
library(minpack.lm)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(plantecowrap)
library(dplyr)
source("figure_basic_functions.R")
library(hms)
library(ggpmisc)
library(psych)
library(tidyverse)
library(visreg)
par(mfrow = c(2, 2))
##LOAD DATA######

my.formula <- y ~ x 
colourgradient <- c("blue3", "blue2","blue2","blue","skyblue3", "skyblue2","skyblue1", "skyblue1", "orange", "orange",
                    "pink", "red", "red", "red2", "red3")
master <- read.csv("themeda_tempexpt_ambient_wide_190523.csv")

master$P_R <- with(master, A_mass/R_mass)
master$gsmmol <- with(master, gs * 1000)
master$Leaf_area <- with(master, Tillers * Leaf_length * Leaf_width)
master$gsmmolnight <- with(master, gs_night * 1000)
master$rain_ratio <- with(master, PWarmestQ_bio18 / PColdestQ_bio19)
master$Location <- as.factor(master$Location)
master$Location <- ordered(master$Location , levels = c("Nile_TAS", "Mornam Rd_VIC", "Sydney_NSW", "Virginia Gardens_SA" , "FUMAR Rd_QLD",
                                                          "Dalby_QLD","Forbes_NSW", "Woodstock road_QLD", "Marlborough Rd_QLD", "Mt Fox National Park_QLD",
                                                          "The Lynd Junction_QLD", "Mt Coolon_QLD", "Palm Valley_NT", "Rainbow Valley_NT", "Panawonica_WA"))

#hightemp <- read.csv("Themeda_high_temp_days_gdd_ndd.csv")
#master <- merge(master, hightemp,by="Genotype", all.x = T)
#soilvars <- read.csv("Raw_Data/Soil_vars.csv")
#master <- merge(master, soilvars,by="Location", all.x = T)
seed_sum <- read.csv("seed_summary.csv")
master <- merge(master, seed_sum,by="ID2", all.x = T)


means <- master %>% group_by(Location, Growth_Temperature, Genotype) %>%
  dplyr::summarise_at(vars(c(A, gs:Seed_mass)), mean, na.rm = TRUE)
#means_raw <- read.csv("themeda_tempexpt_means110823.csv")




#master2 <- subset(master, Genotype != "55834")
#means2 <- subset(means, Genotype != "55834")
#percentagechange2 <- subset(percentagechange, Genotype != "55834")
meanshot <- subset(means, Growth_Temperature == "HT")
meanscold <- subset(means, Growth_Temperature == "LT")
masterhot <- subset(master, Growth_Temperature == "HT")
mastercold <- subset(master, Growth_Temperature == "LT")
#woodstock <- subset (master, Genotype == "55834")
#write.csv(woodstock, "woodstock_road.csv")


############Final Graphs GG REPLEL####
photosumamb <- summarySE(data=master, measurevar="A", na.rm=TRUE, groupvars=c("Location", "MTWQ_bio10", "Growth_Temperature"))
png("kangaroo_Asat_ambient.png", units = "mm", width = 500, height = 300, res = 900)

(photog <- ggplot(photosumamb, aes(x = MTWQ_bio10, y = A, colour= Growth_Temperature))+ 
    geom_errorbar(aes(ymin=A-se, ymax=A+se),position=position_dodge(.4), width=.2) + ylim(0, 30) +
    ylab(expression(paste(A[sat]~(mu*mol~CO[2]~m^-2~s^-1))))+
    geom_point(size=3, position=position_dodge(.4))+  scale_shape_manual(values=c(19,1)) +
    xlab("Mean Temp Warmest Quarter") + theme_classic() + ggtitle("Photosynthesis at Growth Temperature") + 
    scale_color_manual(values=c("red","blue")))+ theme(axis.title = element_text(size = 20)) +
  geom_text_repel(aes(color = Growth_Temperature, label = Location), size = 2, force = 10) 
dev.off()


gssumamb <- summarySE(data=master, measurevar="gs", na.rm=TRUE, groupvars=c("Location", "MTWQ_bio10", "Growth_Temperature"))
png("kangaroo_gs_ambient.png", units = "mm", width = 500, height = 300, res = 900)

ggplot(gssumamb, aes(x = MTWQ_bio10, y = gs, colour= Growth_Temperature))+ 
  geom_errorbar(aes(ymin=gs-se, ymax=gs+se),position=position_dodge(.4), width=.2) + ylim(0, 0.3) +
  ylab(expression(paste(g[sw]~(mol~H[2]~O~m^-2~s^-1))))+
  geom_point(size=3, position=position_dodge(.4))+  scale_shape_manual(values=c(19,1)) +
  xlab("Mean Temp Warmest Quarter") + theme_classic() + ggtitle("Stomatal Conductance at Growth Temperature") + 
  scale_color_manual(values=c("red","blue"))+theme(axis.title = element_text(size = 20)) +
  geom_text_repel(aes(color = Growth_Temperature, label = Location), size = 2, force = 10) 
dev.off()


cicasumamb <- summarySE(data=master, measurevar="Ci_Ca", na.rm=TRUE, groupvars=c("Location", "MTWQ_bio10", "Growth_Temperature"))
png("kangaroo_CiCa_ambient.png", units = "mm", width = 500, height = 300, res = 900)

(photog <- ggplot(cicasumamb, aes(x = MTWQ_bio10, y = Ci_Ca, colour= Growth_Temperature))+ 
    geom_errorbar(aes(ymin=Ci_Ca-se, ymax=Ci_Ca+se),position=position_dodge(.4), width=.2) + 
    ylab("Ci:Ca")+ylim(0, 0.8) +
    geom_point(size=3, position=position_dodge(.4))+  scale_shape_manual(values=c(19,1)) +
    xlab("Mean Temp Warmest Quarter") + theme_classic() + ggtitle("Ci:Ca") + 
    scale_color_manual(values=c("red","blue")))+ theme(axis.title = element_text(size = 20)) +
  geom_text_repel(aes(color = Growth_Temperature, label = Location), size = 2, force = 10) 

dev.off()

png("kangaroo_A_gs_ambient.png", units = "mm", width = 500, height = 300, res = 900)
wuesum <- summarySE(data=master, measurevar="A_gs", na.rm=TRUE, groupvars=c("Location", "MTWQ_bio10", "Growth_Temperature"))
(wueg <- ggplot(wuesum, aes(x = MTWQ_bio10, y = A_gs, colour= Growth_Temperature))+ 
    geom_errorbar(aes(ymin=A_gs-se, ymax=A_gs+se), position=position_dodge(.4), width=.2) + ylim(0, 200) + scale_color_manual(values=c("blue","red"))+ 
    ylab(expression(paste(A[sat]~"/"~g[s]~(mu*mol~CO[2]~"/"~mol~H[2]~O~m^-2~s^-1))))+theme(axis.title = element_text(size = 20)) +
    geom_point(size=3, position=position_dodge(.4))+ xlab("Mean Temp Warmest Quarter") + theme_classic() + ggtitle("Water Use Efficiency"))+
  geom_text_repel(aes(color = Growth_Temperature, label = Location), size = 2, force = 10)
dev.off()

respsumamb <- summarySE(data=master, measurevar="Rdark", na.rm=TRUE, groupvars=c("Location", "TWarmestM_bio5", "MTWQ_bio10","Growth_Temperature"))

png("kangaroo_Rdark_ambient.png", units = "mm", width = 500, height = 300, res = 900)
ggplot(respsumamb, aes(x = MTWQ_bio10, y = Rdark, colour = Growth_Temperature))+ 
  geom_errorbar(aes(ymin=Rdark-se, ymax=Rdark+se), position=position_dodge(.6), width=.2)  +ylim(0,3) +  
  ylab(expression(paste(R[dark]~(mu*mol~CO[2]~m^-2~s^-1))))+scale_shape_manual(values=c(19,1)) +
  geom_point(size=3, position=position_dodge(.6))+ xlab("Mean Temperature of the Warmest Quarter") + theme_classic() + 
  ggtitle("Respiration at Growth Temperature")  + scale_color_manual(values=c("red","blue"))+theme(axis.title = element_text(size = 20)) +
  geom_text_repel(aes(color = Growth_Temperature, label = Location), size = 2, force = 10) 
dev.off()


respmassum <- summarySE(data=master, measurevar="R_mass", na.rm=TRUE, groupvars=c("Location", "TWarmestM_bio5", "MTWQ_bio10","Growth_Temperature"))

png("kangaroo_Rdark_mass_ambient.png", units = "mm", width = 500, height = 300, res = 900)
ggplot(respmassum, aes(x = MTWQ_bio10, y = R_mass, colour = Growth_Temperature))+ 
  geom_errorbar(aes(ymin=R_mass-se, ymax=R_mass+se), position=position_dodge(.6), width=.2)  +ylim(0,0.08) +  
  ylab(expression(paste(R[mass]~(mu*mol~CO[2]~g^-1~s^-1))))+scale_shape_manual(values=c(19,1)) +
  geom_point(size=3, position=position_dodge(.6))+ xlab("Mean Temperature of the Warmest Quarter") + theme_classic() + 
  ggtitle("Respiration at Growth Temperature")  + scale_color_manual(values=c("red","blue"))+theme(axis.title = element_text(size = 20)) +
  geom_text_repel(aes(color = Growth_Temperature, label = Location), size = 2, force = 10) 
dev.off()




RPsumamb <- summarySE(data=master, measurevar="R_P", na.rm=TRUE, groupvars=c("Site", "TWarmestM_bio5", "MTWQ_bio10","Growth_Temperature"))

png("kangaroo_Respiration_photosyntheis_ratio_ambient.png", units = "mm", width = 500, height = 300, res = 900)
ggplot(RPsumamb, aes(x = MTWQ_bio10, y = R_P, colour= Growth_Temperature))+ 
  geom_errorbar(aes(ymin=R_P-se, ymax=R_P+se), position=position_dodge(.2), width=.1)  +ylim(0,25) + scale_color_manual(values=c("red","blue"))+ 
  ylab(expression(paste(R[mass]~":"~A[mass]~"(%)")))+scale_shape_manual(values=c(19,1)) +
  geom_point(size=3, position=position_dodge(.2))+ xlab("Mean Temperature of the Warmest Quarter") + theme_classic() + 
  ggtitle("Ratio of Respiration to Photosynthesis")+ theme(axis.title = element_text(size = 20)) +
  geom_text_repel(aes(color = Growth_Temperature, label = Site), size = 2, force = 10) 
dev.off()

Nareasum <- summarySE(data=master, measurevar="N_area", na.rm=TRUE, groupvars=c("MAT_bio1", "MTWQ_bio10","Location", "Growth_Temperature"))
png("kangaroo_N_area.png", units = "mm", width = 500, height = 300, res = 900)
ggplot(Nareasum, aes(x = MTWQ_bio10, y = N_area, colour = Growth_Temperature))+ 
  geom_errorbar(aes(ymin=N_area-se, ymax=N_area+se),position=position_dodge(.4), width=.2) + ylim(0.5, 1.25) +
  ylab(expression(paste(N[area]~(g~m^2))))+
  geom_point(size=3, position=position_dodge(.4))+  scale_shape_manual(values=c(19,1)) +
  geom_text_repel(aes(color = Growth_Temperature, label = Location), size = 2, force = 10)+
  xlab("Mean Temp Warmest Quarter") + theme_classic() + theme(axis.title = element_text(size = 20)) +
  ggtitle("Leaf Nitrogen") + scale_color_manual(values=c("red","blue"))
dev.off()


lmasum <- summarySE(data=master, measurevar="LMA", na.rm=TRUE, groupvars=c("MAT_bio1", "MTWQ_bio10","Location", "Growth_Temperature"))
lmasumfit=lm(LMA~MAT_bio1,master)
summary(lmasumfit)

png("kangaroo_LMA_ambient.png", units = "mm", width = 500, height = 300, res = 900)
ggplot(lmasum, aes(x = MTWQ_bio10, y = LMA, colour = Growth_Temperature))+ 
  geom_errorbar(aes(ymin=LMA-se, ymax=LMA+se),position=position_dodge(.4), width=.2) + 
  ylab("LMA")+ ylim(20, 60) +
  geom_point(size=3, position=position_dodge(.4))+  scale_shape_manual(values=c(19,1)) +
  geom_text_repel(aes(color = Growth_Temperature, label = Location), size = 2, force = 10)+
  xlab("Mean Temp Warmest Quarter") + theme_classic() + theme(axis.title = element_text(size = 20)) +
  ggtitle("LMA") + scale_color_manual(values=c("red","blue"))
dev.off()


####PNUE###
pnuesum <- summarySE(data=master, measurevar="PNUE", na.rm=TRUE, groupvars=c("Location", "TWarmestM_bio5", "MTWQ_bio10", "Growth_Temperature"))

png("kangaroo_PNUE_ambient.png", units = "mm", width = 500, height = 300, res = 900)
ggplot(pnuesum, aes(x = MTWQ_bio10, y = PNUE, colour = Growth_Temperature))+ 
  geom_errorbar(aes(ymin=PNUE-se, ymax=PNUE+se),position=position_dodge(.4), width=.2) + ylim(0, 35) +
  ylab(expression(paste(PNUE~(mu*mol~CO[2]~g^-1~s^-1))))+
  geom_point(size=3, position=position_dodge(.4))+  scale_shape_manual(values=c(19,1)) +
  xlab("Mean Temp Warmest Quarter") + theme_classic() + ggtitle("Photosynthetic Nitrogen Use Efficiency") + 
  scale_color_manual(values=c("red","blue"))+ theme(axis.title = element_text(size = 20)) +
  geom_text_repel(aes(color = Growth_Temperature, label = Location), size = 2, force = 10)


dev.off()

#######CARBON#
carbsum <- summarySE(data=master, measurevar="Carbon", na.rm=TRUE, groupvars=c("Location", "MTWQ_bio10", "Growth_Temperature"))


(carbong <- ggplot(carbsum, aes(x = MTWQ_bio10, y = Carbon, colour= Growth_Temperature))+ 
    geom_errorbar(aes(ymin=Carbon-se, ymax=Carbon+se),position=position_dodge(.4), width=.2) + ylim(44, 48) +
    ylab(expression(paste(Carbon[sat]~(mu*mol~CO[2]~m^-2~s^-1))))+
    geom_point(size=3, position=position_dodge(.4))+  scale_shape_manual(values=c(19,1)) +
    xlab("Mean Temp Warmest Quarter") + theme_classic() + ggtitle("Photosynthesis at Growth Temperature") + 
    scale_color_manual(values=c("red","blue")))+ theme(axis.title = element_text(size = 20)) +
  geom_text_repel(aes(color = Growth_Temperature, label = Location), size = 2, force = 10) 





##Linear and quadratic regressions#####
plot_linear <- function(data, x_var, y_var) {
  data %>%
    ggplot(aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var), color = Growth_Temperature)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_classic() + theme(legend.position = "none")+ 
    theme(axis.title = element_text(size = 15)) +theme(axis.text=element_text(size=15))+
    stat_poly_eq(
      formula = y ~ x,
      aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")),
      parse = TRUE,
      label.x.npc = "right",
      vstep = 0.05
    )
}
plot_linear_repel <- function(data, x_var, y_var) {
  data %>%
    ggplot(aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var), color = Growth_Temperature)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic() + 
    geom_text_repel(aes(color = Growth_Temperature, label = Location), size = 4, force = 10) +
    stat_poly_eq(
      formula = y ~ x,
      aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")),
      parse = TRUE,
      label.x.npc = "middle",
      vstep = 0.05
    )
}


plot_quad <- function(data, x_var, y_var) {
  data %>%
    ggplot(aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var), color = Growth_Temperature)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = TRUE) +
    theme_classic() +
    theme(legend.position = "none") +
    theme(axis.title = element_text(size = 15)) +
    theme(axis.text = element_text(size = 15)) +
    stat_poly_eq(
      formula = y ~ poly(x, 2, raw = TRUE),
      aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")),
      parse = TRUE,
      label.x.npc = "right",
      vstep = 0.05
    )
}







###Correlation Matrix####


data1 <- meanscold %>%  ungroup()%>% select(R_mass, LMA, Nitrogen, Flowering_time_weeks,Clay.Content, Soil_Nitrogen,Organic.Carbon, PH, Phosphorus,Sand.Content,Silt.Content,Avaliable.Water.Content)

data2 <- meanscold %>%  ungroup()%>% select(PNUE, PNUE2,MAT_bio1, MTWQ_bio10, MAP_bio12, Moisture_Index, PWarmestQ_bio18,isothermality_bio3,
                                           MeanAnnTemp, H38Days, H10_days, H14_days)

data1 <- mastercold %>%  ungroup()%>% select(gs, A_gs, A_mass, R_mass, LMA, Nitrogen, Flowering_time_weeks, Stomatal_density, 
                                         Leaf_length, Tillers)





png("trait-soil relationship HT.png", units = "mm", width = 500, height = 250, res = 600)
pairs.panels(data1,
             smooth = T,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = F,     # If TRUE, adds density plots and histograms
             ellipses = F,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = T,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = T,         # If TRUE, reports correlations
             cex.cor = 1,
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = T,       # If TRUE, adds significance level with stars
             #labels = c(expression(phi~PS2), "MAP", "Moisture_Index", "MAT", "MTWQ"),
             ci = T)

dev.off()




masterhot <- subset(master, Growth_Temperature == "HT")
mastercold <- subset(master, Growth_Temperature == "LT")
data <- mastercold %>% select(gs_night, MAT_bio1, MTWQ_bio10, MAP_bio12, Moisture_Index, PWarmestQ_bio18)

data <- mastercold %>% select(A, A_mass, gs, A_gs, Ci_Ca, Nitrogen, N_area, PNUE, R_mass, R_P, Flowering_time_weeks, 
                             Tillers,Leaf_length, Leaf_width, Leaf_width_new, LDMC, LMA, Stomatal_density)

data <- mastercold %>% select(A, Rdark, Tillers,Leaf_length, Leaf_width, Leaf_width_new, LDMC, LMA, Stomatal_density, Flowering_time_weeks,
                             MeanAnnTemp, H38Days, H10_days, H14_days, Precentile95, H14_days)






#NIGHT TIME CONDUCTANCE####
gsnightsum <- summarySE(data=master, measurevar="gs_night", na.rm=TRUE, groupvars=c("Location", "gs", "Growth_Temperature"))
ggplot(gsnightsum, aes(x = gs, y = gs_night, colour = Growth_Temperature))+ 
  geom_errorbar(aes(ymin=gs_night-se, ymax=gs_night+se),position=position_dodge(.4), width=.2) + 
  ylab(expression(paste(A[sat]~(mu*mol~CO[2]~m^-2~s^-1))))+
  geom_point(size=3, position=position_dodge(.4))+  scale_shape_manual(values=c(19,1)) +
  xlab("Mean Growth_Temperature of the Warmest Quarter") + theme_classic() + ggtitle("Photosynthesis") + 
  scale_color_manual(values=c("blue","red","blue","red"))



(gsnight<- ggplot(means, aes(x = gsmmol, y = gsmmolnight, colour = Growth_Temperature))+ 
    xlab(expression(paste(g[s]~(m*mol~H[2]~O~m^-2~s^-1))))+ ylab(expression(paste(g[s~night]~(m*mol~H[2]~O~m^-2~s^-1))))+
  geom_point(size=3)+ theme_classic() + ggtitle("Ratio of gs day:night")+ 
    theme(axis.title = element_text(size = 30)) +theme(axis.text=element_text(size=30)) + 
    geom_abline()+ lims(x = c(0,200), y = c(0,200)))


(enight <- ggplot(means, aes(x = E, y = gs_night, colour =  Growth_Temperature))+ 
  geom_point(size=3)+ theme_classic() + ggtitle("Ratio of Transpiration day:night")+ geom_abline()+ lims(x = c(0,3.5), y = c(0,3.5)))


png("figs/ratio of gs day to night mmol.png", units = "mm", width = 500, height = 300, res = 500)
#ggarrange(gsnight,enight, common.legend = TRUE, legend = "right")
gsnight
dev.off()


##AMASS & MTWQ LM#####
png("kangaroo_AmassXmtwq_lm.png", units = "mm", width = 500, height = 250, res = 900)
ggplot(means, aes(x = MTWQ_bio10, y = A, color = Growth_Temperature)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)+  theme_classic()+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,label.x.npc = "middle",vstep = 0.05) +
  ylab(expression(paste(A[mass]~(mu*mol~CO[2]~g^-1~s^-1))))+ xlab(expression(paste(Mean~summer~temperature~"("*degree*C*")")))+
  theme(axis.title = element_text(size = 20),plot.title = element_text(size = 25)) + 
  geom_text_repel(aes(label = Location), size = 4, force = 10)

dev.off()

###FLOWERING TIME X PWARMQ#####



png("kangaroo_flowerXPwarmQ_lm.png", units = "mm", width = 350, height = 250, res = 900)
ggplot(master, aes(x = MAP_bio12, y = Flowering_time_weeks, color = Growth_Temperature)) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = FALSE)+  theme_bw()+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,label.x.npc = "middle",cex= 9,vstep = 0.05) +
  ylab("Flowering Time (Weeks)")+ xlab(expression(paste(Summer~rainfall~"(mm)")))+
  theme(axis.title = element_text(size = 30)) +theme(axis.text=element_text(size=30))
dev.off()

###SEED WEIGHT####
seedw <- read.csv("Raw_data/seed_mass_raw.csv")
seedw <- subset(seedw, Growth_Temperature != "Pilot")
seedsum <- summarySE(data=seedw, measurevar="Seed_mass", na.rm=TRUE, groupvars=c("ID"))
#write.csv(seedsum, "seed_summary.csv")

climate <- read.csv("Themeda_populations_climate_master.csv")
seed<- merge(seedsum, climate,by="Genotype", all.x = T)

seedlt<- subset(seed, Growth_Temperature == "LT")
seedht<- subset(seed, Growth_Temperature == "HT")

seed %>% subset(Growth_Temperature == "LT") %>% 
  select(Seed_mass, MAT_bio1, MTWQ_bio10, Moisture_Index, MAP_bio12, TempRange_bio2, MTCQ_bio11, PWettestQ_bio16, PDriestQ_bio17,
         PWarmestQ_bio18, PColdestQ_bio19, isothermality_bio3, TempSeasonality_bio4, TempRange_bio7) %>% 
  pairs.panels(smooth = T, scale = FALSE, density = F, ellipses = F,method = "pearson",
               pch = 21, lm = T, cor = T, cex.cor = 6, jiggle = FALSE,factor = 2,hist.col = 4,       
               stars = T,ci = T) 

plot_linear_repel(seed, "PWarmestQ_bio18", "Seed_mass")
plot_linear(means, "Nitrogen", "gs")



png("kangaroo_seedmassXPwarmQ_lm.png", units = "mm", width = 350, height = 250, res = 900)
ggplot(seed, aes(x = latitude, y = Seed_mass, color = Growth_Temperature)) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = FALSE)+  theme_bw()+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,label.x.npc = "middle",cex = 9, vstep = 0.05) +
  ylab("Seed mass (mg)")+ xlab(expression(paste(Summer~rainfall~"(mm)")))+
  theme(axis.title = element_text(size = 30)) +theme(axis.text=element_text(size=30))

dev.off()


ggplot(means, aes(x = Leaf_length, y = A)) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = FALSE)+  theme_bw()+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,label.x.npc = "middle",cex = 9, vstep = 0.05) +
  ylab("Seed mass (mg)")+ xlab(expression(paste(Summer~rainfall~"(mm)")))+
  theme(axis.title = element_text(size = 30)) +theme(axis.text=element_text(size=30))



#####LMA isothermality####

png("kangaroo_LMAxIso_lm.png", units = "mm", width = 500, height = 250, res = 900)
ggplot(means, aes(x = isothermality_bio3, y = LMA, color = Growth_Temperature)) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = FALSE)+  theme_bw()+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,label.x.npc = "middle",cex= 9,vstep = 0.05) +
  ylab(expression(paste(LMA~(g~m^-2))))+ xlab("Isothermality (%)")+
  theme(axis.title = element_text(size = 25)) +theme(axis.text=element_text(size=30))
dev.off()




##Asat & MTWQ LM#####
png("kangaroo_AXmtwq_lm.png", units = "mm", width = 300, height = 250, res = 900)
ggplot(means, aes(x = MTWQ_bio10, y = A, color = Growth_Temperature)) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = FALSE)+  theme_classic()+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,label.x.npc = "middle",cex = 9,vstep = 0.05) +
  ylab(expression(paste(A[sat]~(mu*mol~CO[2]~m^-2~s^-1))))+ xlab(expression(paste(Mean~summer~temperature~"("*degree*C*")")))+
  theme(axis.title = element_text(size = 20),plot.title = element_text(size = 35))+theme(axis.text=element_text(size=30))

dev.off()


##Rdark & MTWQ LM#####
png("kangaroo_RdarkXmtwq_lm.png", units = "mm", width = 300, height = 250, res = 900)
ggplot(means, aes(x = MTWQ_bio10, y = Rdark, color = Growth_Temperature)) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = FALSE)+  theme_classic()+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,label.x.npc = "middle",cex = 9,vstep = 0.05) +
  ylab(expression(paste(R[dark]~(mu*mol~CO[2]~m^-2~s^-1))))+ xlab(expression(paste(Mean~summer~temperature~"("*degree*C*")")))+
  theme(axis.title = element_text(size = 20),plot.title = element_text(size = 35))+theme(axis.text=element_text(size=30))

dev.off()




##R_P & MTWQ LM#####
png("kangaroo_AXmtwq_lm.png", units = "mm", width = 300, height = 250, res = 900)
ggplot(means, aes(x = MTWQ_bio10, y = A, color = Growth_Temperature)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)+  theme_classic()+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,label.x.npc = "middle",cex = 9,vstep = 0.05) +
  ylab(expression(paste(A[sat]~(mu*mol~CO[2]~m^-2~s^-1))))+ xlab(expression(paste(Mean~summer~temperature~"("*degree*C*")")))+
  theme(axis.title = element_text(size = 20),plot.title = element_text(size = 35))+theme(axis.text=element_text(size=30))

dev.off()


png("kangaroo_AXmtwq_lm.png", units = "mm", width = 300, height = 250, res = 900)
ggplot(means, aes(x = MTWQ_bio10, y = R_P, color = Growth_Temperature)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)+  theme_classic()+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,label.x.npc = "middle",cex = 9,vstep = 0.05) +
  ylab(expression(paste(A[sat]~(mu*mol~CO[2]~m^-2~s^-1))))+ xlab(expression(paste(Mean~summer~temperature~"("*degree*C*")")))+
  theme(axis.title = element_text(size = 20),plot.title = element_text(size = 35))+theme(axis.text=element_text(size=30))

dev.off()

##PARTIAL RESIDUALS PLOTS#####

mod1 <- lm(LMA ~ TempRange_bio2 + TempRange_bio7, data=mastercold)

visreg(mod1, "TempRange_bio2", xlab = "Bio 2- Diurnal temperature range", main = "Cold grown plants")
visreg(mod1, "TempRange_bio7", xlab = "Bio 7- Annual temperature range", main = "Cold grown plants")
anova(mod1)


mod1 <- lm(LMA ~ TempRange_bio2 + TempRange_bio7, data=masterhot)

visreg(mod1, "TempRange_bio2", xlab = "Bio 2- Diurnal temperature range", main = "Hot grown plants")
visreg(mod1, "TempRange_bio7", xlab = "Bio 7- Annual temperature range", main = "Hot grown plants")

AIC(mod1)

plot_linear(master, "TempRange_bio7", "LMA")
mod1 <- lm(MAT_bio1 ~ Leaf_length, data=mastercold)Avaliable.Water.Content
summary(mod1)

mod1 <- lm(Flowering_time_weeks ~ MAP_bio12 + MAT_bio1, data=master)

visreg(mod1, "MAP_bio12", xlab = "Summer Rainfall", main = "Cold grown plants")
visreg(mod1, "MAT_bio1", xlab = "Summer Temperature", main = "Cold grown plants")
anova(mod1)


mod1 <- lm(Flowering_time_weeks ~ PWarmestQ_bio18 + MTWQ_bio10, data=masterhot)

visreg(mod1, "PWarmestQ_bio18", xlab = "Summer Rainfall", main = "Hot grown plants")
visreg(mod1, "MTWQ_bio10", xlab = "Summer Temperature", main = "Hot grown plants")

###Visreg with P values

mod1 <- lm(Flowering_time_weeks ~ MAP_bio12 + MAT_bio1, data=masterhot)
modsum <- summary(mod1)

pvals <- signif(modsum$coefficients[, "Pr(>|t|)"])
pvals_sig <- ifelse(pvals < 0.001, "<0.001", round(pvals, 3))
pvals2 <- paste0("P value = ",pvals_sig)
names(pvals2) <- names(pvals)
pval_MAP_bio12 <- pvals2["MAP_bio12"]
pval_MAT_bio1 <- pvals2["MAT_bio1"]

visreg(mod1, "MAP_bio12", xlab = "MAP", main = "Hot grown plants")
title(sub = paste(pval_MAP_bio12))
visreg(mod1, "MAT_bio1", xlab = "MAT", main = "Hot grown plants")
title(sub = paste(pval_MAT_bio1))

mod1 <- lm(Flowering_time_weeks ~ MAP_bio12 + MAT_bio1, data=mastercold)
modsum <- summary(mod1)
pvals <- signif(modsum$coefficients[, "Pr(>|t|)"])
pvals_sig <- ifelse(pvals < 0.001, "<0.001", round(pvals, 3))
pvals2 <- paste0("P value = ",pvals_sig)
names(pvals2) <- names(pvals)
pval_MAP_bio12 <- pvals2["MAP_bio12"]
pval_MAT_bio1 <- pvals2["MAT_bio1"]

visreg(mod1, "MAP_bio12", xlab = "MAP", main = "Cold grown plants")
title(sub = paste(pval_MAP_bio12))
visreg(mod1, "MAT_bio1", xlab = "MAT", main = "Cold grown plants")
title(sub = paste(pval_MAT_bio1))
anova(mod1)




###PLOT loess function#####
plot_loess <- function(data, x_var, y_var) {
  data %>%
    ggplot(aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var), color = Growth_Temperature)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    theme_bw() +
    stat_poly_eq(
      formula = y ~ x,
      aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*`,`~")),
      parse = TRUE,
      label.x.npc = "right",
      vstep = 0.05
    )
}

# Usage example:
plot_loess(means, "Leaf_width_new", "A_gs")

ggplot(means, aes(x = isothermality_bio3, y = N_area, color = Growth_Temperature)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)+  theme_bw()+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,label.x.npc = "right",vstep = 0.05) 



##PARTIAL RESIDUALS PLOTS#####
library(visreg)

mod1 <- lm(log10(Flowering_time_weeks) ~ PWarmestQ_bio18, data=meanscold)
mod1 <- lm(Flowering_time_weeks ~ PWarmestQ_bio18 + MTWQ_bio10, data=meanscold)
anova(mod1)

visreg(mod1, "TempRange_bio2", xlab = "Bio 2- Diurnal temperature range", main = "Cold grown plants")
visreg(mod1, "TempRange_bio7", xlab = "Bio 7- Annual temperature range", main = "Cold grown plants")
anova(mod1)


hist(mod1$residuals)

# Q-Q plot of residuals
qqnorm(mod1$residuals)
qqline(mod1$residuals)
shapiro.test(mod1$residuals)

# Breusch-Pagan test for heteroscedasticity
library(lmtest)
bptest(mod1)





###PLOT ALL VARIABLE IN ONE ROW####
ggplot(means, aes(x = gs, y = A, color = Growth_Temperature)) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = FALSE)+  theme_classic()+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,label.x.npc = "middle",cex = 9,vstep = 0.05) +
  ylab(expression(paste(A[sat]~(mu*mol~CO[2]~m^-2~s^-1))))+ xlab(expression(paste(Mean~summer~temperature~"("*degree*C*")")))+
  theme(axis.title = element_text(size = 20),plot.title = element_text(size = 35))+theme(axis.text=element_text(size=30))


astg<- plot_linear(master, "MAT_bio1", "A")
rmtg<- plot_linear(master, "MAT_bio1", "R_mass")
gstg<- plot_linear(master, "MAT_bio1", "gs")
agstg<- plot_linear(master, "MAT_bio1", "A_gs")
prtg<- plot_linear(master, "MAT_bio1", "P_R")
pnuetg<- plot_linear(master, "MAT_bio1", "PNUE")

aspg<- plot_linear(master, "MAP_bio12", "A")
rmpg<- plot_linear(master, "MAP_bio12", "R_mass")
gspg<- plot_linear(master, "MAP_bio12", "gs")
agspg<- plot_linear(master, "MAP_bio12", "A_gs")
prpg<- plot_linear(master, "MAP_bio12", "P_R")
pnuepg<- plot_linear(master, "MAP_bio12", "PNUE")

ggarrange(astg,rmtg,gstg,agstg,prtg,pnuetg,
          aspg,rmpg,gspg,agspg,prpg,pnuepg, 
          ncol = 6,
          nrow = 2,common.legend = TRUE, legend = "right")

MAT_bio1, MTWQ_bio10, MAP_bio12, PWarmestQ_bio18,TempRange_bio7)


(,A, gs, PNUE, P_R, A_gs, A_mass, R_mass, LMA, Nitrogen,N_area, Flowering_time_weeks, Stomatal_density, 
  Leaf_length, Tillers,  Leaf_width)


#Plots paired T tests--------------

means <- themeda_scaled %>%
  group_by(location, trt, MT) %>%
  dplyr::summarise(A = mean(A),
                   se = sd(A) / sqrt(n()))

(rawg<- ggplot(means, aes(x=trt, y=A,  colour = location, group= location)) + 
    geom_point() + geom_line()) 
ggplotly(rawg)


t_test_result <- t.test(A ~ trt, data = means)

# Print the result
print(t_test_result)

