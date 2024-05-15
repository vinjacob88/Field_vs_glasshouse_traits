setwd("C:/Users/30062057/OneDrive - Western Sydney University/e.sumner WSU/Field Themeda project/")

#load libraries----
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(stringr)



#gas ex data----
df<-read_excel("data/gas_ex/Themeda-gas_ex_all_cleaned.xlsx")
df <- df %>%
  rename(
    rep = ID           # Rename 'ID' to 'rep'
  )

Asat<-df%>%filter(A>0) #subset photosynthesis

ci<-Asat%>%filter(Ci>0) #0 with negative ci
gsw<-Asat%>%filter(gsw>0.04) #reduces to 142
vpd<-Asat%>%filter(VPDleaf<4.5) #reduces to 134

Asat_clean<-Asat%>%filter(Ci>0, gsw>0.04, VPDleaf<4.5) #filtering out bad rows

#take means for each species rep
Asat_means <- Asat_clean %>%
  group_by(site, rep) %>%
  summarise(
    mean_A = mean(A, na.rm = TRUE),
    mean_Ca = mean(Ca, na.rm = TRUE),
    mean_Ci = mean(Ci, na.rm = TRUE),
    mean_gsw = mean(gsw, na.rm = TRUE),
    mean_RHcham = mean(RHcham, na.rm = TRUE),
    mean_Tleaf = mean(Tleaf, na.rm = TRUE),
    mean_VPDleaf = mean(VPDleaf, na.rm = TRUE),
    .groups = 'drop'  # Drop the grouping structure after summarising
  )


Rdark<-df%>%filter(A<0) #subset respiration
Rd_means<- Rdark%>%
  group_by(site, rep) %>%
  summarise(
    mean_Rd = mean(A, na.rm=TRUE),
    .groups = 'drop'  # Drop the grouping structure after summarising
  )

#merge A and Rd dfs
gas_ex_means<-left_join(Asat_means, Rd_means, by= c("site", "rep"))
gas_ex_means<-gas_ex_means%>%dplyr::filter(site!="Hornsby_Heights_NSW")%>%dplyr::filter(mean_A>3)
write.csv(gas_ex_means, "Analysis/R outputs - dataframes/gas_ex_means.csv")
gas_ex_means<-read.csv("Analysis/R outputs - dataframes/gas_ex_means.csv")

#leaf+plant data----
leaf<-read_excel("data/themeda_leaf_data.xlsx", sheet = 1)

leaf_means <- leaf %>%
  group_by(site, rep) %>%
  summarise(
    leaf_width_mm = mean(width_mm, na.rm = TRUE),
    leaf_length_cm = mean(length_cm, na.rm = TRUE),
    .groups = 'drop'  # Drop the grouping structure after summarising
  )


plant<-read_excel("data/themeda_leaf_data.xlsx", sheet = 2)
plant<-plant%>%mutate(tussock_width_av_cm =(tussock_width_a_cm+tussock_width_b_cm)/2)
plant<-plant%>%mutate(plant_area_cm_sq = pi * (tussock_width_av_cm/2)^2)

#merge leaf and plant dfs
merge<-left_join(plant,leaf_means, by = c("site", "rep"))
merge_2<-left_join(merge,gas_ex_means, by = c("site", "rep"))


reduced_df<-merge_2%>%dplyr::select(-tussock_width_a_cm, -tussock_width_b_cm)

#new variables------
final_df<-reduced_df%>%dplyr::mutate(mean_Rd=ifelse(mean_Rd < 0, abs(mean_Rd), mean_Rd)) #changing Rd to positive values
final_df<-final_df%>%mutate(WUEi = mean_A/mean_gsw, Rmass = mean_Rd*LMA_g_m2)



write.csv(final_df, "Analysis/R outputs - dataframes/themeda_master.csv")


#maps----
library(ggplot2)
library(tidyverse)
library(dplyr)
setwd("C:/Users/30062057/OneDrive - Western Sydney University/e.sumner WSU/Field Themeda project/")
field<-read_xlsx("themeda_sites_Emma.xlsx") #load your occurrence data

library(maps)
# Create a map for Australia 

#load map data to create map of Australia
oz.dat <- read.csv('C:/Users/30062057/OneDrive - Western Sydney University/e.sumner WSU/Code - general/ozdata.csv')


oz_map <- ggplot(oz.dat, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "black", fill = NA) +
  geom_polygon(data = oz.dat, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
  theme(panel.grid = element_blank()) +
  theme_bw()

print(oz_map)

p<-oz_map + 
  geom_jitter(data=field, width = 0.5, height = 0,
              aes(x=longitude, y=latitude,
                  group=1), alpha = .5, size =5, colour = "darkgreen")
p <- p + labs(colour = "all species")+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_classic()
p

ggsave("sorghum_field_sites_map.jpg", plot = p, width = 10, height = 8, dpi = 300, units = "in", device = "jpeg")





#bioclim----
library(sp)
library(tidyverse)
library(terra)
library(stats)
library(raster)


# read in bioclim data
r <- getData("worldclim", var="bio", res="10")


# replace with whatever df you have.
df <- read.csv("Analysis/R outputs - dataframes/themeda_master.csv")

lats <- df$emma_latitude
longs <- df$emma_longitude

# or this is how to just check two points - ignore
# lats <- c(-30.7806, -25.61150585)
# longs <- c(149.4536,    	151.6958687)

coords <- data.frame(x=longs,y=lats)

points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r, points)

df.complete <- cbind.data.frame(coordinates(points), values)
#View(df.complete)
df.complete<-df.complete%>%dplyr::select(-c(x,y))

# recombine to get all the original variables in the df; but change the col numbers in the square brackets.
df.finish <- cbind(df[,1:32], df.complete)

columns_to_divide <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", 
                       "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")

df <- df.finish %>%
  mutate_at(vars(all_of(columns_to_divide)), ~ . / 10)

df<-df%>%rename(
  MAT = bio1,
  MeanDiurnalRange = bio2,
  Isothermality = bio3,
  TempSeasonality = bio4,
  MaxTempWarmestMonth = bio5,
  MinTempColdestMonth = bio6,
  TempAnnualRange = bio7,
  MeanTempWettestQuarter = bio8,
  MeanTempDriestQuarter = bio9,
  MeanTempWarmestQuarter = bio10,
  MeantempColdestQuarter = bio11,
  MAP = bio12,
  PrecipWettestMonth = bio13, 
  PrecipDriestMonth = bio14,
  PrecipSeasonality = bio15,
  PrecipWettestQuarter = bio16, 
  PrecipDriestQuarter = bio17,
  PrecipWarmestQuarter = bio18,
  PrecipColdestQuarter =bio19
)%>%dplyr::select(-X)


write.csv(df, "Analysis/R outputs - dataframes/themeda_master_with_bioclim.csv")

#correlation matrix----
library(GGally)

df<-read.csv("Analysis/R outputs - dataframes/themeda_master_with_bioclim.csv")

library(car)


subset<-df%>%dplyr::select(c(height_av_culm_cm, tillers,flowering_culms, #select few
                             LMA_g_m2, plant_area_cm_sq, leaf_width_mm, leaf_length_cm,
                             mean_A, Rmass , WUEi))


subset<-df%>%dplyr::select(c(height_av_culm_cm, tillers,flowering_culms, #plant morphology and bioclim traits
                            plant_area_cm_sq, MAT, MeanDiurnalRange, Isothermality, TempSeasonality, MaxTempWarmestMonth, 
                            MeanTempWarmestQuarter, MinTempColdestMonth, TempAnnualRange, MeanTempWettestQuarter,
                            MeanTempDriestQuarter, MeantempColdestQuarter, MAP, PrecipWettestMonth,PrecipDriestMonth, PrecipSeasonality,
                            PrecipWettestQuarter,PrecipDriestQuarter, PrecipWarmestQuarter,  PrecipColdestQuarter))

subset<-df%>%dplyr::select(c(leaf_width_mm, leaf_length_cm,LMA_g_m2, #leaf and bioclim traits
                             plant_area_cm_sq, MAT, MeanDiurnalRange, Isothermality, TempSeasonality, MaxTempWarmestMonth, 
                             MeanTempWarmestQuarter, MinTempColdestMonth, TempAnnualRange, MeanTempWettestQuarter,
                             MeanTempDriestQuarter, MeantempColdestQuarter, MAP, PrecipWettestMonth,PrecipDriestMonth, PrecipSeasonality,
                             PrecipWettestQuarter,PrecipDriestQuarter, PrecipWarmestQuarter,  PrecipColdestQuarter))

subset<-df%>%dplyr::select(c(mean_A, mean_Rd, #gas exchange and bioclim traits
                             plant_area_cm_sq, MAT, MeanDiurnalRange, Isothermality, TempSeasonality, MaxTempWarmestMonth, 
                             MeanTempWarmestQuarter, MinTempColdestMonth, TempAnnualRange, MeanTempWettestQuarter,
                             MeanTempDriestQuarter, MeantempColdestQuarter, MAP, PrecipWettestMonth,PrecipDriestMonth, PrecipSeasonality,
                             PrecipWettestQuarter,PrecipDriestQuarter, PrecipWarmestQuarter,  PrecipColdestQuarter))

%>%filter(height_tallest_culm_cm<2.5 )
subset<-df%>%dplyr::select(c(Rmass, MAT, MAP))




my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

subset<-df%>%dplyr::select(c(mean_gsw, MAT, MAP))

log_subset <- subset %>%
  mutate(across(where(is.numeric), log10))


log_subset<-log_subset%>%filter(height_tallest_culm_cm<2.5)
g = ggpairs(log_subset,columns = 1:3, lower = list(continuous = my_fn))

g

ggsave(g, 
       filename = "Analysis/R outputs - figures/correlation matrices/corr_matrix_traits4.pdf",
       device = "pdf",
       height = 20, width = 20, units = "in")

#Correlation heatmap
library(reshape2)

log_subset<-na.omit(log_subset)

cormat <- round(cor(log_subset),2)
head(cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
upper_tri



melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


#PCA----
#will need to update when leaf CN are ready.
library("corrr")
library("FactoMineR")
library("factoextra")
df<-read.csv("Analysis/R outputs - dataframes/themeda_master_with_bioclim.csv")

subset<-df%>%dplyr::select(c(height_av_culm_cm, tillers,flowering_culms, 
                             LMA_g_m2, plant_area_cm_sq, leaf_width_mm, leaf_length_cm,
                             mean_A, Rmass, WUEi))

subset<-subset%>%dplyr::mutate(Rmass=ifelse(Rmass < 0, abs(Rmass), Rmass)) #Rd has to be positive values

log_subset <- subset %>%
  mutate(across(where(is.numeric), log10))

data<-na.omit(log_subset)
data_normalized <- scale(data)
corr_matrix <- cor(data_normalized)
corr_matrix
data.pca <- princomp(corr_matrix)

summary(data.pca) #the first principal component explains ~55% of the total variance in the dataset.This implies that a half of the data in the set of 15 variables can be represented by just the first principal component. The second one explains 24% of the total variance. 
data.pca$loadings[, 1:2] #loading matrix shows that the first principal component has high positive values for height, plant area,leaf length, and MAt etc while the values for tillers and Rd are relatively negative. This suggests that species with higher values for height, long leaves, plant area (competitive traits?) tend to have lower Rd and tillers.
fviz_eig(data.pca, addlabels = TRUE) #to visualise the importance of each principal component which can be used to determine the number of principal components to retain
# Graph of the variables
fviz_pca_var(data.pca, col.var = "black") #all the variables
fviz_cos2(data.pca, choice = "var", axes = 1:2)
p<-fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
p

ggsave(p, 
       filename = "Analysis/R outputs - figures/PCA/PCA_23042024.pdf",
       device = "pdf",
       height = 10, width = 12, units = "in")


rubbish code - just mucking around
ggplot(df, aes(x=bio12_MAP, y = WUEi, color = site))+
  geom_point() +  # Adds points to the scatterplot
  theme_classic()  # Applies a minimalistic theme

ggplot(df, aes(x=bio1_MAT, y = WUEi, color = site))+
  geom_point() +
  geom_smooth(method= "lm", se=TRUE)+# Adds points to the scatterplot
  theme_classic()  # Applies a minimalistic theme


ggplot(log_subset, aes(x=height_av_culm_cm, y = tillers))+
  geom_point() +
  geom_smooth(method= "lm", se=TRUE)+# Adds points to the scatterplot
  theme_classic()  # Applies a minimalistic theme



ggplot(subset, aes(x=height_av_culm_cm, y = tillers))+
  geom_point() +
  geom_smooth(method= "lm", se=TRUE)+# Adds points to the scatterplot
  theme_classic()  # Applies a minimalistic theme



