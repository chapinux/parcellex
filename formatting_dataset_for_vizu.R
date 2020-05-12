library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(plotly)
library(sf)
library(lubridate)

# dfTruth <-  read.csv("~/dev/parcellex/PARCELLEX_data_sup/Parcellex_truthNDVI.csv", header = F )
# dfPred <-   read.csv("~/dev/parcellex/PARCELLEX_data_sup/Parcellex_predictNDVI.csv", header = F)
# 
# pente <-  read.csv("~/dev/parcellex/pente.csv", header = F,col.names = "slope")
# aire <-  read.csv("~/dev/parcellex/aire.csv", header = F, col.names = "surface")
# perimetre <-  read.csv("~/dev/parcellex/perimetre.csv", header = F, col.names = "perimeter")
# IDs <-  read.csv("~/dev/parcellex/ids.csv", header = F, col.names = "ID")
# alti <-  read.csv("~/dev/parcellex/altitude.csv", header = F, col.names = "elevation")
# nbpix <-  read.csv("~/dev/parcellex/nb_de_pixels.csv", header = F, col.names = "nb_pix")
# MSE <-  read.csv("~/dev/parcellex/PARCELLEX_data_sup/MSE_parcellex.csv", header = T, col.names = "MSE")
# MAE <-   read.csv("~/dev/parcellex/PARCELLEX_data_sup/MAE_parcellex.csv", header = F, col.names = "MAE")
# dates <- read.csv("~/dev/parcellex/PARCELLEX_data_sup/Parcellex_dates.csv", header=T)
# Rsquare <-  read.csv("~/dev/parcellex/PARCELLEX_data_sup/R2_parcellex.csv", header = T)


setwd("~/dev/parcellex/[PARCELLEX]_New_Data/")
dfTruth <-  read.csv("Time-Series/Parcellex_TRUTH.csv", header = F, stringsAsFactors = F)
dfPred <-   read.csv("Time-Series/Parcellex_PREDICTIONS.csv", header = F, stringsAsFactors = F)
dfMask <-   read.csv("Time-Series/Parcellex_MASK.csv", header = F, stringsAsFactors = F)

library(stringr)

dfTruth[] <- lapply(dfTruth, gsub, pattern='\\[', replacement='')
dfTruth[] <- lapply(dfTruth, gsub, pattern='\\]', replacement='')
dfTruth[] <- lapply(dfTruth, type.convert)
str(dfTruth)

dfPred[] <- lapply(dfPred, gsub, pattern='\\[', replacement='')
dfPred[] <- lapply(dfPred, gsub, pattern='\\]', replacement='')
dfPred[] <- lapply(dfPred, type.convert)




pente <-  read.csv("External_Data/Parcellex_PENTE.csv", header = T, col.names = "slope")
aire <-  read.csv("External_Data/Parcellex_AIRE.csv", header = T, col.names = "surface")
perimetre <-  read.csv("External_Data/Parcellex_PERIMETRE.csv", header = T, col.names = "perimeter")
IDs <-  read.csv("Parcellex_IDS.csv", col.names = "ID")
alti <-  read.csv("External_Data/Parcellex_ALTITUDE.csv", header = T, col.names = "elevation")
nbpix <-  read.csv("External_Data/Parcellex_NBPIXELS.csv", header = T, col.names = "nb_pix")
MSE <-  read.csv("Metrics/Parcellex_MSE.csv", header = T, col.names = "MSE", blank.lines.skip = F)
MAE <-   read.csv("Metrics/Parcellex_MAE.csv", header = T, col.names = "MAE", blank.lines.skip = F)
dates <- read.csv("Time-Series/Parcellex_DATES.csv", header=T)
Rsquare <-  read.csv("Metrics/Parcellex_R2S.csv", header = T, col.names = "Rsquare", blank.lines.skip = F)
exosition<-  read.csv("External_Data/Parcellex_EXPOSITION.csv", header = T, col.names = "exposition")

names(dfTruth) <- paste("truth",ymd(dates$Dates))
names(dfPred) <- paste("pred",ymd(dates$Dates))


parcelles <-  read_sf("~/dev/parcellex/rpg_2017_T31TFM.geojson")
#uniquement les parcelles concernées par les données du dataset
parcelles <-  parcelles %>%  filter(ID_PARCEL %in% IDs$ID)  %>% select(ID_PARCEL,CODE_CULTU,geometry)



df <- data.frame(IDs, dfTruth, dfPred,pente, aire, perimetre,alti, nbpix, MSE, MAE, Rsquare, parcelles )
names(df)






p1 <- ggplot(df, aes(x=truth, y = pred)) +
  geom_point(size=0.4, colour="deepskyblue4")+
  theme_light()
p1

names(df)


longizer <-  function(lili) {
  xlili <-  lili[2:61] #truth
  ylili <-  lili[62:121] #pred
  dflili <-  cbind(t(xlili), t(ylili)) %>% as.data.frame()

  dflili$surface <- lili[123] %>% as.numeric()
  dflili$perimeter <-  lili[124]%>% as.numeric()
  dflili$ID <-  lili[1]%>% as.numeric()
  dflili$slope <-  lili[122]%>% as.numeric()
  dflili$elevation <- lili[125]%>% as.numeric()
  dflili$nbpix <-  lili[126]%>% as.numeric()
  dflili$MSE <-  lili[127]%>% as.numeric()
    dflili$MAE <-  lili[128]%>% as.numeric()
    dflili$Rsquare <-  lili[129]%>% as.numeric()
    
 # dflili$CodeCulture <-  lili[72] %>% as.character()
#  dflili$geometry <- rep(lili[73] %>% st_as_sf(),32)
  names(dflili) <- c("truth", "pred","surface", "perimeter", "ID_PARCEL", "slope", "elevation", "nbpix", "MSE", "MAE", "Rsquare")
  rownames(dflili) <-  ymd(dates$Dates)
  return(dflili)
}





lili <-  df[1,]


longizer(lili)
#boucle moche à refaire mieux 
yy <- c()
for (i in 1:nrow(df)){
  if (i %% 100==0) {cat(i,"\n")}
  yy <- rbind(yy, longizer(df[i,]))
}

write.csv(yy, "~/dev/parcellex/newData_mask.csv")






