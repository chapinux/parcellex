library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(readr)
library(plotly)
library(sf)

colnamesTruth <- paste0("Truth_V",seq(from = 1, to= 32 ))
colnamesPred <- paste0("Pred_V",seq(from = 1, to= 32 ))

dfTruth <-  read_delim("Documents/bidouille_Anatol/truth_2.csv", skip_empty_rows = T, col_names = colnamesTruth, delim=";")
dfPred <-  read_delim("Documents/bidouille_Anatol/pred_2.csv", skip_empty_rows = T, col_names = colnamesPred, delim = ";" )


pente <-  read.csv("Documents/bidouille_Anatol/pente.csv", header = F,col.names = "slope")
aire <-  read.csv("Documents/bidouille_Anatol/aire.csv", header = F, col.names = "surface")
max(aire)
perimetre <-  read.csv("Documents/bidouille_Anatol/perimetre.csv", header = F, col.names = "perimeter")
IDs <-  read.csv("Documents/bidouille_Anatol/ids.csv", header = F, col.names = "ID")
alti <-  read.csv("Documents/bidouille_Anatol/altitude.csv", header = F, col.names = "elevation")
nbpix <-  read.csv("Documents/bidouille_Anatol/nb_de_pixels.csv", header = F, col.names = "nb_pix")


parcelles <-  read_sf("./Documents/bidouille_Anatol/rpg_2017_T31TFM.geojson")
#uniquement les parcelles concernées par les données du dataset
parcelles <-  parcelles %>%  filter(ID_PARCEL %in% IDs$ID)  %>% select(ID_PARCEL,CODE_CULTU,geometry)

str(parcelles)



df <- data.frame(IDs, dfTruth, dfPred,pente, aire, perimetre,alti, nbpix, parcelles )
names(df)




p1 <- ggplot(df, aes(x=truth, y = pred)) +
  geom_point(size=0.4, colour="deepskyblue4")+
  theme_light()
p1



### version tout le monde à plat
xTruth <- stack(dfTruth)[1]
yPred <- stack(dfPred)[1]




longizer <-  function(lili) {
  xlili <-  lili[2:33] #truth
  ylili <-  lili[34:65] #pred
  dflili <-  cbind(t(xlili), t(ylili)) %>% as.data.frame()

  dflili$surface <- lili[67] %>% as.numeric()
  dflili$perimeter <-  lili[68]%>% as.numeric()
  dflili$ID <-  lili[1]%>% as.numeric()
  dflili$slope <-  lili[66]%>% as.numeric()
  dflili$elevation <- lili[69]%>% as.numeric()
  dflili$nbpix <-  lili[70]%>% as.numeric()
 # dflili$CodeCulture <-  lili[72] %>% as.character()
#  dflili$geometry <- rep(lili[73] %>% st_as_sf(),32)
  names(dflili) <- c("truth", "pred","surface", "perimeter", "ID_PARCEL", "slope", "elevation", "nbpix")
  return(dflili)
}







yy <- c()
for (i in 1:nrow(df)){
  if (i %% 100==0) {cat(i,"\n")}
  yy <- rbind(yy, longizer(df[i,]))
}





str(parcelles)
str(yy)
write.csv(yy, "~/Documents/bidouille_Anatol/dataBienFormee.csv")



yy <- read.csv("~/Documents/bidouille_Anatol/dataBienFormee.csv")

sf_yy <-  inner_join(yy,parcelles, by="ID_PARCEL")

# write.csv(sf_yy, "~/Documents/bidouille_Anatol/dataBienFormee_et_geom.csv")

str(sf_yy)

#outliers 

yy %>%  ggplot() +
  geom_bar(aes(surface), width=100)

hist(yy$surface)
summary(yy$surface)
#2 pentes énormes 
yy <-  yy %>% filter(slope >= 0)
yy <-  yy %>% filter(perimeter < 6000)




p1 <- ggplot(yy, aes(x=truth, y = pred)) + 
  geom_point(aes(color=slope),size=0.4)+
  scale_color_viridis_c()+
  geom_smooth(method="lm", color="orange")+
  theme_light()
p1

p2 <- ggplot(yy, aes(x=truth, y = pred)) + 
  geom_point(aes(color=slope, size=sqrt(slope), alpha= sqrt(slope) ))+
  scale_color_viridis_c()+
  geom_smooth(method="lm")+
  theme_light()
p2

p3 <- ggplot(yy, aes(x=truth, y = pred)) + 
  geom_point(aes(color=perimeter, alpha=perimeter),size=0.4)+
  scale_color_viridis_c()+
  theme_light()
p3

ggplotly


p4 <- ggplot(yy, aes(x=truth, y = pred)) + 
  geom_point(aes(color=elevation),size=0.4)+
  scale_color_viridis_c()+
  theme_light()
p4


p5 <- ggplot(yy %>% filter(nbpix <= 645), aes(x=truth, y = pred)) + 
  geom_point(aes(color=nbpix),size=0.4)+
  scale_color_viridis_c()+
  theme_light()
p5

summary(nbpix)


varint <-  c("surface",   "perimeter",         "slope",     "elevation" ,"nbpix"  )


for (v in varint){
  # yy <- read.csv("~/Documents/bidouille_Anatol/dataBienFormee.csv")
p4 <- ggplot(yy, aes(x=truth, y = pred)) + 
  geom_point(aes(color=yy[,v]),size=0.4)+
  scale_color_viridis_c()+
  labs(color = v)+
  xlim(c(-0.25,1))+
  ylim(c(-0.25,1))+
  geom_smooth(method="lm", color="#FF00DD", lwd=0.5)+
  theme_light()
  
print(p4)
cat(v,"\n")
}


library(plotly)

yy <-  yy %>% sample_n(1500)
p4 <- ggplot(yy, aes(x=truth, y = pred)) + 
  geom_point(aes(color=yy[,v]),size=3)+
  scale_color_viridis_c()+
  labs(color = v)+
  xlim(c(-0.25,1))+
  ylim(c(-0.25,1))+
  theme_light()


xx <-  ggplotly(p4)
print(xx)



plt1 <-  plot_ly(data=sample_n(yy,10000), x=~truth, y=~pred,
                 type='scatter',
                 mode = 'markers',
                 name='',
                 
                 # hovertemplate = ~paste('<b>%{ID}</b>',
                 # '<br>truth:%{truth.3f}',
                 # '<br>Predict:%{pred.3f}',
                 # '<br>surface:%{surface}',
                 # '<br>perimeter:%{perimeter}',
                 # '<br>slope:{%slope}',   
                 # '<br>elevation:%{elevation}',
                 # '<br>nbpix:%{nbpix}', 
                 # '<extra></extra>'),
                 text = ~paste("<br><b>ID",ID_PARCEL,
                               "</b><br>surface:",surface,
                               "<br>perimeter:",perimeter,
                               "<br>slope:", slope,
                               "<br>elevation:", elevation,
                               "<br>nbpix:", nbpix),
                 color =~elevation
                 
                 
                 
) %>%  layout(title = 'Truth/Pred colored by elevation',
              yaxis = list(hoverformat = '.2f'),
              xaxis = list(hoverformat = '.2f'))
plt1


ids <-  yy$ID

library(sf)

parcelles <-  read_sf("./Documents/bidouille_Anatol/rpg_2017_T31TFM.geojson")
#uniquement les parcelles concernées par les données du dataset
parcelles <-  parcelles %>%  filter(ID_PARCEL %in% ids)







library(ade4)
library(factoextra)
library(FactoMineR)



yy %>% filter(truth < -0.5)

yy$surface <- yy$surface %>%  as.numeric()
yy$perimeter <- yy$perimeter %>%  as.numeric()
yy$slope <- yy$slope %>%  as.numeric()
yy$nbpix <- yy$nbpix %>%  as.numeric()


subsetYY <-  yy %>% select(-c(X,ID,truth, pred))


str(subsetYY)


yy <- yy %>% na.omit()
# ACP de base
mypca <- prcomp(subsetYY, scale. = T, center=T)
#ACP avec le package ADE4
mypca2 <-  dudi.pca(subsetYY, center=F, scannf = F, nf=5 )

#dessin
# variance expliquée par les composantes
fviz_eig(mypca)
# graphe des variables dans l'espace des deux premières composantes
fviz_pca_var(mypca,
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE    
)



fviz_pca_ind(mypca,
             col.ind  = "contrib",
             geom="point",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07") )

#idem avec seconde ACP package ade4
fviz_eig(mypca2)
fviz_pca_var(mypca2,
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE    
)






