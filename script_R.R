
library(ggplot2)
library(gstat)
library(dplyr)
library(tidyr)
library(readxl)
library(tibble)
library(tseries)
library(mgcv)
library(visdat)
library(nycflights13)
library(VIM)
library(lubridate)
library(FactoMineR)
library(factoextra)
#library(xlsx)
#library(readr)

#load the data set
## Malaria data set


my_data<-read_excel('/Users/USER/Desktop/Ma_these_WAMWAD/DONNES/malaria_data/donne_DHIS2_consultation_bon1.xlsx')
my_data<-as.data.frame(my_data)
#head(my_data)


## Climate data set

### For Heath District of Dianke Makha

df_enviro_dianke<-read.table("/Users/USER/Desktop/Ma_these_WAMWAD/DONNES/donne_envi_journaliere/donne_envi_dianke_corr_bon.csv", header = TRUE, sep = ",", dec = ".", fill = TRUE, comment.char = "")
#df_enviro_dianke

### For Heath District of Kedougou
df_enviro_kedougou<-read.table('/Users/USER/Desktop/Ma_these_WAMWAD/DONNES/donne_envi_journaliere/donne_envo_kedougou_daily_cor.csv', header = TRUE, sep = ",", dec = ".",)

### For Heath District of Salemata
df_enviro_salemata<-read.table('/Users/USER/Desktop/Ma_these_WAMWAD/DONNES/donne_envi_journaliere/donne_envo_Salemata_daily_cor.csv', header = TRUE, sep = ",",  dec = ".",)
### For Heath District of Saraya
df_enviro_saraya<-read.table('/Users/USER/Desktop/Ma_these_WAMWAD/DONNES/donne_envi_journaliere/donne_envo_Saraya_daily_cor.csv', header = TRUE, sep = ",", dec = ".",)



## Netoyage des bases de données 

### Données du paludisme 



# Add population size

my_data$POPULATION <-as.numeric(ifelse(my_data$DISTRICT=='Kedougou'& my_data$ANNEE=='2018', '92485',
          ifelse(my_data$DISTRICT=='Kedougou'& my_data$ANNEE=='2022', '102689',
                                              ifelse(my_data$DISTRICT=='Kedougou'& my_data$ANNEE=='2019', '95598',
                                                     ifelse(my_data$DISTRICT=='Kedougou'& my_data$ANNEE=='2020', '98836',
                                                            ifelse(my_data$DISTRICT=='Kedougou'& my_data$ANNEE=='2021', '102196',
                                                                   ifelse(my_data$DISTRICT=='Salemata'&my_data$ANNEE=='2022', '29761',
                                                                          ifelse(my_data$DISTRICT=='Salemata'& my_data$ANNEE=='2018', '26042',
                                                                                 ifelse(my_data$DISTRICT=='Salemata'& my_data$ANNEE=='2019', '26917',
                                                                                        ifelse(my_data$DISTRICT=='Salemata'& my_data$ANNEE=='2020', '27831',
                                                                                               ifelse(my_data$DISTRICT=='Salemata'& my_data$ANNEE=='2021','27831',
                                                                                                      ifelse(my_data$DISTRICT=='Saraya'& my_data$ANNEE=='2022', '68273',
                                                                                                             ifelse(my_data$DISTRICT=='Saraya'& my_data$ANNEE=='2018', '59745',
                                                                                                                    ifelse(my_data$DISTRICT=='Saraya'& my_data$ANNEE=='2019', '61756',
                                                                                                                           ifelse(my_data$DISTRICT=='Saraya'& my_data$ANNEE=='2020', '63846',
                                                                                                                                  ifelse(my_data$DISTRICT=='Saraya'& my_data$ANNEE=='2021','28777',
                                                                                                                                         ifelse(my_data$DISTRICT=='Dianke Makha'& my_data$ANNEE=='2022', '9779',
                                                                                                                                                ifelse(my_data$DISTRICT=='Dianke Makha'&my_data$ANNEE=='2018' ,'31751',
                                                                                                                                                       ifelse(my_data$DISTRICT=='Dianke Makha'& my_data$ANNEE=='2019', '32902',
                                                                                                                                                              ifelse(my_data$DISTRICT=='Dianke Makha'& my_data$ANNEE=='2020', '43100',
                                                                                                                                                                     ifelse(my_data$DISTRICT=='Dianke Makha'& my_data$ANNEE=='2021', '35346','0.00')))))))))))))))))))))






###########################################################
# Selection des variables intéressantes 

my_data_palu<- subset(my_data, select= c(ANNEE,PERIODE,DISTRICT,TDR_realise,cas_palu_confirme,POPULATION))                             

# Analyse des valeurs manquantes

my_data_palu %>%
  sample_n(nrow(my_data_palu)) %>%
  vis_miss()


my_data_palu <- subset(my_data_palu ,ANNEE > 2017)



df_na <- my_data_palu[!complete.cases(my_data_palu), ]

df_na

# Impulte data 

# Nous avons utiliser une KNN pour imputer les valeurs manquantes des cas de paludismes.
# On a choisi KKN car les cas de paludisme sont autocerrelées.
# on a choisi k=5 car le temp d'incubation du paludisme à plasmodium falciparum est de 
# 9 à 14 jours. donc les cas de paludisme d'une semaine données i est affecter pas celle 
# des deux semaines précédente mais aussi peut impacter les cas de paludisme des deux 
# semaines qui suit 

#Imputer les données avec kNN
set.seed(123)

df_KNN<- kNN(my_data_palu,variable="cas_palu_confirme", k = 5)

my_data_palu_impute <- df_KNN[, 1:6]


dim(my_data_palu_impute)

my_data_palu_impute %>%
  sample_n(nrow(my_data_palu_impute)) %>%
  vis_miss()

 head(my_data_palu_impute)

#Nous definition une colone de date définissant la date de debut de chaque semaine

# Charger le paquet lubridate


my_data_palu_impute$date <- as.Date(paste0(my_data_palu_impute$PERIODE, "-1"), format = "W%W %Y-%u")


###======================================================================

### Netoyage des donées environnementales

#Saraya


df_enviro_saraya$date <- as.Date(paste(df_enviro_saraya$YEAR, df_enviro_saraya$MO, df_enviro_saraya$DY, sep = "-"))
# numbre de jour de precipitation par semaine
df_enviro_saraya$number_day_rainy <-as.numeric(ifelse(df_enviro_saraya$PRECTOTCORR==0, '0','1'))

df_enviro_saraya$week <- floor_date(df_enviro_saraya$date, "week")+1
# aggreger les données par week
df_agg_saraya<-aggregate(.~week, data=df_enviro_saraya,FUN=mean)
df_agg_saraya2<-aggregate(.~ week, data=df_enviro_saraya,FUN=sum)
df_agg_saraya$number_day_rainy<-df_agg_saraya2$number_day_rainy
df_agg_saraya

#salemata


df_enviro_salemata$date <- as.Date(paste(df_enviro_salemata$YEAR, df_enviro_salemata$MO, df_enviro_salemata$DY, sep = "-"))
#numbre de jour de precipitation par semaine
df_enviro_salemata$number_day_rainy <-as.numeric(ifelse(df_enviro_salemata$PRECTOTCORR==0, '0','1'))

df_enviro_salemata$week <- floor_date(df_enviro_salemata$date, "week")+1
df_agg_salemata<-aggregate(.~ week, data=df_enviro_salemata,FUN=mean)

df_agg_salemata2<-aggregate(.~week, data=df_enviro_salemata,FUN=sum)
df_agg_salemata$number_day_rainy<-df_agg_salemata2$number_day_rainy

#df_agg_salemata



#Kédougou


df_enviro_kedougou$date <- as.Date(paste(df_enviro_kedougou$YEAR, df_enviro_kedougou$MO, df_enviro_kedougou$DY, sep = "-"))
# numbre de jour de precipitation par semaine
df_enviro_kedougou$number_day_rainy <-as.numeric(ifelse(df_enviro_kedougou$PRECTOTCORR==0, '0','1'))
df_enviro_kedougou$week <- floor_date(df_enviro_kedougou$date, "week")+1
df_agg_kedougou<-aggregate(.~ week, data=df_enviro_kedougou,FUN=mean)
df_agg_kedougou2<-aggregate(.~week, data=df_enviro_kedougou,FUN=sum)
df_agg_kedougou$number_day_rainy<-df_agg_kedougou2$number_day_rainy

#df_agg_kedougou


#Dianké Makha


df_enviro_dianke$date <- as.Date(paste(df_enviro_dianke$YEAR, df_enviro_dianke$MO, df_enviro_dianke$DY, sep = "-"))
# numbre de jour de precipitation par semaine
df_enviro_dianke$number_day_rainy <-as.numeric(ifelse(df_enviro_dianke$PRECTOTCORR==0, '0','1'))
df_enviro_dianke$week <- floor_date(df_enviro_dianke$date, "week")+1
df_agg_dianke<-aggregate(.~ week, data=df_enviro_dianke,FUN=mean)
df_agg_dianke2<-aggregate(.~week, data=df_enviro_dianke,FUN=sum)
df_agg_dianke$number_day_rainy<-df_agg_dianke2$number_day_rainy


### concatener les 4 bases


df_agg_kedougou$DISTRICT<-"Kedougou"
df_agg_dianke$DISTRICT<-"Dianke Makha"
df_agg_saraya$DISTRICT<-"Saraya"
df_agg_salemata$DISTRICT<-"Salemata"

df_environnement<-bind_rows(df_agg_dianke,df_agg_saraya,df_agg_salemata,df_agg_kedougou)

df_environnement$YEAR<- format(as.Date(df_environnement$week, format="%y%m%d"), "%Y")
df_environnement$MO<- format(as.Date(df_environnement$week, format="%y%m%d"), "%m")

df_environnement[, c('DY', 'date')] <- list(NULL)
#df_environnement
length(df_environnement$MO)

### extraire la période d'étude

df_environnement<-df_environnement %>%filter(YEAR <= 2022)

dim(df_environnement)
# Merger les deux bases 
my_data_base_palu<- merge(my_data_palu_impute,df_environnement, by.x = c('date', 'DISTRICT','ANNEE'), by.y = c('week', 'DISTRICT','YEAR'))

dim(my_data_base_palu)

my_data_base_palu$MOIS=my_data_base_palu$MO
# Calculer l'incidence
my_data_base_palu$incidence <- my_data_base_palu$cas_palu_confirme * 1000 / my_data_base_palu$POPULATION


#==================================================================================
#malaria data anaysis


#analyse du nombre de cas pas années et par DISTRICT

result <- my_data_base_palu%>%
  group_by(DISTRICT, ANNEE) %>%
  summarise(Sum_x = sum(cas_palu_confirme), .groups = 'drop')  # .groups = 'drop' pour ne pas garder la structure de groupe

# Afficher le résultat

print(result)


#=========Plot ==================================
comp_cas <- my_data_etude %>%
  select(date, DISTRICT, cas_palu_confirme) %>%
  pivot_wider(names_from = DISTRICT, values_from = cas_palu_confirme)


ggplot(comp_cas, aes(x = date)) +
  geom_line(aes(y = Kedougou, color = "Kedougou"), size = 1.3) +
  geom_line(aes(y = Saraya, color = "Saraya"), size = 1.3) +
  geom_line(aes(y = Salemata, color = "Salemata"), size = 1.3) +
  geom_line(aes(y = `Dianke Makha`, color = "Dianke Makha"), size = 1.3) +
  
  # Personnaliser les couleurs
  scale_color_manual(values = c("Kedougou" = "blue", 
                                "Saraya" = "red", 
                                "Salemata" = "darkcyan", 
                                "Dianke Makha" = "orange")) +
  
  # Ajuster les axes et le thème
  scale_x_date(date_breaks = "16 weeks", date_labels = "W%W-%Y",  limits = as.Date(c("2018-01-01", "2022-12-31")) ) +
  theme_minimal() +
  
  # Rotation des labels sur l'axe des X
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5,size = 12, color = "black"),
        axis.text.y = element_text(size = 12,colour="black"),                     # Taille des valeurs (Y)
        axis.title.x = element_text(size = 14),                    # Taille du titre de l'axe X
        axis.title.y = element_text(size = 14)  ) +
  
  # Ajouter des titres
  labs(title = " ", 
       x = "Weeks", 
       y = "MAlaria cases", 
       color = "Districts")



#=====================box plot ==============================

ggplot(my_data_etude, aes(x = DISTRICT, y = cas_palu_confirme, fill = DISTRICT)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +   # Boîte avec outliers en rouge
  theme_minimal() +
  labs(
    title = "Répartition des cas de paludisme par district",
    x = "District", 
    y = "Nombre de cas"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),   # Rotation des étiquettes
    legend.position = "none"                             # Masquer la légende
  )

#===================================================================

# Meteorological data analysis

#==============================================================================
# Tracer des variables meteorologique par district et pas annéee
#================================================================================

# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)


# Calculer la température moyenne, l'humidité et les précipitations par mois et par localité
A<-my_data_etude[,c("DISTRICT","MOIS","ANNEE","T2M_MIN","T2M_MAX","RH2M","WS10M_MAX","PRECTOTCORR")]
monthly_data <- A %>%
  group_by(DISTRICT,ANNEE,MOIS) %>%
  summarise(
    avg_temp_min = mean(T2M_MIN),
    avg_temp_max = mean(T2M_MAX),
    avg_humidity = mean(RH2M),
    avg_vent = mean(WS10M_MAX),
    total_precipitation = sum(PRECTOTCORR),
    .groups = 'drop'
  )


# Convertir les chaînes de caractères en valeurs numériques
monthly_data$Month <- as.numeric(monthly_data$MOIS)

# Convertir les mois en facteur avec des étiquettes
monthly_data$MOIS <- factor(monthly_data$MOIS, 
                            levels = 1:12, 
                            labels = c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin", 
                                       "Juil", "Août", "Sep", "Oct", "Nov", "Déc"))
# Tracer le graphique avec deux axes des y
ggplot(monthly_data, aes(x = Month)) +
  # Tracer les lignes pour la température minimale, maximale et l'humidité
  geom_line(aes(y = avg_temp_min, color = "Temperature Min"), size = 1.5) +
  geom_line(aes(y = avg_temp_max, color = "Temperature Max"), size = 1.5) +
  geom_line(aes(y = avg_humidity, color = "Humidity"), size = 1.5) +
  geom_line(aes(y = avg_vent*10, color = "Wind spreed"), size = 1.5) +
  # Tracer les barres pour les précipitations
  geom_bar(aes(y = total_precipitation, fill = "Rainyfall"), stat = "identity", position = "dodge", alpha = 0.5) +  # Diviser par 10 pour ajuster l'échelle
  
  # Spécifier les couleurs
  scale_color_manual(values = c("Temperature Min" = "burlywood3", "Temperature Max" = "orange1", "Wind spreed"="red","Humidity" = "royalblue")) + #"burlywood","cornflowerblue
  scale_fill_manual(values = c("Rainyfall" = "midnightblue")) +
  # Facettes
  
  facet_grid(ANNEE ~ DISTRICT,scales = "fixed") +
  labs(title = " ",
       x = "Month",
       y = "Values",
       color = "",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Taille des étiquettes de l'axe x
        axis.text.y = element_text(size = 10),  # Taille des étiquettes de l'axe y
        axis.title.x = element_text(size = 11),  # Taille du titre de l'axe x
        axis.title.y = element_text(size = 11),  # Taille du titre de l'axe y
        strip.text = element_text(color = "black", size = 14),  # Couleur et taille des titres des facettes
        plot.title = element_text(size = 14, hjust = 0.5),
        strip.placement = "outside",
        strip.text.y.right = element_text(margin = margin(l =20, r =0), size = 14,color="darkblue"),
        axis.title.y.right = element_text(size = 14, margin = margin(t = 0, r =0, b = 0, l =5))) +
  
  # Ajouter un deuxième axe y pour les précipitations
  scale_y_continuous(sec.axis = sec_axis(~ . * 10, name = "Precipitations (mm)",breaks = seq(0,1200, by =200)))+  # Ajuster l'échelle pour les précipitations
  # Définir les étiquettes des mois
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "March", "April", "May", "June", 
                                               "July", "Aug", "Sep", "Oct", "Nov", "Dec"))



#=======================================================================

#=======================================================================
# Principale composante analysis#===============================================


df_meteo <- subset(my_data_base_palu, select=c("QV2M", "RH2M","PRECTOTCORR" ,"WS2M","T2M_MIN","T2M_MAX" ,"TS", "T2M" , "PS","WS10M","WD10M","WS10M_MAX","WS10M_MIN" ,"number_day_rainy" )) 


set.seed(123)
# ACP sur l'ensemble des données
mod_pac_meteo<-PCA(df_meteo,scale.unit=T,graph=T)


summary(mod_pac_meteo)



# critre de kaisier

par(mfrow=c(1,1))
barplot(mod_pac_meteo$eig[,1], names.arg=1:nrow(mod_pac_meteo$eig),col="darkblue",ylim=c(0,10),ylab="Eigenvalues",xlab="Composantes",font=1,cex.axis = 1,cex.main=1,cex.lab=1,col.axis="black") 
abline(h=1,col="red",lwd=5,lty=2)
box(lwd=3)

library(ggplot2)
n_components <- 11# on represente que les 11 composant car à partir de 11 in a
# o pourcente de variance expliqué
selected_eigenvalues <- mod_pac_meteo$eig[1:n_components, 1]

# Calculer le total et les pourcentages
total <- sum(selected_eigenvalues)
percentages <- (selected_eigenvalues / total) * 100
barplot_heights <- barplot(selected_eigenvalues,
                           col = "darkcyan", # ou "chocolate"
                           width = rep(8, n_components),  # Largeur constante pour chaque barre
                           space = rep(1, n_components),  # Espacement constant entre les barres
                           main = "", #Percentage of Variance explained by each component,
                           ylim=c(0,max(selected_eigenvalues) * 1.1),ylab="",xlab=" ",font=1,cex.axis = 1,cex.main=1,cex.lab=1,las=2,lwd=4)
abline(h=1,col="red",lwd=4,lty=3)
# Ajouter les pourcentages sur les barres
text(x = barplot_heights, 
     y = selected_eigenvalues+0.5,  # Positionner les étiquettes au-dessus des barres
     labels = paste0(round(percentages, 1), "%"), 
     cex = 0.9,font=1,col="black")  # Ajuster la taille du texte
mtext("Principal Components", side = 1, line =4)
mtext("Percentage of expained variances", side = 2, line =2)

box(lwd=2,col="darkgrey")


## Diagramme de pareto

n_components <- 11# on represente que les 11 composant car à partir de 11 in a
# o pourcente de variance expliqué
selected_eigenvalues <- mod_pac_meteo$eig[1:n_components, 1]

# Calculer le total et les pourcentages
total <- sum(selected_eigenvalues)
percent_variance <- (selected_eigenvalues / total) * 100


# Création du dataframe
df <- data.frame(
  Component =factor(1:n_components, levels = 1:n_components),
  Variance = percent_variance
)

ggplot(df, aes(x = Component, y = Variance)) +
  geom_bar(stat = "identity", fill = "orange", color = "black", width = 0.7) +  # Barres orange avec contour bleu
  geom_line(aes(group = 1), color = "red", linewidth = 1) +  # Ligne rouge
  geom_point(color = "red", size = 2) +  # Points rouges sur la ligne
  geom_text(aes(label = paste0(round(Variance, 1), "%")), vjust = -0.6, size = 6) +  # Étiquettes au-dessus des barres
  labs(x = "Principal Components", y = "Percentage of explained variances") +
  #scale_x_discrete(expand = c(0, 0)) +  # Affiche toutes les composantes
  scale_x_discrete(expand = c(0.05, 0))+
  #theme_minimal() +
  theme_classic() +
  theme(axis.text.x = element_text(size = 16, angle = 0, vjust = 0.5),  # Labels horizontaux
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))



#===========================================================================

# Extraire les contributions des variables
contributions <-  mod_pac_meteo$var$contrib
# Extraire les coefficients de corrélation
correlations <-  mod_pac_meteo$var$cor
figure<-list()
# Convertir en format long pour ggplot
contributions_long <- as.data.frame(contributions) %>%
  rownames_to_column(var = "Variable") %>%
  pivot_longer(cols = -Variable, names_to = "Composante", values_to = "Contribution")

# Convertir les coefficients de corrélation en format long
correlations_long <- as.data.frame(correlations) %>%
  rownames_to_column(var = "Variable") %>%
  pivot_longer(cols = -Variable, names_to = "Composante", values_to = "Correlation")

# Joindre les contributions et les corrélations
data_long <- contributions_long %>%
  left_join(correlations_long, by = c("Variable", "Composante"))

# Calculer la contribution attendue
#n_variables <- nrow(var_envi)
n_variables <- ncol(df_meteo)
contribution_attendue <- 100 / n_variables

# Définir les couleurs spécifiques pour chaque composante

couleurs <- c("Dim.1" = "coral" , "Dim.2" ="cyan3", "Dim.3" ="cornflowerblue" )
# Tracer les graphiques séparés pour chaque composante
composantes_a_tracer <- c("Dim.1", "Dim.2", "Dim.3")
for (composante in composantes_a_tracer) {
  # Filtrer les données pour la composante actuelle
  data_composante <- data_long %>% filter(Composante == composante)
  
  # Ordonner les contributions par ordre croissant
  data_composante <- data_composante %>% arrange(Contribution)
  
  # Créer le graphique
  p <- ggplot(data_composante, aes(x = Contribution, y = reorder(Variable, Contribution), fill = Composante)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = contribution_attendue, linetype = "dashed", color = "red",lwd=1.5) +
    geom_text(aes(label = paste("r =", round(Correlation, 2)), 
                  hjust = -0.1), nudge_x = 0.1,color="black",size =4,fontface = "bold")+ # Ajuster la position du texte
    # position = position_stack(vjust = 0.5) ) +  # Centrer le texte
    labs(#title =(" ") #paste("Contribution des Variables pour la Composante", composante),
      x = "Contribution (%)",
      y = "Names of variables") +
    scale_fill_manual(values = couleurs) +  # Appliquer les couleurs spécifiques
    scale_x_continuous(limits = c(0, max(data_composante$Contribution) +1.8)) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size =1),
          axis.text.y = element_text(angle = 0, hjust = 1, face = "bold",colour="black", size =12),
          axis.text.x = element_text(angle = 0, hjust = 1, face = "bold",colour="black", size = 12),
          axis.title.x = element_text(face = "bold",colour="black",size = 14),  # Titre de l'axe x en gras
          axis.title.y = element_text(face = "bold",colour="black", size =14),  # Titre de l'axe y en gras
          plot.title = element_text(face = "bold",colour="black",size = 14),
          legend.position = "none") 
  
  # Afficher le graphique
  
  figure[[composante]]<-p
  
  print(p)
 
  ggsave(filename = paste0("figure_", composante, ".jpg"), 
     path = "C:/Users/USER/Desktop/document_article_1/Figure2", width =10, height = 6, units = "in", dpi = 300)
}


# determinons les profiles des variables environnementaux en faisant une classification 
# assendant hierarechique sur les 3 composantes choisit
pca2 = PCA(df_meteo, ncp = 3, graph = FALSE)
hcpc <- HCPC(pca2, graph = FALSE)

fviz_dend(hcpc, cex = 0.8, palette = "Dark2", rect = T, rect_fill = T, rect_border = "Dark2", show_labels = F) 

fviz_dend(hcpc, 
          cex =2,k=3,lwd=1.2, 
          palette = c("darkcyan", "darkorange", "burlywood1"),  # Couleurs pour les trois clusters
          rect = TRUE, 
          rect_fill = TRUE, 
          rect_border = "white", 
          
          show_labels = FALSE, 
)



# affichons les données d'origine avec les clusters
head(hcpc$data.clust, 10)


# nous allons afficher les variables quantitative qui décrivent le mieux chaque cluster

hcpc$desc.var$quanti

# nous allons afficher les dimensions associées aux clusters

#pour afficher les dimensions principales les plus associées aux clusters
hcpc$desc.axes$quanti
# ajoutons les scores aux données des composantes


scores <- mod_pac_meteo$ind$coord
scores_df <- as.data.frame(scores)

# Ajout des composantes principales à data
my_data_etude<-my_data_base_palu
my_data_etude$Dim1 <- scores_df$Dim.1
my_data_etude$Dim2 <- scores_df$Dim.2
my_data_etude$Dim3 <- scores_df$Dim.3
my_data_etude$cluster<-as.factor(hcpc$data.clust$clust) # on recupere les clusters obtenue

my_data_etude

# Visualiser les clusters
ggplot(my_data_etude, aes(x = Dim1, y =Dim3, color = cluster)) +
  geom_point() +
  labs(title = "Clusters basés sur l'ACP et la CHA",
       x = "Première Composante Principale",
       y = "Deuxième Composante Principale") +
  theme_minimal()



# tracons la variation 

# bar plot, with each bar representing 100%

ggplot(my_data_etude, aes(x = MOIS, fill = cluster)) + 
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("1" = "darkcyan", 
                               "2" = "darkorange", 
                               "3" = "burlywood")) +
  labs(x = "Month", y = "Proportion")


#=====================================================================

#================================ Analysons les variations des variables metéorologique en fonction 
#======================== des clusters ==========================================
#=======================================================================

#===================================================================================
#data_base
kedougou<- my_data_etude[my_data_etude$DISTRICT=="Kedougou", ]
salemata<- my_data_etude[my_data_etude$DISTRICT=="Salemata", ]
saraya<- my_data_etude[my_data_etude$DISTRICT=="Saraya", ]
Dianke<- my_data_etude[my_data_etude$DISTRICT=="Dianke Makha", ]
colnames(my_data_etude)


# Définir les couleurs pour chaque cluster (saisons)
cluster_colors <- c("2" = "darkorange", "1" = "chocolate4", "3" = "burlywood")

ggplot(saraya, aes(x = date)) +
  # Ajouter des bandes colorées pour les clusters (saisons)
  geom_rect(data = saraya, 
            aes(xmin = date, xmax = lead(date), ymin = -Inf, ymax = Inf, fill = as.factor(cluster)), 
            alpha = 0.5) +  
  
  # Tracer les courbes des variables climatiques avec mêmes couleurs et formes
  geom_line(aes(y = QV2M, color = "QV2M"), size = 1) + 
  geom_point(aes(y = QV2M, color = "QV2M"), size = 1) + 
  
  geom_line(aes(y = T2M_MAX, color = "T2M_MAX"), size = 1) + 
  geom_point(aes(y = T2M_MAX, color = "T2M_MAX"), size = 1) + 
  
  geom_line(aes(y = WS10M_MAX, color = "WS10M_MAX"), size = 1) + 
  
  geom_line(aes(y = PRECTOTCORR, color = "PRECTOTCORR"), size =1) + 
  geom_point(aes(y = PRECTOTCORR, color = "PRECTOTCORR"), size = 1) +
  
  # Définir les couleurs des clusters (saisons)
  scale_fill_manual(values = cluster_colors, name = "Clusters (Periods)") +
  
  # Définir une seule légende commune pour couleurs et formes
  scale_color_manual(values = c("QV2M" = "black",
                                "WS10M_MAX" = "red", 
                                "T2M_MAX" = "darkred",
                                "PRECTOTCORR" = "darkblue"), 
                     name = "Variables") +
  
  # Supprimer la légende des formes (fusionner avec couleur)
  scale_shape_manual(values = c("QV2M" = 20,  
                                "T2M_MAX" = 20,  
                                "PRECTOTCORR" = 20),  
                     guide = "none") +  #  Supprime la légende pour fusionner
  
  # Axe X avec semaines (W1 - W52) et rotation
  scale_x_date(date_breaks = "8 weeks", 
               date_labels = "W%V %Y",  
               expand = c(0, 0)) +  
  
  labs(title = "                                      Saraya",
       x = "Week",
       y = "Values") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  
        legend.position = "right")

#====================================================================

#Salemata
#==================================================================
# Définir les couleurs pour chaque cluster (saisons)
cluster_colors <- c("2" = "darkorange", "1" = "chocolate4", "3" = "burlywood")

ggplot(salemata, aes(x = date)) +
  # Ajouter des bandes colorées pour les clusters (saisons)
  geom_rect(data = salemata, 
            aes(xmin = date, xmax = lead(date), ymin = -Inf, ymax = Inf, fill = as.factor(cluster)), 
            alpha = 0.5) +  
  
  # Tracer les courbes des variables climatiques avec mêmes couleurs et formes
  geom_line(aes(y = QV2M, color = "QV2M"), size = 1) + 
  geom_point(aes(y = QV2M, color = "QV2M"), size = 1) + 
  
  geom_line(aes(y = T2M_MAX, color = "T2M_MAX"), size = 1) + 
  geom_point(aes(y = T2M_MAX, color = "T2M_MAX"), size = 1) + 
  
  geom_line(aes(y = WS10M_MAX, color = "WS10M_MAX"), size = 1) + 
  
  geom_line(aes(y = PRECTOTCORR, color = "PRECTOTCORR"), size =1) + 
  geom_point(aes(y = PRECTOTCORR, color = "PRECTOTCORR"), size = 1) +
  
  # Définir les couleurs des clusters (saisons)
  scale_fill_manual(values = cluster_colors, name = "Clusters (Periods)") +
  
  # Définir les couleurs des variables climatiques
  scale_color_manual(values = c("QV2M" = "black",
                                "WS10M_MAX" = "red", 
                                "T2M_MAX" = "darkred",
                                "PRECTOTCORR" = "darkblue")) +
  
  # Supprimer toutes les légendes (fill et color)
  guides(fill = "none", color = "none") +
  
  # Axe X avec semaines (W1 - W52) et rotation
  scale_x_date(date_breaks = "8 weeks", 
               date_labels = "W%V %Y",  
               expand = c(0, 0)) +  
  
  labs(title = "                                      Salemata",
       x = "Week",
       y = "Values") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  
        legend.position = "none")  # Supprime totalement la légende


#====================================================================

#Salemata
#==================================================================
# Définir les couleurs pour chaque cluster (saisons)
cluster_colors <- c("2" = "darkorange", "1" = "chocolate4", "3" = "burlywood")

ggplot(kedougou, aes(x = date)) +
  # Ajouter des bandes colorées pour les clusters (saisons)
  geom_rect(data = kedougou, 
            aes(xmin = date, xmax = lead(date), ymin = -Inf, ymax = Inf, fill = as.factor(cluster)), 
            alpha = 0.5) +  
  
  # Tracer les courbes des variables climatiques avec mêmes couleurs et formes
  geom_line(aes(y = QV2M, color = "QV2M"), size = 1) + 
  geom_point(aes(y = QV2M, color = "QV2M"), size = 1) + 
  
  geom_line(aes(y = T2M_MAX, color = "T2M_MAX"), size = 1) + 
  geom_point(aes(y = T2M_MAX, color = "T2M_MAX"), size = 1) + 
  
  geom_line(aes(y = WS10M_MAX, color = "WS10M_MAX"), size = 1) + 
  
  geom_line(aes(y = PRECTOTCORR, color = "PRECTOTCORR"), size =1) + 
  geom_point(aes(y = PRECTOTCORR, color = "PRECTOTCORR"), size = 1) +
  
  # Définir les couleurs des clusters (saisons)
  scale_fill_manual(values = cluster_colors, name = "Clusters (Periods)") +
  
  # Définir les couleurs des variables climatiques
  scale_color_manual(values = c("QV2M" = "black",
                                "WS10M_MAX" = "red", 
                                "T2M_MAX" = "darkred",
                                "PRECTOTCORR" = "darkblue")) +
  
  # Supprimer toutes les légendes (fill et color)
  guides(fill = "none", color = "none") +
  
  # Axe X avec semaines (W1 - W52) et rotation
  scale_x_date(date_breaks = "8 weeks", 
               date_labels = "W%V %Y",  
               expand = c(0, 0)) +  
  
  labs(title = "                                      Kedougou",
       x = "Week",
       y = "Values") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  
        legend.position = "none")  # Supprime totalement la légende

#====================================================================

#Dianke
#==================================================================
# Définir les couleurs pour chaque cluster (saisons)
cluster_colors <- c("2" = "darkorange", "1" = "chocolate4", "3" = "burlywood")

ggplot(Dianke, aes(x = date)) +
  # Ajouter des bandes colorées pour les clusters (saisons)
  geom_rect(data = Dianke, 
            aes(xmin = date, xmax = lead(date), ymin = -Inf, ymax = Inf, fill = as.factor(cluster)), 
            alpha = 0.5) +  
  
  # Tracer les courbes des variables climatiques avec mêmes couleurs et formes
  geom_line(aes(y = QV2M, color = "QV2M"), size = 1) + 
  geom_point(aes(y = QV2M, color = "QV2M"), size = 1) + 
  
  geom_line(aes(y = T2M_MAX, color = "T2M_MAX"), size = 1) + 
  geom_point(aes(y = T2M_MAX, color = "T2M_MAX"), size = 1) + 
  
  geom_line(aes(y = WS10M_MAX, color = "WS10M_MAX"), size = 1) + 
  
  geom_line(aes(y = PRECTOTCORR, color = "PRECTOTCORR"), size =1) + 
  geom_point(aes(y = PRECTOTCORR, color = "PRECTOTCORR"), size = 1) +
  
  # Définir les couleurs des clusters (saisons)
  scale_fill_manual(values = cluster_colors, name = "Clusters (Periods)") +
  
  # Définir les couleurs des variables climatiques
  scale_color_manual(values = c("QV2M" = "black",
                                "WS10M_MAX" = "red", 
                                "T2M_MAX" = "darkred",
                                "PRECTOTCORR" = "darkblue")) +
  
  # Supprimer toutes les légendes (fill et color)
  guides(fill = "none", color = "none") +
  
  # Axe X avec semaines (W1 - W52) et rotation
  scale_x_date(date_breaks = "8 weeks", 
               date_labels = "W%V %Y",  
               expand = c(0, 0)) +  
  
  labs(title = "                                      Dianke Makha",
       x = "Week",
       y = "Values") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  
        legend.position = "none")  # Supprime totalement la légende





#Statistiques descriptives : Calculez des statistiques descriptives pour chaque variable météorologique dans chaque cluster.



# Calculer des statistiques descriptives pour chaque cluster
cluster_summary <- my_data_etude %>%
  group_by(cluster) %>%
  summarise(across( c("QV2M", "RH2M","PRECTOTCORR" ,"WS2M","T2M_MIN","T2M_MAX" ,"TS",
                      "T2M_RANGE" , "T2M" , "PS","WS10M","WD10M","WS10M_MAX","WS10M_MIN" ,"WS10M_RANGE" ,
                      "number_day_rainy" ) ,
                    list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE))

print(cluster_summary)






# test d'anova pour voir d'il existe une difference significative entre les cluters

# ANOVA pour tester les différences entre les clusters pour la température moyenne
anova_result1 <- aov(QV2M ~ cluster, data = my_data_etude)
summary(anova_result1)

anova_result2 <- aov(RH2M ~ cluster, data = my_data_etude)
summary(anova_result2)

anova_result3 <- aov(PRECTOTCORR ~ cluster, data = my_data_etude)
summary(anova_result3)

anova_result4 <- aov(WS2M ~ cluster, data = my_data_etude)
summary(anova_result4)

anova_result5 <- aov(T2M_MIN~ cluster, data = my_data_etude)
summary(anova_result5)

anova_result6 <- aov(T2M_MAX ~ cluster, data = my_data_etude)
summary(anova_result6)

#anova_result7 <- aov(T2M_RANGE ~ cluster, data = my_data_etude)
#summary(anova_result7)

anova_result8<- aov(TS ~ cluster, data = my_data_etude)
summary(anova_result8)

anova_result9 <- aov(T2M ~ cluster, data = my_data_etude)
summary(anova_result9)

anova_result10 <- aov(number_day_rainy ~ cluster, data = my_data_etude)
summary(anova_result10)

anova_result11 <- aov(PS ~ cluster, data = my_data_etude)
summary(anova_result11)
anova_result12 <- aov(WS10M ~ cluster, data = my_data_etude)
summary(anova_result12)
anova_result13 <- aov(WS10M_MAX ~ cluster, data = my_data_etude)
summary(anova_result13)
anova_result14 <- aov(WD10M ~ cluster, data = my_data_etude)
summary(anova_result14)

anova_result15 <- aov(WS10M_MIN ~ cluster, data = my_data_etude)
summary(anova_result15)
#anova_result16 <- aov(WS10M_RANGE ~ cluster, data = my_data_etude)
#summary(anova_result16)

colnames(my_data_etude)
#Si la valeur p est inférieure à un seuil alpha (généralement 0,05), vous rejetez 
#l'hypothèse nulle. Cela signifie qu'il existe des différences significatives entre
#au moins deux des groupes.


# Test post-hoc de Tukey
# le test de Tukey nous permet d'identifier les paire de clusteur diffenrente
tukey_result2 <- TukeyHSD(anova_result15)
print(tukey_result2)



colnames(my_data_etude)
anova_dim1 <- aov(Dim1 ~ cluster, data = my_data_etude)
summary(anova_dim1)

anova_dim2 <- aov(Dim2 ~ cluster, data = my_data_etude)
summary(anova_dim2)
anova_dim3 <- aov(Dim3 ~ cluster, data = my_data_etude)
summary(anova_dim3)



# Test post-hoc de Tukey
# le test de Tukey nous permet d'identifier les paire de clusteur diffenrente
tukey_dim1 <- TukeyHSD(anova_dim1)
print(tukey_dim1)

tukey_dim2 <- TukeyHSD(anova_dim2)
print(tukey_dim2)


tukey_dim3 <- TukeyHSD(anova_dim3)
print(tukey_dim3)


#Le test post-hoc de Tukey (Tukey's Honestly Significant Difference, ou Tukey HSD) 


#diff : Si la valeur est positive, cela signifie que la moyenne du premier groupe est supérieure à celle du second groupe. Si elle est négative, cela signifie que la moyenne du premier groupe est inférieure à celle du second groupe.

#lwr et upr : Ces valeurs représentent les bornes de l'intervalle de confiance à 95 % pour la différence de moyenne. Si cet intervalle contient zéro, cela signifie qu'il n'y a pas de différence significative entre les deux groupes. Si l'intervalle ne contient pas zéro, cela indique une différence significative.

#p adj : Si la valeur p ajustée est inférieure à un seuil alpha (généralement 0,05), cela signifie que la différence entre les deux groupes est significative. Par exemple, si p adj < 0.05, vous pouvez conclure qu'il existe une différence significative entre les deux groupes comparés.




#===========================

library(vioplot)
library(ellipse) 
library(vioplot)
#colnames(my_data_etude)
library(ggplot2,)
# fonction multivaolon
multivioplot <- function(data,cat,screen=c(16,10),col=c(1:100)) {
  
  parameters <- colnames(data)
  
  cat("Parameters : ",parameters,"\n\n")
  
  nb_col <- length(parameters)
  
  nb_graphics <- nb_col
  
  nb_graphics_y <- floor(nb_graphics/sum(screen)*screen[2])
  
  nb_graphics_x <- ceiling(nb_graphics/nb_graphics_y)
  
  # Conversion des couleurs
  
  # Fonction de conversion des couleurs en hexadécimal
  
  col2hex <- function(col, alpha=1) rgb(t(col2rgb(col)), alpha=alpha*255, maxColorValue=255) 
  
  colors <- col2hex(col) # cf. col2rgb
  
  # Affichage
  
  layout(matrix(1:(nb_graphics_x*nb_graphics_y),nb_graphics_y,nb_graphics_x))
  
  require(vioplot)
  
  for (i in c(1:nb_col)) {
    
    plot(c(0,(length(unique(cat))+1)),c(min(data[,i]),max(data[,i])),col="white",type="n",axes=F,xlab="",ylab="")
    
    axis(2) # Ajoute l'axe des y à gauche
    
    axis(1,at=c(1:length(unique(cat))),, labels=unique(cat),xlab="") # Ajoute l'axe des x en bas
    
    mtext(parameters[i],side=3,col="black",line=1.5,font=2)
    
    grid()
    
    for (j in c(1:length(unique(cat)))) {vioplot(data[,i][cat==unique(cat)[j]],at=j,horizontal=F,add=T,col=colors[j])  }
    # Ajouter une légende après avoir tracé tous les violons
    legend("topright", legend = unique(cat), fill = colors[1:length(unique(cat))], title = "Cluster", cex = 0.8)
  }
  
}


par(mfrow = c(1, 1)) 
multivioplot(my_data_etude[,c("RH2M","T2M_MIN","T2M_MAX","WS10M")],my_data_etude$cluster,col=c("darkorange","burlywood","cyan3"),screen=c(3,3))

multivioplot(my_data_etude[,c("Dim1","Dim2","Dim3")],my_data_etude$cluster,col=c("darkorange","burlywood","cyan3"),screen=c(3,3))



multivioplot(my_data_etude[,c("PRECTOTCORR","WD10M","number_day_rainy","PS")],my_data_etude[,29],col=c("darkorange","burlywood","cyan3"),screen=c(3,3))


#====================================================================================
#====================================================================================

#=======================================================================================
#=============== fonction multiplot

multiplot <- function(data,cat,screen=c(16,10),col=c(1:100),pch=rep(16,100),grid=T,chull=F,chull.alpha = 0.5,ellipse=F,conf.ellipse=0.95) {
  
  parameters <- colnames(data)
  
  cat("Parameters",parameters,"\n\n")
  
  nb_col <- length(parameters)
  
  nb_graphics <- sum(c(1:(nb_col-1)))
  
  nb_graphics_y <- floor(nb_graphics/sum(screen)*screen[2])
  
  nb_graphics_x <- ceiling(nb_graphics/nb_graphics_y)
  
  # Fonctions pour faire les nappes convexes et les ellipses
  
  convexhull<-function(x, y, col="#00000000",border="black",lwd=lwd){
    
    hpts <- chull(x = x, y = y)
    
    hpts <- c(hpts, hpts[1])
    
    polygon(x[hpts], y[hpts],col=col,border=border,lwd=lwd)
    
  } 
  
  confellipse <-function(x=c(),y=c(),level=0.95,col,lwd=2,lty=1){
    
    # Fonction pour éviter le problème d'interférence entre librairies : detach
    
    #detach("package:car");detach("package:mixtools")
    
    # Tracer une ellipse (intervalle de confiance à 50%)
    
    xy_ell <- data.frame(x,y)
    
    require("ellipse")
    
    lines(ellipse(cov(xy_ell),centre=colMeans(xy_ell),level=level),type="l", lty=lty,lwd=lwd, col=col)
    
  }
  
  # Fonction de conversion des couleurs en hexadécimal
  
  col2hex <- function(col, alpha=1) rgb(t(col2rgb(col)), alpha=alpha*255, maxColorValue=255)
  
  # Mise en place d'une couleur par catégorie
  
  #type_col <- is(col)[1]
  
  type_pch <- is(pch)[1]
  
  #if (type_col == "character") {colors <- rep("A",length(cat))
  
  #} else {colors <-rep(1,length(cat))} #(type_col == "numeric" |"integer") 
  
  col <- col2hex(col) # cf. col2rgb
  
  colors <- rep("#000000",length(cat))
  
  if (type_pch == "character") {pchs <-rep("A",length(cat))
  
  } else {pchs <-rep(1,length(cat))}
  
  list_cat <- c(); list_col <- c() ; list_pch <- c()
  
  cpt_temp = 0
  
  # Répéter les lists col et pch si pas assez d'entrée // nombre de catégories
  
  if (nb_col > length(col)) {col = rep(col,ceiling(nb_col/length(col)))}
  
  if (nb_col > length(pch)) {pch = rep(pch,ceiling(nb_col/length(pch)))}
  
  for (i in unique(cat)) {
    
    cpt_temp <- cpt_temp+1
    
    indices <- which(cat==i)
    
    colors[indices] <- col[cpt_temp]
    
    pchs[indices] <- pch[cpt_temp]
    
    list_cat <- c(list_cat,i)
    
    list_col <- c(list_col,col[cpt_temp])
    
    list_pch <- c(list_pch,pch[cpt_temp])
    
  }
  
  compil_legend <- data.frame(list_cat,list_col,list_pch)
  
  colnames(compil_legend) <- c("Catégories","Couleurs","Pch")
  
  layout(matrix(1:(nb_graphics_x*nb_graphics_y),nb_graphics_y,nb_graphics_x))
  
  max_i <- nb_col-1
  
  for (i in c(1:max_i)) {
    
    for (j in c((i+1):nb_col)) {
      
      plot(data[,i],data[,j],xlab=parameters[i],ylab=parameters[j],col=colors,pch=pchs)
      
      if (grid==T) {grid()}
      
      if (chull ==T) {
        
        for (k in c(1:length(compil_legend[,1]))) {
          
          temp_col <- col2hex(compil_legend[k,2],alpha=chull.alpha)  
          
          convexhull(data[,i][cat==compil_legend[k,1]],data[,j][cat==compil_legend[k,1]],border=temp_col,lwd=2,col=temp_col)
          
        }
        
      }
      
      if (ellipse ==T) {
        
        for (k in c(1:length(compil_legend[,1]))) {
          
          temp_col <- col2hex(compil_legend[k,2],alpha=chull.alpha)  
          
          confellipse(data[,i][cat==compil_legend[k,1]],data[,j][cat==compil_legend[k,1]],level=conf.ellipse,lwd=2,col=temp_col)
          
        }  }  } }
  
  cat("Légende:\n")
  compil_legend}



multiplot(my_data_etude[,c("QV2M","number_day_rainy","T2M_MAX","WS10M")],my_data_etude[,91],col=c("darkorange","burlywood","cyan3"),screen=c(16,9),chull=T,chull.alpha=0.2)

multiplot(my_data_etude[,c("Dim1","Dim2","Dim3")],my_data_etude$cluster,col=c("darkorange","burlywood","cyan3"),screen=c(16,9),chull=T,chull.alpha=0.2)


#===================================Model Gam ==============================
# modele 
#Une option pour une distribution où la variance augmente plus rapidement 
#avec la moyenne est la distribution binomiale négative (ou Poisson-gamma)
#. Rappelons que la distribution binomiale négative répond à l’hypothèse selon
#laquelle la variance est proportionnelle au carré de la moyenne.
# Surdispersion?


library ( oddsratio ) 
library( mgcv ) 


#==========================================================================
my_data_etude$MONTH<-as.numeric(my_data_etude$MOIS)
my_data_etude$Year<-as.numeric(my_data_etude$ANNEE)

set.seed(123)
# Définir la plage de décalage
decalage1 <- 0:15
# Initialiser des matrices pour stocker les MSE et AIC
mse_all <- array(NA, dim = c(length(decalage1), length(decalage1), length(decalage1)))
AIC_all <- array(NA, dim = c(length(decalage1), length(decalage1), length(decalage1)))

# Initialiser un dataframe vide pour stocker les résultats
df_result <- data.frame(
  Model_Number = integer(),
  Lag_Comp1 = integer(),
  Lag_Comp2 = integer(),
  Lag_Comp3 = integer(),
  AIC = numeric(),
  MSE = numeric(),
  stringsAsFactors = FALSE
)



# boucle sur les décalages pour Dim1, Dim2 et Dim3
for (i in seq_along(decalage1)) {
  for (j in seq_along(decalage1)) {
    for (k in seq_along(decalage1)) {
      # Décaler les variables
      lagdf <-my_data_etude %>%
        group_by(DISTRICT)%>%
        mutate(var1=lag(Dim1,decalage1[i]),
               var2=lag(Dim2,decalage1[j]),
               var3=lag(Dim3,decalage1[k]))
      datalag <-na.omit(lagdf)
      # diviser en données de test et données apprentissage
      nlag <- nrow(datalag)
      tlag <- floor(0.8 * nlag) # 80 % des données
      
      # Diviser le dataframe
      trainlag <- datalag[1:tlag, ]  # Ensemble d'apprentissage (80 %)
      testlag<- datalag[(tlag + 1):nlag, ]
      
      # Ajuster le modèle GAM multiple
      modele_gam <- gam(cas_palu_confirme ~ POPULATION+offset(log(TDR_realise))+ s(MONTH,k=12,bs="cc")+s(var1, bs = "ts") + s(var2, bs = "ts") + s(var3, bs = "ts")+DISTRICT+s(Year,k=4), data =trainlag, family = nb(link = log), method = "ML")
      # Calculer les résidus et MSE
      residus <- residuals(modele_gam)
      mse_all[i, j, k] <- mean(residus^2)
      AIC_all[i, j, k] <- AIC(modele_gam)
      
      # Remplir le dataframe avec les résultats
      df_result <- rbind(df_result, data.frame(
        Model_Number = nrow(df_result) + 1,  # Numéro du modèle
        Lag_Comp1 = decalage1[i],
        Lag_Comp2 = decalage1[j],
        Lag_Comp3 = decalage1[k],
        AIC = AIC_all[i, j, k],
        MSE = mse_all[i, j, k]
      ))
    }
  }
}






# Trouver le décalage associé au plus petit AIC
min_AIC_index <- which.min(AIC_all)
best_decalage1 <- decalage1[(min_AIC_index - 1) %% length(decalage1) + 1]
best_decalage2 <- decalage1[((min_AIC_index - 1) %/% length(decalage1))%%length(decalage1) + 1]
best_decalage3 <- decalage1[(min_AIC_index - 1) %/% (length(decalage1)*length(decalage1)) + 1]

AIC_all

print(paste("Le meilleur décalage pour Dim1 est :", best_decalage1))
print(paste("Le meilleur décalage pour Dim2 est :", best_decalage2))
print(paste("Le meilleur décalage pour Dim3 est :", best_decalage3))

print(paste("L'AIC du meilleur modele est :",Aic_best_model))
Aic_best_model=AIC(best_model)
summary(best_model)

#=============================================================


lagdf <-my_data_etude %>%
  group_by(DISTRICT)%>%
  mutate(lag2_dim1=lag(Dim1,2),
         lag2_dim2=lag(Dim2,2),
         lag15_dim3=lag(Dim3,15))
datalag <-na.omit(lagdf)
# diviser en données de test et données apprentissage
nlag <- nrow(datalag)
tlag <- floor(0.8 * nlag) # 80 % des données
# Diviser le dataframe
trainlag<- datalag[1:tlag, ]  # Ensemble d'apprentissage (80 %)
testlag<- datalag[(tlag + 1):nlag, ]

# Afficher le meilleur modèle
best_model <- gam(cas_palu_confirme ~ POPULATION+offset(log(TDR_realise)) +s(MONTH,k=12,bs="cc")+
                    s(lag2_dim1, bs = "cr") + 
                    s(lag2_dim2, bs = "cr") + 
                    s(lag15_dim3, bs = "cr")+DISTRICT+s(Year,k=4,bs="cr"), 
                  data =trainlag, family = nb(link = log), method = "ML",select=TRUE)


summary(best_model)
gam.check(best_model) 


#============================Modele avec les variables qui contribue le plus ==================

#==============================================================================

set.seed(123)
# Définir la plage de décalage
decalage <- 0:15
# Initialiser des matrices pour stocker les MSE et AIC
mse_all1 <- array(NA, dim = c(length(decalage1), length(decalage1), length(decalage1)))
AIC_all1 <- array(NA, dim = c(length(decalage1), length(decalage1), length(decalage1)))

# Initialiser un dataframe vide pour stocker les résultats
df_result1 <- data.frame(
  Model_Number1 = integer(),
  Lag_RH2M = integer(),
  Lag_T2M_MIN = integer(),
  Lag_T2M_MIN = integer(),
  AIC1 = numeric(),
  MSE1 = numeric(),
  stringsAsFactors = FALSE
)



# boucle sur les décalages 
for (i in seq_along(decalage1)) {
  for (j in seq_along(decalage1)) {
    for (k in seq_along(decalage1)) {
      # Décaler les variables
      varimport<-my_data_etude %>%
        group_by(DISTRICT)%>%
        mutate(lag_RH2M=lag(RH2M,decalage1[i]),
               lag_T2M_MIN=lag(T2M_MIN,decalage1[j]),
               lag_PRECTOTCORR=lag(PRECTOTCORR,decalage1[k]))
      varimportlag <-na.omit(varimport) # 
      #diviser en données de test et données apprentissage
      nlag1 <- nrow(varimportlag)
      tlag1 <- floor(0.8 * nlag1) # 80 % des données
      trainvar <- varimportlag[1:tlag1, ]  #train set (80 %)
      testvar<- varimportlag[(tlag + 1):nlag1, ] # test set
      
      # Ajuster le modèle GAM multiple
      modele_gam_var <- gam(cas_palu_confirme ~ POPULATION+offset(log(TDR_realise))+ s(MONTH,k=12,bs="cc")+s(lag_RH2M, bs = "ts") + s( lag_T2M_MIN, bs = "ts") + s(lag_PRECTOTCORR, bs = "ts")+DISTRICT+s(Year,k=4), data = trainvar, family = nb(link = log), method = "ML")
      # Calculer les résidus et MSE
      residus1 <- residuals(modele_gam_var)
      mse_all1[i, j, k] <- mean(residus1^2)
      AIC_all1[i, j, k] <- AIC(modele_gam_var)
      
      # Remplir le dataframe avec les résultats
      df_result1 <- rbind(df_result1, data.frame(
        Model_Number1 = nrow(df_result1) + 1,
        Lag_RH2M = decalage1[i],
        Lag_T2M_MIN = decalage1[j],
        Lag_PRECTOTCORR = decalage1[k],
        AIC1 = AIC_all1[i, j, k],
        MSE1= mse_all1[i, j, k]
      ))
    }
  }
}




 

# Trouver le décalage associé au plus petit AIC
min_AIC_index1 <- which.min(AIC_all1)
best_decalage_RH2M <- decalage1[(min_AIC_index1- 1) %% length(decalage1) + 1]
best_decalage_T2M_MIN <- decalage1[((min_AIC_index1 - 1) %/% length(decalage1))%%length(decalage1) + 1]
best_decalage_PRECTOTCORR <- decalage1[(min_AIC_index1 - 1) %/% (length(decalage1)*length(decalage1)) +1]
                                  
print(paste("Le meilleur décalage pour RH2M est :", best_decalage_RH2M))
print(paste("Le meilleur décalage pour T2M_MIN est :", best_decalage_T2M_MIN))
print(paste("Le meilleur décalage pour PRECTOTCORR est :", best_decalage_PRECTOTCORR))







#=================best  modele avec les variables qui contribut le plus ====================

# model with best lag of predictors
varimport <-my_data_etude %>%
  group_by(DISTRICT)%>%
  mutate(lag4_RH2M=lag(RH2M,4),
         lag14_T2M_MIN=lag(T2M_MIN,14),
         lag15_PRECTOTCORR=lag(PRECTOTCORR,15))

varimportlag <-na.omit(varimport)


#diviser en données de test et données apprentissage
nvar <- nrow(varimportlag)
tvar <- floor(0.8 * nvar) 
trainvar <- varimportlag[1:tvar , ]  # train set (80 %)
testvar <- varimportlag[(tvar  + 1):nvar , ]# test set



# Afficher le meilleur modèle
set.seed(123) 
best_modelvar <- gam(cas_palu_confirme ~ POPULATION+offset(log(TDR_realise)) +s(MONTH,k=12,bs="cc")+
                       s(lag4_RH2M, bs = "cr") + 
                       s(lag14_T2M_MIN, bs = "cr") + 
                       s(lag15_PRECTOTCORR, bs = "cr")+DISTRICT+s(Year,k=4,bs="cr"), 
                     data =trainvar, family =nb(link = log), method = "ML",select=TRUE)


summary(best_modelvar)
gam.check(best_modelvar)
AIC(best_modelvar,best_model)


dispersion1 <- sum(residuals(best_modelvar, type = "pearson")^2) /best_modelvar$df.residual
print(dispersion1)


#============ modele avec cluster ==========================================

set.seed(123) 
model_with_interaction  <-  gam(cas_palu_confirme ~  POPULATION+offset(log(TDR_realise))+ s(lag2_dim1,bs="cr" ) +  s (lag2_dim2,bs="cr")+ s(lag15_dim3,by=cluster,k=10,bs="cr" )+
                          DISTRICT +s(MONTH,bs="cc",k=12) +s(Year,k=5,bs="cr"), data  =trainlag,family = nb(link=log),methode="ML",select=TRUE)  # modèle d'ajustement
summary.gam(model_with_interaction)

gam.check(model_with_interaction)






par(mfrow = c(2, 2))
gam.check(model_with_interaction,lwd=3,col="red",cex = 1, pch = 16,cex.axis= 1,cex.lab= 1,cex.main=1,font = 1)


# Modifier la fonction gam.check pour ajouter des couleurs personnalisées
par(mfrow = c(2, 2)) # Disposition des 4 graphiques

# QQ Plot
qqnorm(residuals(model_with_interaction, type = "deviance"), main = "QQ Plot", 
       col = "black", pch = 19)
qqline(residuals(fit_gamciq, type = "deviance"), col = "red", lwd = 3)

# Résidus vs valeurs ajustées
plot(fitted(model_with_interaction), residuals(model_with_interaction, type = "deviance"),
     main = "Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Residuals",
     col = "black", pch = 19)
abline(h = 0, col = "red", lwd = 3)

# Histogramme des résidus
hist(residuals(model_with_interaction, type = "deviance"), main = "Histogram of residuals",
     col = "red", border = "black", xlab = "Residuals", freq = TRUE)



#Valeurs ajustées vs réponse
plot(fitted(model_with_interaction), model_with_interaction$y, main = "Fitted Values vs Response",
     xlab = "Fitted Values", ylab = "Response",
     col = "black", pch = 19)




#=================================================================

# comparaison des trois modèles
#======================================================================

AIC(fit_gamciqlag, best_modelvar, best_model)

summary(model_with_interaction)$dev.expl
summary(best_modelvar)$dev.expl
summary(best_model)$dev.expl
summary(best_model)
summary(best_modelvar)
# erreur d'ajustement 

# Prédictions
fitting1 <- predict(model_with_interaction, newdata=trainlag, type="response")
fitting2 <- predict(best_modelvar, newdata=trainvar, type="response")
fitting3 <- predict(best_model, newdata=trainlag, type="response")

# RMSE
rmse_train1 <- sqrt(mean((trainlag$cas_palu_confirme - fitting1)^2))
rmse_train2 <- sqrt(mean((trainvar$cas_palu_confirme - fitting2)^2))
rmse_train3 <- sqrt(mean((trainlag$cas_palu_confirme - fitting3)^2))
rmse_train1
rmse_train2
rmse_train3





# Prédictions
pred1 <- predict(model_with_interaction, newdata=testlag, type="response")
pred2 <- predict(best_modelvar, newdata=testvar, type="response")
pred3 <- predict(best_model, newdata=testlag, type="response")

# RMSE
rmse1 <- sqrt(mean((testlag$cas_palu_confirme - pred1)^2))
rmse2 <- sqrt(mean((testvar$cas_palu_confirme - pred2)^2))
rmse3 <- sqrt(mean((testlag$cas_palu_confirme - pred3)^2))
rmse1
rmse2
rmse3


#==============================================================
#biais 

mean(best_model$y)
biais_model_with_interaction=mean(fitted(model_with_interaction)-model_with_interaction$y)
biais_model_with_interaction
biais_best_modelvar=mean(fitted(best_modelvar)-best_modelvar$y)
biais_best_modelvar

biais_best_model=mean(fitted(best_model)-best_model$y)
biais_best_model

#================================================================================
#==========================================================================
#============= calculons les risque relative pour chaque composante =========


## Calculer les OR pour les incréments de pourcentage de la distribution des prédicteurs 
## (ici : 20 %)
library("oddsratio")
X<-or_gam ( 
  data  =trainlag,  model  = model_with_interaction, pred  =  "lag2_dim1" , 
  percentage  =  10 ,  slice  =  TRUE
)
X

names(X)<-c("predictor","value1","value2","perc1","perc2","oddsratio","CI_low","CI_high")
# Créer la colonne d'intervalle
X$value1<- round(X$value1, 2)
X$value2<- round(X$value2, 2)

X <- X %>%
  mutate(intervall = paste0("[",value1,", ",value2, "[" ))  # Créer l'intervalle sous forme de chaîne



# Extraire les odds ratios et les intervalles de confiance
or_data <- data.frame(
  value1 = X$value1,
  value2=X$value2,
  perc2=X$perc2,
  perc1=X$perc1,
  odds_ratio = exp(X$oddsratio ),  # Exponentier les coefficients pour obtenir les odds ratios
  lower_ci = exp(X$CI_low),        # Exponentier les bornes inférieures
  upper_ci = exp(X$CI_high),
  intervall=X$intervall         # Exponentier les bornes supérieures
)
or_data
# Tracer les odds ratios avec ggplot2
library(ggplot2)

ggplot(or_data, aes(x =reorder(intervall, odds_ratio), y = odds_ratio)) +
  geom_point(size=3,col="orangered1") +
  #geom_line(size=1,col="orangered1")+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3,size=1) +
  #coord_flip() +  # Inverser les axes pour une meilleure lisibilité
  labs(title ="",# "Odds Ratios avec Intervalles de Confiance",
       x = "lag2_Dim1",
       y = "Odds Ratio") +
  #theme_minimal()+
  theme(#legend.position = "none",strip.text = element_text(color = "black", face = "bold"),
    axis.text.x = element_text(size = 11,color="black",angle =90, hjust = 1),  # Augmenter la taille des étiquettes de l'axe x
    axis.text.y = element_text(size = 11,color="black"),  # Augmenter la taille des étiquettes de l'axe y
    axis.title.x = element_text(size = 12,face="bold"),  # Augmenter la taille du titre de l'axe x
    axis.title.y = element_text(size = 12,face="bold"))  



ggplot(or_data, aes(x =intervall, y = odds_ratio)) +
  geom_point(size=3,col="orangered2") +
  #geom_line(size=1,col="orangered2")+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3,color="black",size=1) +
  #coord_flip() +  # Inverser les axes pour une meilleure lisibilité
  labs(title = " ",
       x = "lag2_Dim1",
       y = "Odds Ratio") +
  #theme_minimal()+
  theme(#legend.position = "none",strip.text = element_text(color = "black", face = "bold"),
    axis.text.x = element_text(size = 11,color="black",angle =90, hjust = 1),  # Augmenter la taille des étiquettes de l'axe x
    axis.text.y = element_text(size = 11,color="black"),  # Augmenter la taille des étiquettes de l'axe y
    axis.title.x = element_text(size = 12,face="bold"),  # Augmenter la taille du titre de l'axe x
    axis.title.y = element_text(size = 12,face="bold"))  




#pour la dim2

B<-or_gam ( 
  data  =trainlag,  model  = model_with_interaction ,  pred ="lag2_dim2" , 
  percentage  =  10 ,  slice  =  TRUE
)

B
names(B)<-c("predictor","value1","value2","perc1","perc2","oddsratio","CI_low","CI_high")

# Créer la colonne d'intervalle
B$value1<- round(B$value1, 2)
B$value2<- round(B$value2, 2)
B
B <- B%>%mutate(intervall = paste0("[",value1,", ",value2, "[" ))  # Créer l'intervalle sous forme de chaîne

# Extraire les odds ratios et les intervalles de confiance
or_dataB<- data.frame(
  value1 = B$value1,
  value2=B$value2,
  perc2=B$perc2,
  perc1=B$perc1,
  odds_ratio = exp(B$oddsratio ),  # Exponentier les coefficients pour obtenir les odds ratios
  lower_ci = exp(B$CI_low),        # Exponentier les bornes inférieures
  upper_ci = exp(B$CI_high),
  intervall=B$intervall)

# Exponentier les bornes supérieures


# Tracer les odds ratios avec ggplot2
library(ggplot2)
or_dataB

ggplot(or_dataB, aes(x = reorder(intervall, odds_ratio), y = odds_ratio)) +
  geom_point(size=3,col="orangered2")+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3,size=1) +
  #coord_flip() +  # Inverser les axes pour une meilleure lisibilité
  labs(title = " ",
       x = "lag2_Dim2",
       y = "Odds Ratio") +
  #theme_minimal()+
  theme(#legend.position = "none",strip.text = element_text(color = "black", face = "bold"),
    axis.text.x = element_text(size = 11,color="black",angle =90, hjust = 1),  # Augmenter la taille des étiquettes de l'axe x
    axis.text.y = element_text(size = 11,color="black"),  # Augmenter la taille des étiquettes de l'axe y
    axis.title.x = element_text(size = 12,face="bold"),  # Augmenter la taille du titre de l'axe x
    axis.title.y = element_text(size = 12,face="bold"))  



library(ggplot2)
or_dataB

ggplot(or_dataB, aes(x =intervall, y = odds_ratio)) +
  geom_point(size=3,col="red")+
  #+geom_line(size=1,col="red")+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3,size=1) +
  #coord_flip() +  # Inverser les axes pour une meilleure lisibilité
  labs(title = " ",
       x = "lag2_Dim2",
       y = "Odds Ratio") +
  # theme_minimal()+
  theme(#legend.position = "none",strip.text = element_text(color = "black", face = "bold"),
    axis.text.x = element_text(size = 11,color="black",angle =90, hjust = 1),  # Augmenter la taille des étiquettes de l'axe x
    axis.text.y = element_text(size = 11,color="black"),  # Augmenter la taille des étiquettes de l'axe y
    axis.title.x = element_text(size = 12,face="bold"),  # Augmenter la taille du titre de l'axe x
    axis.title.y = element_text(size = 12,face="bold"))  


# Odd  ration entre les passages de regime: changement climatique

or_gam(
  data = trainlag, model =model_with_interaction,
  pred = "cluster", values = c("2", "3")
)


or_gam(
  data = trainlag, model =model_with_interaction,
  pred = "cluster", values = c("1", "2")
)


or_gam(
  data = trainlag, model =model_with_interaction,
  pred = "cluster", values = c("3", "1")
)



data <- data.frame(
  cluster = c("3-1","1-2","2-3"),
  ODD_ration=c(0.98,1.00,1.01),
 
  levels= c("drop", "constant", "rise")
)
data

# Les faibles températures pendant le cluster 2 peut augementer les cas de 
# paludisme pendant la periode 3.\\
# les températures très élévéé pendant la periode 3 peuvent entrainais 
# une diminution des cas en début de saison 1 (forte transmssion)
# le dim1  decalé de 15 semaine à les meme effets pendant la periode 1 que pendant
# la periode 2.





#pour dim3=================================================



# Valeur actuelle de Dim1 pour le cluster 1


current_value_dim3 <- mean(trainlag$lag15_dim3[trainlag$cluster == "1"])  # ou une autre valeur spécifique

#Calculer les odds pour cluster = "1" et Dim3 augmenté de 20%
odds_result3 <- or_gam(
  data = trainlag, 
  model = model_with_interaction,
  pred = c("lag15_dim3"),  # Spécifiez uniquement Dim3 ici
  values = c(current_value_dim3, current_value_dim3 * 1.1),  # Valeur actuelle et augmentation de 10%
  percentage = 10,  # Pourcentage d'augmentation
  slice = TRUE
)

#Afficher les résultats
print(odds_result3)

# Créer la colonne d'intervalle
C<-odds_result3

C
names(C)<-c("predictor","value1","value2","perc1","perc2","oddsratio","CI_low","CI_high")


C$value1<- round(C$value1, 2)
C$value2<- round(C$value2, 2)

CC <- C%>%mutate(intervallC = paste0("[",value1,", ",value2, "[" ))  # Créer l'intervalle sous forme de chaîne

# Extraire les odds ratios et les intervalles de confiance
or_dataC<- data.frame(
  value1 = C$value1,
  value2=C$value2,
  perc2=C$perc2,
  perc1=C$perc1,
  odds_ratio = exp(CC$oddsratio ),  # Exponentier les coefficients pour obtenir les odds ratios
  lower_ci = exp(CC$CI_low),        # Exponentier les bornes inférieures
  upper_ci = exp(CC$CI_high),
  intervallC=CC$intervallC)



# Tracer les odds ratios avec ggplot2
library(ggplot2)
or_dataC

ggplot(or_dataC, aes(x = intervallC, y = odds_ratio)) +
  geom_point(size=3,col="orangered3") +
  #geom_line(size=1,col="red")+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3,size=1) +
  #coord_flip() +  # Inverser les axes pour une meilleure lisibilité
  labs(title = " ",
       x = "lag15_Dim3",
       y = "Odds Ratio") +
  #theme_minimal()+
  theme(#legend.position = "none",strip.text = element_text(color = "black", face = "bold"),
    axis.text.x = element_text(size = 11,color="black",angle =90, hjust = 1),  # Augmenter la taille des étiquettes de l'axe x
    axis.text.y = element_text(size = 11,color="black"),  # Augmenter la taille des étiquettes de l'axe y
    axis.title.x = element_text(size = 12,face="bold"),  # Augmenter la taille du titre de l'axe x
    axis.title.y = element_text(size = 12,face="bold"))  


#=================================================================
#=================================================================



#=================================================================

#============================ traçons les modèles===============================

#Visualiser les effets spécifiques de Dim3 par cluster
summary(model_with_interaction)

# les tracer separements

# Tracer la courbe principale pour st1
plot(st1$Dim1, st1$fit, type = "l", col ="black", lwd = 4, ylim = y_limits,
     main = " ", 
     xlab = "Dim1", ylab = "Derivatives",
     cex.axis = 1, cex.lab = 1, cex.main = 1, font = 2)



# Ajouter la bande de l'intervalle de confiance pour st2
polygon(c(st2$Dim3, rev(st2$Dim3)), 
        c(y_upper2, rev(y_lower2)), 
        col = "orange", border = NA)
lines(st2$Dim3, st2$fit, col = "black", lwd = 3)

lines(st2$Dim3, y_lower2, col = "red", lty = 2, lwd = 2) # lty = 2 pour pointillés
lines(st2$Dim3, y_upper2, col = "red", lty = 2, lwd = 2)


#======================================================================

#======================================================================


# Tracer la courbe principale pour st1
plot(st1$Dim1, st1$fit, type = "l", col ="black", lwd = 4, ylim = y_limits,
     main = " ", 
     xlab = "Dim1", ylab = "Derivatives",
     cex.axis = 1, cex.lab = 1, cex.main = 1, font = 2)


# Ajouter la bande de l'intervalle de confiance pour st1
polygon(c(st1$Dim1, rev(st1$Dim1)), 
        c(y_upper1, rev(y_lower1)), 
        col ="darkcyan", border = NA)

# Ajouter la courbe principale pour st1
lines(st1$Dim1, st1$fit, col = "black", lwd = 3)


lines(st1$Dim1, y_lower1, col = "red", lty = 2, lwd = 2) # lty = 2 pour pointillés
lines(st1$Dim1, y_upper1, col = "red", lty = 2, lwd = 2)






#=======================  dim1 ==========================
# Get model term data:
st <- get_modelterm(model_with_interaction, select=1)

summary(model_with_interaction)


y_lower<-st$fit - 1.96 * st$se.fit
y_upper<-st$fit + 1.96 * st$se.fit


# Tracer la courbe principale pour st1
plot(st$lag2_dim1, st$fit, type = "l", col ="black", lwd = 4, ylim = range(c(y_lower, y_upper)),
     main = " ", 
     xlab = "lag2_Dim1", ylab = "s(lag2_Dim1)",
     cex.axis = 1, cex.lab = 1, cex.main = 1, font = 1,font.lab=2)

# Ajouter la bande de l'intervalle de confiance pour st1
polygon(c(st$lag2_dim1, rev(st$lag2_dim1)), 
        c(y_upper, rev(y_lower)), 
        col ="gray", border = NA)

# Ajouter la courbe principale pour st1
lines(st$lag2_dim1, st$fit, col = "orangered", lwd = 3)


lines(st$lag2_dim1, y_lower, col = "black", lty = 2, lwd = 2) 
lines(st$lag2_dim1, y_upper, col = "black", lty = 2, lwd = 2)

#=================== dim2==================================


st0 <- get_modelterm(model_with_interaction, select=2)
summary(model_with_interaction)


y_lower0 <-  st0$fit - 1.96 * st0$se.fit
y_upper0 <-st0$fit + 1.96 * st0$se.fit

# Tracer la courbe principale pour st0
plot(st0$lag2_dim2, st0$fit, type = "l", col ="black", lwd = 4, ylim = range(c(y_lower0, y_upper0)),
     main = " ", 
     xlab = "lag2_Dim2", ylab = "s(lag2_Dim2)",
     cex.axis = 1, cex.lab = 1, cex.main = 1, font = 1,font.lab=2)

# Ajouter la bande de l'intervalle de confiance pour st1
polygon(c(st0$lag2_dim2, rev(st0$lag2_dim2)), 
        c(y_upper0, rev(y_lower0)), 
        col ="gray", border = NA)

# Ajouter la courbe principale pour st1
lines(st0$lag2_dim2, st0$fit, col = "orangered", lwd = 3)


lines(st0$lag2_dim2, y_lower0, col = "black", lty = 2, lwd = 2) 
lines(st0$lag2_dim2, y_upper0, col = "black", lty = 2, lwd = 2)

#=============================== dim3=======================================

# Get model term data:


st3<- get_modelterm(model_with_interaction, select=3)


y_lower3<-st3$fit - 1.96 * st3$se.fit
y_upper3<-st3$fit + 1.96 * st3$se.fit


# Tracer la courbe principale pour st3
plot(st3$lag15_dim3, st3$fit, type = "l", col ="black", lwd = 4, ylim = range(c(y_lower3, y_upper3)),
     main = " ", 
     xlab = "lag15_Dim3,cluster1", ylab = "s(lag15_dim3,cluster1)",
     cex.axis = 1, cex.lab = 1, cex.main = 1, font = 1,font.lab=2)


# Ajouter la bande de l'intervalle de confiance pour st1
polygon(c(st3$lag15_dim3, rev(st3$lag15_dim3)), 
        c(y_upper3, rev(y_lower3)), 
        col ="gray", border = NA)

# Ajouter la courbe principale pour st1
lines(st3$lag15_dim3, st3$fit, col = "orangered", lwd = 3)


lines(st3$lag15_dim3, y_lower3, col = "black", lty = 2, lwd = 2) # lty = 2 pour pointillés
lines(st3$lag15_dim3, y_upper3, col = "black", lty = 2, lwd = 2)




#================================= saisonnality==================================
st6 <- get_modelterm(model_with_interaction, select=6)
summary(model_with_interaction)
y_lower6<-st6$fit - 1.96 * st6$se.fit
y_upper6<-st6$fit + 1.96 * st6$se.fit

# Tracer la courbe principale pour st1
plot(st6$MONTH, st6$fit, type = "l", col ="black", lwd = 4, ylim = range(c(y_lower6, y_upper6)),
     main = " ", 
     xlab = "MONTH", ylab = "s(MONTH)",
     cex.axis = 1, cex.lab = 1, cex.main = 1, font = 1,font.lab=2)


# Ajouter la bande de l'intervalle de confiance pour st1
polygon(c(st6$MONTH, rev(st6$MONTH)), 
        c(y_upper6, rev(y_lower6)), 
        col ="gray", border = NA)

# Ajouter la courbe principale pour st1
lines(st6$MONTH, st6$fit, col = "orangered", lwd = 3)


lines(st6$MONTH, y_lower6, col = "black", lty = 2, lwd = 2) # lty = 2 pour pointillés
lines(st6$MONTH, y_upper6, col = "black", lty = 2, lwd = 2)


#================================= trend ==================================
st7 <- get_modelterm(model_with_interaction, select=7)
y_lower7<-st7$fit - 1.96 * st7$se.fit
y_upper7<-st7$fit + 1.96 * st7$se.fit

# Tracer la courbe principale pour st1
plot(st7$Year, st7$fit, type = "l", col ="black", lwd = 4, ylim = range(c(y_lower7, y_upper7)),
     main = " ", 
     xlab = "Year", ylab = "s(Year)",
     cex.axis = 1, cex.lab = 1, cex.main = 1, font = 1,font.lab=2)


# Ajouter la bande de l'intervalle de confiance pour st1
polygon(c(st7$Year, rev(st7$Year)), 
        c(y_upper7, rev(y_lower7)), 
        col ="gray", border = NA)

# Ajouter la courbe principale pour st1
lines(st7$Year, st7$fit, col = "orangered", lwd = 3)


lines(st7$Year, y_lower7, col = "black", lty = 2, lwd = 2) # lty = 2 pour pointillés
lines(st7$Year, y_upper7, col = "black", lty = 2, lwd = 2)





#==============================================================
#  Diagnosis model=============================================
par(mfrow = c(1, 2))
acf(resid(model_with_interaction), lag.max = 36, main = "ACF",lwd=3)
pacf(resid(model_with_interaction), lag.max = 36, main = "PACF",lwd=3)

summary(model_with_interaction)$s.table

#======================================================================
# Prediction
# ========================================================


kedougoutest<-testlag[testlag$DISTRICT=="Kedougou", ]
salematatest<-testlag[testlag$DISTRICT=="Salemata", ]
sarayatest<-testlag[testlag$DISTRICT=="Saraya", ]
Dianketest<-testlag[testlag$DISTRICT=="Dianke Makha", ]

kedougou_train<-trainlag[trainlag$DISTRICT=="Kedougou", ]
salemata_train<-trainlag[trainlag$DISTRICT=="Salemata", ]
saraya_train<-trainlag[trainlag$DISTRICT=="Saraya", ]
Dianke_train<-trainlag[trainlag$DISTRICT=="Dianke Makha", ]


predict_fit_1 <- predict(model_with_interaction, newdata =kedougou_train)

kedougou_train$predicte1<-predict_fit_1
kedougoutest$predicte1<-predict(model_with_interaction, newdata =kedougoutest)


# Créer les dates pour l'axe avec un espacement de 2 semaines
dates_axe <- seq(min(kedougoutest$date), max(kedougoutest$date), by = "2 weeks")

# Tracer les données
par(mfrow = c(1, 1))
plot(kedougoutest$date, kedougoutest$cas_palu_confirme, col = "black", type = "l", 
     xaxt = "n", xlab = " ", ylab = "Malaria cases", lwd = 5, cex.lab = 1, 
     ylim = c(0, 600), font = 1, col.axis = "black", las = 2, cex.axis = 0.8)

# Ajouter les lignes des prédictions
lines(kedougoutest$date, exp(kedougoutest$predicte1), col = "orangered1", type = "l", lwd = 5)

# Ajouter la légende
legend("topleft", legend = c("Reported cases", "Predicted cases"), 
       col = c("black", "orangered1"), lty = 1, cex = 0.8, lwd = 2)





#Ajouter l'axe des dates avec le format semaine-année
axis(1, at = dates_axe, labels = format(dates_axe, "W%V %Y"), las = 2, cex.axis = 0.75, font = 1, col = "black")

# Ajouter un titre à l'axe des X
mtext("Week", side = 1, line = 4.1, col = "black", cex = 0.8)

# Encadrer le graphique
box(lwd = 2)

#Ajouter un titre
mtext("Kedougou", line = 1, col = "black", cex = 1.1)



#===================================================================
#===================== salemata =====================================


salemata_train$fitting<- predict(model_with_interaction, newdata =salemata_train)

salematatest$predict<-predict(model_with_interaction, newdata =salematatest)


#Créer les dates pour l'axe avec un espacement de 2 semaines
dates_axe <- seq(min(salematatest$date), max(salematatest$date), by = "2 weeks")

# Tracer les données
par(mfrow = c(1, 1))
plot(salematatest$date, salematatest$cas_palu_confirme, col = "black", type = "l", 
     xaxt = "n", xlab = " ", ylab = "Malaria cases", lwd = 5, cex.lab = 1, 
     ylim = c(0, 500), font = 1, col.axis = "black", las = 2, cex.axis = 0.8)

#Ajouter les lignes des prédictions
lines(salematatest$date, exp(salematatest$predict), col = "orangered1", type = "l", lwd = 5)

# Ajouter la légende
legend("topleft", legend = c("Reported cases", "Predicted cases"), 
       col = c("black", "orangered1"), lty = 1, cex = 0.8, lwd = 2)

#Ajouter l'axe des dates avec le format semaine-année
axis(1, at = dates_axe, labels = format(dates_axe, "W%V %Y"), las = 2, cex.axis = 0.75, font = 1, col = "black")

# Ajouter un titre à l'axe des X
mtext("Week", side = 1, line = 4.1, col = "black", cex = 0.8)

# Encadrer le graphique
box(lwd = 2)

#Ajouter un titre
mtext("Salemata", line = 1, col = "black", cex = 1.1)





#===================================================================
#===================== saraya =====================================


saraya_train$fitting <- predict(model_with_interaction, newdata =saraya_train)
sarayatest$predict<-predict(model_with_interaction, newdata =sarayatest)

# Créer les dates pour l'axe avec un espacement de 2 semaines
dates_axe <- seq(min(sarayatest$date), max(sarayatest$date), by = "2 weeks")

# Tracer les données
par(mfrow = c(1, 1))
plot(sarayatest$date, sarayatest$cas_palu_confirme, col = "black", type = "l", 
     xaxt = "n", xlab = " ", ylab = "Malaria cases", lwd = 5, cex.lab = 1, 
     ylim = c(0, 2500), font = 1, col.axis = "black", las = 2, cex.axis = 0.8)

# Ajouter les lignes des prédictions
lines(sarayatest$date, exp(sarayatest$predict), col = "orangered1", type = "l", lwd = 5)

#Ajouter la légende
legend("topleft", legend = c("Reported cases", "Predicted cases"), 
       col = c("black", "orangered1"), lty = 1, cex = 0.8, lwd = 2)



#Ajouter l'axe des dates avec le format semaine-année
axis(1, at = dates_axe, labels = format(dates_axe, "W%V %Y"), las = 2, cex.axis = 0.75, font = 1, col = "black")

# Ajouter un titre à l'axe des X
mtext("Week", side = 1, line = 4.1, col = "black", cex = 0.8)

# Encadrer le graphique
box(lwd = 2)

#Ajouter un titre
mtext("Saraya", line = 1, col = "black", cex = 1.1)


#===================================================================
#===================== Dianke =====================================


Dianke_train$fitting<- predict(model_with_interaction, newdata =Dianke_train)

Dianketest$predict<-predict(model_with_interaction, newdata =Dianketest)

# Créer les dates pour l'axe avec un espacement de 2 semaines
dates_axe <- seq(min(Dianketest$date), max(Dianketest$date), by = "2 weeks")

# Tracer les données
par(mfrow = c(1, 1))
plot(Dianketest$date, Dianketest$cas_palu_confirme, col = "black", type = "l", 
     xaxt = "n", xlab = " ", ylab = "Malaria cases", lwd = 5, cex.lab = 1, 
     ylim = c(0, 1000), font = 1, col.axis = "black", las = 2, cex.axis = 0.8)

# Ajouter les lignes des prédictions
lines(Dianketest$date, exp(Dianketest$predict), col = "orangered1", type = "l", lwd = 5)

# Ajouter la légende
legend("topleft", legend = c("Reported cases", "Predicted cases"), 
       col = c("black", "orangered1"), lty = 1, cex = 0.8, lwd = 2)



#Ajouter l'axe des dates avec le format semaine-année
axis(1, at = dates_axe, labels = format(dates_axe, "W%V %Y"), las = 2, cex.axis = 0.75, font = 1, col = "black")

# Ajouter un titre à l'axe des X
mtext("Week", side = 1, line = 4.1, col = "black", cex = 0.8)

# Encadrer le graphique
box(lwd = 2)

#Ajouter un titre
mtext("Dianke Makha", line = 1, col = "black", cex = 1.1)


#========================================================

# Exporter le DataFrame en CSV sur le bureau
write.csv(my_data_base_palu, file = "C:/Users/USER/Desktop/document_article_1/data_base.csv", row.names = FALSE)






