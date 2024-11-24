#Projet réalisé sous:
#R version 4.4.1 (2024-06-14 ucrt)
#Platform: x86_64-w64-mingw32/x64
#Running under: Windows 11 x64 (build 22631)

##############################################
# Chargement des packages nécessaires
##############################################
library(dplyr)
library(tidyr)

##############################################
#IMPORTATION ET PREPARATION
##############################################
# Il s'agit initialement du jeu de données REPHY_Med_1987-2022. Des modifications sur LibreOffice Calc ont été 
#effectuées avant l'importation sur Rstudio:
#              - les colonnes qui ne servaient pas pour le projet ont été supprimées
#              - suite à un problème d'encodage, les accents n'apparaissaient pas. le problème
#              a été résolu en utilisant la fonction "chercher-remplacer"
#              - les données ne datant pas 2002 ou 2022 ont été supprimées
#              - le nom des colonnes a été changés pour être plus court et explicite
#              - les lignes vides pour la colonne de l'identification ont été remplis par la chaîne de 
#              caractères "rien" pour en faciliter l'identification future 
# Le jeu de données nouvellement formé a été nommé "traite"

traite <- read.csv("~/qgis_master/projet_REPHY/traite.csv")#importation du jeu de données
taxon<- subset(traite, tax_ref!='rien ') #création d'un sous-tableau avec toutes les lignes contenant une information
                                         #taxonomique

taxon$tax_ref <- sub(" .*", "", taxon$tax_ref)#opération pour conserver uniquement les noms de genre, sans les
                                              #espèces
taxon$date <- as.Date(taxon$date, format = "%d/%m/%Y")#convertissage de la colonne des dates en format "Date"
                                                      #reconnu par R


taxon$jour <- format(taxon$date, "%d")  # Création de trois colonnes subdivisant la date en jour, mois et année
taxon$mois <- format(taxon$date, "%m")  
taxon$annee <- format(taxon$date, "%Y")  

#####################################################
#CREATION DES TABLEAUX D'ABONDANCE ET DE REPARTITION
#####################################################
taxon2002<-subset(taxon, annee==2002) #création des deux sous-tableaux: un avec les observations de 2002, l'autre
taxon2022<-subset(taxon, annee==2022) #avec les observations de 2022

ez2002<-taxon2002[,c(2,3,7)]          #pour les deux tableaux on supprime les colonnes inutiles ainsi que toutes les        
ez2002<-subset(ez2002, val_mes !=0 )  #pour lesquelles la valeur mesurée est nulle.

ez2022<-taxon2022[,c(2,3,7)]
ez2022<-subset(ez2022, val_mes !=0 )



##########################################################
#SOUS-TABLEAUX DES MOYENNES PONDEREES D'ABONDANCE TOTALE
##########################################################
ez2002 <- ez2002 %>%                                          #création de tableaux qui pour chaque paire de coordonées
  mutate(val_mes = as.numeric(val_mes))                       #calcule la moyenne pondérée de l'abondance. C'est à dire la
result <- ez2002 %>%                                          #somme de toutes abondances, quelle que soit l'espèce, pour
  group_by(coord_x, coord_y) %>%                              #chaque paire unique de coordonées, diviser par le nombre d'observations
  summarise(val_mes = sum(val_mes) / n(), .groups = "drop")   #utilisées pour réaliser la somme. Les regroupements formés
                                                              #sont supprimés après le summarise
ez2022 <- ez2022 %>%
  mutate(val_mes = as.numeric(val_mes))
result2 <- ez2022 %>%
  group_by(coord_x, coord_y) %>%
  summarise(val_mes = sum(val_mes) / n(), .groups = "drop")

##############################################
#SOUS-TABLEAUX DES PARTS DES TAXONS
##############################################
part2002<-taxon2002[,c(2,3,6,7)]        #création de tableaux simplifiés avec uniquement les coordonnées
part2022<-taxon2022[,c(2,3,6,7)]        #le genre et l'abondance. Les lignes où les valeurs d'abondance
                                        #sont nulles ont été supprimées
part2022<-subset(part2022, val_mes !=0)
part2002<-subset(part2002, val_mes !=0)

#2002
part2002$val_mes <- as.numeric(part2002$val_mes)#convertissage de la valeur mesurée de 'string' à numérique

summary_table <- part2002 %>%
  group_by(coord_x, coord_y, tax_ref) %>% #Les données sont regroupées par triplets uniques de colonnes (coord x et y, taxons)
  summarise(total_val = sum(val_mes), .groups = "drop") %>% #somme des valeurs val_mes et enregistrement dans nouvelle colonne total_val
  group_by(coord_x, coord_y) %>%
  mutate(total_coord = sum(total_val),  # Somme pour les mêmes paires de coordonnées
         percentage = (total_val / total_coord) * 100) %>% # Part du taxon en %
  select(coord_x, coord_y, tax_ref, percentage) %>%  # Garde uniquement les colonnes utiles
  pivot_wider(names_from = tax_ref, values_from = percentage, values_fill = 0) # Passage des taxons en colonne

print(summary_table)# Résultat final avec les coordonnées et la part d'abondance représentée par chaque taxon pour
                    #chaque paire de coordonées uniques

#2022
part2022$val_mes <- as.numeric(part2022$val_mes)             #répétition des opéarations pour l'année 2022

summary_table2 <- part2022 %>%
  group_by(coord_x, coord_y, tax_ref) %>%
  summarise(total_val = sum(val_mes), .groups = "drop") %>%
  group_by(coord_x, coord_y) %>%
  mutate(total_coord = sum(total_val),
         percentage = (total_val / total_coord) * 100) %>% 
  select(coord_x, coord_y, tax_ref, percentage) %>% 
  pivot_wider(names_from = tax_ref, values_from = percentage, values_fill = 0)

############################################
# EXPORTATION DES TABLEAUX EN CSV
#############################################

write.csv(summary_table, file = "summary_table.csv", row.names = FALSE)
write.csv(summary_table2, file = "summary_table2.csv", row.names = FALSE)
write.csv(result, file = "result", row.names = FALSE)
write.csv(result2, file = "result2", row.names = FALSE)

#############################
# OPERATIONS COMPLEMENTAIRES
#############################
#Pour simplifier les cartes au moment du passage sur QGIS, les tableaux ont été modifiées sur LibreOffice Calc.
#Pour chaque paire de coordonnées les deux espèces les plus représentées et le pourcentage associé ont été ajoutés
#aux coordonnées.

###########################################
#SOUS-TABLEAUX DES ESPECES TOXIQUES
############################################
traite$date <- as.Date(traite$date, format = "%d/%m/%Y") #répétion des opérations des lignes 31-39
traite$jour <- format(traite$date, "%d")  
traite$mois <- format(traite$date, "%m")
traite$annee <- format(traite$date, "%Y")  
traite$tax_ref <- sub(" .*", "", traite$tax_ref)

juaou<-subset(traite, mois == '06' ) #on garde uniquement les données de juin 2022
juaou<-subset(juaou, annee=='2022')


juaou<-subset(juaou,tax_ref=='Alexandrium' | tax_ref=='Dinophysis' | tax_ref=='Pseudo-nitzchia' 
              | tax_ref=='rien'| tax_ref=='Ostreopsis') #on garde uniquement les lignes avec des espèces toxiques
                                                        #ou 'rien' qui signifie qu'un paramètre a été mesuré

double<-subset(juaou,tax_ref!='rien')#création d'un tableau avec toutes les lignes contenant un taxon
double<-subset(double,val_mes!=0)    #et une valeur mesurée non-nulle


double <- double %>%                    #création d'un mini tableau pour voir quelle est le genre la plus présent
  mutate(val_mes = as.numeric(val_mes)) #pour chaque paire de coordonnées
cory <- double %>%
  group_by(coord_x, coord_y, tax_ref) %>%
  summarise(mean_val_mes = mean(val_mes), .groups = "drop")



coordx<-unique(double$coord_x)
coordy<-unique(double$coord_y)
espe_domi<-c('Alexandrium','Alexandrium',rep('Dinophysis',8))
tab_final<-data.frame(coordx,coordy,espe_domi) #création d'un tableau avec les paires de cordonnées unique ainsi que l'espèce
                                               #toxique la plus abondante pour cette paire



sous_tab<-subset(juaou,parametre=="Nitrate + nitrite") #extraction des données de nitrates et de phosphate pour toutes
sous_tab<-sous_tab[,c(3,7)]                            #les lignes
sous_tab2<-subset(juaou,parametre=="Phosphate")
sous_tab2<-sous_tab2[,c(3,7)]

sous_tab <- subset(juaou, parametre == "Nitrate + nitrite") 
sous_tab2 <- subset(juaou, parametre == "Phosphate")#Extraire les lignes de où le`parametre` est égale à "Phosphate" ou
                                                    #"Nitrate + nitrite"

sous_tab <- sous_tab[, c(3, 7)]  
sous_tab2 <- sous_tab2[, c(3, 7)] #suppression des colonnes inutiles

paires_uniques <- double %>%# Obtention des paires uniques de coordonnées
  distinct(coord_x, coord_y)

sous_tab <- sous_tab %>%
  mutate(val_mes = as.numeric(gsub(",", ".", val_mes)))# Convertir la colonne `val_mes` de `sous_tab` en numérique
sous_tab2 <- sous_tab2 %>%
  mutate(val_mes = as.numeric(gsub(",", ".", val_mes)))

resain <- sous_tab %>%
  group_by(coord_y) %>%  # Regroupement les données par `coord_y`
  summarise(mean_val_mes = mean(val_mes, na.rm = TRUE), .groups = "drop")  # Calcul de lla moyenne en ignorant les NA
resdeux <- sous_tab2 %>%
  group_by(coord_y) %>% 
  summarise(mean_val_mes = mean(val_mes, na.rm = TRUE), .groups = "drop") 


fusion1 <- merge(tab_final, resdeux, by.x = "coordy", by.y = "coord_y") #fusion des 3 différents tableaux
fusion2 <- merge(fusion1, resain, by.x = "coordy", by.y = "coord_y")
colnames(fusion2)<-c('coordy','coordx','espe_domi',"Phosphate","Nitrate + nitrite")#renommage des colonnes plus explicite

write.csv(fusion2, file = "toxique.csv", row.names = FALSE)#exportation du jeu de données final au format csv







