#--------------- Part 3... Manipulation des Dates, Creation des graphiques et manipulation des chaines de caracteres (strings)
View(jointure_v_c)

#Relation Visuelle, relation entre la Quantite et le Price 
#Est ce qu'il y a nue relation entre les deux variables ? #Ainsi nous avons besoin de faire un nuage des points 
#axe X Quantite et axe Y Prix, car c'est le prix qu'on veut expliquer par rapport 

#utilisation d'une fonction native de R (plot) et apres on peut toute fois utiliser ggplot pour plus de graphique personnalisé 

?plot #pour plus d'informations sur plot, voir cette petite doc 

plot(x = jointure_v_c$Quantity, y= jointure_v_c$Price, xlab = "Quantity", ylab = "Price", main = "Relation du prix en fonction de la Quantite", col="blue")
#pas vraiment de correlation, car mm si la quantite de produits augemente, ca n'influence en rien le prix, ce sont deux elements distincs
install.packages("ggplot2")
library(ggplot2)

?ggplot2
ggplot() #pour preparer une zone de graphique vide, prete to receive datas
#Elements important dans le ggplot, Jeu de donnees (data), aes(esthetique des donnes, colonnes qu'on veut afficher sur le graphique),
#couches (layers), types de graphique qu'on veut afficher (ex un nuage de points) on met toujours + avant de preciser les couches 
#Personnalisation + ajout des lements (legend, labs), facet_wrap() pour une categorisation separee et groupee selon un critere 
#scales color pour les echelles de couleur selon les varaibles que nous avons

#Graphique sans facet_wrap()
#relation entre Quantite et Prix avec un nuage des points (geom_points)
#Graphique pour variables Quantitatives 
ggplot(data = jointure_v_c, aes(x = Quantity, y = Price)) + 
  geom_point(size=3) +
  labs(x= "Quantite", y= "Prix", title = "Prix en fonction de la Quantity") 
  
  
#on souhaite une categorisation par CustomerCategory 

ggplot(data = jointure_v_c, aes(x = Quantity, y = Price, fill = CustomerCategory)) +
  geom_point(size = 3) +
  labs(x = "Qte", y = "Price", title ="Price By Qte With Customer Category") +
  facet_wrap(~CustomerCategory)
  
 

#Reprensenter l'histogramme des prix (histogramme pour des variables quantitatives)
#native 
hist(jointure_v_c$Price)
#avec ggplot 
ggplot(data = jointure_v_c, aes(Price)) +
  geom_histogram(bins = 40, color = "purple", fill = "purple")
  
#possible d'ajouter le aes dans la partie fill pour colorer sur base d'une autre variable 
ggplot(data = jointure_v_c, aes(Price))+
  geom_histogram(bins = 40, color = "purple", aes(fill = CustomerCategory))
  


#BoxPlot pour voir la mediane, les valeurs abberantes, extremes , etc... 
#native 
?boxplot()
boxplot(Price ~ CustomerCategory, data = jointure_v_c, col = "lightgray")


#utilisation de GGPLot de notre prix par rapport a category 

ggplot(data = jointure_v_c, aes(x = CustomerCategory, y = Price)) + 
  geom_boxplot(aes(fill = CustomerCategory )) +
  scale_fill_manual(values = c("blue", "green", "orange", "purple"))
  
  
  

#Graphique pour variables Qualitatives
#Rpartition d'age 
#Diagramme en barre 
#native 

#ggplot 
ggplot(data = jointure_v_c, aes(AgeGroup)) + 
  geom_bar(aes(fill = AgeGroup))
  
#Les time series et les Strings data Type en R 
#Les dates, utilisation la library lubridate 

View(jointure_v_c)
#verifier si la date est une date ou une chaine de caractere, alors transformer les variables date en vraie Date
str(jointure_v_c) #pour l'instant c'est une chaine des caracteres
#vivement la changer en data pour etre exploitable 
install.packages("lubridate")
install.packages("stringr")

library(lubridate)
library(stringr)
#creer une colonne qui prendra le nouveau format avant la transformation en Date 
jointure_v_c$before_date <- dmy_hm(jointure_v_c$Date)
str(jointure_v_c)
#changer le before Date au format final Date et sera la vairable a utiliser 
jointure_v_c$Date_Achat <- as.Date(jointure_v_c$before_date)

str(jointure_v_c)
#Extraire le mois 
jointure_v_c$Mois_Achat <- month(jointure_v_c$Date_Achat)
#jour
jointure_v_c$Jour_Achat <- day(jointure_v_c$Date_Achat)
#annee 
jointure_v_c$Anne_Achat <- year(jointure_v_c$Date_Achat)

View(jointure_v_c)
#utilisation de chaines de caracteres 
ma_chaine1 <- "Bienvenue sur le R World"
ma_chaine2 <- "2024 Fire"
str_c(ma_chaine1," - ",ma_chaine2) #Concatenate #Important pour concater deux variables
str_length(ma_chaine)

#tester le montant total en fonction de Annees 

ggplot(data = jointure_v_c, aes(x = Date_Achat, y = Montant_Total))+
  geom_line()

