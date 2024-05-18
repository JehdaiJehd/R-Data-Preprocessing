#importer un fichier avec R 
#csv ?read.csv
#lien_emplacement du fichier: C:\Users\MERCYCORPS\OneDrive - mercycorps.org\Desktop\R practice\Datasets\Part 2 Data Manipulation\Base_Clients.csv

#toujours mieux de stocker les data dans une variable 

?read.csv

clients_df <- read.csv("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/Desktop/R practice/Datasets/Part 2 Data Manipulation/Base_Clients.csv",sep=";", dec = ".")

#toujours utiliser le / au lieu de \, ou bien aller dans le option de Package MAIN FOLDER dans options du RProfile et changer / en \
#voir les donnees 

#importation de la base sde donnees Vente
ventes_df <- read.csv("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/Desktop/R practice/Datasets/Part 2 Data Manipulation/Base_Ventes.csv", sep=";", dec=".")

View(clients_df)
View(ventes_df)

#installation du package pour lire les fichiers ou bdd excel 
install.packages("readxl")
#importer la librairie de chargement des fichiers Excel 
library(readxl)

test_excel <- read_excel("C:/Users/MERCYCORPS/OneDrive - mercycorps.org/Desktop/R practice/Datasets/Part 2 Data Manipulation/Base_Ventes.xlsx")

#Demarches du data analyst:

#1. Check la base des donnees
str(clients_df)

#Voir les 10 premieres lignes de la table clients 

head(clients_df, 10)

tail(clients_df, 5) #5 dernieres 

nrow(clients_df) #nombre observations
ncol(clients_df) #nombre colones 
dim(clients_df) #nobre LiCo

colnames(clients_df) #noms des colonnes 
colnames(ventes_df)

#avoir une description globale des toutes les donnees de ma BDD 
summary(ventes_df)
View(clients_df)
#Pour client, essayons de treansformer les AgeGroup et Education en Facteurs pour les categoriser en nombre 

clients_df$AgeGroup <- as.factor(clients_df$AgeGroup)

summary(clients_df)

#factoriser ou categoriser la variable EducationLevel 
clients_df$EducationLevel <- as.factor(clients_df$EducationLevel)

#1.2 Valeurs manquantes 
#nombre des valeurs manquantes sur la base de donnees 
is.na(clients_df$AgeGroup)

sum(is.na(clients_df$AgeGroup)) #0 pour dire pas de valeurs manquantes
#tett missing values on ventes table 


is.na(ventes_df) #ramene la situation sur toutes les colonnes 
sum(is.na(ventes_df))
#savoir exactement le nombre des valeurs manquantes par colonne pour mieux s'y prendre 
colSums(is.na(ventes_df))
rowSums(is.na(ventes_df)) 
#duplication de la base des donnees avant le remplacement des valeurs manquantes par 0
vente_bis <- ventes_df

vente_bis
#remplacement des valeurs manquantes par 0 au niveau de la colonne pricve
vente_bis$Price[is.na(vente_bis$Price)] <- 0
#verification 
sum(is.na(vente_bis$Price))

#supprimer les lignes des valeurs manquantes et stocker la nouvelle bdd dans vcentes_c 
ventes_c <- na.omit(ventes_df)
#renommer une colonne 
#installer d'abord le package deeplyr 
install.packages("dplyr")
library(dplyr)
rename(clients_df, Nom = Name)




#ou bien renommer avec une autre methode 
#pas trop important 



#2. Staitsituqes preliminaires 
summary(clients_df)
#Les groupes d'ages de la base des donnees unique
unique(clients_df$AgeGroup)



#nombre individus par groupe d'age, mieux de renommer apres chaque etape, renommer et stocker le result dans une variable pour ne pas anhiler la source 
cateforie_age_clients <- table(clients_df$AgeGroup)



cateforie_age_clients
#pourcentage par groupe d'age 
prop.table(cateforie_age_clients)

#3. Faire des calcus dans R 
#creer une nouvelle colonne qui calcule le prix total (Quantite * Price)

ventes_backup <- ventes_df #backup de la bdd principale 
#on cree et on calcule une colonne qui n'existe pas

ventes_df$Montant_Total <- ventes_df$Quantity * ventes_df$Price
#en python 
#vente_bis['Montant Total'] = vente_bis['Price'] * vente_bis['Quantity']

View(ventes_df)
#voir la somme du montant total pour avoir le chiffre d'affaire 
c_a <- sum(ventes_df$Montant_Total, na.rm= TRUE) #na.rm=TRUE, on supprime toutes les val manquantes NA


c_a
#petit exemple, recevoir les produits dont le prix est >40
duplicate_table <-ventes_df

liste_produits_prix_40 <- duplicate_table[duplicate_table$Price > 40,] #pas de condition sur les colonnes, on les affiche toutes
#[lignes, colonnes]
#[condition sur ligne, condition sur colonnes] -> question des valeurs 
#liste_produits_prix_inf40 <- database[database$prix < 40, ]
#conditions sur les colonnes 
#liste_produits_prix_inf_40 <- database[database$prix <40, c("productID", "Date Register")]


View(liste_produits_prix_40)
#rajouter des conditions sur les colonnes, on veut juste le customer ID et le productID 

liste_produits_prix_40 <- duplicate_table[duplicate_table$Price >40, c("CustomerID", "ProductID")]
View(liste_produits_prix_40)

#conditions imbriquees, prix > 40 et ne pas considrer les valeurs manquantes 
liste_produits_prix_40 <- duplicate_table[(duplicate_table$Price >40) & is.na(duplicate_table$Price) == FALSE, c("CustomerID", "ProductID")]

View(liste_produits_prix_40)
#Liste des clients qui sont premium 
View(clients_df)
clients_bis <- clients_df
clients_premium <- clients_bis[clients_bis$CustomerCategory == "Premium", c("Name", "AgeGroup")]
#clients_prem <- database[database$cat == "prem", c("name", "idclient")]

#clients 

View(clients_premium)
#imbriquer deux conditions, clients premium et dont age varie entre 36-45
clients_premium_under_50 <-clients_bis[(clients_bis$CustomerCategory == "Premium")& clients_bis$AgeGroup=="36-45", c("Name", "AgeGroup")]

#clients_prem_under_50 <- database[(database$cat =="pre") & (database$age ==50), c("ID")]

View(clients_premium_under_50)
#on peut utiliser la fonction filter de dyplyr 
liste_with_filter  <- clients_bis %>% filter(CustomerCategory == "Premium")



#4. Utilisations des fonctions de groupage 
#En sachant que nous avons une table Ventes et Clients, les infos du client sont dans la table clients et des ventes dans ventes, mais on peut fusionner 
#Jointure et Fusion de deux tables, CustomerID est dans Clients et Ventes 
View(clients_df)
View(ventes_df)
#left, right, inner join 
jointure_v_c <- inner_join(clients_df, ventes_df, by = "CustomerID")
View(jointure_v_c)
#possible de prendre considerer uniquement quelques colonnes 
clients_troncated <- clients_df[, c("Name", "CustomerCategory")]

View(clients_troncated)
#categoriser le CA(Chiffre d'Affaire) sur base de CustomerCategory , moyenne et etc...
#savoir utiliser la fonction summarise qui peut prendre une infinite des calculs sur base des filtres specified dans le group by 
jointure_v_c %>% group_by(CustomerCategory) %>% summarise(CA_VALUE=sum(Montant_Total, na.rm = T), Moyen_CA_Value= mean(Montant_Total, na.rm = T), Nombre = n())

#database %% group_by(cat) %% summarise(CA_VAL = sum(Montna, na.rm=TRUE), Moyen = mean(Montant, na.rm = T), Effectif = n())

#ou bien sans utiliser le %>%
#summarise(group_by(jointure_v_c,CustomerCategory), nb = n())

#--------------- Part 3... Manipulation des Dates, Creation des graphiques et manipulation des chaines de caracteres (strings)
View(jointure_v_c)

#Relation Visuelle, relation entre la Quantite et le Price 
#Est ce qu'il y a nue relation entre les deux variables ? 




