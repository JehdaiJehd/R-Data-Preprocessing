#Part 4 Astuces et Programmation avancees en R pour une grande efficience

View(jointure_v_c)
colnames()

#utilisation de la fonction mutate 
#Changeons mois achat, si 1 Janvier et si 2 Fevrier 



#creer une fonction somme 
somme_fonction <- function(var1, var2)
{
  somme <- var1 + var2
  return(somme)
}

somme_fonction(100,400)

#utilisation de la table vente 
View(ventes_df)

#verification des variables quantitatives
names(ventes_df)
is.numeric(ventes_df$Quantity)

#Ecrire une fonction qui teste si une variable est quantitative 

quanti_check <- function(variable)
{
  if(is.numeric(variable) == TRUE)
  {
    message = "Une variable quantitative"
  } else{
    message = "Une variable Qualitative"
  }
  return (message)
}

quanti_check(ventes_df$Quantity)
quanti_check(ventes_df$ProductID)

#utilisation de la boucle for 
#A noter qu'avec la names(ventes_df) on recupere les noms des variables , avec ventes[[variable]], on accede a la valeur 

for (variable in names(ventes_df)) {
  if (is.numeric(ventes_df[[variable]])) {
    print(paste0(variable, " est un numerique"))
    
  }else {
    print(paste0(variable, " est un NON numerique"))
  }
}
#Creer une fonction qui va realiser les statis descriptives de toutes les les variables quanti presentes dans notre dataframe 
#Et va retourner un data frame 

statistic_descriptive_fonction <- function(base_de_donnees)
{
  #monter un dataframe qui contienne les resultats des stats
  resultat_df <- data.frame(Nom_Variable = character(1), min = numeric(1), max = numeric(1), moyenne = numeric(1),variance = numeric(1)) #etape 1 (definition)
  #liste des variables concernees (toutes les variables numeriques tout simplement)
  liste_variables <- names(base_de_donnees)
  
  for (variable in liste_variables) { #parcourt toutes mes variables
    if (is.numeric(base_de_donnees[[variable]])) { #si numeric, trouver les elements en dessous (max,min, etc)
      min <- min(base_de_donnees[[variable]], na.rm = T)
      max <- max(base_de_donnees[[variable]], na.rm = T)
      moyenne <- mean(base_de_donnees[[variable]], na.rm = T)
      variance <- var(base_de_donnees[[variable]], na.rm = T)
      
      #ajouter les resultats des valeurs calculees dans le dataframe
      
      resultat_df[nrow(resultat_df)+1, ] <- c(variable, min,max,moyenne,variance)
    }
  }
  
  
  return (resultat_df)
}

#tester sur une base de donnees (sur la base de donnees ventesdf)
statistic_descriptive_fonction(ventes_df)



