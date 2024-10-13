# Charger les bibliothèques nécessaires pour effectuer des requêtes, manipuler des données et gérer les données spatiales.
library(httr)      # Pour faire des requêtes HTTP vers des API
library(readr)     # Pour lire des fichiers CSV
library(dplyr)     # Pour la manipulation des données
library(jsonlite)  # Pour convertir les données JSON
library(sf)        # Pour travailler avec des données spatiales

# Définir le répertoire de travail où se trouve le fichier CSV
setwd("C:/Users/mekki/Desktop")

# Lire les adresses depuis un fichier CSV
data = read.csv("adresses-69.csv", header=TRUE, sep=";", dec=".")

# Extraire les codes postaux de la colonne correspondante
adresses = data$code_postal

# URL de base pour l'API des DPE pour logements existants (ancien)
base_url = "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"

# Initialiser un dataframe vide pour stocker les résultats des requêtes pour les logements existants
dff = data.frame()

# Boucle sur chaque code postal unique du fichier d'adresses
for (i in unique(data$code_postal)) {
  
  # Paramètres de la requête : sélectionner les colonnes et filtrer par code postal
  params = list(
    select = "Année_construction,Date_réception_DPE,Type_bâtiment,Etiquette_DPE,Etiquette_GES,Coût_total_5_usages,Coût_ECS,Coût_chauffage,Coût_refroidissement,Coût_éclairage,Coordonnée_cartographique_X_(BAN),Coordonnée_cartographique_Y_(BAN),N°DPE,Code_postal_(BAN),Adresse_(BAN),Conso_chauffage_é_finale,Conso_éclairage_é_finale,Conso_refroidissement_é_finale,Conso_ECS_é_finale,Conso_5_usages_é_finale",
    size = 10000,  # Nombre maximum de résultats à récupérer
    q = i,  # Filtrer sur le code postal actuel
    q_fields = "Code_postal_(BAN)"  # Champ sur lequel appliquer le filtre
  ) 
  
  # Construire l'URL avec les paramètres encodés
  url_encoded = modify_url(base_url, query = params)
  
  # Envoyer la requête GET à l'API et récupérer la réponse
  response = GET(url_encoded)
  
  # Convertir le contenu de la réponse de JSON en dataframe
  content = fromJSON(rawToChar(response$content), flatten = FALSE)
  dfe = content$result  # Extraire les résultats spécifiques
  
  # Ajouter les résultats au dataframe principal
  dff = rbind(dff, dfe)
  
  # Afficher le code postal actuel pour suivre la progression
  print(i)
}

# URL de base pour l'API des DPE pour logements neufs (neuf)
base_url = "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-neufs/lines"

# Initialiser un dataframe vide pour stocker les résultats des requêtes pour les logements neufs
dfff = data.frame()

# Même boucle que précédemment, mais cette fois pour les logements neufs
for (i in unique(data$code_postal)) {
  
  # Paramètres de la requête pour les logements neufs
  params = list(
    select = "Date_réception_DPE,Type_bâtiment,Etiquette_DPE,Etiquette_GES,Coût_total_5_usages,Coût_ECS,Coût_chauffage,Coût_refroidissement,Coût_éclairage,Coordonnée_cartographique_X_(BAN),Coordonnée_cartographique_Y_(BAN),N°DPE,Code_postal_(BAN),Adresse_(BAN),Conso_chauffage_é_finale,Conso_éclairage_é_finale,Conso_refroidissement_é_finale,Conso_ECS_é_finale,Conso_5_usages_é_finale",
    size = 10000,  # Nombre maximum de résultats à récupérer
    q = i,  # Filtrer sur le code postal actuel
    q_fields = "Code_postal_(BAN)"  # Champ sur lequel appliquer le filtre
  ) 
  
  # Construire l'URL avec les paramètres encodés
  url_encoded = modify_url(base_url, query = params)
  
  # Envoyer la requête GET à l'API et récupérer la réponse
  response = GET(url_encoded)
  
  # Convertir le contenu de la réponse de JSON en dataframe
  content = fromJSON(rawToChar(response$content), flatten = FALSE)
  dfe = content$result  # Extraire les résultats spécifiques
  
  # Ajouter les résultats au dataframe principal
  dfff = rbind(dfff, dfe)
  
  # Afficher le code postal actuel pour suivre la progression
  print(i)
}

# Ajouter une colonne pour indiquer que ces données concernent des logements anciens
dff$Type_hab = "ancien"

# Ajouter une colonne pour indiquer que ces données concernent des logements neufs
# et assigner l'année de construction à 2024 (comme par défaut pour ces logements neufs)
dfff$Type_hab = "neuf"
dfff$Année_construction = 2024

# Fusionner les deux dataframes (logements anciens et neufs)
df = rbind(dff, dfff)

# Afficher le dataframe résultant
View(df)

# Convertir les coordonnées cartographiques en objet spatial (pour manipuler des données géographiques)
# Ici, les colonnes Coordonnée_cartographique_X_(BAN) et Coordonnée_cartographique_Y_(BAN) sont utilisées comme coordonnées
coordinates_sf <- st_as_sf(df, coords = c("Coordonnée_cartographique_X_(BAN)", "Coordonnée_cartographique_Y_(BAN)"), crs = 2154)

# Transformer les coordonnées au système de référence WGS 84 (latitude/longitude)
coordinates_lat_long <- st_transform(coordinates_sf, crs = 4326)

# Extraire les coordonnées (longitude et latitude) dans un dataframe
lat_long <- st_coordinates(coordinates_lat_long)

# Ajouter les colonnes Longitude et Latitude dans le dataframe principal
df$Longitude = lat_long[, 1]
df$Latitude = lat_long[, 2]

# Gérer les valeurs manquantes en remplaçant certaines coordonnées par NA si elles sont suspectes ou invalides
df = df %>%
  mutate(
    Longitude = ifelse(round(Longitude, 2) == -1.36, NA, Longitude),  # Remplacer par NA si la longitude est -1.36
    Latitude = ifelse(round(Latitude, 2) == -5.98, NA, Latitude)  # Remplacer par NA si la latitude est -5.98
  )

# Assigner des couleurs pour chaque classe d'étiquette DPE (Diagnostic de Performance Énergétique)
df <- df %>%
  mutate(Couleur_echelle = case_when(
    Etiquette_DPE == "A" ~ "darkgreen",  # Couleur pour DPE A : vert foncé
    Etiquette_DPE == "B" ~ "green",      # Couleur pour DPE B : vert
    Etiquette_DPE == "C" ~ "lightgreen", # Couleur pour DPE C : vert clair
    Etiquette_DPE == "D" ~ "yellow",     # Couleur pour DPE D : jaune
    Etiquette_DPE == "E" ~ "orange",     # Couleur pour DPE E : orange
    Etiquette_DPE == "F" ~ "darkorange", # Couleur pour DPE F : orange foncé
    Etiquette_DPE == "G" ~ "red",        # Couleur pour DPE G : rouge
    TRUE ~ NA_character_  # Si aucune correspondance, assigner NA
  ))


write.csv(df, file = "DonnesNeufsAnciens.csv", row.names = FALSE)
