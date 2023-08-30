library(tools)
#SUPPRESION DES tirets ds les noms des fichiers des contours 
# Définition du répertoire contenant les fichiers CSV
chemin <- "~/Bureau/contour/contour_2021/results_2021"


# Liste des fichiers CSV dans le répertoire
fichiers <- list.files(chemin, pattern = "\\.png.csv$", full.names = TRUE)

# Parcours de la liste des fichiers
for (fichier in fichiers) {
  # Extraction du nom de fichier sans l'extension
  nom_sans_extension <- tools::file_path_sans_ext(basename(fichier))
  
  # Suppression des tirets du nom de fichier
  nouveau_nom <- gsub("-", "", nom_sans_extension)
  
  # Ajout de l'extension .csv au nouveau nom de fichier
  nouveau_nom <- paste0(nouveau_nom, ".csv")
  
  # Renommage du fichier
  file.rename(fichier, file.path(dirname(fichier), nouveau_nom))
}

# Message de confirmation
cat("Les fichiers ont été renommés avec succès.")
