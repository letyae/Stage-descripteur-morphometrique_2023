
################################################"""
##selection de pommes dont la coloration est sup à la moyenne (pomme de couleur rouge)
# Lire les données à partir du fichier texte
donnees <- read.table("resultats.txt", header = T, sep ="\t")

##Names of shape
af<-csv_files[1:length(csv_files)]
# Caractères à supprimer
characters_to_remove <- c("-")
# Supprimer les caractères de la chaîne
af <- gsub(paste(characters_to_remove, collapse = "|"), "", af)

##vecteur avec donnees couleurs et individus
af=as.data.frame(af)
colnames(af)<-c("image")
colour_data<-left_join(af,donnees, by=join_by(image==image))
colour_data <- na.omit(colour_data)
colour_data <- colour_data[colour_data$L_var > mean(colour_data$L_var) , ]
#csv_files<-colour_data$image
################################################################""""

