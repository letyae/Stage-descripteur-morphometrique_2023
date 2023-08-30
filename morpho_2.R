library(Momocs)
library(tidyverse)
library(dplyr)
library(geomorph)
library(grDevices)
library("FactoMineR")
library("factoextra")


# library('imager')
# im<-load.image()


###MORPHOMETRIC DATA
#setwd("~/Bureau/contour/results_2021")
setwd("~/Bureau/contour/contour_2021/results_2021")

csv_files <- list.files(pattern = "*.png.csv") 

data_list=list()
# Loop over the CSV files
for (file in csv_files) {
  # Read the CSV file into a data frame
  data <- read.csv(file,sep=";")
  
  # Add the data frame to the list with the file name as the first dimension
  data_list[[file]] <- data
}

pattern<-Ldk(data_list)

pattern.scale<-pattern %>% 
  coo_center %>% coo_scale %>% 
  coo_alignxax() %>% coo_slidedirection("up") 


get_ldk(pattern)

#ldk_plot(pattern.scale[720])
#coo_plot(pattern[720])


#coo_oscillo(pattern.scale[8], "efourier")

#MSHAPES(pattern.scale)


# Procrustes-aligned and slided molars
pattern.al <- fgProcrustes(pattern, tol = 1e-4) %>% coo_slidedirection("left")
stack(pattern.al, title="Aligned pattern")

pattern.pca <- PCA(pattern)
#plot(pattern.scale[1])

#points(pattern.scale[1][50,1],pattern.scale[1][50,2],col='red')





Select3<- function(start,end){

# Calcul de l'espacement équidistant
#n <-74 # Nombre de points à sélectionner

n <-784
  
#n <-  18

spacing <- (end - start) / (n - 1)

# Sélection des points équidistants
#points <- seq(start+spacing, end-spacing, by = spacing)
points <- seq(start, end, by = spacing)

# Affichage des points sélectionnés
#print(points)
}


All_landmarks <-  c()
All_perimeter<-  c()
All_circ<-  c()
All_rect<-  c()

All_solidite <- c()
All_elong<- c()
All_aspect_ratio<- c()
All_rugosite <- c()
npoints<- c()
k<-1

#k<-530
for(i in 1:length(pattern.scale)){
#for(i in 1:150){
    
  l<-1
  #plot(pattern.scale[k])
  #print(length(pattern.scale[k][,1]))
  
  X<- c()
  Y<- c()
  
  #LIGNE VERTICALE X=0
  pts_X0<- c()
  ptsX_X0<- c()
  
  #LIGNE Horizontale Y=0
  pts_Y0<- c()
  ptsX_Y0<- c()
  
  for(j in 1:length(pattern.scale[k][,1])){
    shp<-names(pattern.scale[k])
    npoints <- append(npoints,length(pattern.scale[k][,1]))
    #print(pattern.scale[k][l,])
    x=pattern.scale[k][l,1]
    y=pattern.scale[k][l,2]
    X<-append(X,x)
    Y<-append(Y,y)
    
    tolerance <- 0.04
    l<-1+l
  }
  
  i_max <- which.max(X)
  end<-X[i_max]
  
  i_min <- which.min(X)
  start<-X[i_min]

  landMarkX<- c()
  landMarkY<- c()
  
  landMarkX_tmp<- c()
  landMarkY_tmp<- c()
  
 
  #le points le plus haut
  
  #points(X[i_max],Y[i_max],col='red')
  
 
  
  #a<-X[i_max]
  #b<-Y[i_max]
  
  #le points le plus bas

  #points(X[i_min],Y[i_min],col='red')
  

  landMarkX<-append(landMarkX,X[i_min])
  landMarkY<-append(landMarkY,Y[i_min])
  
  
 list_pts=Select3(start,end)

 
 for(dx in 1:length(list_pts)){

   indice <- which(abs(X - list_pts[dx]) <= tolerance)
  # points(X[indice],Y[indice],col='blue')
  AYind<- Y[indice]
  AXind<- X[indice]
   ind<-which.max(AYind) 
  # points(AXind[ind],AYind[ind],col='red')
   landMarkX<-append(landMarkX,AXind[ind])
   landMarkY<-append(landMarkY,AYind[ind])
   #les points les plus bas
   ind<-which.min(AYind) 
   #points(AXind[ind],AYind[ind],col='red')
   
   landMarkX_tmp<-append(landMarkX_tmp,AXind[ind])
   landMarkY_tmp<-append(landMarkY_tmp,AYind[ind])
 }
 
 #Ajout du dernier point de l'axe Y=0
 landMarkX_tmp<-append(landMarkX_tmp,X[i_max])
 landMarkY_tmp<-append(landMarkY_tmp,Y[i_max])
 
 
 landMarkX_tmp <- rev(landMarkX_tmp)
 landMarkY_tmp <- rev(landMarkY_tmp)
 
 landMarkX <- c(landMarkX,landMarkX_tmp)
 landMarkY <- c(landMarkY,landMarkY_tmp)
 
 #par(mfrow = c(1, 2))
 #plot(pattern.scale[k])
# plot(landMarkX,landMarkY)
 
 
 
 #A jout du point depart pour avoir un contour ferme
 landMarkX<-append(landMarkX,landMarkX[1])
 landMarkY<-append(landMarkY,landMarkY[1])

 All_landmarks[[k]]<-cbind(landMarkX,landMarkY)

 #landMark<-cbind(landMarkX,landMarkY) 
 #coo_oscillo(landMark, "efourier")
 
 #plot(All_landmarks[[1]][,1],All_landmarks[[1]][,2])
 
 
 landMark<-cbind(landMarkX,landMarkY) 
 #coo_oscillo(landMark, "efourier",nb.pts = 100)
 #A=coo_oscillo(landMark)
 
 All_perimeter <-append(All_perimeter,coo_perim(landMark))
 All_circ <-append(All_circ,coo_circularity(landMark))
 All_solidite <-append(All_solidite,coo_solidity(landMark))
 All_elong<-append(All_elong,coo_elongation(landMark))
 All_aspect_ratio<-append(All_aspect_ratio,coo_width(landMark)/coo_length(landMark))
 All_rect<-append(All_rect,coo_rectangularity(landMark))
 
 #Ecart  des rayons
 Rayon=sqrt(landMarkX^2+landMarkY^2)
 All_rugosite<-append(All_rugosite,Rayon);

 k<-1+k

Rayon=sqrt(landMarkX^2+landMarkY^2)
Rugosite=var(Rayon)
}

##Names of shape
af=csv_files[1:length(pattern.scale)]

# Caractères à supprimer
characters_to_remove <- c("-")

# Supprimer les caractères de la chaîne
af <- gsub(paste(characters_to_remove, collapse = "|"), "", af)





a<-Out(All_landmarks)
names(a)<-af

All_distcentre=coo_centdist(a)

table_discentre<-data.frame()

table_discentre<-c()
for (i in 1:length(All_distcentre)){
  table_discentre<-rbind(table_discentre,unlist(All_distcentre[i]))
}
colnames(table_discentre)<-c()





# Lire les données à partir du fichier texte
#donnees <- read.table("donnees_couleurs_2021.csv", header = T, sep ="\t")
donnees <- read.csv("donnees_couleurs_2021.csv", header = T, sep =";")

##vecteur avec donnees couleurs et individus
af=as.data.frame(af)
colnames(af)<-c("image")
colour_data<-left_join(af,donnees, by=join_by(image==image))



pca.dist_centre<- prcomp(table_discentre)

fviz_pca_ind(pca.dist_centre)


#Biplot simple
fviz_pca_ind(pca.dist_centre, col.ind = colour_data$a_mean,
             gradient.cols = "Reds",
                axes=c(1,2),
                pointshape=16, label = "var",
                #col.var = "red",
                repel = TRUE, # Évite le chevauchement de texte
                pointsize = 2.5,
                labelsize = 3,
)+theme_gray()





test<-get_pca(pca.dist_centre)
test$coord[,1]

test_id<-get_pca_ind(pca.dist_centre)

coordinates <- pca.dist_centre$x[, 1:2]

individual_names<-af;
# Combine names with PCA coordinates
result <- cbind(Name = individual_names, coordinates)
df_result <- data.frame(Name = individual_names, Dim1 = coordinates[, 1], Dim2 = coordinates[, 2])
write.csv(df_result, file = "~/Bureau/contour/Dist_cent_pca_Coords_2021.csv", row.names = T)



###################################
bt<-bot
bot.f <- efourier(bt , nb.h=10,norm = T)
bot.f
class(bot.f)
inherits(bot.f, "Coe")
hist(bot)

bot.p <- PCA(bot.f)
class(bot.p)        # a PCA object, let's plot it
plot_PCA(bot.p)


bt<-c()
bt<-Out(All_landmarks)
bot.f <- tfourier(bt , nb.h=100,norm = T)
bot.f
hist(bot.f, drop=1)
bot.p <- PCA(bot.f)
class(bot.p)        # a PCA object, let's plot it
plot_PCA(bot.p)

###################################







Ptolemy(a)
bot.f <- efourier(a,norm = T, nb.h=12)
class(bot.f)
#inherits(bot.f, "OutCoe")
hist(bot.f)
bot.f


#TODO : rajouter les noms 
#a<-Out(All_landmarks)


it<-1
for (i in 0:length(a)){
  a.f[it]<-efourier(a[it], nb.h = 10)
  it=it+1
}
boxplot(a.f[[25]])

#############################################################
panel(a, fac="aut", names="aut")

a_f <- efourier(a,norm = T)
a_p <- PCA(a_f)
class(a_p)        # a PCA object, let's plot it
plot_PCA(a_p)
plot(a_p)
###
ht <- measure(a, coo_area, coo_circularity,d(1, 3))
class(ht)
ht$coe
ht %>% PCA() %>% plot_PCA()
 ##
stack(a, title = "Non-aligned molars")
 ##
mol.al <- fgProcrustes(a, tol = 1e-4) %>% coo_slidedirection("left")
stack(mol.al, title="Aligned molars")

pile(a) 



panel(a, names="aut")


coo_plot(a[19])
ef <- efourier(a[19], 700)
efourier_shape(ef$an, ef$bn, ef$cn, ef$dn)



rf <- rfourier(a[5])
rfourier_shape(rf$an, rf$bn)


tf <- tfourier(a[5])
tfourier_shape(rf$an, rf$bn)


panel(Out(a2l(replicate(100,efourier_shape(nb.h=6, alpha=2.5, plot=FALSE))))) # Bubble family


cat <- a[5]
Ptolemy(cat, main="contour")







string <-csv_files[1:length(pattern.scale)]


# Caractères à supprimer
characters_to_remove <- c("-")

# Supprimer les caractères de la chaîne
clean_string <- gsub(paste(characters_to_remove, collapse = "|"), "", string)

# Afficher la chaîne de caractères nettoyée
print(clean_string)
clean_string=as.data.frame(clean_string)
colnames(clean_string)<-c("image")


