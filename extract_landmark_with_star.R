library(Momocs)
#library(tidyverse)
library(dplyr)
#library(geomorph)
#library(grDevices)

setwd("~/Bureau/contour/contour_2021/results_2021")
#setwd("~/Bureau/contour/contour_2021/contour_class1")

#Extraction des fichiers.Csv contenant les coordonnées des contours
csv_files <- list.files(pattern = "*png.csv") 

data_list=list()
# Loop over the CSV files
for (file in csv_files) {
  # Read the CSV file into a data frame
  
  data <- read.csv(file,sep=";")
  #data <- read.csv(file,sep=",", as.is = TRUE, header = T)
  # Add the data frame to the list with the file name as the first dimension
  data_list[[file]] <- data
}

pattern<-Ldk(data_list)

# Alignement et centrage de l'objet, mis à echelle des coordonne
# pattern.scale<-pattern %>% 
#  coo_center %>% coo_scale %>% 
# coo_alignxax() %>% coo_slidedirection("up") 


#Alignement et centrage de l'objet
 pattern.scale<-pattern %>% 
   coo_center %>%  coo_alignxax() %>%
  coo_slidedirection("up") 

#Affichage de contour
ldk_plot(pattern.scale[1])
coo_plot(pattern.scale[1])



# generation d'un tableau contenant un nombre de point n équidistant entre start et end (inclus)
#qui seront utilisés pour définir les equations de droite

#start,end extremite des objets suivant x ou suivant y
# n= nombre de droite ou  Nombre de points à sélectionner



#on genere ue droite passant par l'origine et le point (x1,y1) jusquà point2 (x2,y2)
#X,Y: coord de ts les points de l'objet

#index: 1 si les points de depart des droites se trouve sur les bords les cotés haut et bas  
#index: 2 si les points de depart des droites se trouve sur les bords les cotés de l'objet  

select_intersection_point<- function(X,Y,x1,y1,point2,index){
pas=5
contour_id<-c()
tolerance<-5
#point2 : dernier point
for(o in seq(0,(point2+pas),by=pas)) {
  #coord. du point (de depart) a partir duquel on definit une droite
  if(index==2){
      ptx<-x1
      pty<-o+y1
  }else if(index==1){ 
    ptx<-o+x1
    pty<-y1
  }
  
  #points(ptx,pty,col='green')
 
  #Coef directeur de la droite passant par l'origine
  coef_dir<-pty/ptx
  
  yy<-coef_dir*X
  xx<-Y/coef_dir
  
  indice_x <-c()
  indice_y <-c()
  
  #selection des pnt le long de la droite avec une tolerance
  indice_y <- which(abs(Y - yy) <= tolerance)
  indice_x <- which(abs(X - xx) <= tolerance)

  #les points le long de la droite avec une tolérance
  
 # points(X[indice_y],Y[indice_y],col='blue')
 # points(X[indice_x],Y[indice_x],col='blue')
  
  #distance au 1er point de la droite, puis on selectionne
  #l'indice du plus proche du point de depart qui est proche du contour
  
  
    indice<-c()
    
    #dist au centre du pnt de depart de la droite
    dist_cent<-sqrt(ptx^2+pty^2);
    
    #dist des  points le long de la droite par rapport au pnt de depart
    dist_to_points<-c()
    if(length(indice_y)!=0 || length(indice_x)!=0 ){ 
            
         if(length(indice_y)>=length(indice_x)){ 
                 
              dist_to_points<-sqrt((X[indice_y]-ptx)^2 +(Y[indice_y]-pty)^2)
              indice<-indice_y
              }else{ 
                dist_to_points<-sqrt((X[indice_x]-ptx)^2 +(Y[indice_x]-pty)^2)
                indice<-indice_x
              }
              id<-c()
              # On garde le plus proche du point de depart 
              id<- which.min(dist_to_points)
              
              #seul les points dont la dist. au centre ne depasse pas celui du point de depart sont gardés
            
                if(dist_to_points[id]<=dist_cent){
                    #print(paste(dist_to_points[id],dist_cent))
                    contour_id<-append(contour_id,indice[id])
                }
    }   
   
}

#Affichage des points retenus
#points( X[contour_id],Y[contour_id],col='red')

# index des points retenu pour décrire le contour contour_id
  return(contour_id)
}



#parcours de chaque objet et extraction des landmarks 
###### Landmark extrait à l'aide de droites passant par l'origine,  genere à partir des points le long du rectangle englobant l'objet
All_landmarks <-  c()
npoints<- c()
k<-1
for(i in 1:length(pattern.scale)){
  print(paste("....EXTRACTING LANDMARKS OF Shape No:",k))
  
  l<-1
  X<- c()
  Y<- c()
  
  #contour de l'objet initial
  #plot(pattern.scale[k])
  

    #Recuperation des coords de l'objets 
    X<-pattern.scale[k][,1]
    Y<-pattern.scale[k][,2]
    
  landMarkX<- c()
  landMarkY<- c()

  
  #Extraction des points du rectangle englobant (point du coin inferieur gauche et point du coin superieur droit du rectangle)
  q=coo_boundingbox(pattern.scale[k])
  
  #Suivant les points le long du rectangle avec un pas nous generons les droites passant par le centre de l'objet
  #puis on  selectione le point d'intersection de l'objet le plus proche du 1er point de la droite
  
  # point du coin inf gauche du rectangle
  #points( q$x0,q$y0,col='red')

  #point2: distance  du coin inf. gauche jusqu'au coin sup. gauche,(les landmarks sont générer dans ce sens sur ce coté)
  point2=abs(q$y0)+abs(q$y1)
  
  # coord. du coin inf gauche du rectangle
  x<-q$x0
  y<-q$y0
  contour_id<-c()
  contour_id<-select_intersection_point(X,Y,x,y,point2,2);
  landMarkX<-append(landMarkX,X[contour_id])
  landMarkY<-append(landMarkY,Y[contour_id])
    

  # point du coin superieur gauche
  #points( q$x0,q$y1,col='red')
  
  #point2: distance  du coin sup gauche jusqu'au coin sup. droit,(les landmarks sont générer dans ce sens sur ce coté)
  point2=abs(q$x0)+abs(q$x1)
  
  # coord. du point du coin superieur gauche
  x<-q$x0
  y<-q$y1
  contour_id<-c()
  contour_id<-select_intersection_point(X,Y,x,y,point2,1);
  landMarkX<-append(landMarkX,X[contour_id])
  landMarkY<-append(landMarkY,Y[contour_id])
  
  
  # point du coin superieur droite 
  #points(q$x1,q$y1,col='red')
  
  #point2: distance du coin inf. droit jusqu'au coin sup. droit,(les landmarks sont générer dans ce sens sur ce coté)
  
  point2<-abs(q$y0)+abs(q$y1)
  
  # coord. du point du coin inf. droit
  x<-q$x1
  y<-q$y0
  
  contour_id<-c()
  contour_id<-select_intersection_point(X,Y,x,y,point2,2);
  contour_id<-rev(contour_id)
  landMarkX<-append(landMarkX,X[contour_id])
  landMarkY<-append(landMarkY,Y[contour_id])
  
  # point du coin inferieur droite
 # points(q$x1,q$y0,col='red')
  
  #point2: distance du coin inf. gauche jusqu'au coin inf. droit,(les landmarks sont générer dans ce sens sur ce coté)
  point2<-abs(q$x1)+abs(q$x0)
  
  # coord. du point du coin inf. gauche
  x<-q$x0
  y<-q$y0
  
  contour_id<-c()
  contour_id<- select_intersection_point(X,Y,x,y,point2,1);
  contour_id<-rev(contour_id)
  landMarkX<-append(landMarkX,X[contour_id])
  landMarkY<-append(landMarkY,Y[contour_id])
  
  #Affichage de tous landmarks
 # points(landMarkX,landMarkY,col="red")

  
  All_landmarks[[k]]<-cbind(landMarkX,landMarkY)
  
  
  #par(mfrow = c(1, 2))
 #plot(pattern.scale[k])
 # coo_plot(All_landmarks[[k]])
  
  k<-1+k
} 


##Names of shape
Names<-csv_files[1:length(pattern.scale)]

All_landmarks<-Out(All_landmarks)
#names(a)<-Names

#par(mfrow = c(1, 2))
panel(All_landmarks)
#panel(pattern.scale)


#Construction de l'espace morpho
#Inconvenient, le nombre d'harmonic (nb.h) est limité par rapport au plus petit contour pour toutes les formes
pattern.f <- efourier(All_landmarks, nb.h = 91)
#pattern.f <- fourier(a, nb.h = 91)
pattern.p <- PCA(pattern.f) 
plot_PCA(pattern.p)
#plot_PCA(pattern.p,labelpoints=T)


boxplot(pattern.f)


#### Calcul des coef. de efouier independament de la forme, au max nb.h=91
#creer repertoire pour enregistrer les coefs

folder_path="Efourier_coef/"
#folder_path="Efourier_coef_l_var"
if (!file.exists(folder_path)) {
  # Create the folder
  dir.create(folder_path)
  cat("Folder created successfully.\n")
} else {
  cat("Folder already exists.\n")
}

#nb.h: Nombre d'harmonique
nb.h = 91

n_col=nb.h*4
#n_col=nb.h*4+2
n_ligne=length(csv_files)
F.Coef_tab <- matrix(0, nrow = n_ligne, ncol =n_col)
noms_colonnes<-c()

tab=c("A","B","C","D");
#initialisation des noms de coef.
for (i in 1:4){ 
  for(k in 1:91){
    noms_colonnes<-append(noms_colonnes,paste0(tab[i],k))
  }
}

#Calcul des coefs de efourier et stockages ds une matrice, on complete par zeros
#ceux dont le nbre de coef est inf. à 91

for(k in 1:n_ligne){ 
    f=csv_files[k]
    pattern.f <- c()
    
    pattern.f<-efourier(All_landmarks[k], nb.h ) 
  
     n_coef<-length(pattern.f$shp$an)
     
    F.Coef_tab[k,1:n_coef]= t(pattern.f$shp$an)
    
    F.Coef_tab[k,seq(nb.h+1,(nb.h+n_coef))]= t(pattern.f$shp$bn)
    
    F.Coef_tab[k,seq(2*nb.h+1,(2*nb.h+n_coef))]= t(pattern.f$shp$cn)
    
    F.Coef_tab[k,seq(3*nb.h+1,(3*nb.h+n_coef))]= t(pattern.f$shp$dn)
    
    #F.Coef_tab[k,4*nb.h+1]= pattern.f$shp$ao 
    
    #F.Coef_tab[k,4*nb.h+2]= pattern.f$shp$co
  
   
}


F.Coef_tab <- data.frame(F.Coef_tab)
colnames(F.Coef_tab) <- noms_colonnes
rownames(F.Coef_tab) <-csv_files

#rownames(F.Coef_tab) <-seq(1,length(csv_files))

#Exportations des coefs de fouriers pour ACP avec FACTOMINER, dans un autre projet
#car il y a interaction avec momocs

write.table(F.Coef_tab, file = "Efourier_coef/Fourier.Coef_tab.csv", sep = ";", quote = FALSE, col.names = TRUE, row.names = TRUE)

### Reconstruction des formes à partir des coef moyenné  par arbre
#suppression des nums de pomme pour avoir que les noms d'arbre

#Construction des noms d'arbre en supprimant les num. de pomme
tree_name<-rownames(F.Coef_tab) 
for (k in 1:length(tree_name)){ 
  tree_name[k]<-substr(tree_name[k],1,nchar(tree_name[k])-10)
}



F.Coef_tab<-cbind(tree_name,F.Coef_tab)
#data<-as_tibble(data)
F.Coef_tab<-as_data_frame(F.Coef_tab)

#Moyennage des coefs par arbre  et par colone
F.Coef_by_tree<-F.Coef_tab %>%
  group_by(tree_name) %>%
  summarise(across(A1:D91,mean,na.rm=F))


rownames(F.Coef_by_tree)<-NULL
colnames(F.Coef_by_tree)<-NULL


Matrice_F.Coef_by_tree<-as.matrix(F.Coef_by_tree[,2:length(F.Coef_by_tree[1,])])

#Listes des coord reconstruit
coo_by_tree<-c()

for(k in 1:length(Matrice_F.Coef_by_tree[,1])){
coo=efourier_shape(Matrice_F.Coef_by_tree[k,1:91],Matrice_F.Coef_by_tree[k,92:182],Matrice_F.Coef_by_tree[k,183:273],Matrice_F.Coef_by_tree[k,274:364])
coo_by_tree<-append(coo_by_tree,list(coo))
}
coo_by_tree<-Out(coo_by_tree)

#Forme moyen par arbre
panel(coo_by_tree)

