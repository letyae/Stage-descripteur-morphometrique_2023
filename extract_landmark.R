#Extraction de landmark par des droites verticales parallele

library(Momocs)
#library(tidyverse)
library(dplyr)
#library(geomorph)
#library(grDevices)



setwd("~/Bureau/contour/contour_2021/results_2021")

#Extraction des fichiers.Csv contenant les coordonnées des contours

csv_files <- list.files(pattern = "*png.csv") 
#csv_files <-csv_files[1436:1536]

data_list=list()
# Loop over the CSV files
for (file in csv_files) {
  # Read the CSV file into a data frame
  
  data <- read.csv(file,sep=";")
  # Add the data frame to the list with the file name as the first dimension
  data_list[[file]] <- data
}

pattern<-Ldk(data_list)

# Alignement et centrage de l'objet, mis à echelle des coordonne
# pattern.scale<-pattern %>% 
#  coo_center %>% coo_scale %>% 
#  coo_alignxax() %>% coo_slidedirection("up") 


#Alignement et centrage de l'objet
pattern.scale<-pattern %>% 
  coo_center %>%  coo_alignxax() %>%
  coo_slidedirection("up") 

ldk_plot(pattern.scale[10])
coo_plot(pattern.scale[1])
coo_plot(pattern.scale[2])
coo_plot(pattern.scale[10])
#coo_plot(pattern.scale[1536])



#ldk_plot(pattern.scale[20])
#coo_plot(pattern[720])




# generation d'un tableau contenant un nombre de point n équidistant entre start et end (inclus)
#qui seront des equations de droite

#start,end extremite des objets suivant x ou suivant y
# n= nombre de droite ou  Nombre de points à sélectionner


Select3<- function(start,end,n){
  # Calcul de l'espacement équidistant
  #n <-74 # Nombre de points à sélectionner
  
  #n <-784
  
  #n <-  18
  
  spacing <- (end - start) / (n - 1)
  
  # Sélection des points équidistants
  #points <- seq(start+spacing, end-spacing, by = spacing)
  points <- seq(start, end, by = spacing)
  
  # Affichage des points sélectionnés
  #print(points)
  return(points)
}

All_landmarks <-  c()
npoints<- c()
#377   383
#k<-376
k=58
#k<-383
#parcours de chaque objet et extraction des landmarks
###### Landmark avec droite parallele à x=0 d'une part et de y=0 dautre part

#for(i in 1:length(pattern.scale)){
  for(i in 1:1){ 
  print(paste("....EXTRACTING LANDMARKS OF Shape No:",k))
  
  l<-1
  X<- c()
  Y<- c()
  #contour de l'objet initial
  plot(pattern.scale[k])
  
  for(j in 1:length(pattern.scale[k][,1])){
    npoints <- append(npoints,length(pattern.scale[k][,1]))
    #print(pattern.scale[k][l,])
    x=pattern.scale[k][l,1]
    y=pattern.scale[k][l,2]
    X<-append(X,x)
    Y<-append(Y,y)
    
   # tolerance <- 0.04
    tolerance <- 3
    l<-1+l
  }
  
  ix_max <- which.max(X)
  end<-X[ix_max]
  ix_min <- which.min(X)
  start<-X[ix_min]
  
  
  
  iy_max <- which.max(Y)
  end<-Y[iy_max]
  
  iy_min <- which.min(Y)
  start<-X[iy_min]
  
  
  
  landMarkX<- c()
  landMarkY<- c()
  
  landMarkX_tmp<- c()
  landMarkY_tmp<- c()
  
  
  
  #points(X[i_min],Y[i_min],col='red')
  #points(X[i_max],Y[i_max],col='red')

  n <-round(length(pattern.scale[k][,1])*0.3)
  
  list_pts=Select3(start,end,n)
  
  N_pts=round(length(list_pts)/6)
  list_pts1=list_pts
  list_pts=list_pts[N_pts:(length(list_pts)-N_pts)]
  
  for(dx in 1:length(list_pts)){
    
    indice <- which(abs(X - list_pts[dx]) <= tolerance)
    
    if(length(indice)>1){    
     # points(X[indice],Y[indice],col='green')
      AYind<- Y[indice]
      AXind<- X[indice] 
      
      #indice du points le plus haut
      indx<-which.max(AYind) 
      
      #indice du points le plus bas
      indy<-which.min(AYind) 
      
      points(AXind[indx],AYind[indx],col='blue')
      landMarkX<-append(landMarkX,AXind[indx])
      landMarkY<-append(landMarkY,AYind[indx])
      
      
      points(AXind[indy],AYind[indy],col='green')
      landMarkX_tmp<-append(landMarkX_tmp,AXind[indy])
      landMarkY_tmp<-append(landMarkY_tmp,AYind[indy])
      
    }
    
    ##############
    
    else if(length(indice)==1){ 
      
      ### un seul point trouve
      
      # points(X[indice],Y[indice],col='yellow')
      AYind<- Y[indice]
      AXind<- X[indice] 
      
      #indice de ce point
      indx<-1 
      
      
      if(AYind[indx]>=0){ 
        points(AXind[indx],AYind[indx],col='blue')
        landMarkX<-append(landMarkX,AXind[indx])
        landMarkY<-append(landMarkY,AYind[indx])
      }else { 
        points(AXind[indx],AYind[indx],col='green')
        landMarkX_tmp<-append(landMarkX_tmp,AXind[indx])
        landMarkY_tmp<-append(landMarkY_tmp,AYind[indx])
      } 
    }
    
    #######################"""   
    
    
  }
  
  lX=landMarkX;   lY=landMarkY;
  lX_tmp=landMarkX_tmp; lY_tmp=landMarkY_tmp
  
  points(lX[1],lY[1],col='red')  
  points(lX_tmp[1],lY_tmp[1],col='red')  
  
  points(lX[length(lX)],lY[length(lY)],col='red') 
  points(lX_tmp[length(lX_tmp)],lY_tmp[length(lY_tmp)],col='red')  
  
  starty<-lY_tmp[1]
  endy<-lY[1] 

  #nombre de droite parallele à y=0
  n <-round(length(pattern.scale[k][,1])*0.1)
  
  landMarkX_tmp_y<-c()
  landMarkY_tmp_y<-c()
  
  landMarkX_tmp_y<-append(landMarkX_tmp_y,lX_tmp[1])
  landMarkY_tmp_y<-append(landMarkY_tmp_y,lY_tmp[1])
  
  list_pts2=Select3(starty,endy,n)
  
  for(dy1 in 1:length(list_pts2)){
    indice_y <-c()
    indice_y <- which(abs(Y - list_pts2[dy1]) <= tolerance)
    
    if(length(indice_y)>1){    
      # points(X[indice_y],Y[indice_y],col='yellow')
      AYindy_y<- Y[indice_y]
      AXindy_x<- X[indice_y] 
      #indice du points le plus bas
      indy_2<-which.min(AXindy_x) 
    
        points(AXindy_x[indy_2],AYindy_y[indy_2],col='blue')
        landMarkX_tmp_y<-append(landMarkX_tmp_y,AXindy_x[indy_2])
        landMarkY_tmp_y<-append(landMarkY_tmp_y,AYindy_y[indy_2]) 
    }
    
    
    else if(length(indice_y)==1){ 
      ### un seul point trouve
      # points(X[indice_y],Y[indice_y],col='green')
      AYindy_y<- Y[indice_y]
      AXindy_x<- X[indice_y] 
      
      #indice de ce point
      indx<-1 

      if(AXindy_x[indx]>=0){ 
          points(AXindy_x[indx],AYindy_y[indx],col='green')
          landMarkX_y<-append(landMarkX_y,AXindy_x[indx])
          landMarkY_y<-append(landMarkY_y,AYindy_y[indx])
     
      }else{  
    
         points(AXindy_x[indx],AYindy_y[indx],col='blue')
         landMarkX_tmp_y<-append(landMarkX_tmp_y,AXindy_x[indx])
         landMarkY_tmp_y<-append(landMarkY_tmp_y,AYindy_y[indx]) 
    
      } 
    }
    
  }
  
  
  ##Description du contour droit de l'objet
   starty2<-lY_tmp[length(lY_tmp)]
   endy2<- lY[length(lY)] 
 
   landMarkX_y<-c()
   landMarkY_y<-c()
  
  list_pts3=Select3(starty2,endy2,n)
  
  for(dy2 in 1:length(list_pts3)){
    indice_y <-c()
    indice_y <- which(abs(Y - list_pts3[dy2]) <= tolerance)
    
    #Si on trouve plus d'un point
    if(length(indice_y)>1){    
      # points(X[indice_y],Y[indice_y],col='yellow')
      AYindy_y<- Y[indice_y]
      AXindy_x<- X[indice_y] 
      
      #indice du points le plus haut
      indy_1<-which.max(AXindy_x) 
  
        points(AXindy_x[indy_1],AYindy_y[indy_1],col='green')
        landMarkX_y<-append(landMarkX_y,AXindy_x[indy_1])
        landMarkY_y<-append(landMarkY_y,AYindy_y[indy_1])
    }else if(length(indice_y)==1){ 
      
      ### un seul point trouve
      
      # points(X[indice_y],Y[indice_y],col='green')
      AYindy_y<- Y[indice_y]
      AXindy_x<- X[indice_y] 
      
      #indice de ce point
      indx<-1 
      if(AXindy_x[indx]>=0){ 
        points(AXindy_x[indx],AYindy_y[indx],col='green')
        landMarkX_y<-append(landMarkX_y,AXindy_x[indx])
        landMarkY_y<-append(landMarkY_y,AYindy_y[indx])
      }else{ 
        points(AXindy_x[indx],AYindy_y[indx],col='blue')
        landMarkX_tmp_y<-append(landMarkX_tmp_y,AXindy_x[indx])
        landMarkY_tmp_y<-append(landMarkY_tmp_y,AYindy_y[indx]) 
      } 
    }
    
  }
  
  
  landMarkX <- c(landMarkX_tmp_y,landMarkX)
  landMarkY <- c(landMarkY_tmp_y,landMarkY)
  #plot(landMarkX,landMarkY, type = "l")
  
  landMarkX_tmp <- c(landMarkX_tmp,landMarkX_y)
  landMarkY_tmp <- c(landMarkY_tmp,landMarkY_y)
 # plot(landMarkX_tmp,landMarkY_tmp, type = "l")
  
  landMarkX_tmp <- rev(landMarkX_tmp)
  landMarkY_tmp <- rev(landMarkY_tmp)
  
  
  landMarkX <- c(landMarkX,landMarkX_tmp)
  landMarkY <- c(landMarkY,landMarkY_tmp)

  plot(landMarkX,landMarkY, type = "l")
  All_landmarks[[k]]<-cbind(landMarkX,landMarkY)
  
  k<-1+k
 } 



folder_path="Landmark/"
if (!file.exists(folder_path)) {
  # Create the folder
  dir.create(folder_path)
  cat("Folder created successfully.\n")
} else {
  cat("Folder already exists.\n")
}

for(k in 1:length(csv_files)){ 
  
  f=csv_files[k]
  write.csv(All_landmarks[[k]], file = paste0(folder_path, f), row.names = FALSE)
}

##Names of shape
af=csv_files[1:length(pattern.scale)]

a<-Out(All_landmarks)
names(a)<-af

#par(mfrow = c(1, 2))
panel(a)

k=8
par(mfrow = c(1, 2))
ldk_plot(pattern.scale[k])
coo_plot(a[k])


pattern.f <- efourier(a, nb.h = 91)
pattern.p <- PCA(pattern.f)
pattern.p
plot_PCA(pattern.p)
plot_PCA(pattern.p,labelpoints=T)














panel(pattern.scale)


k=40
coo_plot(pattern.scale[k])


point=12
ef=efourier(pattern.scale[k],point)
efi <- efourier_i(ef)
coo_draw(efi, border='red', col=NA)



plot(All_landmarks[[1]])
for(k in 1:120){ 
  points(All_landmarks[[1]][k,1],All_landmarks[[1]][k,2], col="green")
}

