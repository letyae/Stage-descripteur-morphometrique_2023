# -*- coding: utf-8 -*-

#upload data
#from google.colab import drive
#drive.mount('/content/gdrive')

#cd "gdrive/"

#cd "My Drive/"

#cd 'Mini_projet/'

#pwd

import cv2
import csv
import numpy as np
from google.colab.patches import cv2_imshow
from matplotlib import pyplot as plt
from copy import deepcopy
import os
from math import *
import pandas as pd
from scipy.fftpack import fft
from skimage.measure import label, regionprops, regionprops_table
import shutil

from skimage.measure import label, regionprops, regionprops_table
from skimage.morphology import remove_small_objects
from scipy import ndimage
from skimage.morphology import convex_hull_image

from sklearn.cluster import KMeans
from yellowbrick.cluster import KElbowVisualizer





#Recherche du nombre optimal de classe

#Img: matrice de l'image RGB à classifiée
def Elbow_function(Img):
      K_range=range(2,20)

      #cv2_imshow(Img)

      # Transformer l'image en un tableau 2D de pixels
      pixel_vals = Img.reshape((-1, 3))

      # Initialiser le visualiseur Kelbow
      model = KMeans()

      visualizer = KElbowVisualizer(model, k=K_range)

      # Appliquer le visualiseur Kelbow à l'ensemble de données
      visualizer.fit(pixel_vals)

      # Afficher le graphique
      #visualizer.show()

      # Extraire le nombre de clusters correspondant au point de coude
      elbow_point = visualizer.elbow_value_
      print("elbow_point:",elbow_point)
      return elbow_point





#Classification d'image par kmen; k: nombre de classe

#img: matrice de l'image RGB à classifiée
#k : nombre de classe (y compris le fond)

def K_Means(img,k):
  vectorized = img.reshape((-1,3))
  vectorized = np.float32(vectorized)
  criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 10, 1.0) # Critères d'arrêt
  attempts=10
  ret,label,center=cv2.kmeans(vectorized,k,None,criteria,attempts,cv2.KMEANS_PP_CENTERS)
  center = np.uint8(center)
  res = center[label.flatten()]
  result_image2 = res.reshape((img.shape))
  return result_image2







#Circularité
#region: un motif compact obtenu par un regionprops
def circ(region):
  if(region.perimeter!=0):
   return  (4 * pi * region.area) / (region.perimeter * region.perimeter)
  else:
    return 0

#distance au centre
#size: size de l'image
#centroid: coordonnée du centre de motif
def Dist_centre(centroid,size):
  return sqrt((centroid[1]-size[0]/2)**2+(centroid[0]-size[1]/2) **2)

#Calcul du mask  sur le seuil tab[i] de la classe i
#img_class: matrice de l'image (en niveau de gris) issu de la classification
#seuil: tableau contenant les seuils des classes
#i+1: la classe traité

def Mask(img_class,i,seuil):
  #seuil=[0,70,107,144,181,218,255]
  img_class[img_class!=seuil[i]]=0
  img_class[img_class==seuil[i]]=255
  return img_class

#Aspect ratio
def Aspect_Ratio(min_ax,max_ax):
  if(min_ax!=0):
    return float(max_ax)/float(min_ax)
  else:
    return 0

#Concavite
#region: un motif compact obtenu par un regionprops
#Region_img: la matrice associé à region

def Concavite(region,Region_img):
    #Recherche de l'enveloppe convexeKmeans.
    Convexe_img = convex_hull_image(Region_img)
    Convexe_label = label(Convexe_img)
    Convexe_region = regionprops(Convexe_label)

    #print("Convex perim: ",Convexe_region[0].perimeter )
    #print("reg perim: ",region.perimeter )


    if(region.perimeter!=0):
       #print("Concavité:",Convexe_region[0].perimeter/region.perimeter)
       return  Convexe_region[0].perimeter/region.perimeter
    else:

      return 0

#Variance sur la somme des colones  et des lignes
#img_contour= matrice associé à l'image de contour d'un motif
def profile(img_contour):
  img_contour_norm=img_contour/255
  sum_col = np.sum(img_contour_norm, axis=0)
  sum_lgn = np.sum(img_contour_norm, axis=1)
  return [np.var(sum_col),np.var(sum_lgn)]

#size_threshold: seuil en pixel à supprimer
#image: matrice associé à une image
def Delete_small_area(image,size_threshold):
  print("Delete small area less than ", size_threshold, " pixels...")
  # calculate properties of regions
  props = regionprops(label(image))


  # remove small regions
  for prop in props:
      if prop.area < size_threshold:
         image[prop.coords[:, 0], prop.coords[:, 1]] = 0
  return image







#Extraction  des masks de toutes les classes
#image: image en niveau de gris issu de la classification
def Extract_class(image):
    img = cv2.imread(image)
    img=img[:,:,0]
    histr = cv2.calcHist([img],[0],None,[256],[0,256])
    seuils=[]
    #for i in range(len(histr)):
    for i in range(len(histr)-1, -1, -1):
      if(histr[i]!=0):
        seuils.append(i)
    All_mask=[]
   # for i in range(1,len(seuils)):
    for i in range(0,len(seuils)-1):
      All_mask.append(Mask(deepcopy(img),i,seuils))
    return All_mask

#Mesure de propriete morphometriques

#mask_file: chemin menant au fichier du mask traité,
# file: chemin menant au fichier csv dans lequel les données extraits seront regroupé
def Shape_carac(mask_file,file):
    mask=cv2.imread(mask_file)

    #Selection d'un canal
    mask=mask[:,:,0]
    size=mask.shape

    #cv2_imshow(im)

    #FILL HOLES
    #img_fill_holes=ndimage.binary_fill_holes(im)*255

    img_fill_holes=mask

    #Delete small area
    #Area_threshold = 600
    #img_del_small=Delete_small_area(img_fill_holes,Area_threshold)
    #cv2_imshow(img_del_small)

    img_del_small=img_fill_holes
    #Saving Mask after denoise and delete small area
    #cv2.imwrite(img_file, img_del_small)

    label_im = label(img_del_small)

    Regions = regionprops(label_im)



####################################################
    #kernel = np.ones((2,2), np.uint8)
    #Dilatation des regions
    #img_dilation = cv2.dilate(im, kernel, iterations=3)

    #Erosion des regions
    #img_erosion = cv2.erode(img_dilation, kernel, iterations=2)


   #Filtrage de bruit
   # img_filtre = cv2.medianBlur(img_erosion, 7)


    # DELETE small Area

    #labelisation des différentes regions
    #label_im = label(img_filtre)
    #label_im = label(im)


    #Mesure des proprietes des regions
    #Regions = regionprops(label_im)


    #if(os.path.isfile(file) is False):
    Label=[];Trou=[]; Area=[]; Circ=[]; Perimetre=[]; Perim_crofton=[]; Orientation=[]; Major_axis=[]; Minor_axis=[]; Equivalent_diameter=[]; Distance_Centre=[]; Centroid=[];
    Aspect_Ratio1=[]; Aspect_Ratio2=[];Rugosite=[]; Feret_diameter=[];Concavities=[];ProfileX=[];ProfileY=[]; Euler0=[]; Rectangularite=[]; Solidite=[];Energie=[];
    Elongation=[];

    for region in Regions:
              min_ax=region.minor_axis_length
              max_ax=region.major_axis_length

              #Aspect ratio avec les axes de l'ellipse englobant
              aspect_rat1=Aspect_Ratio(min_ax,max_ax)

              #Aspect ratio avec le rectangle englobant
              y0, x0, y1, x1 = region.bbox
              aspect_rat2=Aspect_Ratio(y1-y0,x1-x0)

              elongation=np.abs(1-(1/aspect_rat2))

              #cordonnees des regions
              Coord=region.coords

              #Matrice nulle pour y representer  chaque region dans subimage

              subimage=np.zeros(mask.shape)

              for coord in Coord:
                  subimage[coord[0], coord[1]] = 255;


              #Profile sur le contour des régions
              #Profile=profile(subimage)


              #Coordonnées du centre de region
              cx=round(region.centroid[1])
              cy=round(region.centroid[0])


              #cv2_imshow(subimage)

              # Appliquer l'algorithme Canny pour détecter les bords
              edges = cv2.Canny(np.uint8(subimage), 10, 150)

              # Trouver les contours
              contours, _ = cv2.findContours(edges, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)

              image_contour=np.ones((subimage.shape[0],subimage.shape[1],3))*255

              # Dessiner les contours sur l'image
              cv2.drawContours(image_contour, contours, -1, (255, 0,0), 1)

              #Profile sur le contour des régions
              Profile=profile(image_contour)

              ##Dessiner le centre en rouge BGR
              cv2.circle(image_contour, (cx,cy), 2, (0, 0,255),-1)

              #cv2_imshow(image_contour)

              #Sauvegarde des contours
              #j=0
              #contourName="Contour"+str(j)+".png"
              #cv2.imwrite(contourName,image_contour)
              #j=+1


              # Obtenir les coordonnées x et y des pixels du contour
              x, y = np.nonzero(edges)


              contour_point=[]
              for i in range(len(x)):
                  contour_point.append([x[i],y[i]])


              # Calculer les distances des bords du contour au centre pour chaque pixel
              distances = np.sqrt((x - cx)**2 + (y - cy)**2)

              # Ecart aux distances
              if (len(distances)!=0):


                      #Calcul de la rugosité du contour (ecart types entre les distance)
                      rugosite=np.std(distances)
                      Rugosite.append(rugosite);
                      #print("Rugosite:",rugosite)

              else:
                #Cas ou il n'y a aucune distance
                      rugosite=-1
                      Rugosite.append(rugosite);





              #print("Region No:"+str(region.label)+" "+str(1-region.euler_number)+" "+str(region.area)+" "+str(circ(region))+" "+str(aspect_rat1)+" "+str(aspect_rat2)+" "+str(region.perimeter)+" "+str(region.perimeter_crofton)+"  "+str(region.orientation)+" "+str(region.major_axis_length)+" "+str(region.minor_axis_length)+" "+str(region.equivalent_diameter)+" "+str(Dist_centre(region.centroid,size))+" " +str(region.centroid)+" " +str(rugosite)+" " +str(Profile[0])+" " +str(Profile[1]))
              Label.append(region.label);Trou.append(1-region.euler_number); Area.append(region.area);
              Rectangularite.append(region.extent); Solidite.append(region.solidity); Circ.append(circ(region));
              Perimetre.append(region.perimeter); Perim_crofton.append(region.perimeter_crofton); Orientation.append(region.orientation);   Major_axis.append(region.major_axis_length); Minor_axis.append(region.minor_axis_length); Feret_diameter.append(region.feret_diameter_max); Equivalent_diameter.append(region.equivalent_diameter); Distance_Centre.append(Dist_centre(region.centroid,size));   Centroid.append(region.centroid); Aspect_Ratio1.append(aspect_rat1); Aspect_Ratio2.append(aspect_rat2); Concavities.append(Concavite(region,subimage)); ProfileX.append(Profile[0]); ProfileY.append(Profile[1]); Euler0.append(region.euler_number); Elongation.append(elongation);

    data = {
            "Label" : Label,
            "Trou":Trou,
            "Euler":Euler0,
            "Area":Area,
            "Rectangularite":Rectangularite,
            "Elongation":Elongation,
            "Solidity":Solidite,
            "Circularite":Circ,
            "Aspect_ratio1":Aspect_Ratio1,
            "Aspect_ratio2":Aspect_Ratio2,
            "Perimetre":Perimetre,
            "Perim_crofton":Perim_crofton,
            "Orientation":Orientation,
            "Major_axis":Major_axis,
            "Minor_axis":Minor_axis,
            "Feret_diameter":Feret_diameter,
            "Equivalent_diameter":Equivalent_diameter,
            "Distance_Centre":Distance_Centre,
            "CentroidX" :Centroid,
            "Rugosite":Rugosite,
            "Concavite":Concavities,
            "ProfileX":ProfileX,
            "ProfileY":ProfileY
           }

    #load data into a DataFrame object:
    df = pd.DataFrame(data)
    df.to_csv(file)

    #file_b=file[:len(file)-4]+"_trou.csv"

    #label_image_brut = label(im)   # image brute pour compter les trous
    #Regions_brut = regionprops(label_image_brut)
    #Les trous brutes
    #Trou_brut=[]; Label_b=[]; Euler =[];

    #for reg in Regions_brut:
    #   Label_b.append(reg.label);

    #   Trou_brut.append(1-reg.euler_number);
    #   Euler.append(reg.euler_number);


    # data_brut ={"Label" : Label_b,
    #        "Trou":Trou_brut,
    #         "Euler":Euler
    #         }
    # dp = pd.DataFrame(data_brut)
    # dp.to_csv(file_b)



#INITIALISATION DE 3 FICHIERS VIDE avec entete POUR REGROUPER TOUTES LES 3 CLASSES DE TOUTES LES POMMES de par arbre le tout  mis dans un DOSSIER   ....summary
#file: nom du fichier qui represente la classe qui y sera regroupé
#folder: chemin menant au dossier de l'arbre concerné

def init_df(file,folder):

        data = {
                    "Label" : [],
                    "Trou":[],
                    "Euler":[],
                    "Area":[],
                    "Rectangularite":[],
                    "Elongation":[],
                    "Solidity":[],
                    "Circularite":[],
                    "Aspect_ratio1":[],
                    "Aspect_ratio2":[],
                    "Perimetre":[],
                    "Perim_crofton":[],
                    "Orientation":[],
                    "Major_axis":[],
                    "Minor_axis":[],
                    "Feret_diameter":[],
                    "Equivalent_diameter":[],
                    "Distance_Centre":[],
                    "CentroidX" :[],
                    "Rugosite":[],

                    "Concavite":[],
                    "ProfileX":[],
                    "ProfileY":[]
                }

        df = pd.DataFrame(data)

        df.to_csv(folder+"/"+file)

#Pour regrouper 2 classes contenues dans 2 fichiers csv

#A chaque fois on ajoute à summary_class le contenu de file_class


#summary_class: contient le nom du fichier csv  pour les regroupements des d'une classe d'un arbre donné
#file_class: nom du fichier contenant la données à regouper dans summary_class
#path2: chemin menant a un dossier nommé au nom de l'arbre dont les données sont regroupés

def Contat2csv(summary_class, file_class,folder):
    df1 = pd.read_csv(folder+"/"+summary_class)
    os.remove(folder+"/"+summary_class)
    df2 = pd.read_csv(file_class)

#suppression des regions d'aire inferieur à 600 px

#    df1 = df1.drop(df1[df1['Area'] < 600].index)
#    df2 = df2.drop(df2[df2['Area'] < 600].index)

    # Concaténer les deux fichiers
    df_concat = pd.concat([df1, df2], ignore_index=True)

    # Enregistrer le fichier concaténé
    df_concat.to_csv(folder+"/"+summary_class, index=False)

#REGROUPEMENT DES CLASSES PAR ARBRE

#destination folder

# summary_folder: contient le nom du Dossier (sera creer automatiquement) qui contiendra les fichier de regroupement par arbre
#morphometric_path: contient le chemin qui mene au dossier connenant les données de morphometries à regrouper pour chaque pomme de chaque arbre

def REGROUPEMENT_DES_CLASSES_PAR_ARBRE(morphometric_path,summary_folder):
    print("*********************************REGROUPEMENT DES DONNEES  MORPHEMETRIES PAR ARBRE POUR CHAQUE CLASSE**********************************************")
    if not os.path.exists(summary_folder):
            print("create:",summary_folder)
            os.mkdir(summary_folder)



    number_of_class=4                #number  of real class +1 (background)
    tab=[]

   # Mask_path="im_test_morphometric/"

    num_of_tree=0;
    num_of_image=0;
    for path, dirs, files in os.walk(morphometric_path):
          # if(path!=folder_path):
            path0=path.split("/")
            path0= path0[len(path0)-1]

            if "pdf" in path0:
                path2=summary_folder+"/"+path0
                print("***** ",path0," *****")
                num_of_tree=num_of_tree+1;


                print("****Grouping of the ",str(number_of_class-1)+" morphometrics csv files of the tree " +" "+path0+" in the folder 'Summary'*****")
                if not os.path.exists(path2):
                        print("create",path2)
                        os.mkdir(path2)

                for  i in range(1,number_of_class):
                  file="Class"+str(i)+".csv"
                  init_df(file,path2)

            if(len(files)!=0):
              num_of_image=num_of_image+1;
              tab.append(len(files))
              for f in files:
                  for  i in range(1,number_of_class):
                      if f.endswith("Class"+str(i)+".csv"):
                            file_class=path+"/"+f
                            summary_class="Class"+str(i)+".csv"

                            Contat2csv(summary_class, file_class,path2)
    print("Number of tree:",num_of_tree)
    print("Number of image(pomme):",num_of_image)



#Apres avoir regrouper les classes de chaque arbre nous moyennons le tout ds un fichier le tout dans un fichier

#summary_folder: chemin menant au dossier de regoupement par arbre
#mean_folder: nom du Dossier (sera creer automatiquement) contenant le fichier des données moyennées

def DATA_MEANS(summary_folder,mean_folder):
              print("*********************************Moyenne DES DONNEES DE MORPHEMETRIES PAR ARBRE**********************************************")


              if not os.path.exists(mean_folder):
                  print("Creating the folder:  " + mean_folder+"....")
                  os.mkdir(mean_folder)

              total_Area=[]; nbre_region=[]; tree_name=[]; Classes=[]; Number_of_region=[]; Trou=[];
              Euler=[]; Area=[]; Rectangularite=[]; Elongation=[]; Solidity=[]; Circularite=[]; Aspect_Ratio1=[]; Aspect_Ratio2=[];
              Perimetre=[]; Perim_crofton=[]; Orientation=[]; Major_axis=[]; Minor_axis=[]; Feret_diameter=[]; Equivalent_diameter=[];
              Distance_Centre=[]; Rugosite=[];  Concavite=[]; ProfileX=[]; ProfileY=[];

              cpt=0
              number_of_class=4                  #number  of real class +1 (background)

              # path of the files to group
              #Mask_path="/content/gdrive/MyDrive/Mini_projet/Summary_by_tree_k4_2022/"



              num_of_tree=0;
              for path, dirs, files in os.walk(summary_folder):
                    # if(path!=folder_path):
                      path0=path.split("/")
                      path0= path0[len(path0)-1]

                      if "pdf" in path0:
                          #print()
                          #print("***** ",path0," *****")
                          num_of_tree=num_of_tree+1;
                          for i in range(number_of_class-1):
                              tree_name.append(path0)


                      if(len(files)!=0):
                        for f in files:
                            if f.endswith("trou.csv"):
                              os.remove(path+"/"+f)

                            for  i in range(1,number_of_class):
                                if f.endswith("Class"+str(i)+".csv"):
                                      cpt=cpt+1
                                      file_class=path+"/"+f
                                      df2 = pd.read_csv(file_class)
                                      #print("Number of region of ",f," :",len(df2['Label']))
                                      #print("Taille (pixels): ",sum(df2['Area']))

                                      Classes.append("Class"+str(i));
                                      Number_of_region.append(len(df2['Label']))

                                      total_Area.append(np.mean(df2['Area']))

                                      Trou.append(np.mean(df2['Trou'])); Area.append(np.mean(df2['Area'])); Rectangularite.append(np.mean(df2['Rectangularite'])); Elongation.append(np.mean(df2['Elongation']));

                                      Solidity.append(np.mean(df2['Solidity'])); Circularite.append(np.mean(df2['Circularite'])); Perimetre.append(np.mean(df2['Perimetre']));
                                      Perim_crofton.append(np.mean(df2['Perim_crofton']));

                                      Orientation.append(np.mean(df2['Orientation']));   Major_axis.append(np.mean(df2['Major_axis'])); Minor_axis.append(np.mean(df2['Minor_axis']));

                                      Feret_diameter.append(np.mean(df2['Feret_diameter'])); Equivalent_diameter.append(np.mean(df2['Equivalent_diameter'])); Distance_Centre.append(np.mean(df2['Distance_Centre']));

                                      Aspect_Ratio1.append(np.mean(df2['Aspect_ratio1'])); Aspect_Ratio2.append(np.mean(df2['Aspect_ratio2'])); Concavite.append(np.mean(df2['Concavite']));

                                      ProfileX.append(np.mean(df2['ProfileX'])); ProfileY.append(np.mean(df2['ProfileY'])); Euler.append(np.mean(df2['Euler']));
                                      Rugosite.append(np.mean(df2['Rugosite']));



                                      break;

              data={
                  "Arbre" :tree_name,
                  "Classe":Classes,
                  "Area":Area,
                  "Number_of_region":Number_of_region ,
                  "Euler":Euler,
                  "Trou":Trou,
                  "Rectangularite":Rectangularite,
                  "Elongation":Elongation,
                  "Solidity":Solidity,
                  "Circularite":Circularite,
                  "Perimetre":Perimetre,
                  "Perim_crofton":Perim_crofton,

                  "Orientation":Orientation,
                  "Major_axis":Major_axis,
                  "Minor_axis":Major_axis,

                  "Aspect_ratio1":Aspect_Ratio1,
                  "Aspect_ratio2":Aspect_Ratio2,

                  "Feret_diameter":Feret_diameter,
                  "Equivalent_diameter":Equivalent_diameter,
                  "Distance_Centre":Distance_Centre,
                  "Concavite":Concavite,

                  "ProfileX":ProfileX,
                  "ProfileY":ProfileY,
                  "Rugosite":Rugosite
                  }



              #print("Trou:",len(data["Trou"]))
              #print("All_Area:",len(data["Area"]))
              #print("Number_of_region:",len(data["Number_of_region"]))
              #print("Concavite:",len(data["Concavite"]))

              #print("Distance_Centre:",len(data["Distance_Centre"]))
              #print("Feret_diameter:",len(data["Feret_diameter"]))
              #print("Concavite:",len(data["Concavite"]))


              #print("Orientation:",len(data["Orientation"]))
              #print("Major_axis:",len(data["Major_axis"]))
              #print("Minor_axis:",len(data["Minor_axis"]))

              #print("Aspect_ratio1:",len(data["Aspect_ratio1"]))
              #print("Aspect_ratio2:",len(data["Aspect_ratio2"]))
              #print("Perim_crofton:",len(data["Perim_crofton"]))

              #print("Perimetre:",len(data["Perimetre"]))
              #print("ProfileX:",len(data["ProfileX"]))
              #print("ProfileY:",len(data["ProfileY"]))

              #print("Euler:",len(data["Euler"]))
              #print("Solidity:",len(data["Solidity"]))
              #print("Elongation:",len(data["Elongation"]))
              #print("Energie:",len(data["Energie"]))
              #print("Rugosite:",len(data["Rugosite"]))


              df = pd.DataFrame(data);
              #df.to_csv("Summary_by_tree"+"/"+"Summary_by_tree.csv");
              print("***Save the mean by tree files in : ",os.path.join(mean_folder,"Mean_by_tree_.csv"))
              df.to_csv(os.path.join(mean_folder,"Mean_by_tree_.csv"));
              print("***Number of tree: ",num_of_tree)

              print("Nombre total de region: ",sum(Number_of_region))
              #print("Cpt=",cpt)



# Extraire et enregister les contour d'un mask


#mask: matrice du mask
#n_class num de la classe
#mask_name= nom de l'arbre suivis du numero de pomme
#contour_folder: dossie d'acceuil des contours

def Extact_contour(mask,n_class,mask_name,contour_folder):

        #mask=mask[:,:,0]
        #cv2_imshow(mask)

        # Chemin du dossier parent
        #contour_folder = "im_test_contour"

        # Création du dossier parent
        os.makedirs(contour_folder, exist_ok=True)

        # Liste des noms des sous-dossiers
        subdirectories = ["contour_class1", "contour_class2", "contour_class3"]

        # Création des sous-dossiers à l'intérieur du dossier parent
        for subdirectory in subdirectories:
            path = os.path.join(contour_folder, subdirectory)
            os.makedirs(path, exist_ok=True)



        contours, _ = cv2.findContours(mask, cv2.RETR_CCOMP, cv2.CHAIN_APPROX_SIMPLE)

        label_im = label(mask)
        Regions = regionprops(label_im)


        #print("Nbre regions: ",len(Regions))

        # Extraire le contour externe dans le cas d'un objet compact sinon extraire tout

        contour_array = []

        if len(Regions)==1:
          #largest_contour = max(contours, key=cv2.contourArea)
          #largest contour
          contours = [max(contours, key=cv2.contourArea)]



        for contour in contours:

            image_contour=np.ones((mask.shape[0],mask.shape[1],3))*255

                      # Dessiner le contour sur l'image
                      #cv2.drawContours(contour_image, [external_contour], 0, (0, 0, 255), 2)
            cv2.drawContours(image_contour, contour, -1, (255, 0,0), 1)

                      # Afficher l'image avec le contour
            #cv2_imshow(image_contour)


            for point in contour:
                x, y = point[0]
                contour_array.append([float(x), float(y)])

        with open(os.path.join(contour_folder,subdirectories[n_class-1],mask_name+'.csv'), 'w', newline='') as csvfile:
            writer = csv.writer(csvfile)
            writer.writerow(['X', 'Y'])  # En-têtes des colonnes
            writer.writerows(contour_array)

        print("Save contour point",os.path.join(contour_folder,subdirectories[n_class-1],mask_name+'.csv'))





#Parcours des dossiers images,classification (kmeans), extractions et application des morphometries au masks puis enregistrements
#dans un dossier pour chaque pomme de chaque arbre


# k: Number of class for the kmeans images

#folder_path: nom du dossier contennant les images par arbre
def Main(folder_path,k):

    #Mask and morpho files folder
    Mask_path=folder_path+"_morphometric"

     #Mask and morpho files folder
    All_Mask_path=folder_path+"_All_MASK"

    if not os.path.exists(Mask_path):
       print("Creating the folder:  " + Mask_path+"....")
       os.mkdir(Mask_path)


    if not os.path.exists(All_Mask_path):
       print("Creating the folder:  " + All_Mask_path+"....")
       os.mkdir(All_Mask_path)



    for path, dirs, files in os.walk(folder_path):

        if(path!=folder_path):
            path0=path.split("/")

            #Nom de l'arbre
            path0= path0[len(path0)-1]

            print("**Arbre***"+path0+"*****")
            #path1=Mask_path+path0+"/"
            path1=os.path.join(Mask_path,path0)
            if not os.path.exists(path1):
              print("Creating the folder:  " + path0+"....")
              os.mkdir(path1)


            num_pomme=0

            for num_pomme in range(1,5):
                print()
                filename="page_0_pomme_"+str(num_pomme)+"_NA.png"

                if filename in files:


                  current_path=os.getcwd()
                  Im_RGB_file=os.path.join(current_path,path,filename)


                  Class_img_Name=os.path.join(current_path,path,filename.replace(".png","_Kmeansk"+str(k)+".png"))
                  imgRGB = cv2.imread(Im_RGB_file)
                  #cv2_imshow(imgRGB)

                  # Classification de l'image RGB
                  Classified_img=K_Means(imgRGB,k)

                  Classified_img=Classified_img[:,:,0]
                  #cv2_imshow(Classified_img)

                  cv2.imwrite(Class_img_Name, Classified_img)


                  path2=os.path.join(path1,filename[0:len(filename)-4])

                  if not os.path.exists(path2):
                    print("Creating the folder:  "+filename[0:len(filename)-4]  +"....")
                    os.mkdir(path2)

                  im_fileRGB=path2+"/"+filename
                  print("Saving RGB image: "+filename+"....")
                  cv2.imwrite(im_fileRGB, imgRGB)


                  Class_img_Name=os.path.join(path2,filename.replace(".png","_Kmeansk"+str(k)+".png"))
                  print("Saving classified image: "+filename.replace(".png","_Kmeansk"+str(k)+".png")+"....")
                  cv2.imwrite(Class_img_Name, Classified_img)


                  print("Computing Masks of "+filename+"....")

                  #Extraction des masks et calcul des morphometries
                  Masks=Extract_class(Class_img_Name)


                  for i in range(len(Masks)):

                      mask_file=os.path.join(path2,filename.replace(".png","_Kmeansk"+str(k)+"_Class"+str(i+1)+".png"))
                      all_mask_file=os.path.join(All_Mask_path,path0[0:len(path0)-4]+"_"+filename.replace(".png","_Kmeansk"+str(k)+"_Class"+str(i+1)+".png"))

                      #imfile=path2+"/"+filename[0:len(filename)-4]+"_Class"+str(i+1)+".png"

                      Area_threshold = 600
                      Mask_del_small=Delete_small_area(Masks[i],Area_threshold)

                      #Saving mask in folder a specific folder (arbre et pomme)
                      print("Saving Mask "+filename.replace(".png","_Kmeansk"+str(k)+"_Class"+str(i+1)+".png")+"....")
                      cv2.imwrite(mask_file, Mask_del_small)

                      #Saving mask in the same folder
                      cv2.imwrite(all_mask_file, Mask_del_small)

                      #Extraire les contours et les enregistrer dans  des fichiers csv regrouper  par classe dans un dossier.
                      contour_folder=folder_path+"_contour"
                      n_class=i+1 # la classe du mask

                      mask_name=path0[0:len(path0)-5]+str(num_pomme)+".png"

                      #mask_name=mask_name.replace("page_0_pomme_","")
                      print("Extract and saving contour of "+mask_name+" de Classe"+str(i+1)+"....")


                      Extact_contour(Mask_del_small,n_class,mask_name,contour_folder)


                      #File in which we will store morphometrics of a mask
                      #csv_file=path2+"/"+filename[0:len(filename)-4]+"_Class"+str(i+1)+".csv"

                      csv_file=os.path.join(path2,filename[0:len(filename)-4]+"_Class"+str(i+1)+".csv")

                      Shape_carac(mask_file,csv_file)

                  print()

    #destination folder
    #REGROUPEMENT_DES_CLASSES_PAR_ARBRE
    summary_folder=folder_path+'_Summary_by_tree'

    REGROUPEMENT_DES_CLASSES_PAR_ARBRE(Mask_path,summary_folder)


   #dossier contenant les moyennes par arbre
    mean_folder=folder_path+'_mean_by_tree'

    #Moyenne DES donnée PAR_ARBRE et par classe
    DATA_MEANS(summary_folder,mean_folder)









#Image_folder: dossier contenant dossier des images RBG par  arbre (dossier arbre)
Image_folder ='im_test'
#Image_folder ='images2021'
#Image_folder ='Images2022'
#Number of class for the kmeans segmentations images
k=4

#Comme sortis on aura:
 #...._morphometric: dossier contenant les données de mophométrie pour chaque pommes de chaque arbre y compris l'image RGB, l'image classifié et les image des motifs de chaque classe
 # ....._All_MASK: dossier contenant tous les masks générer
 #....._contour: dossier contenant les coordonnées des contours dans un fichier csv regroupé par classe
 #....._Summary_by_tree: dossier contenant les données de mophométrie regroupé par arbre et par classe
 #....._mean_by_tree: dossier contenant les données de mophométrie moyennées par arbre et par classe
Main(Image_folder,k)



