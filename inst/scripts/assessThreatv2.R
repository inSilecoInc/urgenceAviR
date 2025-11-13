### 2025-07-29 Premier jet cartes et enjeux
library("urgenceAviR")
library(sf)
library(scales)
library(plyr)
library(dplyr)
library(lubridate)
library(ks)
library(terra)
library(mapview)


#dossier de base
dir<-"//int.ec.gc.ca/sys/ingeo/GW/EC1130MigBirds_OiseauxMig/QC_SCF_OM/UrgenceAviR_EmeRgencyApp/Scripts"
#dossier de l'incident
dir2<-"Z:/PIU/Requete de données/Patrimoine naturel_cegrim"
# 
# 
# ### dataset----------
# source(paste0(dir,"/zzz20250722.R"))
# source(paste0(dir,"/get_species_codes.R"))
# source(paste0(dir,"/load_biomq.R"))
# source(paste0(dir,"/load_somec.R"))
# source(paste0(dir,"/load_macreuses.R"))
# source(paste0(dir,"/load_eiders.R"))
# source(paste0(dir,"/load_oies.R"))
# source(paste0(dir,"/load_garrots.R"))
# source(paste0(dir,"/load_canards.R"))
# source(paste0(dir,"/load_sauvagine_fleuve.R"))
# source(paste0(dir,"/load_Iles_Nunavik.R"))
# source(paste0(dir,"/load_Inventaire_aerien_Nunavik.R"))
# source(paste0(dir,"/load_all_datasets.R"))
# 
# d<-load_all_datasets()
# d <- st_as_sf(d[!is.na(d$latitude),], coords = c("longitude", "latitude"), crs = 4326)
# nrow(d)
# 
# ### espèces-----------
# source(paste0(dir,"/Species.R"))
# source(paste0(dir,"/SpeciesGroups.R"))
# seaduck
# seaduckfr
# duck
# goose
# head(taxo)
# head(speciescode)
# 
# ### spatial-----------
# source(paste0(dir,"/LoadLand.R"))
# unique(land$name)
# unique(land.$NAME_Fr)
### hydro
# wc<-st_read("Land/Hydro/canvec_250K_QC_Hydro.gdb","watercourse_1")
# wb<-st_read("Land/Hydro/canvec_250K_QC_Hydro.gdb","waterbody_2")
# ### installation de manutention d'hydrocarbures
# imh<-st_read(paste0("C:/Users/BeaumontMa/Desktop/Temp/Land/Localisation IMH - Région du Québec.shp"))
# imh<-st_transform(imh,st_crs(land))
# ### cadre ecologique de référence
# cer<-st_read("Land/CER_GOLFE/CER_GOLFE.shp")
# ### ecoreqion
# bio<-st_read("Land/DFO_Marine_Bioregions.gdb","DFO_Marine_Bioregions")
# ### zico
# zico<-st_read("//int.ec.gc.ca/sys/ingeo/gdr/NON_EC_DATA/007-environment/CA/ZICO_IBA/ZICO_IBA_Canada.gdb","ZICO_IBA_Canada")
# #terres fédérales
# ribf<-st_read("//int.ec.gc.ca/sys/ingeo/gdr/NON_EC_DATA/015-planningCadastre/CA-QC/RBIF_QC/RBIF_QC.gdb","DFRP_RBIF_Property_Bien_a")
# #Aires protégées
# ap<-st_read("//int.ec.gc.ca/sys/ingeo/GW/EC1142ProtAreas_AiresProt/QC_SCF_Donnees/007Environnement/Aires_protegees/CPCAD_CARTS/CPCAD/AireProtegeeConservee_2023.gdb",
#             "AireProtegeeConservee_2023")
# ### rnf et rom
# ap.scf<-ap[ap$MGMT_F%in%"Service canadien de la faune, Région du Québec" ,]
# rm(ap)#trop lourd
# ### municipalités
# mun<-st_read("//int.ec.gc.ca/sys/InGEO/gdr/NON_EC_DATA/003-boundaries/CA-QC/Decoupages_Administratifs/Decoupages_Administratifs.gdb","munic_s")
# ### cdpnq polygones
# 
# 
# ### Sites de concentration de sauvagine les plus vulnérables-----------
# zone<-data.frame(nom=c("Section fluviale Est",
#                        "Estuaire Ouest",
#                        "Estuaire Est",
#                        "Saguenay Lac Saint-Jean",
#                        "Baie des Chaleurs",
#                        "Baie de Carillon",
#                        "Baie Clément",
#                        "Lacolle",
#                        "Richelieu",
#                        "Saint-Régis",
#                        "Pointe Castagner",
#                        "Canal de Beauharnois",
#                        "Lac Staint-Louis",
#                        "Lac des Deux Montagnes",
#                        "Iles de Boucherville",
#                        "Iles de Contrecoeur",
#                        "Iles de Sorel",
#                        "Baie de Maskinongé",
#                        "Baieville",
#                        "Pointe du Lac",
#                        "Gentilly",
#                        "Grondines",
#                        "Portneuf",
#                        "Donnacona",
#                        "Neuville",
#                        "Ile d'Orléans",
#                        "Cap Tourmente",
#                        "Saint-Vallier",
#                        "Montmagny",
#                        "Ile aux Grues",
#                        "Saint-Jean-Port-Joly",
#                        "Baie de Kamouraska",
#                        "Iles de Kamouraska",
#                        "Notre-Dame-du-Portage",
#                        "Isle-Verte",
#                        "Trois-Pistoles",
#                        "Les Méchins",
#                        "Grande-Vallée",
#                        "Cap des Rosiers",
#                        "Tadoussac",
#                        "Les Escoumins",
#                        "Baie des Milles Vaches",
#                        "Baie aux Outardes",
#                        "Chûte aux Outardes",
#                        "Pointe des Monts",
#                        "Baie des Homards",
#                        "Baie des Sept-Iles",
#                        "Moisie",
#                        'Manitou',
#                        "Mingan",
#                        "Natashquan",
#                        "Anticosti Ouest",
#                        "Anticosti Est",
#                        "Penouil",
#                        "Percé",
#                        "Chandler",
#                        "Bonaventure",
#                        "Baie de Cascapédia",
#                        "Bassin de la rivière Nouvelle",
#                        "Restigouche"))
# 
# for(i in zone$nom){
#   print(i)
#   if(i%in%zone$nom[1]){zone_sf<-st_read(paste0(dir2,"/kml/",i,".kml"))}
#   if(!i%in%zone$nom[1]){zone_sf<-rbind(zone_sf,st_read(paste0(dir2,"/kml/",i,".kml")))}
# }
# st_write(zone_sf,paste0(dir2,"/kml/AtlasSitesConcentrationOiseauxAquatiquesVulnérables.kml"),append=FALSE)

# ###cut hydro 
# wb<-st_intersection(wb,st_union(st_transform(zone_sf,st_crs(wb))))
# wc<-st_intersection(wc,st_union(st_transform(zone_sf,st_crs(wc))))

# save.image(paste0(dir2,"/RData/","PatrimoineNaturelCegrim_",Sys.Date(),".RData"))
load(paste0(dir2,"/RData/","PatrimoineNaturelCegrim_2025-08-05.RData"))

d$code_id<-ifelse(d$code_id%in%"UNSC","SCOT_UNI",d$code_id)
d$code_id<-ifelse(d$code_id%in%"UNLG_UNI","GULL_UNI",d$code_id)
taxo$English_Name[grep("Gu",taxo$English_Name)]
tmp<-data.frame(matrix(ncol=ncol(taxo),nrow=1))
names(tmp)<-names(taxo)
taxo<-rbind(taxo,tmp)
taxo$Species_ID[nrow(taxo)]<-"GULL_UNI"
taxo$English_Name[nrow(taxo)]<-"Unidentified Gull"
### cartes-------------
i<-zone$nom[3]
dev.off()
d$year<-year(d$date)
for (j in c("sauvagine","canards de mer", "oiseaux marins")){
  for(i in zone$nom){
    
    area<-zone_sf[zone_sf$Name%in%i,]
    
    #sauvagine
    dtmp2<-st_intersection(d[d$year>1999,],area)
    #Sauvagine
    if(j%in%"sauvagine"){
      dtmp<-st_intersection(dtmp2[dtmp2$code_id%in%c(taxo$Species_ID[match(c(goose,duck),taxo$English_Name)],"DUCK_UNI" ),],area)
      dtmp<-dtmp[!is.na(dtmp$abondance),]
      if(nrow(dtmp)<5){next}
      ### sommaire par jour
      case<-ddply(dtmp,.(code_id),summarize,
                  date=length(unique(date)),
                  sum=sum(abondance))
      png(paste0(dir2,"/png/",i,"_sauvagine.png"),res=300,width=10,height=6,unit="in")
    }
    
    #Canards de mer
    if(j%in%"canards de mer"){
      tmp<-c(taxo$Species_ID[match(c(seaduck),taxo$English_Name)],"SCAU_UNI","SCOT_UNI","GOLD_UNI" ,"LOON_UNI")
      tmp<-tmp[!is.na(c(taxo$Species_ID[match(c(seaduck),taxo$English_Name)],"SCAU_UNI","SCOT_UNI","GOLD_UNI" ,"LOON_UNI"))]
      dtmp<-st_intersection(dtmp2[dtmp2$code_id%in%tmp,],area)
      dtmp<-dtmp[!is.na(dtmp$abondance),]
      if(nrow(dtmp)<5){next}
      ### sommaire par jour
      case<-ddply(dtmp,.(code_id),summarize,
                  date=length(unique(date)),
                  sum=sum(abondance))
      png(paste0(dir2,"/png/",i,"_canards de mer.png"),res=300,width=10,height=6,unit="in")
    }
    
    #Oiseaux marins
    if(j%in%"oiseaux marins"){
      unique(dtmp2$dataset)
      dtmp<-st_intersection(dtmp2[dtmp2$dataset%in%"biomq",],area)
      dtmp<-dtmp[!is.na(dtmp$abondance),]
      if(nrow(dtmp)<5){next}
      ### sommaire par année
      case<-ddply(dtmp,.(code_id),summarize,
                  date=length(unique(year)),
                  sum=sum(abondance))
      png(paste0(dir2,"/png/",i,"_oiseaux marins.png"),res=300,width=10,height=6,unit="in")
    }
    
    #hydro
    wb.tmp<-st_intersection(wb,st_transform(area,st_crs(wb)))
    wb.tmp<-st_transform(wb.tmp,st_crs(area))
    wc.tmp<-st_intersection(wc,st_transform(area,st_crs(wc)))
    wc.tmp<-st_transform(wc.tmp,st_crs(area))
    
    print(i)
    case$rel<-case$sum/case$date
    options(scipen=999)
    print(case[order(case$rel,decreasing=TRUE),])
    
    # Create a grid
    cellsize <- 0.01#en degré lat long
    if(i%in%c("Baie des Chaleurs",
              "Saguenay Lac Saint-Jean",
              "Estuaire Est",
              "Estuaire Ouest",
              "Les Méchins",
              "Grande-Vallée",
              "Pointe des Monts",
              "Baie des Homards",
              "Manitou",
              "Mingan",
              "Natashquan",
              "Anticosti Ouest",
              "Anticosti Est",
              "Bonaventure",
              "Section fluviale Est")){
      cellsize <- 0.1#en degré lat long
    }
    grid <- st_make_grid(area, cellsize = cellsize, square = TRUE)
    bbox <- st_bbox(grid)
    x_range <- seq(bbox["xmin"], bbox["xmax"], by = cellsize)
    y_range <- seq(bbox["ymin"], bbox["ymax"], by = cellsize)
    centroids <- expand.grid(x = x_range, y = y_range)
    centroids_sf <- st_as_sf(centroids, coords = c("x", "y"), crs = st_crs(d))
    centroids_sf$id_pixel <- seq_len(nrow(centroids_sf))
    centroids_sf <- st_transform(centroids_sf, st_crs(d))
    centroids_sf$longitude_centroide <- st_coordinates(centroids_sf)[, 1]
    centroids_sf$latitude_centroide <- st_coordinates(centroids_sf)[, 2]
    
    #pixel
    df_centroids <- st_join(dtmp, centroids_sf, join = st_nearest_feature)
    centroids_table <- df_centroids |>
      dplyr::select(id_pixel, longitude_centroide,latitude_centroide)
    df_pixel = df_centroids   |>  
      st_drop_geometry()
    df_pixel = df_pixel  |>
      group_by(id_pixel,longitude_centroide, latitude_centroide)|>
      summarise(n_date = length(unique((date))),
                median = median(abondance))
    df_pixel <- st_as_sf(df_pixel, coords = c("longitude_centroide", "latitude_centroide"), 
                         crs = 4326)
    
    #voir
    layout <- layout(matrix(c(1,1,1,1,1,1,1,1,2,2,3,3), 4, 3, byrow = F))
    
    #indice d'abondance
    brk<-c(0,quantile(df_pixel$median,0.25),median(df_pixel$median),quantile(df_pixel$median,0.75),max(df_pixel$median))[!duplicated(
      c(0,quantile(df_pixel$median,0.25),median(df_pixel$median),quantile(df_pixel$median,0.75),max(df_pixel$median))
    )]
    cols<-c("yellow","pink3","red","darkred")
    layout <- plot(wb.tmp$Shape,lwd=1.5,main="Quantile d'abondance",line=-0.5,col="white")+
      plot(df_pixel$geometry,pch=21,
           bg=alpha(cols[cut(df_pixel$median,brk)],0.3),add=TRUE,cex=3,
           col=alpha("grey25",0.2))+
      plot(df_pixel$geometry,pch=21,
           bg=alpha(cols[cut(df_pixel$median,brk)],0.3),add=TRUE,cex=3,
           col=alpha("grey25",0.2))+
      plot(dtmp2$geometry,pch=16,cex=0.6,add=TRUE,col=alpha("grey25",0.5))+
      axis(1)+
      axis(2)
    
    #occurence
    layout <- plot(df_pixel$geometry,pch=16,
                   col=alpha("grey25",0.5),cex=c(0.5,1,1.5,2.5,3,4)[cut(df_pixel$n_date,c(-1,1,2,3,10))],
                   main=paste0("Occurence"))+
      plot(wb$Shape,add=TRUE,lwd=0.2)+
      axis(1)+
      axis(2)
    
    #barplot
    case<-case[order(case$rel),]
    layout <-  barplot(case$rel+1,names.arg=taxo$English_Name[match(case$code_id,taxo$Species_ID)],log="x",
                       xlab="Abondance relative",
                       ylab="",horiz=TRUE,las=1,xlim=c(1,100000))
    
    title(i,outer=TRUE,line=-1.5)
    dev.off()
  }
}


