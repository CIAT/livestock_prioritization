require(smoothr)
SaveDir<-"Data/GADM/4.1"
if(!dir.exists(SaveDir)){
    dir.create(SaveDir)
    }


tolerance<-0.2

# https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
WB_countries<-read.csv("Data/GADM/WB Countries.csv")

Countries<-WB_countries$ISO3

options(timeout=1000)

admin1<-lapply(1:length(Countries),FUN=function(i){
    
    URL<-paste0("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_",Countries[i],"_shp.zip")
    destfile<-paste0(SaveDir,"/",Countries[i],".zip")
    
    # Display progress
    cat('\r                                                ')
    cat('\r',paste0("Downloading file: ",i,"-",URL))
    flush.console()
    
    if(!file.exists(destfile)){
            download.file(URL, destfile)
        }  
    
    unzip(destfile,exdir=SaveDir)
    
    shapefile<-grep(paste0(Countries[i],"_1.shp"),list.files(SaveDir,full.names=T),value=T)
    if(length(shapefile)==0){
      print(paste0(i," ",Countries[i]," - no admin1 file available using admin0"))
      shapefile<-grep(paste0(Countries[i],"_0.shp"),list.files(SaveDir,full.names=T),value=T)
     }
    
    X<-suppressWarnings(terra::vect(shapefile))
    X$Region<-WB_countries$Region[i]
    Rm<-list.files(SaveDir,Countries[i],full.names=T)
    Rm<-Rm[!grepl(".zip",Rm)]
    unlink(Rm,recursive=T)
    X
})

# May need to remove KIR n=63

admin1<-do.call(rbind,admin1)
admin1<-terra::simplifyGeom(admin1,tolerance=tolerance)


terra::plot(admin1)
terra::writeVector(admin1,file=paste0(SaveDir,"/gadm41_1.shp"),overwrite=T)


# Create Admin0 layer

# Minimum polygon size (to sieve out tiny islands)
min_size<-100 #km2
area_thresh <- units::set_units(min_size, km^2)

admin0<-lapply(1:length(Countries),FUN=function(i){
  
  URL<-paste0("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_",Countries[i],"_shp.zip")
  destfile<-paste0(SaveDir,"/",Countries[i],".zip")
  

  if(!file.exists(destfile)){
    # Display progress
    cat('\r                                                ')
    cat('\r',paste0("Downloading file: ",i,"-",URL))
    flush.console()
    download.file(URL, destfile)
  }  
  
  # Display progress
  cat('\r                                                ')
  cat('\r',paste0("Processing file: ",i,"-",URL))
  flush.console()
  
  unzip(destfile,exdir=SaveDir)
  
  shapefile<-grep(paste0(Countries[i],"_1.shp"),list.files(SaveDir,full.names=T),value=T)
  if(length(shapefile)==0){
    print(paste0(i," ",Countries[i]," - no admin1 file available using admin0"))
    shapefile<-grep(paste0(Countries[i],"_0.shp"),list.files(SaveDir,full.names=T),value=T)
  }
  
  X<-suppressWarnings(terra::vect(shapefile))
  X$Region<-WB_countries$Region[i]
  Rm<-list.files(SaveDir,Countries[i],full.names=T)
  Rm<-Rm[!grepl(".zip",Rm)]
  unlink(Rm,recursive=T)
  
  X<-terra::aggregate(X, by="COUNTRY", dissolve=TRUE)
  if(!i %in% c(8,36,71,73,118,122)){
    X<-sf::st_as_sf(X)
    X <- smoothr::drop_crumbs(X, threshold = area_thresh)
    X<-terra::vect(X)
  }
  X
})
admin0<-do.call(rbind,admin0)

WB_Regions<-terra::aggregate(admin0,by="Region",dissolve=T)
WB_Regions<-terra::simplifyGeom(WB_Regions,tolerance=tolerance)
WB_Regions<-terra::fillHoles(WB_Regions)
terra::plot(WB_Regions)
terra::writeVector(WB_Regions,file=paste0(SaveDir,"/WB_Regions.shp"),overwrite=T)


admin0<-terra::simplifyGeom(admin0,tolerance=tolerance)
terra::plot(admin0)

terra::writeVector(admin0,file=paste0(SaveDir,"/gadm41_0.shp"),overwrite=T)


