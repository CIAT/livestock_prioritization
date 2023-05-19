require(terra)
require(data.table)
require(dplyr)

# downloaded from cg labs https://eia.scio.services:18002/hub/user-redirect/lab/tree/common_data/atlas_hazards/cmip6
data_dir<-"C:/Datasets/atlas_hazards/cmip6" 
save_dir<-"D:/livestock_prioritization/data_tz"
if(!dir.exists(save_dir)){
  dir.create(save_dir,recursive = T)
}

# Choose hazards
hazards<-c("NDD","NTx40","HSM_NTx35","HSH_max","THI_max","NDWS","TAI","NDWL0")

# Get meta-data
metafiles<-list.files(data_dir,".rda",recursive = T,full.names = T)
Meta.data<-rbindlist(lapply(metafiles, FUN=function(FILE){
  load(FILE)
  Meta.data[,c("dataset.title_short","dataset.title_long","dataset.desc","variable.name","method.description","file.filename")]
  
}))

Meta.data<-Meta.data[grepl(paste(hazards,collapse = "|"),file.filename)]
fwrite(Meta.data,paste0(save_dir,"/metadata.csv"))

# Get hazard layers
files<-list.files(data_dir,".tif",recursive = T,full.names = T)
files<-files[!grepl("aux",files)]
files<-grep(paste0(hazards,collapse="|"),files,value = T)

lapply(files,FUN=function(FILE){
  print(FILE)
  rast<-terra::rast(FILE)
  layers<-grep("mean.tif",names(rast),value=T)
  layers<-grep("historical|ENSEMBLE",layers,value=T)
  rast<-rast[[layers]]
  save_file<-tstrsplit(FILE,"/")
  save_file<-save_file[[length(save_file)]]
  writeRaster(rast,file=paste0(save_dir,"/",save_file),overwrite=T)
})


# Get stress classes
devtools::source_url("https://raw.githubusercontent.com/AdaptationAtlas/hazards/main/R/05_final_maps/makeClassTable.R")
classes<-data.table(make_class_tb())[index_name %in% c("HSH",hazards)]
fwrite(classes,paste0(save_dir,"/classes.csv"))
