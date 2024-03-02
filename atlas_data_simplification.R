require(terra)
require(sf)
require(data.table)

# Load vector of CGIAR countries ####
# To be compatible with the atlas these vectors should be changed to the data in s3://digital-atlas/boundaries
# The atlas boundaries should not require any corrections
# However you will to employ the countrycodes package to match iso3 country codes the un codes used in FAOstat data (for direct emissions calculations)

CGIAR_countries_sf<-sf::read_sf("Data/GADM/4.1/gadm41_0.shp",options = "ENCODING=UTF8")
CGIAR_countries_sf$NAME_0<-CGIAR_countries_sf$COUNTRY
CGIAR_countries_sf$ADMIN<-CGIAR_countries_sf$COUNTRY
CGIAR_countries_sf$ADMIN[grep("xico",CGIAR_countries_sf$ADMIN)]<-"Mexico"
CGIAR_countries_sf$ADMIN[grep("and Pr",CGIAR_countries_sf$ADMIN)]<-"Sao Tome and Principe"
CGIAR_countries_sf$ADMIN[grep("d'Ivoire",CGIAR_countries_sf$ADMIN)]<-"Cote d'Ivoire"
CGIAR_countries_sf$ADMIN[grep("Swaziland",CGIAR_countries_sf$ADMIN)]<-"eSwatini"
CGIAR_countries<-terra::vect(CGIAR_countries_sf)

CGIAR_countries2_sf<-CGIAR_countries_sf
CGIAR_countries2_sf$ADMIN[CGIAR_countries2_sf$ADMIN=="United Republic of Tanzania"]<-"Tanzania"
CGIAR_countries2_sf$ADMIN[CGIAR_countries2_sf$ADMIN=="Democratic Republic of the Congo"]<-"DRC"
CGIAR_countries2_sf$ADMIN[CGIAR_countries2_sf$ADMIN=="Central African Republic"]<-"CAR"
CGIAR_countries2<-terra::vect(CGIAR_countries2_sf)


# BaseRaster ####
# Raster layer to resample other layers
BaseRaster<-terra::rast("Data/Exposure/cell5m_livestock_vop.tif")
BaseRaster<-terra::crop(BaseRaster,CGIAR_countries2)
CellSize.km<-terra::cellSize(BaseRaster,mask=T, unit="km")

# TLUs ####
# Load [Number of livestock rasters](https://dataverse.harvard.edu/dataverse/glw_3) by [Gilbert et al., 2018](https://www.nature.com/articles/sdata2018227):
Cattle<-terra::rast("Data/GLW3/5_Ct_2010_Da.tif")
Buffalo<-terra::rast("Data/GLW3/5_Bf_2010_Da.tif")
Chicken<-terra::rast("Data/GLW3/5_Ch_2010_Da.tif")
Goat<-terra::rast("Data/GLW3/5_Gt_2010_Da.tif")
Horse<-terra::rast("Data/GLW3/5_Ho_2010_Da.tif")
Pig<-terra::rast("Data/GLW3/5_Pg_2010_Da.tif")
Sheep<-terra::rast("Data/GLW3/5_Sh_2010_Da.tif")
# We are using interpolated rasters indicated by `_Da` in the filename.

# Convert number of livestock into total livestock units (TLU) as per [Rothman-Ostrow et al. 2020](https://www.frontiersin.org/articles/10.3389/fvets.2020.556788/full):
TLU<-Cattle*0.7 + Buffalo*0.7 + Sheep*0.1 + Goat*0.1 + 0.01*Chicken + 0.2*Pig + 0.8*Horse
Livestock<-c(Cattle,Buffalo,Chicken,Goat,Horse,Pig,Sheep,TLU)
names(Livestock)<-c("Cattle.LU","Buffalo.LU","Chicken.LU","Goat.LU","Horse.LU","Pig.LU","Sheep.LU","Total.LU")

# VoP ####
# note vop, ruralpop and pasturearea are from cell5m so do not require resampling

# unit of vop is 100 USD
vop<-terra::rast(list.files("./Data/Exposure","cell5m_livestock_vop",full.names = T))
vop<-terra::resample(vop,BaseRaster,method="near")

# Rural Pop ####
ruralpop<-terra::rast(list.files("./Data/Exposure","cell5m_ruralpop_2020_v3",full.names = T))
ruralpop<-terra::resample(ruralpop,BaseRaster,method="near")

# Pasture Area ####
pasturearea<-terra::rast(list.files("./Data/Exposure","Pasture_area",full.names = T))
pasturearea<-terra::resample(pasturearea,BaseRaster,method="near")

TLU2<-TLU/terra::cellSize(TLU,unit="km")
TLU2<-terra::resample(TLU2,BaseRaster,method="cubic")
TLU2<-TLU2*CellSize.km

# Direct Emissions ####

# Load FAOSTAT Emissions data:
# Load FAO Emissions
FAO_Emissions<-data.table::fread("Data/FAO/Emissions_Totals_E_All_Data_NOFLAG.csv",encoding = "Latin-1")
# *Note that the FAO data includes emissions from manure*  

# Subset and aggregate [FAO Emissions](https://www.fao.org/faostat/en/#data/GT) Stats:
# Load FAOSTAT Emissions data:
FAO_Emissions<-data.table::fread("Data/FAO/Emissions_Totals_E_All_Data_NOFLAG.csv",encoding = "Latin-1")
# *Note that the FAO data includes emissions from manure*  
  
#Subset and aggregate [FAO Emissions](https://www.fao.org/faostat/en/#data/GT) Stats:
 FAO.Items<-c("Enteric Fermentation", "Manure applied to Soils", "Manure left on Pasture", "Manure Management")
 Elements<-c("Emissions (CO2eq) from CH4 (AR5)","Emissions (CO2eq) from N2O (AR5)")
 
 # Sum emissions from livestock across CH4 and N2O elements, calculate % change from 2000 to 2019, rename countries to match regions layer 
 FAO_Emissions<-FAO_Emissions[Item %in% FAO.Items & Source=="FAO TIER 1" & Element %in% Elements,list(Area,Item,Element,Unit,Y2000,Y2019)
 ][grep("CH4",Element),Element:="CH4"
 ][grep("N2O",Element),Element:="N2O"
 ][,list(Y2000=sum(Y2000),Y2019=sum(Y2019)),by=list(Area,Element,Unit)
 ][,Change:=Y2019/Y2000]

# Harmonize CGIAR and FAO Emissions country names:
  # Make data table of FAO vs CGIAR names where they differ
     UpdateFAOCountries<-data.table(
       FAO=c("Bolivia (Plurinational State of)",
             "Iran (Islamic Republic of)",
             "Syrian Arab Republic",
             "Congo",
             "Viet Nam",
             "Lao People's Democratic Republic",
             "Côte d'Ivoire",
             "Brunei Darussalam",
             "Venezuela (Bolivarian Republic of)",
             "Bahamas",
             "United Republic of Tanzania",
             "Eswatini",
             "Democratic People's Republic of Korea",
             "China, Taiwan Province of"),
       CGIAR=c("Bolivia",
               "Iran",
               "Syria",
               "Republic of the Congo",
               "Vietnam",
               "Laos",
               "Cote d'Ivoire",
               "Brunei",
               "Venezuela",
               "The Bahamas",
               "Tanzania",
               "eSwatini",
               "North Korea",
               "Taiwan")
     )
                                     
# Match then update FAO names with CGIAR names
 N<-match(FAO_Emissions[,Area],UpdateFAOCountries[,FAO])
FAO_Emissions[which(!is.na(N)),Area:=UpdateFAOCountries[N[!is.na(N)],CGIAR]]
FAO_Emissions[grep("d'Ivoire",FAO_Emissions$Area),Area:="Cote d'Ivoire"]

 # Reshape data
FAO_Emissions<-data.table::dcast(FAO_Emissions[,list(Area,Element,Change)],Area~Element,value.var = "Change")

# Check for missing countries:
CGIAR_countries$ADMIN[!CGIAR_countries$ADMIN %in% FAO_Emissions[,Area]]

# Add increase in Emissions Data to CGIAR_countries vectors & set NA values to 1 (no change):
CGIAR_countries$CH4<-FAO_Emissions[match(CGIAR_countries$ADMIN,Area),CH4]
CGIAR_countries$CH4[is.na(CGIAR_countries$CH4)]<-1
CGIAR_countries$N2O<-FAO_Emissions[match(CGIAR_countries$ADMIN,Area),N2O]
CGIAR_countries$N2O[is.na(CGIAR_countries$N2O)]<-1

# Rasterize increase in livestock emissions:
FAO.CH4.Change<-terra::rasterize(CGIAR_countries,BaseRaster,field="CH4")
names(FAO.CH4.Change)<-"CH4"
FAO.N2O.Change<-terra::rasterize(CGIAR_countries,BaseRaster,field="N2O")
names(FAO.N2O.Change)<-"N2O"

# Load livestock emissions data from [Herrero et al. 2013](https://www.pnas.org/content/110/52/20888):
LS.Files<-data.table::fread("Data/LPS/LS Files.csv")
LS.Files[,Meth:=paste0("^",Meth)][,N2O:=paste0("^",N2O)]

# Detailed methods for Herrero et al. 2013 can be found [here](https://www.pnas.org/doi/suppl/10.1073/pnas.1308149110/suppl_file/sapp.pdf)
CH4.km2.00<-terra::resample(terra::rast(LS.Files[,list.files(".",Meth,recursive = T),by=Meth][,V1]), BaseRaster)*CellSize.km
N2O.km2.00<-terra::resample(terra::rast(LS.Files[,list.files(".",N2O,recursive = T),by=N2O][,V1]), BaseRaster)*CellSize.km
names(CH4.km2.00)<-LS.Files[,Choice]
names(N2O.km2.00)<-LS.Files[,Choice]

# Update Herrero 2000 data by applying proportion change in emissions between 2000-2019 as calculated from FAO data:
CH4.km2.19<-CH4.km2.00*FAO.CH4.Change
N2O.km2.19<-N2O.km2.00*FAO.N2O.Change

# This is total direct emissions by livestock groups according to Herrero et al.
LS.CO2e<-CH4.km2.19+N2O.km2.19

plot(LS.CO2e)

# Indirect emissions (Forest Loss) ####
WRI.Pasture.CarbonLoss.ha.yr<-terra::rast("Data/GCLD/WRI.Pasture.CarbonLoss.ha.yr.tif")
WRI.Soy.CarbonLoss.ha.yr<-terra::rast("Data/GCLD/WRI.Soy.CarbonLoss.ha.yr.tif")

WRI.Pasture.CarbonLoss.pix.yr<-terra::rast("Data/GCLD/WRI.Pasture.CarbonLoss.pix.yr.tif")
WRI.Soy.CarbonLoss.pix.yr<-terra::rast("Data/GCLD/WRI.Soy.CarbonLoss.pix.yr.tif")

WRI.Both.CarbonLoss.ha.yr<-terra::rast("Data/GCLD/WRI.Both.CarbonLoss.ha.yr.tif")
WRI.Both.CarbonLoss.pix.yr<-terra::rast("Data/GCLD/WRI.Both.CarbonLoss.pix.yr.tif")