# loading packages ----
library(sf) # spatial data


# # importing shape files of Canadian national parks (visualisation only)----
# CAshape <- vect("data/shapefiles/CLAB_CA_2023-09-08/CLAB_CA_2023-09-08.shp")
# plot(CAshape)

# importing polygons with sf ----

ABpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_AB_2023-09-08/CLAB_AB_2023-09-08.shp")
plot(ABpolygon)
saveRDS(ABpolygon,file ="data/old/ABpolygon.rds")

BCpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_BC_2023-09-08/CLAB_BC_2023-09-08.shp")
plot(BCpolygon)
saveRDS(BCpolygon,file ="data/old/BCpolygon.rds")

MBpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_MB_2023-09-08/CLAB_MB_2023-09-08.shp")
plot(MBpolygon)
saveRDS(MBpolygon,file ="data/old/MBpolygon.rds")

NBpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_NB_2023-09-08/CLAB_NB_2023-09-08.shp")
plot(NBpolygon)
saveRDS(NBpolygon,file ="data/old/NBpolygon.rds")

NLpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_NL_2023-09-08/CLAB_NL_2023-09-08.shp")
plot(NLpolygon)
saveRDS(NLpolygon,file ="data/old/NLpolygon.rds")

NSpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_NS_2023-09-08/CLAB_NS_2023-09-08.shp")
plot(NSpolygon)
saveRDS(NSpolygon,file ="data/old/NSpolygon.rds")

NTpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_NT_2023-09-08/CLAB_NT_2023-09-08.shp")
plot(NTpolygon)
saveRDS(NTpolygon,file ="data/old/NTpolygon.rds")

NUpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_NU_2023-09-08/CLAB_NU_2023-09-08.shp")
plot(NUpolygon)
saveRDS(NUpolygon,file ="data/old/NUpolygon.rds")

ONpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_ON_2023-09-08/CLAB_ON_2023-09-08.shp")
plot(ONpolygon)
saveRDS(ONpolygon,file ="data/old/ONpolygon.rds")

PEpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_PE_2023-09-08/CLAB_PE_2023-09-08.shp")
plot(PEpolygon)
saveRDS(PEpolygon,file ="data/old/PEpolygon.rds")

QCpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_QC_2023-09-08/CLAB_QC_2023-09-08.shp")
plot(QCpolygon)
saveRDS(QCpolygon,file ="data/old/QCpolygon.rds")

SKpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_SK_2023-09-08/CLAB_SK_2023-09-08.shp")
plot(SKpolygon)
saveRDS(SKpolygon,file ="data/old/SKpolygon.rds")

YTpolygon <- st_read("data/shapefiles/ca_provinces/CLAB_YT_2023-09-08/CLAB_YT_2023-09-08.shp")
plot(YTpolygon)
saveRDS(YTpolygon,file ="data/old/YTpolygon.rds")

# fitering for my 30 parks out of all the parks in each polygon ----

#AB

waterton_lakes <- ABpolygon[ABpolygon$CLAB_ID == "WATE", ]
plot(waterton_lakes)
saveRDS(waterton_lakes,file ="data/old/waterton_lakes.rds")

elk_island <- ABpolygon[ABpolygon$CLAB_ID == "ELKI", ]
plot(elk_island)
saveRDS(elk_island,file ="data/old/elk_island.rds")

jasper <- ABpolygon[ABpolygon$CLAB_ID == "JASP", ]
plot(jasper)
saveRDS(jasper,file ="data/old/jasper.rds")

wood_buffalo <- ABpolygon[ABpolygon$CLAB_ID == "WOOD", ]
plot(wood_buffalo)
saveRDS(wood_buffalo,file ="data/old/wood_buffalo.rds")

banff <- ABpolygon[ABpolygon$CLAB_ID == "BANF", ]
plot(banff)
saveRDS(banff,file ="data/old/banff.rds")

# no polygon for grasslands

# BC

yoho <- BCpolygon[BCpolygon$CLAB_ID == "YOHO", ]
plot(yoho)
saveRDS(yoho,file ="data/old/yoho.rds")

kootenay <- BCpolygon[BCpolygon$CLAB_ID == "KOOT", ]
plot(kootenay)
saveRDS(kootenay,file ="data/old/kootenay.rds")

mount_revelstoke <- BCpolygon[BCpolygon$CLAB_ID == "REVE", ]
plot(mount_revelstoke)
saveRDS(mount_revelstoke,file ="data/old/mount_revelstoke.rds")

pacific_rim <- BCpolygon[BCpolygon$CLAB_ID == "PRIM", ]
plot(pacific_rim)
saveRDS(pacific_rim,file ="data/old/pacific_rim.rds")

glacier <- BCpolygon[BCpolygon$CLAB_ID == "GLAC", ]
plot(glacier)
saveRDS(glacier,file ="data/old/glacier.rds")

# MB

wapusk <- MBpolygon[MBpolygon$CLAB_ID == "WAPU", ]
plot(wapusk)
saveRDS(wapusk,file ="data/old/wapusk.rds")

# no polygon for prince of wales fort

# NB

fundy <- NBpolygon[NBpolygon$CLAB_ID == "FUND", ]
plot(fundy)
saveRDS(fundy,file ="data/old/fundy.rds")

kouchibouguac <- NBpolygon[NBpolygon$CLAB_ID == "KOUC", ]
plot(kouchibouguac)
saveRDS(kouchibouguac,file ="data/old/kouchibouguac.rds")

# NL

terra_nova <- NLpolygon[NLpolygon$CLAB_ID == "NOVA", ]
plot(terra_nova)
saveRDS(terra_nova,file ="data/old/terra_nova.rds")

# NS

kejimkujik <- NSpolygon[NSpolygon$CLAB_ID == "KEJI", ]
plot(kejimkujik)
saveRDS(kejimkujik,file ="data/old/kejimkijik.rds")

# no polygon on sable island

# NT

aulavik <- NTpolygon[NTpolygon$CLAB_ID == "AULA", ]
plot(aulavik)
saveRDS(aulavik,file ="data/old/aulavik.rds")

nahanni <- NTpolygon[NTpolygon$CLAB_ID == "NAHA", ]
plot(nahanni)
saveRDS(nahanni,file ="data/old/nahanni.rds")

# no polygon for grizzly bear

# no NU parks in my data

#ON

fathom_five <- ONpolygon[ONpolygon$CLAB_ID == "FIVE", ]
plot(fathom_five)
saveRDS(fathom_five,file ="data/old/fathom_five.rds")

point_pelee <- ONpolygon[ONpolygon$CLAB_ID == "PELE", ]
plot(point_pelee)
saveRDS(point_pelee,file ="data/old/point_pelee.rds")

georgian_bay_islands <- ONpolygon[ONpolygon$CLAB_ID == "GBIS", ]
plot(georgian_bay_islands)
saveRDS(georgian_bay_islands,file ="data/old/georgian_bay_islands.rds")

thousand_islands <- ONpolygon[ONpolygon$CLAB_ID == "THIS", ]
plot(thousand_islands)
saveRDS(thousand_islands,file ="data/old/thousand_islands.rds")

# no polygon for bruce peninsula

# PE

prince_edward_island <- PEpolygon[PEpolygon$CLAB_ID == "PEIS", ]
plot(prince_edward_island)
saveRDS(prince_edward_island,file ="data/old/prince_edward_island.rds")

# QC

forillon <- QCpolygon[QCpolygon$CLAB_ID == "FORI", ]
plot(forillon)
saveRDS(forillon,file ="data/old/forillon.rds")

# SK

prince_albert <- SKpolygon[SKpolygon$CLAB_ID == "PALB", ]
plot(prince_albert)
saveRDS(prince_albert,file ="data/old/prince_albert.rds")

# YT

ivvavik <- YTpolygon[YTpolygon$CLAB_ID == "IVVA", ]
plot(ivvavik)
saveRDS(ivvavik,file ="data/old/ivvavik.rds")

# 5 parks do not have polygons