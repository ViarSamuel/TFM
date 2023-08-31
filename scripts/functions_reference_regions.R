library(rgdal)
library(sp)
library(RColorBrewer)

climate4R <- list("loadeR", "transformeR", "downscaleR", "visualizeR", "climate4R.climdex","geoprocessoR")
lapply(climate4R, require, character.only = TRUE)

reference_regions_tas = function(obs, pred, region){
    load("/home/oem/Desktop/TFM/Correlacion_tas/TFM/referenceregions/IPCC-WGI-reference-regions-v4_R.rda", verbose = TRUE)
    refregions <- as(IPCC_WGI_reference_regions_v4, "SpatialPolygons")
    
    regions <- refregions[region]
    regions <- spTransform(regions, CRSobj = proj4string(refregions))
    
    obs_grid <- projectGrid(obs, original.CRS = proj4string(refregions))
    obs_grid.au <- overGrid(obs_grid, regions)
    obs_grid.au.ann <- aggregateGrid(obs_grid.au, aggr.y = list(FUN = "mean", na.rm = TRUE))

    pred_grid <- projectGrid(pred, original.CRS = proj4string(refregions))
    pred_grid.au <- overGrid(pred_grid, regions)
    pred_grid.au.ann <- aggregateGrid(pred_grid.au, aggr.y = list(FUN = "mean", na.rm = TRUE))
    
    temporalPlot("Observation" = obs_grid.au.ann,"Prediction" = pred_grid.au.ann,
             aggr.spatial = list(FUN = "mean", na.rm = TRUE))
    
}

reference_regions_pr = function(obs, pred, region){
    load("/home/oem/Desktop/TFM/Correlacion_tas/TFM/referenceregions/IPCC-WGI-reference-regions-v4_R.rda", verbose = TRUE)
    refregions <- as(IPCC_WGI_reference_regions_v4, "SpatialPolygons")
    
    regions <- refregions[region]
    regions <- spTransform(regions, CRSobj = proj4string(refregions))
    
    obs_grid <- projectGrid(obs, original.CRS = proj4string(refregions))
    obs_grid.au <- overGrid(obs_grid, regions)
    obs_grid.au.ann <- aggregateGrid(obs_grid.au, aggr.y = list(FUN = "sum", na.rm = TRUE))

    pred_grid <- projectGrid(pred, original.CRS = proj4string(refregions))
    pred_grid.au <- overGrid(pred_grid, regions)
    pred_grid.au.ann <- aggregateGrid(pred_grid.au, aggr.y = list(FUN = "sum", na.rm = TRUE))
    
    temporalPlot("Observation" = obs_grid.au.ann,"Prediction" = pred_grid.au.ann,
             aggr.spatial = list(FUN = "sum", na.rm = TRUE))
    
    
}
